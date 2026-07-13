# =============================================================================
# season_sim.R — Daily preseason projection job
#
# Runs once a day via GitHub Actions cron. Only produces output between
# July 1 and the first game of the new season; any other time it exits
# immediately without touching the repo.
#
# Output: data/season_sim/{season_year}.json
#   { season, generated_at, n_sims, results: [ {team_abbrev, proj_points,
#     playoff_pct, cup_pct}, ... ] }
#
# KNOWN SIMPLIFICATIONS (flagging these explicitly rather than hiding them):
#   1. Team strength is 100% roster-driven (recency-weighted player values,
#      no blending with any team's historical performance). Game outcomes
#      are simulated shot-by-shot (see "Shot-based team offense/defense
#      profile" below), adapted from HockeyStats.com's win-odds methodology,
#      scoped to data this app actually has. The weakest link: shots-against
#      is a blocks/hits-based proxy, not real shot-suppression data (that
#      needs on-ice tracking this app doesn't have).
#   2. Schedule template = last season's actual schedule, abbrev-mapped onto
#      this year's teams. Next season's real schedule usually isn't public
#      this early. Franchise relocations (e.g. ARI->UTA) are patched via
#      `abbrev_fix` below — extend that list if another team moves/renames.
#   3. Playoff seeding tiebreaker is points only (no reg-wins tiebreak) and
#      wild-card reseeding uses the common 1v8/2v7/3v6/4v5 approximation
#      within each conference, not the NHL's exact division-runner-up rule.
#   4. OT/shootout resolution is a simplified stand-in (short sudden-death
#      shot burst, then a quality-weighted coinflip) — not a full per-second
#      simulation like HockeyStats does, but keeps the same spirit.
# =============================================================================

suppressMessages({
  library(dplyr)
  library(httr)
  library(jsonlite)
})

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

GH_STATS        <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/stats"
GH_EDGE         <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/edge"
GH_GAME_RESULTS <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/game_results"
OUT_DIR         <- "data/season_sim"
N_SIMS          <- 10000L
# resid_sd removed — the shot-based simulation engine below doesn't need a
# game-margin standard deviation; variance comes naturally from simulating
# actual shots and goal probabilities per game.

current_year  <- as.integer(format(Sys.Date(), "%Y"))
current_month <- as.integer(format(Sys.Date(), "%m"))

# IMPORTANT: app.R's season_year labels a season by the year it ENDS, and
# only rolls forward to the next season in September — so during July/August
# app.R's season_year still points at the season that just concluded (which
# is correct for app.R's own "current season" display). This script, however,
# always needs to project the UPCOMING season during its whole July-1-to-
# season-start window, regardless of month. The season starting this fall
# always has calendar starting year = current_year, so its season_year
# (ending-year) label is always current_year + 1 — no month branching needed,
# since if the season had already started, the already_started check below
# would have already caused an exit.
season_year <- current_year + 1L

# ── Opt-in backtest mode ─────────────────────────────────────────────────────
# Purely additive: when unset, everything below behaves exactly as before.
# When set (e.g. NHLSIM_BACKTEST_SEASON=2026), targets a PAST, already-
# completed season instead of the upcoming one — this lets us calibrate the
# compression-correction amplification factor against REAL, KNOWN final
# standings (fetched further below) instead of guessing values and eyeballing
# against a third-party site, which is what we were doing before. Skips the
# "already started" gate entirely, since we're deliberately targeting a
# season that's long since finished.
BACKTEST_SEASON <- suppressWarnings(as.integer(Sys.getenv("NHLSIM_BACKTEST_SEASON", unset = NA)))
IS_BACKTEST <- !is.na(BACKTEST_SEASON)
if (IS_BACKTEST) {
  season_year <- BACKTEST_SEASON
  cat("── BACKTEST MODE: targeting completed season", season_year, "(real final standings will be fetched for calibration) ──\n")
}

# app.R requests results via load_season_sim_results(season_year) using ITS
# OWN season_year variable, which (per the above) can still equal current_year
# during July/August. We must save the output under THAT filename so app.R's
# lookup finds it, even though our projections are computed for `season_year`
# (current_year + 1) above. These two values only coincide once app.R itself
# rolls over in September.
app_season_year <- ifelse(current_month >= 9, current_year + 1L, current_year)

szn_folder    <- function(s) paste0(s - 1L, "-", substr(as.character(s), 3, 4))
szn_id        <- function(szn) paste0(szn - 1L, szn)

nhl_get <- function(url, timeout_s = 25) {
  resp <- tryCatch(GET(url, timeout(timeout_s)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE),
           error = function(e) NULL)
}
gh_read <- function(url, max_retries = 3) {
  for (attempt in seq_len(max_retries)) {
    con <- tryCatch(url(url), error = function(e) NULL)
    if (is.null(con)) return(NULL)
    result <- tryCatch({
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      read.csv(con, stringsAsFactors = FALSE)
    }, error = function(e) e)
    if (!inherits(result, "error")) return(result)
    # raw.githubusercontent.com rate-limits by IP over a short window —
    # this file had its own separate copy of gh_read with no retry logic
    # at all, so every 429 during a backfill/sim run failed instantly and
    # silently returned NULL, which downstream code just treated as
    # "missing data" (confirmed directly: this is what caused a goalie's
    # GSAx sample to drop to ~1/5 size in one run, dragging that team's
    # whole projection down for reasons that had nothing to do with their
    # actual roster).
    is_rate_limit <- grepl("429|too many requests", conditionMessage(result), ignore.case = TRUE)
    if (attempt < max_retries) Sys.sleep(if (is_rate_limit) 2^attempt else 0.5)
  }
  NULL
}

# ── 0. Bail out if we're outside the projection window ──────────────────────
if (IS_BACKTEST) {
  cat("Skipping the offseason-window gate — backtest mode targets a completed past season on purpose.\n")
} else {
cat("Checking whether the", season_year, "season has already started...\n")
standings_now <- nhl_get("https://api-web.nhle.com/v1/standings/now")
already_started <- FALSE
if (!is.null(standings_now) && !is.null(standings_now$standings) && length(standings_now$standings) > 0) {
  # IMPORTANT: /v1/standings/now keeps returning LAST season's final standings
  # (every team at 82 GP) all through the offseason, right up until the new
  # season actually starts. Checking gp>0 alone would wrongly treat that
  # leftover data as "season started" on July 1. Instead, compare the
  # seasonId embedded in the response against our target season — only
  # treat it as started if the API is actually reporting OUR season with
  # games played.
  live_season_id   <- tryCatch(as.character(standings_now$standings[[1]]$seasonId %||% NA), error = function(e) NA)
  target_season_id <- as.character(szn_id(season_year))
  if (!is.na(live_season_id) && live_season_id == target_season_id) {
    gps <- sapply(standings_now$standings, function(s) tryCatch(as.integer(s$gamesPlayed %||% 0L), error = function(e) 0L))
    already_started <- any(gps > 0, na.rm = TRUE)
  }
}
if (Sys.Date() < as.Date(paste0(current_year, "-07-01")) || already_started) {
  cat("Outside projection window (before July 1, or season already underway). Exiting.\n")
  quit(save = "no", status = 0)
}
}

# ── 1. Season window for player/team averaging ───────────────────────────────
historical_seasons <- seq(2022L, season_year - 1L)
recent3 <- tail(historical_seasons, 3L)
cat("Averaging player/team values over seasons:", paste(recent3, collapse = ", "), "\n")

# Weights: most-recent season weighted highest, QUADRATICALLY — for 3
# seasons, weights are 1,4,9 normalized (~7%/29%/64%), so the most recent
# season dominates rather than being diluted down to a 50/50ish blend with
# two older seasons. (Previously linear 1,2,3 -> 1/6,2/6,3/6, i.e. only 50%
# on the most recent season — this was found to meaningfully underproject
# teams that had just gotten good and mostly kept the same roster.)
# A player/team with fewer seasons just gets fewer weight terms.
# Used for TEAM-level ratings, where every season is ~82 games for everyone
# so there's no small-sample distortion to correct for.
recency_weights <- function(seasons_present) {
  n <- length(seasons_present)
  if (n == 0) return(numeric(0))
  w <- seq_len(n)^2
  w / sum(w)
}

# Player-level version: weights by recency (quadratically, same reasoning
# as above) AND games played that season. Without the GP factor, a tiny
# sample (e.g. a 6-game rookie cameo) gets nearly the same weight as a full
# breakout season the very next year, dragging down both the player's
# projected rate stats AND their projected games-played. Weighting by GP
# means a 6-game season naturally contributes almost nothing regardless of
# how recent it was, without needing an arbitrary cutoff that might also
# exclude legitimate partial (e.g. injury-shortened) seasons.
recency_weights_gp <- function(seasons_present, gp_present) {
  n <- length(seasons_present)
  if (n == 0) return(numeric(0))
  # Squared, not cubed — cubed (27:1 most-recent:oldest for 3 seasons)
  # turned out to be too aggressive. Squared is a 9:1 ratio (~64% of
  # total weight on the most recent season when GP is equal) — a real
  # recency tilt without being extreme, and what this whole session's
  # diagnostics were already validated against.
  recency_factor <- seq_len(n)^2
  raw_w <- recency_factor * pmax(coalesce(gp_present, 0), 1)
  raw_w / sum(raw_w)
}

# ── 2. Load & value skaters across last <=3 seasons ─────────────────────────
load_all_skater_vals <- function(seasons) {
  rows <- lapply(seasons, function(s) {
    folder <- szn_folder(s)
    sk <- gh_read(paste0(GH_STATS, "/", folder, "/skater_summary.csv"))
    es <- gh_read(paste0(GH_EDGE,  "/", folder, "/skater_summary.csv"))
    if (is.null(sk)) return(NULL)
    for (col in c("player_id", "playerId")) if (col %in% names(sk) && col != "player_id") sk$player_id <- sk[[col]]
    sk$player_id <- as.character(sk$player_id)
    sk$season <- s
    for (col in c("player_name", "skaterFullName")) if (col %in% names(sk) && col != "player_name") sk$player_name <- sk[[col]]
    for (col in c("position", "positionCode")) if (col %in% names(sk) && col != "position") sk$position <- sk[[col]]
    rn <- function(df, from, to) { if (from %in% names(df) && !to %in% names(df)) df[[to]] <- df[[from]]; df }
    sk <- rn(sk, "shooting_pct", "sh_pct"); sk <- rn(sk, "blocked_shots", "blocks"); sk <- rn(sk, "takeaways", "ta")
    sk <- rn(sk, "giveaways", "ga_p");      sk <- rn(sk, "points", "pts")
    # TOI: source CSV stores this as an MM:SS string in one of a few possible
    # column names (or already as raw seconds) — without this conversion,
    # toi_pg_sec stays NA for every player and the toi_min>=5 filter later
    # silently drops the entire roster.
    if (!"toi_pg_sec" %in% names(sk)) {
      toi_col <- intersect(c("toi_per_game", "time_on_ice_per_game", "avg_toi"), names(sk))
      if (length(toi_col) > 0) {
        sk$toi_pg_sec <- sapply(sk[[toi_col[1]]], function(x) {
          s2 <- as.character(x)
          if (grepl(":", s2, fixed = TRUE)) {
            tryCatch({ p <- strsplit(s2, ":")[[1]]; as.numeric(p[1]) * 60 + as.numeric(p[2]) },
                     error = function(e) NA_real_)
          } else {
            suppressWarnings(as.numeric(s2))
          }
        })
      }
    }
    if (!is.null(es)) {
      if (!"player_id" %in% names(es)) for (col in c("player_id", "playerId")) if (col %in% names(es)) es$player_id <- as.character(es[[col]])
      es$player_id <- as.character(es$player_id)
      ek <- intersect(c("player_id", "dist_per60_mi", "bursts_over22", "oz_pct"), names(es))
      if (length(ek) > 1) sk <- left_join(sk, es[, ek, drop = FALSE], by = "player_id")
    }
    for (col in c("hits","blocks","ta","ga_p","pp_goals","sh_goals","plus_minus","pim","sh_pct","pts","gp",
                  "goals","assists","shots","toi_pg_sec","dist_per60_mi","bursts_over22","oz_pct"))
      if (!col %in% names(sk)) sk[[col]] <- NA_real_
    sk
  })
  bind_rows(Filter(Negate(is.null), rows))
}

compute_player_vals <- function(ps) {
  if (is.null(ps) || nrow(ps) == 0) return(NULL)
  ps <- ps %>% mutate(
    gp = coalesce(gp, 0), goals = coalesce(goals, 0), assists = coalesce(assists, 0),
    shots = coalesce(shots, 0), hits = coalesce(hits, 0), blocks = coalesce(blocks, 0),
    ta = coalesce(ta, 0), ga_p = coalesce(ga_p, 0), pim = coalesce(pim, 0),
    plus_minus = coalesce(plus_minus, 0), pp_goals = coalesce(pp_goals, 0),
    sh_goals = coalesce(sh_goals, 0), toi_pg_sec = coalesce(toi_pg_sec, 0), toi_min = toi_pg_sec / 60
  ) %>% filter(gp >= 5, toi_min >= 5) %>% mutate(
    raw_val = goals*3.0 + assists*1.5 + shots*0.1 + hits*0.15 + blocks*0.2 + ta*0.3 - ga_p*0.2 +
      plus_minus*0.3 + pp_goals*1.0 + sh_goals*2.0 - pim*0.05 +
      coalesce(dist_per60_mi, 0)*0.05 + coalesce(bursts_over22, 0)*0.002 + (coalesce(oz_pct, 0) - 0.40)*5,
    val_per60 = ifelse(toi_pg_sec > 0, raw_val / (toi_min / 60) * (1 / gp), 0),
    min_share = toi_min / 20,
    weighted_val = val_per60 * min_share
  )
  med <- median(ps$weighted_val[ps$toi_min >= 15], na.rm = TRUE)
  if (!is.na(med) && med > 0) ps <- ps %>% mutate(weighted_val = weighted_val / med)
  ps
}

# Recency-weighted average across each player's available seasons (<=3 back).
# Players with only 1-2 seasons just average across however many they have.
project_player_vals <- function(vals_df) {
  if (is.null(vals_df) || nrow(vals_df) == 0) return(NULL)
  vals_df %>%
    group_by(player_id) %>%
    arrange(season, .by_group = TRUE) %>%
    summarise(
      player_name = dplyr::last(player_name),
      position    = dplyr::last(position),
      n_seasons   = dplyr::n(),
      proj_val    = { w <- recency_weights_gp(season, gp); sum(w * weighted_val) },
      .groups = "drop"
    )
}

cat("Computing skater values...\n")
skater_hist          <- load_all_skater_vals(recent3)
cat("  skater_hist rows:", nrow(skater_hist), "\n")
skater_vals_by_season <- compute_player_vals(skater_hist)
cat("  skater_vals_by_season rows:", if (is.null(skater_vals_by_season)) 0 else nrow(skater_vals_by_season), "\n")
proj_skater_vals      <- project_player_vals(skater_vals_by_season)
if (is.null(proj_skater_vals) || nrow(proj_skater_vals) == 0)
  stop("proj_skater_vals is empty — check skater_summary.csv column names/URLs for seasons: ", paste(recent3, collapse=", "))
cat("  proj_skater_vals rows:", nrow(proj_skater_vals), "\n")

# ── 3. Load & value goalies across last <=3 seasons ─────────────────────────
load_all_goalie_vals <- function(seasons) {
  rows <- lapply(seasons, function(s) {
    folder <- szn_folder(s)
    gl <- gh_read(paste0(GH_STATS, "/", folder, "/goalie_summary.csv"))
    if (is.null(gl)) return(NULL)
    gl$player_id <- as.character(gl$player_id)
    gl$season <- s
    rn <- function(df, from, to) { if (from %in% names(df) && !to %in% names(df)) df[[to]] <- df[[from]]; df }
    gl <- rn(gl, "ot_losses", "otl")
    for (col in c("wins","losses","otl","gaa","sv_pct","shutouts","saves","shots_against","toi_sec","gp","player_name"))
      if (!col %in% names(gl)) gl[[col]] <- NA
    gl
  })
  bind_rows(Filter(Negate(is.null), rows))
}

compute_goalie_vals <- function(gl) {
  if (is.null(gl) || nrow(gl) == 0) return(NULL)
  lg_sv <- median(gl$sv_pct, na.rm = TRUE); lg_gaa <- median(gl$gaa, na.rm = TRUE)
  out <- gl %>% filter(gp >= 5) %>% mutate(
    raw_val = (coalesce(sv_pct, lg_sv) - lg_sv)*500 + (lg_gaa - coalesce(gaa, lg_gaa))*5 +
      coalesce(shutouts, 0)*3 + coalesce(wins, 0)*0.5,
    weighted_val = raw_val
  )
  med <- median(out$weighted_val[out$gp >= 20], na.rm = TRUE)
  if (!is.na(med) && med != 0) out <- out %>% mutate(weighted_val = weighted_val / med)
  out
}

cat("Computing goalie values...\n")
goalie_hist           <- load_all_goalie_vals(recent3)
cat("  goalie_hist rows:", nrow(goalie_hist), "\n")
goalie_vals_by_season <- compute_goalie_vals(goalie_hist)
cat("  goalie_vals_by_season rows:", if (is.null(goalie_vals_by_season)) 0 else nrow(goalie_vals_by_season), "\n")
proj_goalie_vals      <- if (!is.null(goalie_vals_by_season))
  project_player_vals(goalie_vals_by_season %>% mutate(position = "G")) else NULL
if (is.null(proj_goalie_vals) || nrow(proj_goalie_vals) == 0)
  stop("proj_goalie_vals is empty — check goalie_summary.csv column names/URLs for seasons: ", paste(recent3, collapse=", "))
cat("  proj_goalie_vals rows:", nrow(proj_goalie_vals), "\n")

# ── 3b. Recency-weighted stat-line projections (skaters) ────────────────────
# Same recency weighting as proj_skater_vals, applied to actual box-score
# rates instead of the composite value score, so we can show real projected
# goals/assists/points/etc, not just an abstract "value".
NHL_GAMES <- 84L  # matches app.R's NHL_GAMES — the NHL expanded to an 84-game
                  # regular season starting with 2026-27, which is exactly
                  # the season this script projects.
FULL_SEASON_GP <- NHL_GAMES
GOALIE_GP_CAP  <- round(60L * NHL_GAMES / 82L)  # scaled proportionally from the old 82-game-season cap

project_skater_stats <- function(hist_df) {
  if (is.null(hist_df) || nrow(hist_df) == 0) return(NULL)
  rates <- hist_df %>%
    filter(coalesce(gp, 0) >= 5) %>%
    mutate(
      r_goals   = coalesce(goals, 0)   / gp,
      r_assists = coalesce(assists, 0) / gp,
      r_shots   = coalesce(shots, 0)   / gp,
      r_hits    = coalesce(hits, 0)    / gp,
      r_blocks  = coalesce(blocks, 0)  / gp,
      r_pim     = coalesce(pim, 0)     / gp,
      r_toi_min = coalesce(toi_pg_sec, 0) / 60   # toi_pg_sec is already a per-game average, just converting units
    )
  if (nrow(rates) == 0) return(NULL)
  rates %>%
    group_by(player_id) %>%
    arrange(season, .by_group = TRUE) %>%
    summarise(
      player_name = dplyr::last(player_name),
      position    = dplyr::last(position),
      n_seasons   = dplyr::n(),
      # Assumes a full healthy season rather than projecting games-played off
      # the player's own attendance history — that history can be skewed by
      # the exact same kind of small-sample year (injury, call-up timing)
      # that the rate itself needs GP-weighting to correct for. The RATE
      # below still uses recency_weights_gp — that's what actually protects
      # against a tiny sample distorting the estimate.
      proj_gp      = FULL_SEASON_GP,
      rate_goals   = { w <- recency_weights_gp(season, gp); sum(w * r_goals) },
      rate_assists = { w <- recency_weights_gp(season, gp); sum(w * r_assists) },
      rate_shots   = { w <- recency_weights_gp(season, gp); sum(w * r_shots) },
      rate_hits    = { w <- recency_weights_gp(season, gp); sum(w * r_hits) },
      rate_blocks  = { w <- recency_weights_gp(season, gp); sum(w * r_blocks) },
      rate_pim     = { w <- recency_weights_gp(season, gp); sum(w * r_pim) },
      rate_toi_min = { w <- recency_weights_gp(season, gp); sum(w * r_toi_min) },
      .groups = "drop"
    ) %>%
    select(player_id, player_name, position, n_seasons, proj_gp,
           rate_goals, rate_assists, rate_shots, rate_hits, rate_blocks, rate_pim, rate_toi_min)
}

cat("Computing skater stat-line projections...\n")
proj_skater_stats <- project_skater_stats(skater_hist)
cat("  proj_skater_stats rows:", if (is.null(proj_skater_stats)) 0 else nrow(proj_skater_stats), "\n")

# ── 3c. Recency-weighted stat-line projections (goalies) ────────────────────
project_goalie_stats <- function(hist_df) {
  if (is.null(hist_df) || nrow(hist_df) == 0) return(NULL)
  rates <- hist_df %>%
    filter(coalesce(gp, 0) >= 5) %>%
    mutate(
      r_wins      = coalesce(wins, 0)      / gp,
      r_losses    = coalesce(losses, 0)    / gp,
      r_otl       = coalesce(otl, 0)       / gp,
      r_shutouts  = coalesce(shutouts, 0)  / gp
    )
  if (nrow(rates) == 0) return(NULL)
  rates %>%
    group_by(player_id) %>%
    arrange(season, .by_group = TRUE) %>%
    summarise(
      player_name = dplyr::last(player_name),
      n_seasons   = dplyr::n(),
      # Goalies split starts with a backup, so cap projected GP lower than a
      # full skater season — 60 is a reasonable "clear #1 starter" ceiling.
      proj_gp     = { w <- recency_weights_gp(season, gp); min(GOALIE_GP_CAP, round(sum(w * gp))) },
      proj_gaa    = { w <- recency_weights_gp(season, gp); round(sum(w * coalesce(gaa, median(gaa, na.rm=TRUE))), 2) },
      proj_sv_pct = { w <- recency_weights_gp(season, gp); round(sum(w * coalesce(sv_pct, median(sv_pct, na.rm=TRUE))), 3) },
      rate_wins     = { w <- recency_weights_gp(season, gp); sum(w * r_wins) },
      rate_losses   = { w <- recency_weights_gp(season, gp); sum(w * r_losses) },
      rate_otl      = { w <- recency_weights_gp(season, gp); sum(w * r_otl) },
      rate_shutouts = { w <- recency_weights_gp(season, gp); sum(w * r_shutouts) },
      .groups = "drop"
    ) %>%
    mutate(
      proj_wins     = round(rate_wins * proj_gp, 1),
      proj_losses   = round(rate_losses * proj_gp, 1),
      proj_otl      = round(rate_otl * proj_gp, 1),
      proj_shutouts = round(rate_shutouts * proj_gp, 1)
    ) %>%
    select(player_id, player_name, n_seasons, proj_gp, proj_wins, proj_losses, proj_otl,
           proj_gaa, proj_sv_pct, proj_shutouts)
}

cat("Computing goalie stat-line projections...\n")
proj_goalie_stats <- project_goalie_stats(goalie_hist)
cat("  proj_goalie_stats rows:", if (is.null(proj_goalie_stats)) 0 else nrow(proj_goalie_stats), "\n")

# ── 4. Recency-weighted team net ratings ─────────────────────────────────────
load_team_stats <- function(seasons) {
  rows <- lapply(seasons, function(s) {
    folder <- szn_folder(s)
    ts <- gh_read(paste0(GH_STATS, "/", folder, "/team_summary.csv"))
    if (is.null(ts)) return(NULL)
    ts$season <- s
    rn <- function(df, from, to) { if (from %in% names(df) && !to %in% names(df)) df[[to]] <- df[[from]]; df }
    ts <- rn(ts, "gf_per_game", "gf_pg"); ts <- rn(ts, "ga_per_game", "ga_pg")
    if (!"team_abbrev" %in% names(ts)) ts$team_abbrev <- NA_character_
    ts
  })
  bind_rows(Filter(Negate(is.null), rows))
}

# NOTE: load_team_stats() above is intentionally UNUSED. Team strength is
# now derived entirely from current roster composition (see roster_strength()
# below) — no team's rating is influenced by last season's (or any prior
# season's) team-level performance in any way. TEAM_NET_RATING_SCALE is a
# fixed constant (not derived from history) used only to convert roster
# z-scores into "goals/game" units for net_lookup, which is now a general
# roster-strength diagnostic — actual game simulation uses the shot-based
# team offense/defense profile built further below instead.
TEAM_NET_RATING_SCALE <- 0.5

# ── 5. Fetch upcoming rosters & compute roster-quality adjustment ──────────
fetch_team_abbrevs <- function() {
  if (is.null(standings_now) || is.null(standings_now$standings)) return(character(0))
  sapply(standings_now$standings, function(s) if (is.list(s$teamAbbrev)) s$teamAbbrev$default %||% NA_character_ else as.character(s$teamAbbrev %||% NA))
}
team_abbrevs <- unique(na.omit(fetch_team_abbrevs()))

# Diagnostic-only helper: returns status code + a snippet of the raw body so
# we can see WHY the roster endpoint failed (wrong URL, blocked, rate
# limited, unexpected shape) instead of just getting NULL back.
nhl_get_diag <- function(url, timeout_s = 25) {
  resp <- tryCatch(GET(url, timeout(timeout_s)), error = function(e) NULL)
  if (is.null(resp)) return(list(status = NA_integer_, body = "<request failed / no response>"))
  body <- tryCatch(content(resp, "text", encoding = "UTF-8"), error = function(e) "<unreadable body>")
  list(status = status_code(resp), body = substr(body, 1, 300))
}

# Print one full diagnostic for the FIRST team, on all three URL variants,
# before the bulk loop — this tells us exactly what's going wrong.
if (length(team_abbrevs) > 0) {
  probe_team <- team_abbrevs[1]
  cat("── Roster endpoint diagnostic for", probe_team, "──\n")
  for (slug in c("current", szn_id(season_year), szn_id(season_year - 1L))) {
    url <- paste0("https://api-web.nhle.com/v1/roster/", probe_team, "/", slug)
    d <- nhl_get_diag(url)
    cat("  URL:", url, "\n")
    cat("  Status:", d$status, "| Body snippet:", d$body, "\n")
  }
  cat("──────────────────────────────────────────\n")
}

# season_slug can be "current" (live roster, reflects real-time trades and
# free-agent signings already completed — not tied to a specific season ID)
# or a concatenated season ID like "20262027" (szn_id() output).
fetch_roster <- function(abbrev, season_slug) {
  raw <- nhl_get(paste0("https://api-web.nhle.com/v1/roster/", abbrev, "/", season_slug))
  if (is.null(raw)) return(NULL)
  groups <- c("forwards", "defensemen", "goalies")
  out <- bind_rows(lapply(groups, function(g) {
    plist <- raw[[g]] %||% list()
    bind_rows(lapply(plist, function(p) {
      first <- tryCatch(p$firstName$default %||% "", error = function(e) "")
      last  <- tryCatch(p$lastName$default  %||% "", error = function(e) "")
      bdate <- tryCatch(as.character(p$birthDate %||% NA), error = function(e) NA_character_)
      data.frame(team_abbrev = abbrev, player_id = as.character(p$id %||% NA),
                 position_group = g, roster_name = trimws(paste(first, last)),
                 birth_date = bdate, stringsAsFactors = FALSE)
    }))
  }))
  if (nrow(out) == 0) return(NULL)
  out
}

cat("Fetching rosters for", length(team_abbrevs), "teams (target season", season_year, ")...\n")
roster_method_used <- character(0)
all_rosters <- bind_rows(lapply(team_abbrevs, function(a) {
  r <- NULL; method <- "none"
  if (!IS_BACKTEST) {
    # Only try "current" for real, live projections — using it during a
    # backtest would leak today's actual roster (including trades/drafts
    # that hadn't happened yet) into what should be a fair test of only
    # information available before that past season started.
    r <- tryCatch(fetch_roster(a, "current"), error = function(e) NULL)
    method <- "current"
  }
  if (is.null(r)) {
    r <- tryCatch(fetch_roster(a, szn_id(season_year)), error = function(e) NULL)
    method <- "target_season"
  }
  if (is.null(r)) {
    r <- tryCatch(fetch_roster(a, szn_id(season_year - 1L)), error = function(e) NULL)
    method <- "fallback_season"
  }
  if (is.null(r)) method <- "none"
  roster_method_used[[a]] <<- method
  Sys.sleep(0.1)
  r
}))
cat("  Method breakdown:", paste(names(table(unlist(roster_method_used))), table(unlist(roster_method_used)), sep="=", collapse=", "), "\n")
if (is.null(all_rosters) || nrow(all_rosters) == 0 || !"player_id" %in% names(all_rosters))
  stop("No roster data could be fetched for season ", season_year,
       " or fallback season ", season_year - 1L, " for any team. Aborting.")
cat("  all_rosters rows:", nrow(all_rosters), " (teams:", length(unique(all_rosters$team_abbrev)), ")\n")

# ── Age/experience-based growth adjustment ───────────────────────────────────
# DECLINE side stays age-based — aging affects everyone roughly the same way
# regardless of career path.
# GROWTH side is based on actual years of NHL experience instead of age — a
# 23-year-old with 5 NHL seasons already played is much closer to "who they
# are" as a player than a 23-year-old with only 2 seasons in, even though
# they're the same age. Age alone can't distinguish those two cases; real
# experience can.
# Neither curve is fit to this specific dataset — both are standard hockey-
# analytics heuristics. Tune the breakpoints as needed.
SEASON_START_DATE <- as.Date(paste0(season_year - 1L, "-10-01"))  # approx start of the projected season

all_rosters <- all_rosters %>%
  mutate(
    birth_date_parsed    = suppressWarnings(as.Date(birth_date)),
    age_at_season_start  = as.numeric(difftime(SEASON_START_DATE, birth_date_parsed, units = "days")) / 365.25
  )
cat("  Players with usable birth date:", sum(!is.na(all_rosters$age_at_season_start)), "of", nrow(all_rosters), "\n")

# NHL experience fetch is SKIPPED — it was ~350+ individual API calls per
# run, and the only thing that ever consumed years_in_nhl was
# age_growth_multiplier(), which is currently disabled (see below). No
# point paying that cost for data nothing uses. To bring it back: uncomment
# this block and re-enable the age_mult lines further down.
# fetch_player_experience <- function(player_id) {
#   raw <- nhl_get(paste0("https://api-web.nhle.com/v1/player/", player_id, "/landing"), 15)
#   if (is.null(raw) || is.null(raw$seasonTotals)) return(NA_integer_)
#   nhl_regular <- Filter(function(x) {
#     league <- tryCatch(x$leagueAbbrev %||% NA_character_, error = function(e) NA_character_)
#     gtype  <- tryCatch(as.integer(x$gameTypeId %||% NA), error = function(e) NA_integer_)
#     !is.na(league) && league == "NHL" && !is.na(gtype) && gtype == 2
#   }, raw$seasonTotals)
#   length(nhl_regular)
# }
# young_skater_ids <- all_rosters %>%
#   filter(coalesce(position_group, "") != "goalies",
#          !is.na(age_at_season_start), age_at_season_start < 29) %>%
#   pull(player_id) %>% unique()
# cat("Fetching NHL experience for", length(young_skater_ids), "skaters under 29...\n")
# experience_lookup <- setNames(rep(NA_integer_, length(young_skater_ids)), young_skater_ids)
# for (pid in young_skater_ids) {
#   experience_lookup[pid] <- tryCatch(fetch_player_experience(pid), error = function(e) NA_integer_)
#   Sys.sleep(0.05)
# }
# cat("  Experience data fetched for", sum(!is.na(experience_lookup)), "of", length(young_skater_ids), "\n")
all_rosters <- all_rosters %>%
  mutate(years_in_nhl = NA_integer_)

age_growth_multiplier <- function(age, years_in_nhl) {
  growth_mult <- dplyr::case_when(
    is.na(years_in_nhl) ~ 1.00,
    years_in_nhl <= 1    ~ 1.15,
    years_in_nhl == 2    ~ 1.08,
    years_in_nhl == 3    ~ 1.04,
    years_in_nhl == 4    ~ 1.01,
    TRUE                 ~ 1.00   # 5+ years — treat as an established, known quantity
  )
  decline_mult <- dplyr::case_when(
    is.na(age) ~ 1.00,
    age < 29   ~ 1.00,
    age < 31   ~ 0.98,
    age < 33   ~ 0.95,
    age < 36   ~ 0.90,
    TRUE       ~ 0.85
  )
  # Under 29: growth side (driven by real experience). 29+: decline side (age-driven).
  ifelse(coalesce(age, 100) < 29, growth_mult, decline_mult)
}

roster_strength <- function(rosters, proj_skaters, proj_goalies) {
  combined <- bind_rows(
    if (!is.null(proj_skaters)) proj_skaters %>% select(player_id, proj_val) else NULL,
    if (!is.null(proj_goalies)) proj_goalies %>% select(player_id, proj_val) else NULL
  )
  rosters %>%
    mutate(is_goalie = coalesce(position_group, "") == "goalies") %>%
    left_join(combined, by = "player_id") %>%
    mutate(proj_val = coalesce(proj_val, 0)) %>%
    group_by(team_abbrev) %>%
    summarise(
      skater_strength = { v <- sort(proj_val[!is_goalie], decreasing = TRUE); sum(v[seq_len(min(18, length(v)))], na.rm = TRUE) },
      goalie_strength = { v <- sort(proj_val[is_goalie],  decreasing = TRUE); sum(v[seq_len(min(2, length(v)))], na.rm = TRUE) },
      .groups = "drop"
    )
}
age_lookup <- all_rosters %>% distinct(player_id, .keep_all = TRUE) %>% select(player_id, age_at_season_start, years_in_nhl)
# Age growth/decline curve is DISABLED for now (per request) — proj_val is
# left as the pure recency-weighted historical value with no age
# adjustment. age_growth_multiplier() is still defined above and age_lookup
# still joined here, so re-enabling this is a one-line change if wanted later:
#   mutate(proj_val = proj_val * age_growth_multiplier(age_at_season_start, years_in_nhl))
proj_skater_vals <- proj_skater_vals %>%
  left_join(age_lookup, by = "player_id")

roster_str <- roster_strength(all_rosters, proj_skater_vals, proj_goalie_vals)

lg_skater <- mean(roster_str$skater_strength, na.rm = TRUE)
lg_goalie <- mean(roster_str$goalie_strength, na.rm = TRUE)
sd_skater <- sd(roster_str$skater_strength, na.rm = TRUE); if (is.na(sd_skater) || sd_skater == 0) sd_skater <- 1
sd_goalie <- sd(roster_str$goalie_strength, na.rm = TRUE); if (is.na(sd_goalie) || sd_goalie == 0) sd_goalie <- 1

# Team strength is 100% a function of the current roster — no blending with
# any prior season's team-level performance.
team_proj <- roster_str %>%
  filter(!is.na(team_abbrev)) %>%
  mutate(
    skater_z = coalesce((skater_strength - lg_skater) / sd_skater, 0),
    goalie_z = coalesce((goalie_strength - lg_goalie) / sd_goalie, 0),
    # Composite roster z-score (equal weight skaters/goalies), rescaled by a
    # fixed constant (TEAM_NET_RATING_SCALE) into "goals/game" units for
    # net_lookup (a general roster-strength diagnostic — see note above).
    # Fixed, not derived from any team's or the league's past performance.
    roster_z_raw = skater_z * 0.5 + goalie_z * 0.5
  )

sd_roster_comp <- sd(team_proj$roster_z_raw, na.rm = TRUE); if (is.na(sd_roster_comp) || sd_roster_comp == 0) sd_roster_comp <- 1

team_proj <- team_proj %>%
  mutate(final_net = (roster_z_raw / sd_roster_comp) * TEAM_NET_RATING_SCALE)
net_lookup <- setNames(team_proj$final_net, team_proj$team_abbrev)
cat("  Team net rating range (100% roster-driven):", round(min(net_lookup, na.rm=TRUE), 3),
    "to", round(max(net_lookup, na.rm=TRUE), 3), "\n")
# NOTE: net_lookup is a general roster-strength diagnostic and is still used
# below purely as "the list of 32 known team abbrevs" — actual game outcomes
# are now generated by the shot-based engine below, not by net_lookup values.

# ── Player/goalie stat-line output ───────────────────────────────────────────
# Joins each team's actual roster (all_rosters, from the "current" endpoint —
# reflects real trades/signings) against the recency-weighted projections.
# Players with NO usable history (true rookies, or anyone the join can't
# match) get has_history=FALSE and NA stats rather than a fabricated zero —
# silently zeroing them would understate their real (unknown) contribution.
# position_group comes straight from the roster API, so classification
# doesn't depend on having historical stats (rookies wouldn't have any).
# Built here (earlier than the final output-writing step) because the
# shot-based simulation engine below needs these per-player rate stats to
# build each team's offense/defense profile.
skater_roster <- all_rosters %>% filter(coalesce(position_group, "") != "goalies")
skater_output <- skater_roster %>%
  left_join(proj_skater_stats, by = "player_id") %>%
  mutate(
    has_history = !is.na(rate_goals),
    player_name = coalesce(player_name, roster_name),
    # Age growth/decline curve DISABLED for now (per request) — age_mult is
    # a fixed 1.0 no-op. To re-enable, change this back to
    # age_growth_multiplier(age_at_season_start, years_in_nhl).
    age_mult     = 1.0,
    proj_goals   = round(rate_goals   * proj_gp * age_mult, 1),
    proj_assists = round(rate_assists * proj_gp * age_mult, 1),
    proj_points  = round((rate_goals + rate_assists) * proj_gp * age_mult, 1),
    proj_shots   = round(rate_shots   * proj_gp * age_mult, 1),
    proj_hits    = round(rate_hits    * proj_gp, 1),
    proj_blocks  = round(rate_blocks  * proj_gp, 1),
    proj_pim     = round(rate_pim     * proj_gp, 1)
  ) %>%
  arrange(team_abbrev, desc(coalesce(proj_points, -1)))

goalie_roster <- all_rosters %>% filter(coalesce(position_group, "") == "goalies")
goalie_output <- goalie_roster %>%
  left_join(proj_goalie_stats, by = "player_id") %>%
  mutate(
    has_history = !is.na(proj_wins),
    player_name = coalesce(player_name, roster_name)
  ) %>%
  arrange(team_abbrev, desc(coalesce(proj_wins, -1)))

cat("  skater_output rows:", nrow(skater_output), "(", sum(skater_output$has_history), "with history )\n")
cat("  goalie_output rows:", nrow(goalie_output), "(", sum(goalie_output$has_history), "with history )\n")

# ── Per-player on-ice defense (mirrors how offense is already built) ────────
# Offense is roster-driven: each player's OWN recency-weighted history,
# aggregated by whoever is on the CURRENT roster. Defense now works exactly
# the same way, instead of the earlier approach (a team-level historical
# blend independent of personnel — which didn't reflect roster turnover at
# all, a real gap when a team's D-corps or defensive forwards have changed).
# Uses skater_onice.csv (same play-by-play reconstruction the live app
# uses) for real on-ice Corsi-against/goals-against per player.
load_all_skater_onice <- function(seasons) {
  rows <- lapply(seasons, function(s) {
    d <- gh_read(paste0("https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/onice/", s, "/skater_onice.csv"))
    if (is.null(d) || nrow(d) == 0) return(NULL)
    d$player_id <- as.character(d$player_id)
    d$season <- s
    d
  })
  bind_rows(Filter(Negate(is.null), rows))
}

# ── RAPM (Regularized Adjusted Plus-Minus) — loaded the same way WOWY is,
# per season, then recency-weighted across recent3 using the identical
# weighted_avg_skip_na() function defined below. RAPM is a genuinely
# different (and more rigorous) estimate of the same underlying skill WOWY
# approximates — see fit_rapm.R for the full methodology (two separate
# ridge regressions per strength state, one isolating offense, one
# isolating defense, both cross-validated). Loaded here as a NEW,
# preferred option ahead of WOWY in the existing fallback chain — not a
# replacement of WOWY's own columns, so if RAPM data is missing for a
# player/season (e.g. before this pipeline existed), the existing
# WOWY/box-score fallback chain still works exactly as it did before.
load_all_rapm <- function(seasons) {
  rows <- lapply(seasons, function(s) {
    d <- gh_read(paste0("https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/rapm/", s, "/rapm.csv"))
    if (is.null(d) || nrow(d) == 0) return(NULL)
    d$player_id <- as.character(d$player_id)
    d$season <- s
    d
  })
  bind_rows(Filter(Negate(is.null), rows))
}
load_all_rapm_pppk <- function(seasons) {
  rows <- lapply(seasons, function(s) {
    d <- gh_read(paste0("https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/rapm/", s, "/rapm_pppk.csv"))
    if (is.null(d) || nrow(d) == 0) return(NULL)
    d$player_id <- as.character(d$player_id)
    d$season <- s
    d
  })
  bind_rows(Filter(Negate(is.null), rows))
}

# ── WOWY (With Or Without You) — our own approximation of RAPM's context
# adjustment, without needing a full ridge regression. Real RAPM controls
# for every teammate/opponent simultaneously; WOWY is cruder — it just
# compares "team's rate while this player is on the ice" against "team's
# rate during the rest of their games/shifts, with this player NOT on the
# ice" for the SAME team, SAME season. This isolates individual impact far
# better than a flat box-score sum (which is all we had before), but it's
# genuinely noisier for players who rarely sit — if someone plays nearly
# every 5v5 shift, there's very little "without them" sample to compare
# against, and the comparison becomes unreliable. MIN_WOWY_TOI_FRAC guards
# against exactly that: if less than this fraction of the team's TOI at a
# given strength happened without the player, WOWY for that component
# comes back NA and callers should fall back to the plain rate instead.
MIN_WOWY_TOI_FRAC <- 0.15

compute_wowy_metrics <- function(skater_df, team_df) {
  if (!"xg_for" %in% names(team_df)) team_df$xg_for <- NA_real_        # guard for pre-xG-model team_onice.csv
  if (!"xg_against" %in% names(team_df)) team_df$xg_against <- NA_real_
  if (!"xg_for_per60_5v5" %in% names(skater_df)) skater_df$xg_for_per60_5v5 <- NA_real_        # guard for pre-xG-model skater_onice.csv
  if (!"xg_against_per60_5v5" %in% names(skater_df)) skater_df$xg_against_per60_5v5 <- NA_real_
  if (!"xg_for_5v5" %in% names(skater_df)) skater_df$xg_for_5v5 <- NA_real_
  if (!"xg_against_5v5" %in% names(skater_df)) skater_df$xg_against_5v5 <- NA_real_

  team_ref <- team_df %>%
    select(team_abbrev, team_gf_5v5 = gf_5v5, team_ga_5v5 = ga_5v5, team_toi_5v5 = toi_5v5_sec,
           team_pp_gf = pp_goals, team_toi_pp = toi_pp_sec,
           team_pk_ga = pk_goals_against, team_toi_pk = toi_pk_sec,
           team_xg_for = xg_for, team_xg_against = xg_against)
  df <- skater_df %>% left_join(team_ref, by = "team_abbrev")

  df %>% mutate(
    # EV Offense: player's own on-ice GF/60 minus the team's GF/60 during
    # the ice time this player was NOT out there.
    ev_without_toi   = team_toi_5v5 - coalesce(toi_5v5_sec, 0),
    ev_toi_frac_wo   = ifelse(team_toi_5v5 > 0, ev_without_toi / team_toi_5v5, 0),
    team_gf_wo_per60 = ifelse(ev_without_toi > 0, (team_gf_5v5 - coalesce(gf_5v5, 0)) / (ev_without_toi / 3600), NA_real_),
    ev_offense_wowy  = ifelse(ev_toi_frac_wo >= MIN_WOWY_TOI_FRAC & !is.na(gf_per60_5v5),
                               gf_per60_5v5 - team_gf_wo_per60, NA_real_),
    # EV Defense: team's GA/60 without this player minus their own on-ice
    # GA/60 — positive means they suppress goals better than the team does
    # without them.
    team_ga_wo_per60 = ifelse(ev_without_toi > 0, (team_ga_5v5 - coalesce(ga_5v5, 0)) / (ev_without_toi / 3600), NA_real_),
    ev_defense_wowy  = ifelse(ev_toi_frac_wo >= MIN_WOWY_TOI_FRAC & !is.na(ga_per60_5v5),
                               team_ga_wo_per60 - ga_per60_5v5, NA_real_),
    # xG-based EV Offense/Defense — same WOWY logic, same 5v5 TOI scope,
    # but using xG instead of actual goals. Less noisy: isolates shot-
    # generation/shot-suppression skill from shooting/goaltending luck,
    # which matters more for a FORWARD-LOOKING projection than an exact
    # accounting of what literally happened. Falls back to NA (and the
    # caller falls back to goals-based WOWY) for seasons before the xG
    # model existed.
    team_xgf_wo_per60 = ifelse(ev_without_toi > 0, (team_xg_for - coalesce(xg_for_5v5, 0)) / (ev_without_toi / 3600), NA_real_),
    ev_offense_xgwowy = ifelse(ev_toi_frac_wo >= MIN_WOWY_TOI_FRAC & !is.na(xg_for_per60_5v5),
                                xg_for_per60_5v5 - team_xgf_wo_per60, NA_real_),
    team_xga_wo_per60 = ifelse(ev_without_toi > 0, (team_xg_against - coalesce(xg_against_5v5, 0)) / (ev_without_toi / 3600), NA_real_),
    ev_defense_xgwowy = ifelse(ev_toi_frac_wo >= MIN_WOWY_TOI_FRAC & !is.na(xg_against_per60_5v5),
                                team_xga_wo_per60 - xg_against_per60_5v5, NA_real_),
    # PP Offense: same idea, restricted to power-play ice time.
    pp_without_toi   = team_toi_pp - coalesce(toi_pp_sec, 0),
    pp_toi_frac_wo   = ifelse(team_toi_pp > 0, pp_without_toi / team_toi_pp, 0),
    team_ppgf_wo_per60 = ifelse(pp_without_toi > 0, (team_pp_gf - coalesce(pp_gf_onice, 0)) / (pp_without_toi / 3600), NA_real_),
    pp_offense_wowy  = ifelse(pp_toi_frac_wo >= MIN_WOWY_TOI_FRAC & !is.na(pp_gf_onice_per60),
                               pp_gf_onice_per60 - team_ppgf_wo_per60, NA_real_),
    # PK Defense: same idea, restricted to penalty-kill ice time.
    pk_without_toi   = team_toi_pk - coalesce(toi_pk_sec, 0),
    pk_toi_frac_wo   = ifelse(team_toi_pk > 0, pk_without_toi / team_toi_pk, 0),
    team_pkga_wo_per60 = ifelse(pk_without_toi > 0, (team_pk_ga - coalesce(pk_ga_onice, 0)) / (pk_without_toi / 3600), NA_real_),
    pk_defense_wowy  = ifelse(pk_toi_frac_wo >= MIN_WOWY_TOI_FRAC & !is.na(pk_ga_onice_per60),
                               team_pkga_wo_per60 - pk_ga_onice_per60, NA_real_)
  )
}

# Recency-weighted average that skips NA entries (renormalizing weights
# among only the valid seasons) instead of either propagating NA outward
# or silently treating a missing season as 0 — a season where WOWY came
# back NA (too little "without them" ice time that year) should just drop
# out of the average, not pull it toward zero.
weighted_avg_skip_na <- function(vals, season, gp) {
  keep <- !is.na(vals)
  if (!any(keep)) return(NA_real_)
  w <- recency_weights_gp(season[keep], gp[keep])
  sum(w * vals[keep])
}

project_skater_onice <- function(hist_df) {
  if (is.null(hist_df) || nrow(hist_df) == 0) return(NULL)
  if (!"pk_shots_against" %in% names(hist_df)) hist_df$pk_shots_against <- NA_real_  # guard for pre-update on-ice CSVs
  if (!"pp_shots" %in% names(hist_df)) hist_df$pp_shots <- NA_real_  # guard for pre-update on-ice CSVs
  if (!"xg_for_5v5" %in% names(hist_df)) hist_df$xg_for_5v5 <- NA_real_        # guard for pre-xG-model on-ice CSVs
  if (!"xg_against_5v5" %in% names(hist_df)) hist_df$xg_against_5v5 <- NA_real_
  for (wc in c("ev_offense_wowy", "ev_defense_wowy", "pp_offense_wowy", "pk_defense_wowy", "ev_offense_xgwowy", "ev_defense_xgwowy")) {
    if (!wc %in% names(hist_df)) hist_df[[wc]] <- NA_real_  # guard for pre-update on-ice CSVs or seasons where WOWY couldn't be computed
  }
  rates <- hist_df %>%
    filter(coalesce(gp_onice, 0) > 0) %>%
    mutate(
      ca_pg = coalesce(ca_5v5, 0) / gp_onice,   # on-ice Corsi-against per game while this player is on the ice
      cf_pg = coalesce(cf_5v5, 0) / gp_onice,   # on-ice Corsi-for per game — used only to self-calibrate the SOG/Corsi ratio below
      pk_sa_pg = coalesce(pk_shots_against, 0) / gp_onice,   # real individual on-ice PK shots-against per game (now tracked at the source)
      pp_sf_pg = coalesce(pp_shots, 0) / gp_onice,   # real individual on-ice PP shots-for per game — same purpose as pk_sa_pg above, but for offense: needed to properly scope-match the SOG/Corsi ratio's denominator (roster-aggregated, not team full-game totals)
      # Same idea as ca_pg/cf_pg, but expected-goals instead of Corsi — this
      # is the roster-weighted (not team-level) xG aggregate: built from
      # each CURRENT roster player's own on-ice xG history, so a traded
      # player's real impact carries over onto their new team correctly,
      # the same way onice_ca_pg/onice_cf_pg already do for Corsi. A
      # team-level xg_for/xg_against lookup from last season would be
      # stuck to last year's roster and blind to any offseason move.
      xgf_pg = coalesce(xg_for_5v5, 0) / gp_onice,
      xga_pg = coalesce(xg_against_5v5, 0) / gp_onice
    )
  if (nrow(rates) == 0) return(NULL)
  rates %>%
    group_by(player_id) %>%
    arrange(season, .by_group = TRUE) %>%
    summarise(
      onice_ca_pg = { w <- recency_weights_gp(season, gp_onice); sum(w * ca_pg) },
      onice_cf_pg = { w <- recency_weights_gp(season, gp_onice); sum(w * cf_pg) },
      onice_pk_sa_pg = { w <- recency_weights_gp(season, gp_onice); sum(w * pk_sa_pg) },
      onice_pp_sf_pg = { w <- recency_weights_gp(season, gp_onice); sum(w * pp_sf_pg) },
      onice_xgf_pg = { w <- recency_weights_gp(season, gp_onice); sum(w * xgf_pg) },
      onice_xga_pg = { w <- recency_weights_gp(season, gp_onice); sum(w * xga_pg) },
      ev_offense_wowy_3yr = weighted_avg_skip_na(ev_offense_wowy, season, gp_onice),
      ev_defense_wowy_3yr = weighted_avg_skip_na(ev_defense_wowy, season, gp_onice),
      pp_offense_wowy_3yr = weighted_avg_skip_na(pp_offense_wowy, season, gp_onice),
      pk_defense_wowy_3yr = weighted_avg_skip_na(pk_defense_wowy, season, gp_onice),
      # xG-based versions — preferred over goals-based when available (see
      # team_offense below), since they isolate shot-generation/suppression
      # skill from shooting/goaltending luck.
      ev_offense_xgwowy_3yr = weighted_avg_skip_na(ev_offense_xgwowy, season, gp_onice),
      ev_defense_xgwowy_3yr = weighted_avg_skip_na(ev_defense_xgwowy, season, gp_onice),
      .groups = "drop"
    )
}

# Recency-weighted RAPM projection — same pattern as project_skater_onice
# above, just for RAPM's own columns instead of WOWY's. Uses TOI (already
# present in rapm.csv from fit_rapm.R) as the GP-equivalent weighting
# input, since RAPM output is per-season (not per-game) and doesn't have
# its own gp_onice column — toi_5v5_sec serves the identical purpose
# recency_weights_gp() needs (a sample-size proxy per season).
project_rapm <- function(hist_df) {
  if (is.null(hist_df) || nrow(hist_df) == 0) return(NULL)
  if (!"off_rapm_per60" %in% names(hist_df)) return(NULL)
  hist_df %>%
    filter(!is.na(toi_5v5_sec), toi_5v5_sec > 0) %>%
    group_by(player_id) %>%
    arrange(season, .by_group = TRUE) %>%
    summarise(
      off_rapm_3yr = weighted_avg_skip_na(off_rapm_per60, season, toi_5v5_sec),
      def_rapm_3yr = weighted_avg_skip_na(def_rapm_per60, season, toi_5v5_sec),
      .groups = "drop"
    )
}
project_rapm_pppk <- function(hist_df) {
  if (is.null(hist_df) || nrow(hist_df) == 0) return(NULL)
  if (!"pp_rapm_per60" %in% names(hist_df)) return(NULL)
  # Uses own_pp_shots_per60/own_pk_sa_per60 (already in rapm_pppk.csv) as
  # a rough activity proxy for recency weighting, since PP/PK doesn't have
  # its own dedicated TOI column joined in at this stage — a genuine
  # approximation, acceptable here since this feeds a diagnostic
  # comparison, not the production simulation (see the note below where
  # this gets used).
  hist_df %>%
    mutate(pp_weight_proxy = coalesce(own_pp_shots_per60, 1), pk_weight_proxy = coalesce(own_pk_sa_per60, 1)) %>%
    group_by(player_id) %>%
    arrange(season, .by_group = TRUE) %>%
    summarise(
      pp_rapm_3yr = weighted_avg_skip_na(pp_rapm_per60, season, pp_weight_proxy),
      pk_rapm_3yr = weighted_avg_skip_na(pk_rapm_per60, season, pk_weight_proxy),
      .groups = "drop"
    )
}

abbrev_fix <- c("ARI" = "UTA", "PHX" = "UTA")  # extend if another franchise relocates/renames
cat("Computing skater on-ice defense projections...\n")
skater_onice_hist <- load_all_skater_onice(recent3)

team_onice_by_season <- list()  # always defined, even if the condition below is false — the SOG-to-Corsi ratio fix later needs this to exist regardless
# WOWY needs each season's team baseline computed SEPARATELY (not blended
# across years first) — comparing a player's 2024 on-ice rate against a
# blended 2024-2026 team average would mix seasons incorrectly. Fetch
# team-level data per season, apply compute_wowy_metrics() once per
# season, THEN recency-weight the resulting WOWY values the same way
# everything else in this script gets weighted.
if (!is.null(skater_onice_hist) && nrow(skater_onice_hist) > 0 && "team_abbrev" %in% names(skater_onice_hist)) {
  team_onice_by_season <- lapply(recent3, function(s) {
    t <- gh_read(paste0("https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/onice/", s, "/team_onice.csv"))
    if (is.null(t) || nrow(t) == 0) return(NULL)
    if (nrow(t) > 0) t$team_abbrev <- ifelse(t$team_abbrev %in% names(abbrev_fix), abbrev_fix[t$team_abbrev], t$team_abbrev)
    t$season <- s
    t
  })
  team_onice_by_season <- Filter(Negate(is.null), team_onice_by_season)
  if (length(team_onice_by_season) > 0) {
    wowy_pieces <- lapply(team_onice_by_season, function(t_szn) {
      s <- t_szn$season[1]
      sk_szn <- skater_onice_hist[skater_onice_hist$season == s & !is.na(skater_onice_hist$team_abbrev), ]
      if (nrow(sk_szn) == 0) return(NULL)
      tryCatch(compute_wowy_metrics(sk_szn, t_szn), error = function(e) NULL)
    })
    wowy_pieces <- Filter(Negate(is.null), wowy_pieces)
    if (length(wowy_pieces) > 0) {
      wowy_all <- bind_rows(wowy_pieces)
      cat("  WOWY computed for", length(unique(wowy_all$player_id)), "player-seasons across", length(wowy_pieces), "seasons.\n")
      skater_onice_hist <- skater_onice_hist %>%
        left_join(wowy_all %>% select(player_id, season, ev_offense_wowy, ev_defense_wowy, pp_offense_wowy, pk_defense_wowy,
                                       ev_offense_xgwowy, ev_defense_xgwowy),
                   by = c("player_id", "season"))
    }
  }
}
if (!"ev_offense_wowy" %in% names(skater_onice_hist)) {
  skater_onice_hist$ev_offense_wowy <- NA_real_; skater_onice_hist$ev_defense_wowy <- NA_real_
  skater_onice_hist$pp_offense_wowy <- NA_real_; skater_onice_hist$pk_defense_wowy <- NA_real_
}
if (!"ev_offense_xgwowy" %in% names(skater_onice_hist)) {
  skater_onice_hist$ev_offense_xgwowy <- NA_real_; skater_onice_hist$ev_defense_xgwowy <- NA_real_
}

# Team-level PP/PK goals-per-game — recency-weighted across the same
# 3-season window as everything else. Deliberately NOT shots*conversion-
# rate (the structure this replaces): that approach separated shot volume
# from scoring rate, meaning a team's PK shot-suppression barely affected
# the opponent's actual scoring probability — the same structural flaw
# just proven wrong for 5v5 (fixed there via xG, which combines volume
# and quality into one number). No stable player-level PP/PK xG exists
# yet to do the same fix here (last session's attempt at player-level
# PP/PK WOWY showed severe instability, values as extreme as -33 — PP/PK
# ice time is roughly 10x smaller per game than 5v5, and that thin a
# sample doesn't hold up to this kind of decomposition). Team-level goals-
# per-game sidesteps the specific flaw without needing new data: it's
# already one number combining volume and efficiency, same as xG's
# spirit, just built from real goals instead of expected goals. Shot
# VOLUME for PP/PK still comes from the existing roster-weighted, trade-
# aware onice_pp_sf_pg/onice_pk_sa_pg — only used for diagnostics now,
# not to decompose the goal rate.
team_pp_pk_rates <- if (length(team_onice_by_season) > 0) {
  bind_rows(team_onice_by_season) %>%
    group_by(team_abbrev) %>%
    arrange(season, .by_group = TRUE) %>%
    summarise(
      pp_goals_pg = { w <- recency_weights_gp(season, pmax(coalesce(gp_onice, 0), 1)); sum(w * (coalesce(pp_goals, 0) / pmax(coalesce(gp_onice, 1), 1))) },
      pk_ga_pg    = { w <- recency_weights_gp(season, pmax(coalesce(gp_onice, 0), 1)); sum(w * (coalesce(pk_goals_against, 0) / pmax(coalesce(gp_onice, 1), 1))) },
      .groups = "drop"
    )
} else {
  data.frame(team_abbrev = character(0), pp_goals_pg = numeric(0), pk_ga_pg = numeric(0))
}
lg_avg_pp_goals_pg <- mean(team_pp_pk_rates$pp_goals_pg, na.rm = TRUE)
lg_avg_pk_ga_pg    <- mean(team_pp_pk_rates$pk_ga_pg, na.rm = TRUE)
if (is.na(lg_avg_pp_goals_pg)) lg_avg_pp_goals_pg <- 0.55   # rough real-world PP-goals/game fallback
if (is.na(lg_avg_pk_ga_pg))    lg_avg_pk_ga_pg    <- 0.55   # PK-goals-against/game is symmetric with PP-goals-for/game league-wide

proj_skater_onice <- project_skater_onice(skater_onice_hist)
cat("  proj_skater_onice rows:", if (is.null(proj_skater_onice)) 0 else nrow(proj_skater_onice), "\n")
if (!is.null(proj_skater_onice)) {
  skater_output <- skater_output %>% left_join(proj_skater_onice, by = "player_id")
} else {
  skater_output$onice_ca_pg <- NA_real_; skater_output$onice_cf_pg <- NA_real_; skater_output$onice_pk_sa_pg <- NA_real_; skater_output$onice_pp_sf_pg <- NA_real_
  skater_output$onice_xgf_pg <- NA_real_; skater_output$onice_xga_pg <- NA_real_
  skater_output$ev_offense_wowy_3yr <- NA_real_; skater_output$ev_defense_wowy_3yr <- NA_real_
  skater_output$pp_offense_wowy_3yr <- NA_real_; skater_output$pk_defense_wowy_3yr <- NA_real_
}
if (!"ev_offense_xgwowy_3yr" %in% names(skater_output)) {
  skater_output$ev_offense_xgwowy_3yr <- NA_real_; skater_output$ev_defense_xgwowy_3yr <- NA_real_
}

# ── RAPM (5v5) — loaded and joined the SAME way WOWY is, feeding into the
# coalesce fallback chain below as the new, most-preferred option. Missing
# RAPM data (e.g. seasons before this pipeline existed, or a player with
# no RAPM row for some reason) gracefully falls through to the existing
# WOWY/box-score chain, exactly the same failure mode WOWY itself already
# has relative to the box-score fallback beneath it.
cat("Loading RAPM (5v5) data...\n")
rapm_hist <- tryCatch(load_all_rapm(recent3), error = function(e) NULL)
proj_rapm <- if (!is.null(rapm_hist) && nrow(rapm_hist) > 0) project_rapm(rapm_hist) else NULL
cat("  proj_rapm rows:", if (is.null(proj_rapm)) 0 else nrow(proj_rapm), "\n")
if (!is.null(proj_rapm)) {
  skater_output <- skater_output %>% left_join(proj_rapm, by = "player_id")
} else {
  skater_output$off_rapm_3yr <- NA_real_; skater_output$def_rapm_3yr <- NA_real_
}

# ── RAPM (PP/PK) — PROMOTED TO PRODUCTION. Initially loaded diagnostic-
# only (see the comparison block further below, still present) — after
# reviewing that comparison, the spread looked bounded and reasonable
# (nothing like the -33 extreme instability that got the earlier
# player-level WOWY attempt reverted), so this now feeds the actual
# simulation. HONEST CAVEAT, not fully resolved: one team (MIN) disagreed
# meaningfully in direction between RAPM and the team-level historical
# approach in the comparison that motivated this — plausibly real roster
# effects RAPM correctly picks up that a team-level historical rate can't,
# but not fully investigated. Worth revisiting if MIN's game/series/
# playoff-bracket results look off, or after this has run for a few more
# refresh cycles.
cat("Loading RAPM (PP/PK) data...\n")
rapm_pppk_hist <- tryCatch(load_all_rapm_pppk(recent3), error = function(e) NULL)
proj_rapm_pppk <- if (!is.null(rapm_pppk_hist) && nrow(rapm_pppk_hist) > 0) project_rapm_pppk(rapm_pppk_hist) else NULL
cat("  proj_rapm_pppk rows:", if (is.null(proj_rapm_pppk)) 0 else nrow(proj_rapm_pppk), "\n")
if (!is.null(proj_rapm_pppk)) {
  skater_output <- skater_output %>% left_join(proj_rapm_pppk, by = "player_id")
} else {
  skater_output$pp_rapm_3yr <- NA_real_; skater_output$pk_rapm_3yr <- NA_real_
}

# ── Shot-based team offense/defense profile ──────────────────────────────────
# Adapted from HockeyStats.com's win-odds methodology: simulate goals as
# actual per-shot outcomes (shot happens -> is it a goal?) rather than
# drawing one aggregate normal-distributed margin per game. Scoped to data
# this app actually has:
#   - shots_for_pg and shooting_pct come directly from summing the top-18
#     roster skaters' (by projected points, an approximate "lineup") own
#     projected shot/goal rates — real per-player data, not a guess.
#   - shots_against_pg now comes from summing those SAME top-18 skaters'
#     own real on-ice Corsi-against rates (from play-by-play reconstruction)
#     — fully roster-driven, exactly like offense. Falls back to a blocks/
#     hits proxy only for skaters without enough on-ice history yet.
#   - goalie_sv_pct is the projected #1 starter's recency-weighted save %.
LEAGUE_AVG_SHOTS_PG <- 30    # typical NHL team shots/game — used as the shots-against baseline
LEAGUE_AVG_SV_PCT_FALLBACK <- 0.905  # used only if a team has literally no goalie data
DEF_PROXY_SCALE <- 3         # max approx +/- shots/game swing from the blocks/hits defensive proxy — only used as a fallback now, for skaters without on-ice history
HOME_XG_BOOST <- 0.1         # extra expected goals/game for the home team — approximates the OLD model's combined home-ice edge (HOME_SHOT_BOOST's extra shot volume + HOME_GOAL_BOOST's per-shot probability bump, ~0.09 + ~0.04 goals/game), now expressed directly since goals no longer come from a shots*per-shot-probability chain

# League-wide average WOWY (xG-preferred, same fallback chain used
# everywhere else), computed across the FULL population before restricting
# to each team's top-18 — used to RECENTER below. Crude single-player WOWY
# (team-minus-one-player, not full RAPM) carries a systematic bias where
# almost every player — stars and depth alike, good teams and bad — shows
# positive offense-WOWY and negative defense-WOWY relative to a naive zero.
# Subtracting the league average (not a full z-score — deliberately NOT
# dividing by SD, to keep this in real goals/60 units for the additive
# adjustment below) removes that constant bias while leaving real relative
# differences between players intact.
league_avg_off_wowy <- mean(coalesce(skater_output$off_rapm_3yr, skater_output$ev_offense_xgwowy_3yr, skater_output$ev_offense_wowy_3yr), na.rm = TRUE)
league_avg_def_wowy <- mean(coalesce(skater_output$def_rapm_3yr, skater_output$ev_defense_xgwowy_3yr, skater_output$ev_defense_wowy_3yr), na.rm = TRUE)
if (is.na(league_avg_off_wowy)) league_avg_off_wowy <- 0
if (is.na(league_avg_def_wowy)) league_avg_def_wowy <- 0
# Same recentering, for PP/PK RAPM — no goals-WOWY/box-score fallback
# here since PP/PK RAPM has no earlier-generation equivalent to fall back
# to (that's the whole reason this was diagnostic-only until now).
league_avg_pp_rapm <- mean(skater_output$pp_rapm_3yr, na.rm = TRUE)
league_avg_pk_rapm <- mean(skater_output$pk_rapm_3yr, na.rm = TRUE)
if (is.na(league_avg_pp_rapm)) league_avg_pp_rapm <- 0
if (is.na(league_avg_pk_rapm)) league_avg_pk_rapm <- 0
cat("  League-average WOWY (recentering baseline) — offense:", round(league_avg_off_wowy, 3), "| defense:", round(league_avg_def_wowy, 3), "\n")
# NOTE: a parallel PP/PK adjustment layer was built and reverted here. The
# recentering baseline itself came out at -2.8/+2.6 (mean) and -4.6/+2.9
# (median) — nowhere near zero like EV's does, and switching to median
# (meant to fix outlier sensitivity) made it WORSE, not better. Raw
# individual values ranged as extreme as -33, ~6x more extreme than the
# single case (McDavid, -19.45) already found and fixed. That combination
# — median not helping, and the underlying range still absurd — points to
# a genuine methodological limit rather than a fixable remaining bug: PP/
# PK ice time is roughly 10x smaller per game than EV time, and the same
# "team minus one player" approach that works reasonably at EV's sample
# size may simply not be reliable at PP/PK's. Left as EV-only for now.

# Position-aware roster selection: top 12 forwards + top 6 defensemen,
# matching real NHL active-roster composition, rather than a blind top-18
# by points regardless of position (which could easily grab, say, 15
# forwards and only 3 defensemen if a team's scoring skews forward-heavy —
# not how an actual lineup works, since a team dresses a fixed 12F/6D
# split every night regardless of who's producing the most points).
#
# Selected by rate_toi_min, not proj_points — this is about estimating
# who actually PLAYS and how much, not who scores the most. rate_toi_min
# is a pure per-game RATE (ice time when they were actually in the
# lineup), so a top-pairing defenseman or shutdown-line forward who
# missed games to injury still shows their true role/usage rate rather
# than being penalized for games they didn't get to play — exactly the
# concern about not letting missed time look like reduced usage.
team_offense_f <- skater_output %>%
  filter(has_history, position != "D") %>%
  group_by(team_abbrev) %>%
  arrange(desc(rate_toi_min), .by_group = TRUE) %>%
  slice_head(n = 12)
team_offense_d <- skater_output %>%
  filter(has_history, position == "D") %>%
  group_by(team_abbrev) %>%
  arrange(desc(rate_toi_min), .by_group = TRUE) %>%
  slice_head(n = 6)
team_offense <- bind_rows(team_offense_f, team_offense_d) %>%
  group_by(team_abbrev) %>%
  mutate(
    # Prefer RAPM (ridge-regularized, properly isolates offense from
    # defense by construction — see fit_rapm.R) when available; fall back
    # to xG-based WOWY, then goals-based WOWY for seasons before either
    # existed. Per-player, before aggregation, so the team-level number
    # reflects whichever signal each individual player actually has rather
    # than an all-or-nothing switch. Recentered against the league
    # average — see note above.
    ev_off_wowy_effective = coalesce(off_rapm_3yr, ev_offense_xgwowy_3yr, ev_offense_wowy_3yr) - league_avg_off_wowy,
    ev_def_wowy_effective = coalesce(def_rapm_3yr, ev_defense_xgwowy_3yr, ev_defense_wowy_3yr) - league_avg_def_wowy,
    # PP/PK RAPM — recentered the same way, now feeding the actual
    # simulation (see the note where proj_rapm_pppk gets joined above for
    # the honest caveat this promotion still carries).
    pp_rapm_effective = pp_rapm_3yr - league_avg_pp_rapm,
    pk_rapm_effective = pk_rapm_3yr - league_avg_pk_rapm
  ) %>%
  summarise(
    shots_for_pg      = sum(coalesce(rate_shots, 0), na.rm = TRUE),
    goals_for_pg      = sum(coalesce(rate_goals, 0), na.rm = TRUE),
    def_proxy         = sum(coalesce(rate_hits, 0) * 0.5 + coalesce(rate_blocks, 0), na.rm = TRUE),
    # On-ice CA/CF are SHARED stats — every skater on the ice for a shot
    # gets credited for that same event (~5 skaters at once during 5v5), so
    # summing 18 players' individual on-ice rates counts almost every event
    # roughly 5x over. Shots-for is safe to sum (each shot has exactly one
    # author); on-ice defense is not. Fix: TOI-weighted AVERAGE instead of
    # a sum — a full-time player's real observed on-ice rate counts
    # proportionally more than a 4th-liner's, without the ~5x inflation.
    onice_ca_pg_wtd   = sum(onice_ca_pg * coalesce(rate_toi_min, 12), na.rm = TRUE) / sum(coalesce(rate_toi_min, 12)[!is.na(onice_ca_pg)], na.rm = TRUE),
    onice_cf_pg_wtd   = sum(onice_cf_pg * coalesce(rate_toi_min, 12), na.rm = TRUE) / sum(coalesce(rate_toi_min, 12)[!is.na(onice_cf_pg)], na.rm = TRUE),
    # Same shared-stat reasoning as CA above — multiple PK defenders get
    # credited for the same shot-against event, so this is a TOI-weighted
    # average, not a sum. This is the roster-driven replacement for the
    # earlier team-level PK compromise, now that real per-skater PK shot
    # volume is tracked at the source (onice_stats.R).
    onice_pk_sa_pg_wtd = sum(onice_pk_sa_pg * coalesce(rate_toi_min, 12), na.rm = TRUE) / sum(coalesce(rate_toi_min, 12)[!is.na(onice_pk_sa_pg)], na.rm = TRUE),
    # Same shared-stat reasoning, mirrored for offense — needed so the
    # SOG-to-Corsi ratio's denominator (onice_cf_pg_wtd + this) is scoped
    # the same way as what it gets APPLIED to (onice_ca_pg_wtd + PK-SA,
    # both roster-aggregated player-shift rates) rather than comparing
    # against team-level full-game totals, which are a different scale
    # entirely (a team's full-game Corsi is much bigger than any one
    # player's on-ice rate during just their own shifts).
    onice_pp_sf_pg_wtd = sum(onice_pp_sf_pg * coalesce(rate_toi_min, 12), na.rm = TRUE) / sum(coalesce(rate_toi_min, 12)[!is.na(onice_pp_sf_pg)], na.rm = TRUE),
    # xG — summed and divided by 5, NOT a TOI-weighted average. At 5v5,
    # exactly 5 skaters share full credit for every event at any given
    # moment, so summing every rostered player's own onice_xgf_pg (each
    # already a per-game rate reflecting their own ice-time share) and
    # dividing by 5 reconstructs the team's real total xG-for-per-game —
    # since summing across the full roster gives exactly 5x the true
    # team total (each moment of play counted once per player on the ice).
    # A TOI-weighted AVERAGE, by contrast, is mathematically bounded
    # between the min and max of the players being averaged — it can
    # never be more extreme than one player's own number, which throws
    # away the fact that a team can stack multiple good (or bad) players
    # together. Confirmed this was compressing variance badly: roster-
    # weighted xG spread (best-worst team) was 0.335 under the average,
    # vs. 1.187 for real, direct single-season team xG — a 3.5x gap.
    onice_xgf_pg_wtd  = sum(coalesce(onice_xgf_pg, 0), na.rm = TRUE) / 5,
    onice_xga_pg_wtd  = sum(coalesce(onice_xga_pg, 0), na.rm = TRUE) / 5,
    n_with_onice_xg   = sum(!is.na(onice_xgf_pg)),
    n_with_onice_def  = sum(!is.na(onice_ca_pg)),
    # WOWY is also a shared/context stat (it represents team-level effects
    # attributable to a player, not an individually-authored event like a
    # shot), so this gets the same TOI-weighted-average treatment as CA/CF
    # above, not a sum.
    wowy_ev_off_wtd = sum(coalesce(ev_off_wowy_effective, 0) * coalesce(rate_toi_min, 12), na.rm = TRUE) / sum(coalesce(rate_toi_min, 12)[!is.na(ev_off_wowy_effective)], na.rm = TRUE),
    wowy_ev_def_wtd = sum(coalesce(ev_def_wowy_effective, 0) * coalesce(rate_toi_min, 12), na.rm = TRUE) / sum(coalesce(rate_toi_min, 12)[!is.na(ev_def_wowy_effective)], na.rm = TRUE),
    n_with_wowy_off = sum(!is.na(ev_off_wowy_effective)),
    n_with_wowy_def = sum(!is.na(ev_def_wowy_effective)),
    # PP/PK RAPM — same TOI-weighted-average construction as 5v5 WOWY
    # above (a shared/context stat, not an individually-authored event),
    # using the SAME 12F/6D roster already selected by rate_toi_min —
    # deliberately reusing the exact roster already validated in the
    # earlier diagnostic comparison, rather than introducing a different,
    # untested PP/PK-specific roster-selection method at the same time as
    # promoting this to production.
    pp_rapm_wtd = sum(coalesce(pp_rapm_effective, 0) * coalesce(rate_toi_min, 12), na.rm = TRUE) / sum(coalesce(rate_toi_min, 12)[!is.na(pp_rapm_effective)], na.rm = TRUE),
    pk_rapm_wtd = sum(coalesce(pk_rapm_effective, 0) * coalesce(rate_toi_min, 12), na.rm = TRUE) / sum(coalesce(rate_toi_min, 12)[!is.na(pk_rapm_effective)], na.rm = TRUE),
    n_with_pp_rapm = sum(!is.na(pp_rapm_effective)),
    n_with_pk_rapm = sum(!is.na(pk_rapm_effective)),
    n_with_xgwowy_off = sum(!is.na(ev_offense_xgwowy_3yr)),
    n_with_xgwowy_def = sum(!is.na(ev_defense_xgwowy_3yr)),
    .groups = "drop"
  )

def_mean <- mean(team_offense$def_proxy, na.rm = TRUE)
def_sd   <- sd(team_offense$def_proxy, na.rm = TRUE); if (is.na(def_sd) || def_sd == 0) def_sd <- 1
lg_avg_shooting_pct <- sum(team_offense$goals_for_pg, na.rm=TRUE) / sum(team_offense$shots_for_pg, na.rm=TRUE)

# ── WOWY adjustment to shooting quality and defensive shot suppression ──────
# Converts each team's TOI-weighted average WOWY (a per-60 goal
# differential) into a per-game GOALS adjustment, using roughly how much
# of a 60-minute game is played at 5v5 (the rest is special teams/other
# situations). This is added to real box-score goals (offense) or
# converted into an equivalent shots adjustment via league-average
# conversion rate (defense) — shot VOLUME itself is never touched, only
# the quality/effectiveness layered on top of it. Only applied once at
# least 15 of the top-18 roster have a real (non-NA) WOWY value, same
# coverage bar used for the on-ice defense fix, so a roster full of
# rookies/no-history players doesn't get a distorted adjustment from a
# tiny, unreliable sample.
AVG_5V5_MIN_PER_GAME <- 48  # rough share of a 60-min game played at 5v5
MIN_WOWY_ROSTER_COVERAGE <- 15
# Rough real-world average PP/PK time per team per game — symmetric,
# since a team's PK time is essentially the same as their opponents' PP
# time league-wide. Used the same way AVG_5V5_MIN_PER_GAME converts a
# per-60 rate into a per-game adjustment.
AVG_PP_MIN_PER_GAME <- 4.5
AVG_PK_MIN_PER_GAME <- 4.5

# Self-calibrating SOG-to-Corsi conversion: onice_ca_pg/onice_cf_pg are
# CORSI (shots-on-goal + missed + blocked combined), a bigger number than
# real box-score shots. Derive the conversion ratio from OUR OWN data
# (real league-wide shots_for_pg vs the league-wide Corsi-for sum computed
# the same roster-aggregated way) so both sides of the formula land on the
# same shots-on-goal scale — no guessed constant.
#
# IMPORTANT: must compare MATCHED SCOPES on both sides, and "matched scope"
# means TWO things, not one:
#   1. All-situation vs 5v5-only (PP/PK shot volume must be included on
#      both sides) — this was the first bug found: comparing all-situation
#      shots_for_pg against 5v5-only onice_cf_pg_wtd left PP volume out of
#      the denominator, forcing the ratio to inflate to compensate (it hit
#      2.069, when a genuine Corsi->SOG ratio should be well under 1.0).
#   2. Roster-aggregated (per-player, during just THEIR shifts) vs
#      team-level (full 60 minutes) — this is the second, bigger bug found
#      right after "fixing" the first one: switching the denominator to
#      team-level full-game Corsi (~55-60/game) made it wildly mismatched
#      against what the ratio actually gets APPLIED to below
#      (onice_ca_pg_wtd, which is roster-aggregated — each player's own
#      on-ice rate during their own ~15-20 min of shifts, not the team's
#      full 60 minutes). That mismatch drove the ratio down to 0.539,
#      clamping every team's shots-against to the floor.
# Fix: keep the denominator roster-aggregated (matching onice_ca_pg_wtd's
# scope exactly), and add a roster-aggregated PP-shots-for rate
# (onice_pp_sf_pg_wtd, mirroring the existing onice_pk_sa_pg_wtd) to bring
# PP volume in — solving bug 1 without reintroducing bug 2.
sog_to_corsi_ratio <- mean(team_offense$shots_for_pg, na.rm = TRUE) /
  mean((team_offense$onice_cf_pg_wtd + coalesce(team_offense$onice_pp_sf_pg_wtd, 0))[team_offense$onice_cf_pg_wtd > 0], na.rm = TRUE)
cat("  SOG-to-Corsi conversion ratio (self-calibrated, scope-matched):", round(sog_to_corsi_ratio, 3), "\n")

# Testing a specific hypothesis: our on-ice data is 5v5-ONLY, but real
# box-score shots-against is ALL SITUATIONS combined (5v5+PP+PK+etc). A
# single league-wide conversion ratio assumes every team's PP/PK shot
# patterns are similar enough to not matter — this checks whether that
# assumption is actually breaking down for Edmonton specifically.
edm_raw <- team_offense %>% filter(team_abbrev == "EDM")
if (nrow(edm_raw) > 0) {
  cat("  EDM raw on-ice (5v5-only, pre-conversion): onice_ca_pg_wtd=", round(edm_raw$onice_ca_pg_wtd,2),
      "onice_cf_pg_wtd=", round(edm_raw$onice_cf_pg_wtd,2),
      "-> after conversion:", round(edm_raw$onice_ca_pg_wtd * sog_to_corsi_ratio, 2),
      "| real all-situation shots-against was 26.7\n")
}

cat("  Teams with a full top-18 of on-ice defense data:", sum(team_offense$n_with_onice_def >= 15), "of", nrow(team_offense),
    "(partial coverage falls back to the blocks/hits proxy for those teams)\n")
cat("  Teams with enough WOWY coverage — offense:", sum(team_offense$n_with_wowy_off >= MIN_WOWY_ROSTER_COVERAGE, na.rm=TRUE),
    "defense:", sum(team_offense$n_with_wowy_def >= MIN_WOWY_ROSTER_COVERAGE, na.rm=TRUE), "of", nrow(team_offense), "\n")
cat("  Of which using real xG-based WOWY (vs. goals-based fallback) — offense:",
    sum(team_offense$n_with_xgwowy_off >= MIN_WOWY_ROSTER_COVERAGE, na.rm=TRUE),
    "defense:", sum(team_offense$n_with_xgwowy_def >= MIN_WOWY_ROSTER_COVERAGE, na.rm=TRUE), "of", nrow(team_offense), "\n")

team_offense <- team_offense %>%
  left_join(team_pp_pk_rates, by = "team_abbrev") %>%
  mutate(
    pp_goals_pg = coalesce(pp_goals_pg, lg_avg_pp_goals_pg),
    pk_ga_pg    = coalesce(pk_ga_pg, lg_avg_pk_ga_pg),
    # PP/PK RAPM adjustment — same construction as the 5v5 WOWY adjustment
    # above: converts the roster's TOI-weighted RAPM value into a per-game
    # goals shift, clamped to guard against an extreme roster producing an
    # implausible result, applied only when roster coverage clears the
    # same bar used for 5v5. Added onto the real team-level historical
    # rate (not replacing it), same relationship WOWY has to the
    # box-score goals rate for 5v5.
    pp_rapm_adj_per_game = ifelse(n_with_pp_rapm >= MIN_WOWY_ROSTER_COVERAGE,
                                    pmax(-0.5, pmin(0.5, pp_rapm_wtd * (AVG_PP_MIN_PER_GAME / 60))), 0),
    pk_rapm_adj_per_game = ifelse(n_with_pk_rapm >= MIN_WOWY_ROSTER_COVERAGE,
                                    pmax(-0.5, pmin(0.5, pk_rapm_wtd * (AVG_PK_MIN_PER_GAME / 60))), 0),
    pp_goals_pg = pmax(0.1, pp_goals_pg + pp_rapm_adj_per_game),
    pk_ga_pg    = pmax(0.1, pk_ga_pg + pk_rapm_adj_per_game)
  )

# League-average 5v5 xGA/shot — the denominator for the same kind of
# goalie-relative-to-average adjustment already used elsewhere (see
# league_avg_sv_pct below), just scoped to xG instead of raw shots. A
# league-average xGA-per-shot near the league-average box-score shooting%
# would confirm this is on a comparable scale; wildly different would be
# a red flag worth checking before trusting the Poisson step below.
league_avg_xga_per_shot <- mean(team_offense$onice_xga_pg_wtd / pmax(team_offense$onice_cf_pg_wtd, 1), na.rm = TRUE)
cat("  League-average 5v5 xGA-per-shot (xG-based goalie-adjustment baseline):", round(league_avg_xga_per_shot, 4), "\n")

team_offense <- team_offense %>%
  mutate(
    # WOWY adjustment — converts each team's average context-adjusted
    # on-ice impact into a per-game goals shift, added to real box-score
    # goals for offense, and converted to an equivalent shots adjustment
    # (via league-average conversion rate) for defense. Clamped to keep
    # an unusually extreme roster from producing an implausible result —
    # same kind of safety margin as the shots_against floor below.
    wowy_off_adj_per_game = ifelse(n_with_wowy_off >= MIN_WOWY_ROSTER_COVERAGE,
                                    pmax(-1.5, pmin(1.5, wowy_ev_off_wtd * (AVG_5V5_MIN_PER_GAME / 60))), 0),
    wowy_def_adj_per_game = ifelse(n_with_wowy_def >= MIN_WOWY_ROSTER_COVERAGE,
                                    pmax(-1.5, pmin(1.5, wowy_ev_def_wtd * (AVG_5V5_MIN_PER_GAME / 60))), 0),
    goals_for_pg_wowy_adj     = goals_for_pg + wowy_off_adj_per_game,
    shooting_pct              = ifelse(shots_for_pg > 0, pmax(0.05, pmin(0.20, goals_for_pg_wowy_adj / shots_for_pg)), lg_avg_shooting_pct),
    # ACTUAL FIX: apply the same adjustment to onice_xgf_pg_wtd/
    # onice_xga_pg_wtd — this is what simulate_games() really reads
    # (xgf_lu/xga_lu are built directly from these two columns). The
    # goals_for_pg_wowy_adj/shooting_pct chain above feeds shots_against_lu/
    # shooting_pct_lu, which are defined but never actually referenced again
    # anywhere in this script — confirmed dead code, meaning RAPM/WOWY was
    # only ever live for PP/PK, not 5v5, until this fix.
    onice_xgf_pg_wtd = pmax(0.1, onice_xgf_pg_wtd + wowy_off_adj_per_game),
    onice_xga_pg_wtd = pmax(0.1, onice_xga_pg_wtd + wowy_def_adj_per_game),
    def_z                     = (def_proxy - def_mean) / def_sd,
    shots_against_pg_fallback = pmax(15, LEAGUE_AVG_SHOTS_PG - def_z * DEF_PROXY_SCALE),
    # 5v5-only on-ice shots-against PLUS real roster-driven PK shots-against
    # (both Corsi-scale, both converted via the same SOG ratio) — this is
    # what makes it all-situation instead of 5v5-only. Falls back to 0
    # added PK shots if that data isn't available for enough of the roster,
    # rather than erroring.
    shots_against_onice       = (onice_ca_pg_wtd + coalesce(onice_pk_sa_pg_wtd, 0)) * sog_to_corsi_ratio,
    # Only trust the on-ice aggregate once most of the top-18 actually has
    # on-ice history (e.g. 15+ of 18) — otherwise a roster with several
    # rookies/no-history skaters would understate shots-against just from
    # missing data, not real defensive quality.
    shots_against_pg_preWowy  = ifelse(n_with_onice_def >= 15, shots_against_onice, shots_against_pg_fallback),
    # Positive wowy_def_adj_per_game = fewer goals against than the team's
    # baseline, i.e. better defense = FEWER shots-against equivalent.
    shots_against_pg          = pmax(15, shots_against_pg_preWowy - (wowy_def_adj_per_game / lg_avg_shooting_pct))
  )

for (tm in c("VGK", "MIN", "NSH", "SEA")) {
  tm_raw <- team_offense %>% filter(team_abbrev == tm)
  if (nrow(tm_raw) > 0) {
    cat("  ", tm, "raw on-ice: onice_ca_pg_wtd=", round(tm_raw$onice_ca_pg_wtd,2),
        "onice_cf_pg_wtd=", round(tm_raw$onice_cf_pg_wtd,2),
        "-> shots_for_pg=", round(tm_raw$shots_for_pg,2), "shooting_pct=", round(tm_raw$shooting_pct,4),
        "final shots_against_pg=", round(tm_raw$shots_against_pg,2), "\n")
  }
}

# ── Comprehensive single-team check ──────────────────────────────────────────
# VGK's projection is showing well below an external reference (a mature,
# established WAR-based model) despite last season's Cup Final run. Rather
# than check pieces one at a time, this traces every stage of the pipeline
# for one team so a problem (if there is one) is visible directly rather
# than inferred from the final number.
DEEP_CHECK_TEAMS <- c("VGK", "SEA", "SJS", "EDM", "PIT")
for (DEEP_CHECK_TEAM in DEEP_CHECK_TEAMS) {
dc <- team_offense %>% filter(team_abbrev == DEEP_CHECK_TEAM)
if (nrow(dc) > 0) {
  cat("\n── Deep check:", DEEP_CHECK_TEAM, "— every stage of the offense/defense pipeline ──\n")
  cat("  OFFENSE\n")
  cat("    goals_for_pg (raw, pre-WOWY)      =", round(dc$goals_for_pg, 3), "\n")
  cat("    wowy_off_adj_per_game (EV)        =", round(dc$wowy_off_adj_per_game, 3), "| roster coverage:", dc$n_with_wowy_off, "/ 18\n")
  cat("    goals_for_pg_wowy_adj (final)     =", round(dc$goals_for_pg_wowy_adj, 3), "\n")
  cat("    shots_for_pg                      =", round(dc$shots_for_pg, 3), "\n")
  cat("    shooting_pct (final)              =", round(dc$shooting_pct, 4), "| league avg was", round(lg_avg_shooting_pct, 4), "\n")
  cat("  ACTUAL SIMULATION INPUTS (what simulate_games() really uses now)\n")
  cat("    onice_xgf_pg_wtd (5v5 xG-for)     =", round(dc$onice_xgf_pg_wtd, 3), "| onice_xga_pg_wtd (5v5 xG-against) =", round(dc$onice_xga_pg_wtd, 3), "\n")
  cat("    pp_goals_pg (team-level PP-for)   =", round(dc$pp_goals_pg, 3), "| league avg =", round(lg_avg_pp_goals_pg, 3), "\n")
  cat("    pk_ga_pg (team-level PK-against)  =", round(dc$pk_ga_pg, 3), "| league avg =", round(lg_avg_pk_ga_pg, 3), "\n")
  cat("  DEFENSE\n")
  cat("    onice_ca_pg_wtd (5v5, pre-convert)=", round(dc$onice_ca_pg_wtd, 3), "\n")
  cat("    onice_pk_sa_pg_wtd                =", round(dc$onice_pk_sa_pg_wtd, 3), "\n")
  cat("    shots_against_onice (pre-WOWY)    =", round(dc$shots_against_onice, 3), "| source:", ifelse(dc$n_with_onice_def >= 15, "onice", "proxy fallback"), "(", dc$n_with_onice_def, "/ 18 with on-ice history)\n")
  cat("    wowy_def_adj_per_game (EV)        =", round(dc$wowy_def_adj_per_game, 3), "| roster coverage:", dc$n_with_wowy_def, "/ 18\n")
  cat("    shots_against_pg (final)          =", round(dc$shots_against_pg, 3), "| league avg range was ~15-35\n")
  cat("  DEFENSE — player-level breakdown (same 12F/6D roster feeding the average above)\n")
  dc_roster_f <- skater_output %>% filter(has_history, team_abbrev == DEEP_CHECK_TEAM, position != "D") %>%
    arrange(desc(rate_toi_min)) %>% slice_head(n = 12)
  dc_roster_d <- skater_output %>% filter(has_history, team_abbrev == DEEP_CHECK_TEAM, position == "D") %>%
    arrange(desc(rate_toi_min)) %>% slice_head(n = 6)
  dc_roster <- bind_rows(dc_roster_f, dc_roster_d)
  for (i in seq_len(nrow(dc_roster))) {
    p <- dc_roster[i, ]
    cat("    ", sprintf("%-20s", substr(coalesce(p$player_name, "?"), 1, 20)),
        "| toi_pg_min=", round(coalesce(p$rate_toi_min, NA), 1),
        "onice_ca_pg=", ifelse(is.na(p$onice_ca_pg), "NA", round(p$onice_ca_pg, 2)),
        "onice_cf_pg=", ifelse(is.na(p$onice_cf_pg), "NA", round(p$onice_cf_pg, 2)), "\n")
  }
} else {
  cat("\n── Deep check:", DEEP_CHECK_TEAM, "— no row found in team_offense ──\n")
}
}

# ── GSAx-based goaltending adjustment ────────────────────────────────────────
# Real box-score sv_pct conflates a goalie's own skill with the shot
# quality their team allows in front of them — a goalie behind a weak
# defense faces harder shots and looks worse than their true talent;
# behind a great defense, the reverse. GSAx (Goals Saved Above Expected,
# from onice_stats.R's xG scoring) isolates the goalie's own performance
# relative to shot quality faced. Converted here into an adjusted save
# rate: league-average sv_pct + (this goalie's GSAx per shot faced) — "how
# this goalie would compare to average if facing average-quality shots."
MIN_GSAX_SHOTS_TRACKED <- 200  # minimum pooled shots faced (5v5+PK) before trusting GSAx over box-score sv_pct

goalie_onice_hist <- lapply(recent3, function(s) {
  g <- tryCatch(gh_read(paste0("https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/onice/", s, "/goalie_onice.csv")), error = function(e) NULL)
  if (is.null(g) || nrow(g) == 0) return(NULL)
  if (!"xg_faced" %in% names(g)) g$xg_faced <- NA_real_  # guard for pre-xG-model on-ice CSVs
  g$player_id <- as.character(g$player_id)
  g
})
goalie_onice_hist <- bind_rows(Filter(Negate(is.null), goalie_onice_hist))

if (nrow(goalie_onice_hist) > 0) {
  goalie_gsax_pooled <- goalie_onice_hist %>%
    group_by(player_id) %>%
    summarise(
      shots_tracked = sum(coalesce(shots_5v5, 0)) + sum(coalesce(shots_pk, 0)),
      ga_tracked    = sum(coalesce(ga_5v5, 0)) + sum(coalesce(ga_pk, 0)),
      xg_tracked    = sum(coalesce(xg_faced, 0)),
      .groups = "drop"
    ) %>%
    mutate(gsax_pooled = xg_tracked - ga_tracked)
  lg_avg_sv_pct_tracked <- 1 - sum(goalie_gsax_pooled$ga_tracked, na.rm = TRUE) / sum(goalie_gsax_pooled$shots_tracked, na.rm = TRUE)
  cat("  GSAx goaltending: league-avg tracked sv% =", round(lg_avg_sv_pct_tracked, 4),
      "| goalies with enough sample:", sum(goalie_gsax_pooled$shots_tracked >= MIN_GSAX_SHOTS_TRACKED, na.rm = TRUE), "\n")
  goalie_gsax_pooled <- goalie_gsax_pooled %>%
    mutate(gsax_adj_sv_pct = ifelse(shots_tracked >= MIN_GSAX_SHOTS_TRACKED,
                                     lg_avg_sv_pct_tracked + gsax_pooled / shots_tracked, NA_real_))
} else {
  goalie_gsax_pooled <- NULL
  cat("  No goalie on-ice/xG data found for GSAx — goaltending stays box-score sv_pct only.\n")
}

if (!is.null(goalie_gsax_pooled)) goalie_output <- goalie_output %>% left_join(goalie_gsax_pooled %>% select(player_id, gsax_adj_sv_pct), by = "player_id")
if (!"gsax_adj_sv_pct" %in% names(goalie_output)) goalie_output$gsax_adj_sv_pct <- NA_real_

team_goaltending <- goalie_output %>%
  filter(has_history, coalesce(proj_gp, 0) > 0) %>%
  mutate(sv_pct_for_sim = coalesce(gsax_adj_sv_pct, proj_sv_pct)) %>%
  group_by(team_abbrev) %>%
  summarise(goalie_sv_pct = sum(sv_pct_for_sim * proj_gp, na.rm = TRUE) / sum(proj_gp, na.rm = TRUE), .groups = "drop")

# Goaltending piece of the same deep check — each team's actual rostered
# goalies, box-score vs GSAx-adjusted save rate side by side.
for (DEEP_CHECK_TEAM in DEEP_CHECK_TEAMS) {
dc_goalies <- goalie_output %>% filter(team_abbrev == DEEP_CHECK_TEAM, has_history, coalesce(proj_gp, 0) > 0)
if (nrow(dc_goalies) > 0) {
  cat("  GOALTENDING (", DEEP_CHECK_TEAM, ")\n")
  for (i in seq_len(nrow(dc_goalies))) {
    g <- dc_goalies[i, ]
    gsax_row <- if (!is.null(goalie_gsax_pooled)) goalie_gsax_pooled %>% filter(player_id == as.character(g$player_id)) %>% slice(1) else NULL
    cat("    ", coalesce(g$player_name, g$player_id), "| proj_gp=", round(coalesce(g$proj_gp,0),1),
        "proj_sv_pct (box-score)=", round(coalesce(g$proj_sv_pct,NA),4),
        "gsax_adj_sv_pct (used in sim)=", round(coalesce(g$gsax_adj_sv_pct,NA),4),
        "\n         shots_tracked=", if (!is.null(gsax_row) && nrow(gsax_row)>0) round(gsax_row$shots_tracked[1],0) else NA,
        "(needs >=", MIN_GSAX_SHOTS_TRACKED, ") gsax_pooled=", if (!is.null(gsax_row) && nrow(gsax_row)>0) round(gsax_row$gsax_pooled[1],2) else NA, "\n")
  }
  cat("    -> team_goaltending$goalie_sv_pct =", round(coalesce((team_goaltending %>% filter(team_abbrev==DEEP_CHECK_TEAM))$goalie_sv_pct[1], NA), 4), "\n")
} else {
  cat("  GOALTENDING (", DEEP_CHECK_TEAM, ") — no goalies with history/proj_gp found\n")
}
}

team_off_def <- data.frame(team_abbrev = names(net_lookup), stringsAsFactors = FALSE) %>%
  left_join(team_offense, by = "team_abbrev") %>%
  left_join(team_goaltending, by = "team_abbrev") %>%
  mutate(
    shots_for_pg     = coalesce(shots_for_pg, mean(shots_for_pg, na.rm = TRUE)),
    shots_against_pg = coalesce(shots_against_pg, LEAGUE_AVG_SHOTS_PG),
    shooting_pct     = coalesce(shooting_pct, lg_avg_shooting_pct),
    goalie_sv_pct    = coalesce(goalie_sv_pct, LEAGUE_AVG_SV_PCT_FALLBACK),
    onice_xgf_pg_wtd = coalesce(onice_xgf_pg_wtd, mean(onice_xgf_pg_wtd, na.rm = TRUE)),
    onice_xga_pg_wtd = coalesce(onice_xga_pg_wtd, mean(onice_xga_pg_wtd, na.rm = TRUE)),
    onice_pp_sf_pg_wtd = coalesce(onice_pp_sf_pg_wtd, mean(onice_pp_sf_pg_wtd, na.rm = TRUE)),
    onice_pk_sa_pg_wtd = coalesce(onice_pk_sa_pg_wtd, mean(onice_pk_sa_pg_wtd, na.rm = TRUE)),
    pp_goals_pg      = coalesce(pp_goals_pg, lg_avg_pp_goals_pg),
    pk_ga_pg         = coalesce(pk_ga_pg, lg_avg_pk_ga_pg)
  )

league_avg_sv_pct <- mean(team_off_def$goalie_sv_pct, na.rm = TRUE)
cat("  Team shots/game range:", round(min(team_off_def$shots_for_pg),1), "-", round(max(team_off_def$shots_for_pg),1),
    "| shots-against range:", round(min(team_off_def$shots_against_pg),1), "-", round(max(team_off_def$shots_against_pg),1),
    "| Sv% range:", round(min(team_off_def$goalie_sv_pct),3), "-", round(max(team_off_def$goalie_sv_pct),3), "\n")

# Full league-wide goaltending ranking — checking whether the save% spread
# is genuinely too narrow league-wide (a real, plausible compression
# source: this is a GP-weighted average across 2-3 goalies per team, each
# of whose own GSAx is itself pooled across a 3-year window, which could
# be regressing extreme goaltending performances toward the mean the same
# way skater WOWY did before the sum/5 fix — except goalies genuinely
# don't share credit the way skaters do, so this needs checking rather
# than assuming the same fix applies).
cat("\n  ── League-wide goaltending ranking (goalie_sv_pct — what the sim actually uses) ──\n")
tryCatch({
  goalie_sv_diag <- team_off_def %>% select(team_abbrev, goalie_sv_pct) %>% arrange(desc(goalie_sv_pct))
  for (i in seq_len(nrow(goalie_sv_diag))) {
    r <- goalie_sv_diag[i, ]
    cat(sprintf("  %-4s | goalie_sv_pct=%.4f (rank %d/%d)\n", r$team_abbrev, r$goalie_sv_pct, i, nrow(goalie_sv_diag)))
  }
  cat("  Spread (best - worst):", round(max(goalie_sv_diag$goalie_sv_pct, na.rm=TRUE) - min(goalie_sv_diag$goalie_sv_pct, na.rm=TRUE), 4), "\n")
}, error = function(e) cat("  Diagnostic error:", conditionMessage(e), "\n"))
cat("\n")

# Checking for a SYSTEMATIC (league-wide) bias vs. this being specific to
# one or two teams. Real league-average shots-against last season was
# ~27.83 (2282 SA / 82 GP, from the hockey-reference tables we've been
# comparing against). If the model's own league-wide average is notably
# higher than that across ALL 32 teams, that's a general calibration issue
# worth correcting for everyone — not something to special-case for one team.
model_lg_avg_sa <- mean(team_off_def$shots_against_pg, na.rm = TRUE)
cat("  Model league-avg shots_against_pg:", round(model_lg_avg_sa, 2), "vs. real league average ~27.83",
    "(", ifelse(model_lg_avg_sa > 27.83, "model runs HIGH", "model runs LOW/on-target"), ")\n")

# Player-level diagnostic for specific persistently-underprojected teams —
# rather than continuing to guess at team-level formula changes, this shows
# exactly what each individual player's projected rates look like, so we
# can see whether the root cause is a real data/pipeline issue (e.g. a
# star's history not matching correctly) vs. the model accurately
# reflecting a thin supporting cast that a reference tool weighs differently.
cat("\n  ── Player-level diagnostic: EDM, VGK, MIN, NSH, SEA — actual 12F/6D roster by rate_toi_min ──\n")
for (tm in c("EDM", "VGK", "MIN", "NSH", "SEA")) {
  cat("\n  --", tm, "--\n")
  tm_players_f <- skater_output %>%
    filter(team_abbrev == tm, has_history, position != "D") %>%
    arrange(desc(rate_toi_min)) %>% slice_head(n = 12)
  tm_players_d <- skater_output %>%
    filter(team_abbrev == tm, has_history, position == "D") %>%
    arrange(desc(rate_toi_min)) %>% slice_head(n = 6)
  tm_players <- bind_rows(tm_players_f, tm_players_d) %>% arrange(desc(proj_points))
  for (i in seq_len(nrow(tm_players))) {
    p <- tm_players[i, ]
    cat(sprintf("    %-20s | proj_pts=%.1f rate_goals=%.3f rate_shots=%.3f rate_toi_min=%.1f n_seasons=%d\n",
                substr(coalesce(p$player_name, "?"), 1, 20), coalesce(p$proj_points, NA_real_),
                coalesce(p$rate_goals, NA_real_), coalesce(p$rate_shots, NA_real_),
                coalesce(p$rate_toi_min, NA_real_), coalesce(p$n_seasons, NA_integer_)))
  }
}
cat("\n")

cat("  ── Goalie-level diagnostic: EDM, VGK, MIN, NSH ──\n")
for (tm in c("EDM", "VGK", "MIN", "NSH", "SEA")) {
  cat("\n  --", tm, "goalies --\n")
  tm_goalies <- goalie_output %>% filter(team_abbrev == tm) %>% arrange(desc(coalesce(proj_gp, -1)))
  for (i in seq_len(nrow(tm_goalies))) {
    g <- tm_goalies[i, ]
    cat(sprintf("    %-20s | has_history=%s proj_gp=%.1f proj_wins=%s proj_sv_pct=%s\n",
                substr(coalesce(g$player_name, "?"), 1, 20), g$has_history,
                coalesce(g$proj_gp, NA_real_),
                if (is.null(g$proj_wins) || is.na(g$proj_wins)) "NA" else sprintf("%.1f", g$proj_wins),
                if (is.null(g$proj_sv_pct) || is.na(g$proj_sv_pct)) "NA" else sprintf("%.4f", g$proj_sv_pct)))
  }
}
cat("\n")

# Full per-team diagnostic — every input the shot-based engine actually
# uses, one row per team, sorted by an approximate net quality (shooting %
# minus shots-against-adjusted save advantage) so it's easy to eyeball
# whether a specific team's inputs look wrong.
cat("\n  ── Full team inputs (diagnostic) ──\n")
diag_tbl <- team_off_def %>%
  mutate(approx_quality = round(shots_for_pg * shooting_pct - shots_against_pg * (1 - goalie_sv_pct), 3)) %>%
  arrange(desc(approx_quality))
for (i in seq_len(nrow(diag_tbl))) {
  r <- diag_tbl[i, ]
  def_src <- if (!is.na(r$n_with_onice_def) && r$n_with_onice_def >= 15) "onice" else "proxy"
  cat(sprintf("  %-4s | shots_for=%.1f shooting_pct=%.4f shots_against=%.1f (%s) goalie_sv=%.4f | wowy_off=%+.3f wowy_def=%+.3f | approx_quality=%.3f\n",
              r$team_abbrev, r$shots_for_pg, r$shooting_pct, r$shots_against_pg, def_src, r$goalie_sv_pct,
              coalesce(r$wowy_off_adj_per_game, 0), coalesce(r$wowy_def_adj_per_game, 0), r$approx_quality))
}
cat("\n")

# Roster-weighted xG sanity check — built from each CURRENT roster
# player's own on-ice xG history (onice_xgf_pg_wtd/onice_xga_pg_wtd,
# same TOI-weighted-average construction as the existing Corsi numbers),
# NOT a team-level lookup. A team-level xg_for/xg_against from last
# season would be stuck to last year's roster and blind to any trade or
# free-agent signing since then — this version isn't, for the same
# reason onice_ca_pg_wtd/onice_cf_pg_wtd already aren't.
cat("\n  ── Roster-weighted xG sanity check (current-roster, trade-aware) ──\n")
tryCatch({
  xg_diag <- team_offense %>%
    mutate(xg_diff_pg = onice_xgf_pg_wtd - onice_xga_pg_wtd) %>%
    arrange(desc(xg_diff_pg))
  for (i in seq_len(nrow(xg_diag))) {
    r <- xg_diag[i, ]
    cat(sprintf("  %-4s | onice_xgf_pg_wtd=%.3f onice_xga_pg_wtd=%.3f xg_diff_pg=%+.3f | roster coverage: %d / 18\n",
                r$team_abbrev, r$onice_xgf_pg_wtd, r$onice_xga_pg_wtd, r$xg_diff_pg, r$n_with_onice_xg))
  }
  cat("  Range — onice_xgf_pg_wtd:", round(min(xg_diag$onice_xgf_pg_wtd, na.rm=TRUE),3), "-", round(max(xg_diag$onice_xgf_pg_wtd, na.rm=TRUE),3),
      "| onice_xga_pg_wtd:", round(min(xg_diag$onice_xga_pg_wtd, na.rm=TRUE),3), "-", round(max(xg_diag$onice_xga_pg_wtd, na.rm=TRUE),3), "\n")
  cat("  Spread (best - worst) xg_diff_pg:", round(max(xg_diag$xg_diff_pg, na.rm=TRUE) - min(xg_diag$xg_diff_pg, na.rm=TRUE), 3), "\n")
}, error = function(e) cat("  Diagnostic error:", conditionMessage(e), "\n"))
cat("\n")

# League-wide PP/PK ranking — the EXACT same recency-weighted, 3-year
# team-level rates the simulation actually uses (team_pp_pk_rates),
# sorted so we can directly verify whether a team known for elite special
# teams (e.g. a league-best power play) actually shows up that way here.
cat("\n  ── League-wide PP/PK ranking (recency-weighted, 3yr — what the sim actually uses) ──\n")
tryCatch({
  pp_pk_diag <- team_pp_pk_rates %>% arrange(desc(pp_goals_pg))
  for (i in seq_len(nrow(pp_pk_diag))) {
    r <- pp_pk_diag[i, ]
    cat(sprintf("  %-4s | pp_goals_pg=%.3f (rank %d/%d) | pk_ga_pg=%.3f\n",
                r$team_abbrev, r$pp_goals_pg, i, nrow(pp_pk_diag), r$pk_ga_pg))
  }
  cat("  Range — pp_goals_pg:", round(min(pp_pk_diag$pp_goals_pg, na.rm=TRUE),3), "-", round(max(pp_pk_diag$pp_goals_pg, na.rm=TRUE),3),
      "| pk_ga_pg:", round(min(pp_pk_diag$pk_ga_pg, na.rm=TRUE),3), "-", round(max(pp_pk_diag$pk_ga_pg, na.rm=TRUE),3), "\n")
}, error = function(e) cat("  Diagnostic error:", conditionMessage(e), "\n"))
cat("\n")

# ── DIAGNOSTIC ONLY: what would team-level PP/PK inputs look like if built
# from RAPM instead of the current team-level historical-rate approach?
# NOT fed into the simulation — team_pp_pk_rates (above) is still what
# simulate_games() actually uses. This exists purely to test whether
# RAPM's ridge regularization handles PP/PK's small samples better than
# the earlier, reverted player-level WOWY attempt did — see the note
# where proj_rapm_pppk gets loaded for the full reasoning. If this looks
# stable and sensible across a few runs, that's real evidence worth
# revisiting whether PP/PK RAPM should graduate to actual production use;
# if it shows the same kind of extreme, unstable values the earlier WOWY
# attempt did, that's evidence it shouldn't, at least not without further
# work (e.g. more seasons of data, or a different strength-state handling
# approach).
cat("\n  ── DIAGNOSTIC (not used in simulation): team PP/PK inputs from RAPM vs current team-level approach ──\n")
tryCatch({
  if (is.null(proj_rapm_pppk) || nrow(proj_rapm_pppk) == 0) {
    cat("  No PP/PK RAPM data available for this comparison (proj_rapm_pppk is empty).\n")
  } else {
    pppk_roster <- team_offense_f %>% bind_rows(team_offense_d) %>%
      select(team_abbrev, player_id, rate_toi_min) %>%
      left_join(proj_rapm_pppk, by = "player_id")
    rapm_team_pppk <- pppk_roster %>%
      group_by(team_abbrev) %>%
      summarise(
        # TOI-weighted average, same reasoning as wowy_ev_off_wtd above —
        # this is a shared/context stat, not an individually-authored
        # event, so it gets averaged like WOWY does, not summed like shots.
        pp_rapm_team = sum(coalesce(pp_rapm_3yr, 0) * coalesce(rate_toi_min, 12), na.rm = TRUE) / sum(coalesce(rate_toi_min, 12)[!is.na(pp_rapm_3yr)], na.rm = TRUE),
        pk_rapm_team = sum(coalesce(pk_rapm_3yr, 0) * coalesce(rate_toi_min, 12), na.rm = TRUE) / sum(coalesce(rate_toi_min, 12)[!is.na(pk_rapm_3yr)], na.rm = TRUE),
        n_with_pp_rapm = sum(!is.na(pp_rapm_3yr)),
        n_with_pk_rapm = sum(!is.na(pk_rapm_3yr)),
        .groups = "drop"
      ) %>%
      left_join(team_pp_pk_rates, by = "team_abbrev") %>%
      arrange(desc(pp_rapm_team))
    for (i in seq_len(nrow(rapm_team_pppk))) {
      r <- rapm_team_pppk[i, ]
      cat(sprintf("  %-4s | pp_rapm_team=%+.4f (current pp_goals_pg=%.3f, roster coverage %d/18) | pk_rapm_team=%+.4f (current pk_ga_pg=%.3f, roster coverage %d/18)\n",
                  r$team_abbrev, r$pp_rapm_team, coalesce(r$pp_goals_pg, NA_real_), r$n_with_pp_rapm,
                  r$pk_rapm_team, coalesce(r$pk_ga_pg, NA_real_), r$n_with_pk_rapm))
    }
    cat("  Range — pp_rapm_team:", round(min(rapm_team_pppk$pp_rapm_team, na.rm=TRUE), 4), "to", round(max(rapm_team_pppk$pp_rapm_team, na.rm=TRUE), 4),
        "| pk_rapm_team:", round(min(rapm_team_pppk$pk_rapm_team, na.rm=TRUE), 4), "to", round(max(rapm_team_pppk$pk_rapm_team, na.rm=TRUE), 4), "\n")
    cat("  Compare this spread/stability against team_pp_pk_rates' own range above — wildly extreme\n")
    cat("  values here (like the -33 seen in the earlier, reverted WOWY attempt) would be a sign\n")
    cat("  PP/PK RAPM isn't ready for production use yet either; a reasonable, bounded spread\n")
    cat("  would be evidence it's handling the small-sample problem better than WOWY did.\n")
  }
}, error = function(e) cat("  PP/PK RAPM diagnostic error:", conditionMessage(e), "\n"))
cat("\n")


# no roster reconstruction, no recency-blending across years) for the
# SAME teams — checking whether roster-weighted reconstruction from
# individual players' independent histories is compressing variance
# relative to how extreme real, cohesive team performance actually gets.
# If direct team-level spread is meaningfully wider than the roster-
# weighted spread above, that's the compression showing up concretely.
cat("\n  ── Direct team-level xG comparison (most recent season only, no roster reconstruction) ──\n")
tryCatch({
  most_recent_team_onice <- team_onice_by_season[[length(team_onice_by_season)]]
  if (is.null(most_recent_team_onice) || !"xg_for" %in% names(most_recent_team_onice)) {
    cat("  (most recent season's team_onice.csv doesn't have xg_for/xg_against)\n")
  } else {
    team_xg_direct <- most_recent_team_onice %>%
      mutate(xgf_pg = xg_for / pmax(coalesce(gp_onice, 1), 1),
             xga_pg = xg_against / pmax(coalesce(gp_onice, 1), 1),
             xg_diff_pg = xgf_pg - xga_pg) %>%
      arrange(desc(xg_diff_pg))
    for (i in seq_len(nrow(team_xg_direct))) {
      r <- team_xg_direct[i, ]
      cat(sprintf("  %-4s | xgf_pg=%.3f xga_pg=%.3f xg_diff_pg=%+.3f\n", r$team_abbrev, r$xgf_pg, r$xga_pg, r$xg_diff_pg))
    }
    cat("  Spread (best - worst) xg_diff_pg:", round(max(team_xg_direct$xg_diff_pg, na.rm=TRUE) - min(team_xg_direct$xg_diff_pg, na.rm=TRUE), 3), "\n")
  }
}, error = function(e) cat("  Diagnostic error:", conditionMessage(e), "\n"))
cat("\n")


shots_for_lu     <- setNames(team_off_def$shots_for_pg, team_off_def$team_abbrev)
shots_against_lu <- setNames(team_off_def$shots_against_pg, team_off_def$team_abbrev)
shooting_pct_lu  <- setNames(team_off_def$shooting_pct, team_off_def$team_abbrev)
goalie_sv_lu     <- setNames(team_off_def$goalie_sv_pct, team_off_def$team_abbrev)

# ── Standings-compression correction ────────────────────────────────────────
# Empirically validated (see the Pythagorean-expectation-vs-simulated-win%
# diagnostic below): even though our per-game randomness matches real NHL
# season-to-season variance closely (~8.5 vs. a real 8-10 point SD
# benchmark), the simulation still produces win probabilities that are
# systematically pulled toward 50% relative to an independent Pythagorean
# benchmark — correlation between team quality and the gap was -0.869, a
# very clean, near-monotonic signature, not noise. This is a known
# mathematical consequence of averaging an S-shaped win-probability curve
# over a real, varied schedule (Jensen's inequality): bad teams get pulled
# up, good teams pulled down, roughly in proportion to how extreme they are.
#
# Fix: amplify each team's DEVIATION from league average (5v5 xG and PP/PK
# handled separately, same factor) before it reaches the Poisson simulation,
# rather than touching the simulation's randomness itself (already
# validated as realistic — changing that would break something we
# confirmed is correct to fix something else). League-average teams are
# unaffected (deviation ~0, unchanged regardless of factor).
#
# This is a DELIBERATELY PARTIAL correction, not a full one. The observed
# sim-vs-Pythagorean ratio clustered loosely around 0.4-0.6 (our sim
# currently produces roughly half the separation Pythagorean implies),
# which would suggest ~2x for a full correction — we're using less than
# that because some of the real-world gap plausibly reflects actual
# in-season talent drift (trades, injuries, motivation swings) that a
# fixed-roster, full-season simulation structurally can't capture at all;
# treating the ENTIRE measured gap as pure bias risks overcorrecting.
COMPRESSION_AMPLIFICATION <- 1.4  # dialed back down from 1.75 — a direct backtest against REAL final standings for 3 completed seasons (2023-2025, via season_sim_backtest.R's calibration sweep) found 1.4 minimized RMSE on average, with 1.75 performing noticeably worse, especially in the one backtest season with a full, proper 3-year prior-data window (which alone preferred 1.0, no correction at all). This is direct outcome evidence, a stronger signal than the indirect Pythagorean-correlation tuning that originally justified 1.75 — 1.4 is a middle point that respects both the real backtest evidence (which leans lower) and the independently-validated Jensen's-inequality rationale for some correction (which argues against dropping to 1.0 entirely based on one data point).
amplify_around_mean <- function(x) {
  m <- mean(x, na.rm = TRUE)
  m + COMPRESSION_AMPLIFICATION * (x - m)
}
# Save the ORIGINAL (true, un-amplified) values before correcting — needed
# so the Pythagorean validation check below can compare the CORRECTED
# simulation against TRUE talent, not against an already-amplified number
# (comparing amplified-vs-amplified would test something else entirely and
# give a misleading read on whether the correction actually worked).
xgf_lu_orig      <- setNames(team_off_def$onice_xgf_pg_wtd, team_off_def$team_abbrev)
xga_lu_orig      <- setNames(team_off_def$onice_xga_pg_wtd, team_off_def$team_abbrev)
pp_goals_lu_orig <- setNames(team_off_def$pp_goals_pg, team_off_def$team_abbrev)
pk_ga_lu_orig    <- setNames(team_off_def$pk_ga_pg, team_off_def$team_abbrev)
team_off_def <- team_off_def %>%
  mutate(
    onice_xgf_pg_wtd = amplify_around_mean(onice_xgf_pg_wtd),
    onice_xga_pg_wtd = amplify_around_mean(onice_xga_pg_wtd),
    pp_goals_pg      = amplify_around_mean(pp_goals_pg),
    pk_ga_pg         = amplify_around_mean(pk_ga_pg)
  )
cat("\n  Standings-compression correction applied (amplification factor =", COMPRESSION_AMPLIFICATION,
    ") — all diagnostics below reflect POST-correction inputs (except the Pythagorean\n")
cat("  check, which deliberately uses TRUE pre-correction values as the benchmark).\n")

# xG-based 5v5 inputs (roster-weighted, trade-aware — see the note where
# onice_xgf_pg/onice_xga_pg are built) plus team-level PP/PK goals-per-
# game (direct, not shots*rate — see the note where team_pp_pk_rates is
# built for why that decomposition was avoided).
xgf_lu       <- setNames(team_off_def$onice_xgf_pg_wtd, team_off_def$team_abbrev)
xga_lu       <- setNames(team_off_def$onice_xga_pg_wtd, team_off_def$team_abbrev)
pp_goals_lu  <- setNames(team_off_def$pp_goals_pg, team_off_def$team_abbrev)
pk_ga_lu     <- setNames(team_off_def$pk_ga_pg, team_off_def$team_abbrev)

# Simulates games for parallel vectors of home/away team abbrevs (works for
# a single pair too — used for both the full season schedule, vectorized in
# one call, and individual playoff-series games). Each team's shot rate
# blends its own shot generation with the opponent's shot suppression; goal
# probability per shot is shooting talent adjusted for the opposing
# goaltender relative to league average. Ties go to a simplified OT/shootout
# (a real 600-second sudden-death simulation like HockeyStats does is
# possible but not worth the added complexity here — this keeps the same
# spirit: OT can be won outright, otherwise it's a quality-weighted coinflip).
# Simulates games for parallel vectors of home/away team abbrevs (works for
# a single pair too — used for both the full season schedule, vectorized in
# one call, and individual playoff-series games).
#
# Rewritten to simulate goals directly from expected goals (xG) rather than
# shots-then-shooting%. The old approach treated shot volume and shooting%
# as separate, only weakly-connected inputs — a team could generate a lot
# of shots (a real advantage under the old model) while allowing shots of
# meaningfully higher quality (invisible to the old model, since
# shots_against didn't touch goal probability at all) and the two could
# roughly cancel out with no clear signal either way. xG already combines
# both into one number, so a team that suppresses shot QUALITY (not just
# volume) now shows up correctly. Confirmed directly: a team ranked ~24th
# under the old shots-based approach came out top-4 in the league by
# roster-weighted xG differential, and the gap traced to their opponents'
# shots-against being similar in COUNT but notably higher in average
# danger — exactly what shot-count alone can't see and xG can.
#
# 5v5 uses xG (Poisson) since real player-level, roster-weighted, trade-
# aware xG data exists there. PP/PK still use the older shots-then-rate
# approach (binomial), layered on top additively, since no stable player-
# level PP/PK xG exists yet — shot VOLUME there still comes from the
# roster-weighted, trade-aware onice_pp_sf/onice_pk_sa; only the
# conversion rate falls back to a team-level number (see the note where
# team_pp_pk_rates is built for why).
simulate_games <- function(home_abbrevs, away_abbrevs) {
  n <- length(home_abbrevs)

  # ── 5v5: expected goals directly from xG, adjusted for goaltending ──────
  home_xg_5v5 <- (xgf_lu[home_abbrevs] + xga_lu[away_abbrevs]) / 2
  away_xg_5v5 <- (xgf_lu[away_abbrevs] + xga_lu[home_abbrevs]) / 2
  # Same goalie-relative-to-league-average ratio structure as the old
  # shots-based formula, just applied to xG instead of shots*shooting_pct.
  home_xg_5v5_adj <- home_xg_5v5 * (1 - goalie_sv_lu[away_abbrevs]) / (1 - league_avg_sv_pct)
  away_xg_5v5_adj <- away_xg_5v5 * (1 - goalie_sv_lu[home_abbrevs]) / (1 - league_avg_sv_pct)

  # ── PP/PK: direct goals-per-game (Poisson), not shots*rate ──────────────
  # Deliberately not shots*conversion-rate (what this replaced) — that
  # structure separated shot volume from scoring rate, the same flaw xG
  # fixed for 5v5 (a team's shot-suppression barely affecting the
  # opponent's actual scoring probability). No stable player-level PP/PK
  # xG exists yet to do the identical fix here, so this uses team-level
  # PP-goals-for/PK-goals-against per game directly instead — already one
  # number combining volume and efficiency, same spirit as xG, just real
  # goals instead of expected goals.
  home_pp_xg <- (pp_goals_lu[home_abbrevs] + pk_ga_lu[away_abbrevs]) / 2
  away_pp_xg <- (pp_goals_lu[away_abbrevs] + pk_ga_lu[home_abbrevs]) / 2

  # ── Combine: 5v5 (Poisson, xG-driven) + PP/PK (Poisson, goals-driven) ───
  # HOME_XG_BOOST replaces the old per-shot HOME_GOAL_BOOST — same home-
  # ice-advantage concept, just expressed as a direct expected-goals bump
  # now that goals aren't built from a shots*per-shot-probability chain.
  home_goals_5v5 <- rpois(n, pmax(0.05, home_xg_5v5_adj + HOME_XG_BOOST))
  away_goals_5v5 <- rpois(n, pmax(0.05, away_xg_5v5_adj))
  home_pp_goals  <- rpois(n, pmax(0.01, home_pp_xg))
  away_pp_goals  <- rpois(n, pmax(0.01, away_pp_xg))

  home_goals <- home_goals_5v5 + home_pp_goals
  away_goals <- away_goals_5v5 + away_pp_goals

  tied <- home_goals == away_goals
  if (any(tied)) {
    nt <- sum(tied)
    # ~5 minutes of OT ~ 1/12 of a full game's expected-goals volume.
    total_home_xg <- (home_xg_5v5_adj[tied] + home_pp_xg[tied]) / 12
    total_away_xg <- (away_xg_5v5_adj[tied] + away_pp_xg[tied]) / 12
    ot_h_score <- rpois(nt, pmax(0.01, total_home_xg)) > 0
    ot_a_score <- rpois(nt, pmax(0.01, total_away_xg)) > 0
    home_wins_ot <- ot_h_score & !ot_a_score
    away_wins_ot <- ot_a_score & !ot_h_score
    needs_so <- !(home_wins_ot | away_wins_ot)
    # Same spirit as before (a small, clamped edge based on relative team
    # quality), now built from the xG differential instead of shooting%/
    # save% differences — 0.5 scaling keeps this in a similar ±0.15 range
    # given xG differentials typically run roughly -0.18 to +0.16.
    so_edge <- 0.5 + pmin(0.15, pmax(-0.15,
      ((xgf_lu[home_abbrevs[tied]] - xga_lu[home_abbrevs[tied]]) -
       (xgf_lu[away_abbrevs[tied]] - xga_lu[away_abbrevs[tied]])) * 0.5))
    so_home_wins <- runif(nt) < so_edge
    home_wins_final <- home_wins_ot | (needs_so & so_home_wins)
    idx <- which(tied)
    home_goals[idx[home_wins_final]]  <- home_goals[idx[home_wins_final]] + 1
    away_goals[idx[!home_wins_final]] <- away_goals[idx[!home_wins_final]] + 1
  }
  list(home_goals = home_goals, away_goals = away_goals, went_ot = tied)
}

# ── 6. Schedule template (reuse last season's, remapped to this year's teams) ─
last_season <- season_year - 1L
sched_file  <- paste0(last_season - 1L, "-", substr(as.character(last_season), 3, 4), ".csv")
last_schedule <- gh_read(paste0(GH_GAME_RESULTS, "/", sched_file))
if (is.null(last_schedule)) stop("Could not load schedule template: ", sched_file)

# The game_results CSVs store home_full/away_full (team names), not
# abbreviations — mirrors app.R's load_gh_game_results, which needs the same
# name -> abbrev join. Build that map from live standings plus known
# historical full-name variants (relocations, alternate NHL API naming).
build_name_to_abbrev <- function() {
  base <- data.frame(team_abbrev = character(0), team_full = character(0), stringsAsFactors = FALSE)
  if (!is.null(standings_now) && !is.null(standings_now$standings)) {
    base <- bind_rows(lapply(standings_now$standings, function(s) {
      abbrev <- if (is.list(s$teamAbbrev)) s$teamAbbrev$default %||% NA_character_ else as.character(s$teamAbbrev %||% NA)
      full   <- if (is.list(s$teamName))   s$teamName$default   %||% NA_character_ else as.character(s$teamCommonName$default %||% s$teamFullName %||% NA)
      data.frame(team_abbrev = abbrev, team_full = full, stringsAsFactors = FALSE)
    }))
  }
  aliases <- data.frame(
    team_abbrev = c("UTA","UTA","ARI","PHX","ARI","TBL","TBL","NJD","NJD","SJS","SJS","VGK","VGK"),
    team_full   = c("Arizona Coyotes","Utah Hockey Club","Arizona Coyotes","Arizona Coyotes","Arizona Coyotes",
                    "Tampa Bay Lightning","Tampa Bay","New Jersey Devils","New Jersey",
                    "San Jose Sharks","San Jose","Vegas Golden Knights","Vegas"),
    stringsAsFactors = FALSE
  )
  out <- bind_rows(base, aliases)
  out <- out[!is.na(out$team_abbrev) & !is.na(out$team_full), ]
  out[!duplicated(out$team_full), ]
}
name_to_abbrev <- build_name_to_abbrev()

if (!"home_abbrev" %in% names(last_schedule) && "home_full" %in% names(last_schedule)) {
  last_schedule <- left_join(last_schedule, name_to_abbrev, by = c("home_full" = "team_full"))
  names(last_schedule)[names(last_schedule) == "team_abbrev"] <- "home_abbrev"
}
if (!"away_abbrev" %in% names(last_schedule) && "away_full" %in% names(last_schedule)) {
  last_schedule <- left_join(last_schedule, name_to_abbrev, by = c("away_full" = "team_full"))
  names(last_schedule)[names(last_schedule) == "team_abbrev"] <- "away_abbrev"
}
if (!all(c("home_abbrev", "away_abbrev") %in% names(last_schedule)))
  stop("Schedule template is missing home/away team identifiers after name mapping. Columns present: ",
       paste(names(last_schedule), collapse = ", "))

last_schedule <- last_schedule %>%
  mutate(
    home_abbrev = ifelse(home_abbrev %in% names(abbrev_fix), abbrev_fix[home_abbrev], home_abbrev),
    away_abbrev = ifelse(away_abbrev %in% names(abbrev_fix), abbrev_fix[away_abbrev], away_abbrev)
  ) %>%
  filter(!is.na(home_abbrev), !is.na(away_abbrev),
         home_abbrev %in% names(net_lookup), away_abbrev %in% names(net_lookup))

if (nrow(last_schedule) == 0)
  stop("Schedule template has 0 usable games after filtering to known teams. Check name_to_abbrev mapping.")
cat("  last_schedule rows:", nrow(last_schedule), "\n")

# ── 7. Conference/division map for playoff seeding ──────────────────────────
cd_map <- bind_rows(lapply(standings_now$standings, function(s) {
  ab <- if (is.list(s$teamAbbrev)) s$teamAbbrev$default %||% NA_character_ else as.character(s$teamAbbrev %||% NA)
  data.frame(team_abbrev = ab, conference = s$conferenceName %||% NA_character_,
             division = s$divisionName %||% NA_character_, stringsAsFactors = FALSE)
}))
cd_map$team_abbrev <- ifelse(cd_map$team_abbrev %in% names(abbrev_fix), abbrev_fix[cd_map$team_abbrev], cd_map$team_abbrev)
cd_map <- cd_map %>% distinct(team_abbrev, .keep_all = TRUE)

# ── 8. Simulate N full seasons + playoff brackets ───────────────────────────
cat("Running", N_SIMS, "season simulations (shot-based engine)...\n")

# OT/shootout rate check — a single representative sample (the full
# ~1300-game schedule) rather than tracking this across all 10,000 sims,
# to keep this cheap. Real NHL OT rate is roughly 20-23% of games. If
# ours runs meaningfully higher, that inflates bad teams' point floors
# via guaranteed loser points more than it should, which would show up
# as exactly the "bottom of the league compressed upward" pattern being
# investigated.
tryCatch({
  ot_check <- simulate_games(last_schedule$home_abbrev, last_schedule$away_abbrev)
  ot_rate <- mean(ot_check$went_ot)
  cat("  OT/SO rate check (single sample,", nrow(last_schedule), "games):", round(ot_rate * 100, 1), "% | real NHL rate is roughly 20-23%\n")
}, error = function(e) cat("  OT rate diagnostic error:", conditionMessage(e), "\n"))

# Win-probability sanity check — a specific test of whether the Poisson
# conversion properly reflects a large xG gap, independent of goaltending
# or PP/PK. CHI has by far the worst 5v5 xG in the league (roughly double
# the gap of the next-worst team) while its PP/PK/goaltending are all
# close to league average — so if CHI still comes out anywhere near a
# toss-up here, that's a specific, isolated sign the Poisson math itself
# isn't translating a big xG gap into a correspondingly big win-prob gap,
# separate from the goaltending-spread question checked above.
tryCatch({
  n_check <- 20000
  wp_home <- simulate_games(rep("CAR", n_check), rep("CHI", n_check))
  wp_away <- simulate_games(rep("CHI", n_check), rep("CAR", n_check))
  cat("  Win-prob sanity check — CAR (best 5v5 xG) vs CHI (worst 5v5 xG):\n")
  cat("    CAR at home:", round(mean(wp_home$home_goals > wp_home$away_goals) * 100, 1), "% CAR win |",
      round(mean(wp_home$home_goals) , 2), "vs", round(mean(wp_home$away_goals), 2), "avg goals\n")
  cat("    CHI at home:", round(mean(wp_away$away_goals > wp_away$home_goals) * 100, 1), "% CAR win |",
      round(mean(wp_away$home_goals), 2), "vs", round(mean(wp_away$away_goals), 2), "avg goals\n")
}, error = function(e) cat("  Diagnostic error:", conditionMessage(e), "\n"))

# CAR-vs-CHI is the single most extreme matchup possible (best vs worst) —
# it only tells us the Poisson math is behaving correctly for THAT gap.
# What actually determines a bad team's season point total is their win
# rate against AVERAGE competition, since that's most of their schedule.
# This injects a synthetic "AVG" team at the league-average xG/PP/PK/
# goaltending inputs to test that directly, rather than the extreme case.
tryCatch({
  xgf_lu["AVG"]      <- mean(team_off_def$onice_xgf_pg_wtd, na.rm = TRUE)
  xga_lu["AVG"]      <- mean(team_off_def$onice_xga_pg_wtd, na.rm = TRUE)
  pp_goals_lu["AVG"] <- mean(team_off_def$pp_goals_pg, na.rm = TRUE)
  pk_ga_lu["AVG"]    <- mean(team_off_def$pk_ga_pg, na.rm = TRUE)
  goalie_sv_lu["AVG"]<- mean(team_off_def$goalie_sv_pct, na.rm = TRUE)
  n_check <- 20000
  wp_home2 <- simulate_games(rep("AVG", n_check), rep("CHI", n_check))
  wp_away2 <- simulate_games(rep("CHI", n_check), rep("AVG", n_check))
  cat("  Win-prob sanity check — league-AVERAGE team vs CHI (worst 5v5 xG):\n")
  cat("    AVG at home:", round(mean(wp_home2$home_goals > wp_home2$away_goals) * 100, 1), "% AVG win |",
      round(mean(wp_home2$home_goals), 2), "vs", round(mean(wp_home2$away_goals), 2), "avg goals\n")
  cat("    CHI at home:", round(mean(wp_away2$away_goals > wp_away2$home_goals) * 100, 1), "% AVG win |",
      round(mean(wp_away2$home_goals), 2), "vs", round(mean(wp_away2$away_goals), 2), "avg goals\n")
  # Rough season-points-implied check: if CHI's true win rate against a
  # neutral-quality schedule is p, season points ~ p*2*82 + otl bonus.
  chi_wr <- mean(c(mean(wp_home2$away_goals > wp_home2$home_goals), mean(wp_away2$home_goals > wp_away2$away_goals)))
  cat("    CHI's average win rate vs AVG (both directions):", round(chi_wr * 100, 1), "% -> rough implied points (ignoring OTL bonus):", round(chi_wr * 2 * 82, 1), "\n")
}, error = function(e) cat("  AVG-team diagnostic error:", conditionMessage(e), "\n"))

# League-wide check: our (POST-correction) Poisson-simulated win% vs AVG,
# compared against the Pythagorean expectation formula computed from the
# TRUE, pre-correction GF/GA inputs (not the amplified ones) — this is
# the actual test of whether the correction worked: does our corrected
# simulation now match what Pythagorean says the REAL talent gap should
# produce? Comparing against an already-amplified Pythagorean benchmark
# would be circular (both sides would have moved together) and wouldn't
# actually validate anything.
cat("\n  ── League-wide check: our (corrected) simulated win% vs Pythagorean expectation on TRUE talent ──\n")
tryCatch({
  pytk <- 2.05  # standard hockey-calibrated Pythagorean exponent
  n_check3 <- 4000
  py_rows <- list()
  for (tm in team_off_def$team_abbrev) {
    gf <- coalesce(xgf_lu_orig[tm], NA_real_) + coalesce(pp_goals_lu_orig[tm], NA_real_)
    ga <- coalesce(xga_lu_orig[tm], NA_real_) + coalesce(pk_ga_lu_orig[tm], NA_real_)
    if (is.na(gf) || is.na(ga)) next
    pyth_wp <- gf^pytk / (gf^pytk + ga^pytk)
    wh <- simulate_games(rep(tm, n_check3), rep("AVG", n_check3))
    wa <- simulate_games(rep("AVG", n_check3), rep(tm, n_check3))
    sim_wp <- mean(c(mean(wh$home_goals > wh$away_goals), mean(wa$away_goals > wa$home_goals)))
    py_rows[[tm]] <- data.frame(team_abbrev = tm, pyth_wp = pyth_wp, sim_wp = sim_wp, gap = sim_wp - pyth_wp)
  }
  py_df <- do.call(rbind, py_rows) %>% arrange(desc(gap))
  for (i in seq_len(nrow(py_df))) {
    r <- py_df[i, ]
    cat(sprintf("  %-4s | pyth_wp(TRUE)=%.3f sim_wp(corrected)=%.3f gap=%+.3f\n", r$team_abbrev, r$pyth_wp, r$sim_wp, r$gap))
  }
  cat("  Mean gap:", round(mean(py_df$gap), 4), "| if bad teams (low pyth_wp) cluster toward positive gap\n")
  cat("  and good teams (high pyth_wp) cluster toward negative gap, that's residual compression.\n")
  cat("  Correlation between pyth_wp(TRUE) and gap:", round(cor(py_df$pyth_wp, py_df$gap), 3),
      "(closer to 0 = correction working; this should be smaller in magnitude\n")
  cat("  than the PRE-correction run's -0.869, since this now tests corrected-sim vs TRUE talent.)\n")
}, error = function(e) cat("  Pythagorean diagnostic error:", conditionMessage(e), "\n"))

# Checking whether CHI's REAL schedule opponents are actually weaker than
# league average (which would explain part of the gap between the vs-AVG
# implied estimate and the full simulation's actual output) rather than
# assuming this without checking.
tryCatch({
  chi_games <- last_schedule %>% filter(home_abbrev == "CHI" | away_abbrev == "CHI")
  chi_opponents <- ifelse(chi_games$home_abbrev == "CHI", chi_games$away_abbrev, chi_games$home_abbrev)
  opp_xg_diff <- (xgf_lu[chi_opponents] - xga_lu[chi_opponents])
  cat("  CHI's real schedule — opponent quality check:\n")
  cat("    Games scheduled:", length(chi_opponents), "| avg opponent xg_diff_pg:", round(mean(opp_xg_diff, na.rm = TRUE), 4),
      "(league avg is ~0 by construction; negative here would mean CHI's real schedule is softer than average)\n")

  # Directly running CHI's actual full schedule through simulate_one_season_pts()-
  # style logic in isolation, many times, to get CHI's own point total distribution
  # WITHOUT the noise of also tracking all 31 other teams and playoff brackets —
  # a direct check of whether the full-schedule number matches the implied
  # ~78 from the vs-AVG win rate, or diverges from it.
  n_check2 <- 2000
  chi_pts_samples <- numeric(n_check2)
  for (i in seq_len(n_check2)) {
    g <- simulate_games(chi_games$home_abbrev, chi_games$away_abbrev)
    chi_home <- chi_games$home_abbrev == "CHI"
    home_win <- g$home_goals > g$away_goals
    pts <- ifelse(chi_home, ifelse(home_win, 2, ifelse(g$went_ot, 1, 0)),
                            ifelse(!home_win, 2, ifelse(g$went_ot, 1, 0)))
    chi_pts_samples[i] <- sum(pts)
  }
  cat("    CHI's own full-schedule simulated points (avg over", n_check2, "sims, before the 84-game scaling):",
      round(mean(chi_pts_samples), 1), "| scaled to 84 games:", round(mean(chi_pts_samples) * 1.0244, 1), "\n")
}, error = function(e) cat("  Schedule-mix diagnostic error:", conditionMessage(e), "\n"))

simulate_one_season_pts <- function() {
  g <- simulate_games(last_schedule$home_abbrev, last_schedule$away_abbrev)
  home_win <- g$home_goals > g$away_goals
  pts <- setNames(rep(0, length(net_lookup)), names(net_lookup))
  for (i in seq_len(nrow(last_schedule))) {
    h <- last_schedule$home_abbrev[i]; a <- last_schedule$away_abbrev[i]
    if (home_win[i]) { pts[h] <- pts[h] + 2; if (g$went_ot[i]) pts[a] <- pts[a] + 1 }
    else             { pts[a] <- pts[a] + 2; if (g$went_ot[i]) pts[h] <- pts[h] + 1 }
  }
  pts
}

simulate_bracket_conf <- function(seeds8) {
  # Common 1v8/4v5/3v6/2v7 approximation, not the NHL's exact division-runner-up rule.
  win <- function(hi, lo) { g <- simulate_games(hi, lo); if (g$home_goals > g$away_goals) hi else lo }
  r1 <- c(win(seeds8[1], seeds8[8]), win(seeds8[4], seeds8[5]), win(seeds8[3], seeds8[6]), win(seeds8[2], seeds8[7]))
  r2 <- c(win(r1[1], r1[2]), win(r1[3], r1[4]))
  win(r2[1], r2[2])
}

playoff_hits <- setNames(integer(length(net_lookup)), names(net_lookup))
cup_hits     <- setNames(integer(length(net_lookup)), names(net_lookup))
pts_sum      <- setNames(numeric(length(net_lookup)), names(net_lookup))
car_pts_trace <- numeric(N_SIMS)  # tracking per-sim points for a variance check — see below the main loop
chi_pts_trace <- numeric(N_SIMS)

for (i in seq_len(N_SIMS)) {
  pts_i <- simulate_one_season_pts()
  pts_sum <- pts_sum + pts_i[names(pts_sum)]
  if ("CAR" %in% names(pts_i)) car_pts_trace[i] <- pts_i["CAR"]
  if ("CHI" %in% names(pts_i)) chi_pts_trace[i] <- pts_i["CHI"]

  df <- data.frame(team_abbrev = names(pts_i), pts = as.numeric(pts_i), stringsAsFactors = FALSE) %>%
    left_join(cd_map, by = "team_abbrev")

  conf_champs <- c()
  for (conf in unique(na.omit(df$conference))) {
    cdf <- df %>% filter(conference == conf)
    div_top3 <- cdf %>% group_by(division) %>% arrange(desc(pts)) %>% slice_head(n = 3) %>% ungroup()
    wc <- cdf %>% filter(!team_abbrev %in% div_top3$team_abbrev) %>% arrange(desc(pts)) %>% slice_head(n = 2)
    seeds <- bind_rows(div_top3, wc) %>% arrange(desc(pts)) %>% pull(team_abbrev)
    if (length(seeds) < 8) next
    playoff_hits[seeds[1:8]] <- playoff_hits[seeds[1:8]] + 1L
    conf_champs <- c(conf_champs, simulate_bracket_conf(seeds[1:8]))
  }
  if (length(conf_champs) == 2) {
    g <- simulate_games(conf_champs[1], conf_champs[2])
    cup_winner <- if (g$home_goals > g$away_goals) conf_champs[1] else conf_champs[2]
    cup_hits[cup_winner] <- cup_hits[cup_winner] + 1L
  }
}


# Season-points variance check — testing whether our per-game randomness
# produces realistic season-long luck variance. This matters directly
# for the standings-compression question: win probability is an S-shaped
# (sigmoid-like) function of the quality gap between two teams — steep
# near an even matchup, flattening out at the extremes. By Jensen's
# inequality, averaging that curve over a REAL, varied 82-game schedule
# (rather than one hypothetical "average" opponent) pulls bad teams'
# results up and good teams' results down relative to what their
# underlying quality gap alone would suggest — this is a real, expected
# effect in any low-scoring sport, but its MAGNITUDE depends on how much
# game-to-game randomness the model has. If our per-game variance is
# inflated relative to real NHL games, this compression effect gets
# amplified beyond what actual standings show. Published research on
# NHL season-to-season "luck" variance (points earned by a team of FIXED
# true talent, isolated from real talent changes) puts the standard
# deviation at roughly 8-10 points across an 82-84 game season. If ours
# comes out meaningfully higher, that's a concrete, checkable sign our
# per-game randomness itself needs dampening — not just a philosophical
# question about whether hockey has parity.
cat("\n  ── Season-points variance check (CAR, CHI — is our per-game randomness realistic?) ──\n")
tryCatch({
  car_sd <- sd(car_pts_trace, na.rm = TRUE)
  chi_sd <- sd(chi_pts_trace, na.rm = TRUE)
  cat("  CAR: mean =", round(mean(car_pts_trace, na.rm = TRUE), 1), "| SD =", round(car_sd, 2), "\n")
  cat("  CHI: mean =", round(mean(chi_pts_trace, na.rm = TRUE), 1), "| SD =", round(chi_sd, 2), "\n")
  cat("  Real NHL season-to-season 'luck' variance (fixed true talent) is roughly 8-10 points SD.\n")
  cat("  If ours runs meaningfully higher, per-game randomness is likely inflated, amplifying the\n")
  cat("  standings-compression effect discussed above beyond what real parity would produce.\n")
}, error = function(e) cat("  Variance diagnostic error:", conditionMessage(e), "\n"))


# Games-per-team in the borrowed schedule template (should be 82 for a normal
# season, but computed rather than assumed in case the source season was
# shortened for some reason). The real upcoming season is NHL_GAMES (84) —
# scale the reported point totals up proportionally so they reflect a full
# 84-game season rather than the 82-game template we actually simulated.
# Playoff/cup odds are left as-is: they're based on each simulated season's
# RELATIVE standings, which 2 extra games out of 84 barely moves.
schedule_games_per_team <- mean(table(c(last_schedule$home_abbrev, last_schedule$away_abbrev)))
pts_scale <- NHL_GAMES / schedule_games_per_team
cat("  Schedule template games/team:", round(schedule_games_per_team, 1),
    "| scaling proj_points to", NHL_GAMES, "-game season (x", round(pts_scale, 4), ")\n")

# ── Backtest calibration: solve for the amplification factor directly ──────
# against REAL, KNOWN final standings, instead of guessing values and
# eyeballing against a third-party site. Sweeps a range of candidate
# factors, re-simulating a season for each and comparing projected points
# against the real final points every team actually earned.
if (IS_BACKTEST) {
  cat("\n\n═══ BACKTEST CALIBRATION MODE — season", season_year, "═══\n")
  cat("Fetching REAL final standings to calibrate against...\n")
  # Querying a date well after the regular season ends (this season's late
  # April) returns each team's actual final regular-season points as of
  # that date — a reasonable, simple way to get the real final table.
  standings_date <- paste0(season_year, "-04-25")
  real_standings_raw <- nhl_get(paste0("https://api-web.nhle.com/v1/standings/", standings_date))
  real_standings <- NULL
  if (!is.null(real_standings_raw) && !is.null(real_standings_raw$standings)) {
    real_standings <- bind_rows(lapply(real_standings_raw$standings, function(s) {
      data.frame(
        team_abbrev = tryCatch(s$teamAbbrev$default %||% NA_character_, error = function(e) NA_character_),
        real_points = tryCatch(as.numeric(s$points %||% NA), error = function(e) NA_real_),
        stringsAsFactors = FALSE
      )
    }))
  }
  if (is.null(real_standings) || nrow(real_standings) == 0 || all(is.na(real_standings$real_points))) {
    cat("  Could not fetch real standings for this date — aborting calibration.\n")
  } else {
    real_standings <- real_standings %>% filter(!is.na(team_abbrev), !is.na(real_points))
    cat("  Fetched real final standings for", nrow(real_standings), "teams.\n")
    candidate_factors <- c(1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.5)
    n_calib_sims <- 1500  # reduced from N_SIMS — we need a reasonable RMSE estimate per factor, not precise playoff odds, and this runs once per candidate
    calib_rows <- list()
    for (fac in candidate_factors) {
      m_xgf <- mean(xgf_lu_orig, na.rm = TRUE); m_xga <- mean(xga_lu_orig, na.rm = TRUE)
      m_pp  <- mean(pp_goals_lu_orig, na.rm = TRUE); m_pk <- mean(pk_ga_lu_orig, na.rm = TRUE)
      xgf_lu      <- m_xgf + fac * (xgf_lu_orig - m_xgf)
      xga_lu      <- m_xga + fac * (xga_lu_orig - m_xga)
      pp_goals_lu <- m_pp  + fac * (pp_goals_lu_orig - m_pp)
      pk_ga_lu    <- m_pk  + fac * (pk_ga_lu_orig - m_pk)
      pts_sum_c <- setNames(numeric(length(net_lookup)), names(net_lookup))
      for (i in seq_len(n_calib_sims)) {
        pts_i <- simulate_one_season_pts()
        pts_sum_c <- pts_sum_c + pts_i[names(pts_sum_c)]
      }
      proj_c <- (pts_sum_c[names(net_lookup)] / n_calib_sims) * pts_scale
      cmp <- data.frame(team_abbrev = names(proj_c), proj = as.numeric(proj_c), stringsAsFactors = FALSE) %>%
        inner_join(real_standings, by = "team_abbrev")
      rmse <- sqrt(mean((cmp$proj - cmp$real_points)^2, na.rm = TRUE))
      mae  <- mean(abs(cmp$proj - cmp$real_points), na.rm = TRUE)
      calib_rows[[as.character(fac)]] <- data.frame(factor = fac, rmse = rmse, mae = mae, n_teams = nrow(cmp))
      cat("  factor =", fac, "| RMSE =", round(rmse, 2), "| MAE =", round(mae, 2), "(", nrow(cmp), "teams matched)\n")
    }
    calib_df <- do.call(rbind, calib_rows) %>% arrange(rmse)
    cat("\n  ── Ranked by RMSE (lower = better) ──\n")
    print(calib_df, row.names = FALSE)
    cat("\n  Best factor by RMSE:", calib_df$factor[1], "(RMSE =", round(calib_df$rmse[1], 2), ", MAE =", round(calib_df$mae[1], 2), ")\n")
  }
  cat("\nBacktest calibration complete — not writing to", OUT_DIR, "(this run produces no real projection).\n")
  quit(save = "no", status = 0)
}

results <- data.frame(
  team_abbrev = names(net_lookup),
  proj_points = round((pts_sum[names(net_lookup)] / N_SIMS) * pts_scale, 1),
  playoff_pct = round(100 * playoff_hits[names(net_lookup)] / N_SIMS, 1),
  cup_pct     = round(100 * cup_hits[names(net_lookup)] / N_SIMS, 1),
  stringsAsFactors = FALSE
) %>% arrange(desc(proj_points))

# skater_output and goalie_output were already built earlier (before the
# shot-based simulation engine needed them) — nothing further to do here.

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
# Filename MUST match app.R's own season_year variable (app_season_year here),
# not the `season_year` used above for projection math — see the note near
# the top of this script for why those two differ during July/August.
out_file <- file.path(OUT_DIR, paste0(app_season_year, ".json"))
write(toJSON(list(
  season = season_year,   # the season actually being projected, e.g. 2027 = "2026-27"
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  n_sims = N_SIMS,
  results = results,
  skaters = skater_output,
  goalies = goalie_output
), auto_unbox = TRUE, pretty = TRUE, na = "null"), out_file)

cat("Wrote", out_file, "(projecting season", season_year, ")\n")
