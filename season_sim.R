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
#   1. Team strength = recency-weighted historical net rating + a roster-quality
#      nudge from averaged player values. This is NOT the full SOM/XGBoost
#      pipeline from the main app (that needs team-style features that don't
#      exist yet for a roster that hasn't played a game). It's a reasonable
#      preseason approximation, not a like-for-like match to in-season sims.
#   2. Schedule template = last season's actual schedule, abbrev-mapped onto
#      this year's teams. Next season's real schedule usually isn't public
#      this early. Franchise relocations (e.g. ARI->UTA) are patched via
#      `abbrev_fix` below — extend that list if another team moves/renames.
#   3. Playoff seeding tiebreaker is points only (no reg-wins tiebreak) and
#      wild-card reseeding uses the common 1v8/2v7/3v6/4v5 approximation
#      within each conference, not the NHL's exact division-runner-up rule.
#   4. `resid_sd` (game-margin standard deviation) is hardcoded to a typical
#      NHL value. If your main app's `xgb_rmse`/`resid_sd` is available
#      elsewhere in the repo, swap it in for consistency.
# =============================================================================

suppressMessages({
  library(dplyr)
  library(httr)
  library(jsonlite)
})

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a)) a else b

GH_STATS        <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/stats"
GH_EDGE         <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/edge"
GH_GAME_RESULTS <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/game_results"
OUT_DIR         <- "data/season_sim"
N_SIMS          <- 10000L
resid_sd        <- 1.9   # approx NHL per-game margin SD; replace with your model's resid_sd if you export it

current_year  <- as.integer(format(Sys.Date(), "%Y"))
current_month <- as.integer(format(Sys.Date(), "%m"))
season_year   <- ifelse(current_month >= 9, current_year + 1L, current_year)
szn_folder    <- function(s) paste0(s - 1L, "-", substr(as.character(s), 3, 4))
szn_id        <- function(szn) paste0(szn - 1L, szn)

nhl_get <- function(url, timeout_s = 25) {
  resp <- tryCatch(GET(url, timeout(timeout_s)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE),
           error = function(e) NULL)
}
gh_read <- function(url) tryCatch(read.csv(url(url), stringsAsFactors = FALSE), error = function(e) NULL)

# ── 0. Bail out if we're outside the projection window ──────────────────────
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

# ── 1. Season window for player/team averaging ───────────────────────────────
historical_seasons <- seq(2022L, season_year - 1L)
recent3 <- tail(historical_seasons, 3L)
cat("Averaging player/team values over seasons:", paste(recent3, collapse = ", "), "\n")

# Weights: most-recent season weighted highest. For n seasons present,
# weights are 1,2,...,n normalized — e.g. 3 seasons -> 1/6, 2/6, 3/6.
# A player with fewer seasons just gets fewer weight terms, i.e. a plain
# average of however many seasons they have (1 or 2).
recency_weights <- function(seasons_present) {
  n <- length(seasons_present)
  if (n == 0) return(numeric(0))
  w <- seq_len(n)
  w / sum(w)
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
      proj_val    = { w <- recency_weights(season); sum(w * weighted_val) },
      .groups = "drop"
    )
}

cat("Computing skater values...\n")
skater_hist          <- load_all_skater_vals(recent3)
skater_vals_by_season <- compute_player_vals(skater_hist)
proj_skater_vals      <- project_player_vals(skater_vals_by_season)

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
goalie_vals_by_season <- compute_goalie_vals(goalie_hist)
proj_goalie_vals      <- if (!is.null(goalie_vals_by_season))
  project_player_vals(goalie_vals_by_season %>% mutate(position = "G")) else NULL

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

cat("Computing recency-weighted team ratings...\n")
team_hist <- load_team_stats(recent3)
lg_by_season <- team_hist %>% group_by(season) %>% summarise(lg_gf = mean(gf_pg, na.rm = TRUE), lg_ga = mean(ga_pg, na.rm = TRUE), .groups = "drop")
team_hist <- team_hist %>% left_join(lg_by_season, by = "season") %>%
  mutate(off_rtg = gf_pg - lg_gf, def_rtg = lg_ga - ga_pg, net_rtg = off_rtg + def_rtg)

proj_team_ratings <- team_hist %>%
  filter(!is.na(team_abbrev), nchar(team_abbrev) > 0) %>%
  group_by(team_abbrev) %>% arrange(season, .by_group = TRUE) %>%
  summarise(n_seasons = dplyr::n(), proj_net = { w <- recency_weights(season); sum(w * net_rtg) }, .groups = "drop")

# ── 5. Fetch upcoming rosters & compute roster-quality adjustment ──────────
fetch_team_abbrevs <- function() {
  if (is.null(standings_now) || is.null(standings_now$standings)) return(character(0))
  sapply(standings_now$standings, function(s) if (is.list(s$teamAbbrev)) s$teamAbbrev$default %||% NA_character_ else as.character(s$teamAbbrev %||% NA))
}
team_abbrevs <- unique(na.omit(fetch_team_abbrevs()))
cat("Fetching rosters for", length(team_abbrevs), "teams...\n")

fetch_roster <- function(abbrev, szn) {
  raw <- nhl_get(paste0("https://api-web.nhle.com/v1/roster/", abbrev, "/", szn_id(szn)))
  if (is.null(raw)) return(NULL)
  groups <- c("forwards", "defensemen", "goalies")
  bind_rows(lapply(groups, function(g) {
    plist <- raw[[g]] %||% list()
    bind_rows(lapply(plist, function(p) {
      data.frame(team_abbrev = abbrev, player_id = as.character(p$id %||% NA),
                 stringsAsFactors = FALSE)
    }))
  }))
}

all_rosters <- bind_rows(lapply(team_abbrevs, function(a) {
  r <- tryCatch(fetch_roster(a, season_year), error = function(e) NULL)
  Sys.sleep(0.1)
  r
}))

roster_strength <- function(rosters, proj_skaters, proj_goalies) {
  combined <- bind_rows(
    if (!is.null(proj_skaters)) proj_skaters %>% mutate(is_goalie = FALSE) else NULL,
    if (!is.null(proj_goalies)) proj_goalies %>% mutate(is_goalie = TRUE)  else NULL
  )
  rosters %>%
    left_join(combined, by = "player_id") %>%
    mutate(proj_val = coalesce(proj_val, 0)) %>%
    group_by(team_abbrev) %>%
    summarise(
      skater_strength = { v <- sort(proj_val[!is_goalie], decreasing = TRUE); sum(v[seq_len(min(8, length(v)))], na.rm = TRUE) },
      goalie_strength = { v <- sort(proj_val[is_goalie],  decreasing = TRUE); sum(v[seq_len(min(2, length(v)))], na.rm = TRUE) },
      .groups = "drop"
    )
}
roster_str <- roster_strength(all_rosters, proj_skater_vals, proj_goalie_vals)

lg_skater <- mean(roster_str$skater_strength, na.rm = TRUE)
lg_goalie <- mean(roster_str$goalie_strength, na.rm = TRUE)
sd_skater <- sd(roster_str$skater_strength, na.rm = TRUE); if (is.na(sd_skater) || sd_skater == 0) sd_skater <- 1
sd_goalie <- sd(roster_str$goalie_strength, na.rm = TRUE); if (is.na(sd_goalie) || sd_goalie == 0) sd_goalie <- 1
ROSTER_SWING <- 1.0  # max approx +/- goals/game nudge from roster quality vs history — tune as needed

team_proj <- proj_team_ratings %>%
  full_join(roster_str, by = "team_abbrev") %>%
  filter(!is.na(team_abbrev)) %>%
  mutate(
    skater_z   = coalesce((skater_strength - lg_skater) / sd_skater, 0),
    goalie_z   = coalesce((goalie_strength - lg_goalie) / sd_goalie, 0),
    roster_adj = skater_z * ROSTER_SWING * 0.5 + goalie_z * ROSTER_SWING * 0.5,
    final_net  = coalesce(proj_net, 0) + roster_adj
  )
net_lookup <- setNames(team_proj$final_net, team_proj$team_abbrev)

# ── 6. Schedule template (reuse last season's, remapped to this year's teams) ─
abbrev_fix <- c("ARI" = "UTA", "PHX" = "UTA")  # extend if another franchise relocates/renames
last_season <- season_year - 1L
sched_file  <- paste0(last_season - 1L, "-", substr(as.character(last_season), 3, 4), ".csv")
last_schedule <- gh_read(paste0(GH_GAME_RESULTS, "/", sched_file))
if (is.null(last_schedule)) stop("Could not load schedule template: ", sched_file)

last_schedule <- last_schedule %>%
  mutate(
    home_abbrev = ifelse(home_abbrev %in% names(abbrev_fix), abbrev_fix[home_abbrev], home_abbrev),
    away_abbrev = ifelse(away_abbrev %in% names(abbrev_fix), abbrev_fix[away_abbrev], away_abbrev)
  ) %>%
  filter(home_abbrev %in% names(net_lookup), away_abbrev %in% names(net_lookup))

# ── 7. Conference/division map for playoff seeding ──────────────────────────
cd_map <- bind_rows(lapply(standings_now$standings, function(s) {
  ab <- if (is.list(s$teamAbbrev)) s$teamAbbrev$default %||% NA_character_ else as.character(s$teamAbbrev %||% NA)
  data.frame(team_abbrev = ab, conference = s$conferenceName %||% NA_character_,
             division = s$divisionName %||% NA_character_, stringsAsFactors = FALSE)
}))
cd_map$team_abbrev <- ifelse(cd_map$team_abbrev %in% names(abbrev_fix), abbrev_fix[cd_map$team_abbrev], cd_map$team_abbrev)
cd_map <- cd_map %>% distinct(team_abbrev, .keep_all = TRUE)

# ── 8. Simulate N full seasons + playoff brackets ───────────────────────────
cat("Running", N_SIMS, "season simulations...\n")

simulate_one_season_pts <- function() {
  diffs <- net_lookup[last_schedule$home_abbrev] - net_lookup[last_schedule$away_abbrev] + 0.15  # small home-ice edge
  margins <- rnorm(nrow(last_schedule), mean = diffs, sd = resid_sd)
  home_win <- margins > 0
  pts <- setNames(rep(0, length(net_lookup)), names(net_lookup))
  is_close <- abs(margins) < 0.6  # treat close games as OT/SO (loser gets a point)
  for (i in seq_len(nrow(last_schedule))) {
    h <- last_schedule$home_abbrev[i]; a <- last_schedule$away_abbrev[i]
    if (home_win[i]) { pts[h] <- pts[h] + 2; if (is_close[i]) pts[a] <- pts[a] + 1 }
    else             { pts[a] <- pts[a] + 2; if (is_close[i]) pts[h] <- pts[h] + 1 }
  }
  pts
}

simulate_bracket_conf <- function(seeds8) {
  # Common 1v8/4v5/3v6/2v7 approximation, not the NHL's exact division-runner-up rule.
  win <- function(hi, lo) { wp <- pnorm((net_lookup[hi] - net_lookup[lo]) / resid_sd); if (runif(1) < wp) hi else lo }
  r1 <- c(win(seeds8[1], seeds8[8]), win(seeds8[4], seeds8[5]), win(seeds8[3], seeds8[6]), win(seeds8[2], seeds8[7]))
  r2 <- c(win(r1[1], r1[2]), win(r1[3], r1[4]))
  win(r2[1], r2[2])
}

playoff_hits <- setNames(integer(length(net_lookup)), names(net_lookup))
cup_hits     <- setNames(integer(length(net_lookup)), names(net_lookup))
pts_sum      <- setNames(numeric(length(net_lookup)), names(net_lookup))

for (i in seq_len(N_SIMS)) {
  pts_i <- simulate_one_season_pts()
  pts_sum <- pts_sum + pts_i[names(pts_sum)]
  
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
    wp <- pnorm((net_lookup[conf_champs[1]] - net_lookup[conf_champs[2]]) / resid_sd)
    cup_winner <- if (runif(1) < wp) conf_champs[1] else conf_champs[2]
    cup_hits[cup_winner] <- cup_hits[cup_winner] + 1L
  }
}

# ── 9. Assemble & write output ───────────────────────────────────────────────
results <- data.frame(
  team_abbrev = names(net_lookup),
  proj_points = round(pts_sum[names(net_lookup)] / N_SIMS, 1),
  playoff_pct = round(100 * playoff_hits[names(net_lookup)] / N_SIMS, 1),
  cup_pct     = round(100 * cup_hits[names(net_lookup)] / N_SIMS, 1),
  stringsAsFactors = FALSE
) %>% arrange(desc(proj_points))

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(OUT_DIR, paste0(season_year, ".json"))
write(toJSON(list(
  season = season_year,
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  n_sims = N_SIMS,
  results = results
), auto_unbox = TRUE, pretty = TRUE), out_file)

cat("Wrote", out_file, "\n")