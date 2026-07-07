# =============================================================================
# onice_stats.R — Daily incremental on-ice reconstruction from NHL
# play-by-play + shift-chart data.
#
# THREE OUTPUT FILES per season:
#   - team_onice.csv:   TEAM-level CF/CA/GF/GA at 5v5, PP goals/shots,
#                        PK goals/shots against, TOI by strength.
#   - goalie_onice.csv: GOALIE-level shots faced / goals allowed at 5v5
#                        and while shorthanded (PK).
#   - skater_onice.csv: SKATER-level on-ice Corsi/Goals at 5v5, individual
#                        EV/PP scoring, on-ice PK goals-against.
#
# IMPORTANT ARCHITECTURE NOTE: team- and goalie-level stats are computed
# DIRECTLY from play-by-play alone — every shot/goal event already carries
# eventOwnerTeamId (which team) and goalieInNetId (which goalie was facing
# it), so NO shift-chart data is needed for those two files. Only
# SKATER-level on-ice attribution needs shifts (to know who else was on the
# ice). This matters because the NHL's shift-chart feed has real, recurring
# gaps (see the header note below) — team and goalie stats have ZERO gaps
# as a result of this design, even for games where skater-level data has to
# be skipped.
#
# HONEST CAVEATS (read these before debugging a discrepancy):
#   - situationCode format ("1551" etc: [awayGoalie][awaySkaters][homeSkaters]
#     [homeGoalie]) and play-by-play field names (eventOwnerTeamId,
#     goalieInNetId, scoringPlayerId, assist1PlayerId, assist2PlayerId) have
#     been CONFIRMED against real live API responses during development —
#     these are not guesses.
#   - The shift-chart endpoint (api.nhle.com/stats/rest/en/shiftcharts) has
#     a real, recurring gap of missing games each season (confirmed via
#     testing — a multi-hundred-game stretch with clean valid-but-empty
#     responses, with data intact before and after). This is NOT a bug in
#     this script and is not fixable by changing the query — it appears to
#     be a genuine upstream data gap. Affected games simply don't get
#     skater-level on-ice stats; team/goalie stats are unaffected.
#   - PK "shots against"/"goals against" logic doesn't finely distinguish
#     the rare case of a shorthanded goal scored BY the penalty-killing team
#     (a true "shorthanded goal") — this edge case is simplified rather than
#     precisely tracked, since it's uncommon and not central to what this
#     data feeds (team shot-rate simulation, PP/PK percentile rankings).
#   - Delayed penalties, own-goals, and penalty-shot goals fall into "other"
#     situationCode buckets and simply won't count toward 5v5/PP/PK totals
#     — the safe (if slightly conservative) behavior.
#   - Goalies are excluded from all SKATER on-ice accumulation.
#
# BACKFILL BEHAVIOR: this script scans the WHOLE season so far (not just a
# few recent days) in backfill mode, bounded to that season's actual end
# date (not always today) so backfilling an older season doesn't sweep in
# later seasons' games too. No per-run cap in backfill mode — a full season
# processes in one run (can take 30-90+ minutes). Daily mode only looks back
# 4 days and always targets the current season.
# =============================================================================

suppressMessages({
  library(dplyr)
  library(httr)
  library(jsonlite)
  library(xgboost)
})

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# ── Mode: "daily" (default) only looks back a few days and targets the
# CURRENT season. "backfill" scans an entire season's schedule (optionally
# for a past season via --season=YYYY) and is meant to be triggered
# manually. See onice_daily.yml and onice_backfill.yml.
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^--", name, "="), "", hit[1])
}
MODE <- get_arg("mode", "daily")
SEASON_ARG <- get_arg("season", NA_character_)

current_year  <- as.integer(format(Sys.Date(), "%Y"))
current_month <- as.integer(format(Sys.Date(), "%m"))
season_year <- if (!is.na(SEASON_ARG)) as.integer(SEASON_ARG) else
  ifelse(current_month >= 9, current_year + 1L, current_year)

cat("Mode:", MODE, "| Target season:", season_year, "\n")

OUT_DIR     <- file.path("data", "onice", as.character(season_year))
STATE_FILE  <- file.path(OUT_DIR, "processed_games.txt")
SKATER_OUT  <- file.path(OUT_DIR, "skater_onice.csv")
TEAM_OUT    <- file.path(OUT_DIR, "team_onice.csv")
GOALIE_OUT  <- file.path(OUT_DIR, "goalie_onice.csv")
SHOTS_RAW_OUT <- file.path(OUT_DIR, "shots_raw.csv")  # event-level shot data for future xG training — one row per shot attempt

# ── xG model loading and scoring ────────────────────────────────────────────
# Reads the model trained by train_xg_model.R (a local file read, not a
# GitHub fetch, since this script runs with the repo already checked out
# via GitHub Actions — same working directory both scripts write/read
# relative to). If no model has been trained yet, shots simply get scored
# as NA rather than erroring — everything downstream (goalie GSAx) treats
# NA gracefully rather than breaking.
XG_MODEL_PATH <- file.path("data", "xg_model", "xg_model.rds")
xg_model_obj <- if (file.exists(XG_MODEL_PATH)) tryCatch(readRDS(XG_MODEL_PATH), error = function(e) NULL) else NULL
if (is.null(xg_model_obj)) {
  cat("No trained xG model found at", XG_MODEL_PATH, "— shots will be written without xg scores, goalie GSAx will be NA.\n")
} else {
  cat("Loaded xG model (trained", as.character(xg_model_obj$trained_at), "| seasons:", paste(xg_model_obj$seasons, collapse=", "), ")\n")
}

# Replicates train_xg_model.R's feature engineering EXACTLY — any drift
# between this and the training script would silently produce wrong scores,
# so if the training script's feature engineering ever changes, this needs
# to change with it.
score_shots_with_xg <- function(shots_df, xg_obj) {
  if (is.null(xg_obj) || is.null(shots_df) || nrow(shots_df) == 0) {
    if (!is.null(shots_df)) shots_df$xg <- NA_real_
    return(shots_df)
  }
  s <- shots_df %>%
    mutate(
      is_home_shooter = (owner_team_id == home_id),
      target_side = case_when(
        is_home_shooter  & home_defending_side == "left"  ~ "right",
        is_home_shooter  & home_defending_side == "right" ~ "left",
        !is_home_shooter & home_defending_side == "left"  ~ "left",
        !is_home_shooter & home_defending_side == "right" ~ "right",
        TRUE ~ NA_character_
      ),
      norm_x = ifelse(target_side == "right", x_coord, -x_coord),
      norm_y = ifelse(target_side == "right", y_coord, -y_coord),
      dist_to_net  = sqrt((89 - norm_x)^2 + norm_y^2),
      angle_to_net = abs(atan2(norm_y, pmax(89 - norm_x, 0.1)) * 180 / pi),
      shooter_strength = case_when(
        situation_label == "5v5" ~ "even",
        situation_label == "home_pp" & is_home_shooter  ~ "pp",
        situation_label == "home_pp" & !is_home_shooter ~ "pk",
        situation_label == "away_pp" & !is_home_shooter ~ "pp",
        situation_label == "away_pp" & is_home_shooter  ~ "pk",
        TRUE ~ "other"
      ),
      shot_type_clean = ifelse(is.na(shot_type) | shot_type == "", "unknown", shot_type)
    ) %>%
    arrange(game_id, t_abs) %>%
    group_by(game_id, owner_team_id) %>%
    mutate(time_since_own_last_shot = t_abs - lag(t_abs),
           is_rebound = !is.na(time_since_own_last_shot) & time_since_own_last_shot <= 3) %>%
    ungroup()

  # Match training's exact factor levels — an unseen category (e.g. a shot
  # type that never appeared in training data) maps to NA rather than
  # silently shifting every other column's meaning.
  s$shot_type_clean  <- factor(s$shot_type_clean,  levels = xg_obj$shot_type_levels)
  s$shooter_strength <- factor(s$shooter_strength, levels = xg_obj$shooter_strength_levels)

  valid <- !is.na(s$dist_to_net) & !is.na(s$angle_to_net) & !is.na(s$is_rebound) &
           !is.na(s$shot_type_clean) & !is.na(s$shooter_strength) & !is.na(s$target_side) & !is.na(s$goalie_id)
  xg_vals <- rep(NA_real_, nrow(s))
  if (any(valid)) {
    X_raw <- model.matrix(~ dist_to_net + angle_to_net + is_rebound + shot_type_clean + shooter_strength - 1,
                            data = s[valid, c("dist_to_net","angle_to_net","is_rebound","shot_type_clean","shooter_strength")] %>%
                              mutate(is_rebound = as.integer(is_rebound)))
    # Pre-allocate ONE clean matrix with exactly the training columns and
    # fill it via indexing, rather than cbind()-ing two separately
    # allocated matrices together — cbind() on a model.matrix() output can
    # produce a result with memory-alignment quirks that trip up XGBoost's
    # C++ prediction backend ("Input pointer misalignment"), which is
    # exactly the error this fixes. Always wrapping in xgb.DMatrix() before
    # predict() (rather than passing a raw matrix directly) is also what
    # train_xg_model.R itself does — this scoring function just wasn't
    # doing the same thing consistently before.
    train_cols <- xg_obj$model$feature_names
    X_new <- matrix(0, nrow = nrow(X_raw), ncol = length(train_cols), dimnames = list(NULL, train_cols))
    common_cols <- intersect(train_cols, colnames(X_raw))
    X_new[, common_cols] <- X_raw[, common_cols]
    # Known xgboost R package issue: a matrix built via matrix()+indexed
    # assignment can end up with a memory layout that fails XGBoost's C++
    # "array interface" alignment check ("Input pointer misalignment"),
    # even though it's a perfectly valid R matrix. Forcing a clean re-copy
    # via as.numeric()+reshape allocates a genuinely fresh, standard
    # buffer that satisfies the alignment requirement — this is the
    # standard workaround for this exact, documented issue.
    X_new <- matrix(as.numeric(X_new), nrow = nrow(X_new), ncol = ncol(X_new), dimnames = dimnames(X_new))
    xg_vals[valid] <- predict(xg_obj$model, xgb.DMatrix(data = X_new))
  }
  shots_df_scored <- s %>% select(-is_home_shooter, -target_side, -norm_x, -norm_y, -dist_to_net, -angle_to_net,
                                    -shooter_strength, -shot_type_clean, -time_since_own_last_shot, -is_rebound)
  shots_df_scored$xg <- xg_vals
  # Also worth keeping is_rebound and shooter_strength on the output row
  # for anyone inspecting shots_raw.csv later — re-attach from s directly
  # since select() above dropped them from the returned frame.
  shots_df_scored$is_rebound_at_scoring <- s$is_rebound
  shots_df_scored
}



# Caps how many NEW games get processed in a single run.
# - daily mode: small safety-net cap (a normal day only has ~5-15 new games).
# - backfill mode: NO cap — processes the entire remaining backlog for the
#   season in one run.
MAX_GAMES_PER_RUN <- if (MODE == "backfill") Inf else 50L

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

nhl_get <- function(url, timeout_s = 25) {
  resp <- tryCatch(GET(url, timeout(timeout_s)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE),
           error = function(e) NULL)
}

# ── 1. Find completed games not yet processed ────────────────────────────
processed_ids <- if (file.exists(STATE_FILE)) readLines(STATE_FILE) else character(0)

find_recent_games <- function(days_back) {
  ids <- character(0)
  for (d in 0:days_back) {
    date_str <- format(Sys.Date() - d, "%Y-%m-%d")
    raw <- nhl_get(paste0("https://api-web.nhle.com/v1/schedule/", date_str))
    if (is.null(raw) || is.null(raw$gameWeek)) next
    for (day in raw$gameWeek %||% list()) {
      for (g in day$games %||% list()) {
        state <- tryCatch(g$gameState %||% "", error = function(e) "")
        gtype <- tryCatch(as.integer(g$gameType %||% 0), error = function(e) 0L)
        if (state %in% c("OFF", "FINAL") && gtype %in% c(2L, 3L)) ids <- c(ids, as.character(g$id))
      }
    }
  }
  unique(ids)
}

find_season_games <- function(season_end_year) {
  ids <- character(0)
  start_date <- as.Date(paste0(season_end_year - 1L, "-09-01"))
  # Bound the end to THIS season's end, not always today — otherwise
  # backfilling an older season would sweep in every later season's games.
  end_date <- min(Sys.Date(), as.Date(paste0(season_end_year, "-07-15")))
  d <- start_date
  while (d <= end_date) {
    raw <- nhl_get(paste0("https://api-web.nhle.com/v1/schedule/", format(d, "%Y-%m-%d")))
    if (!is.null(raw) && !is.null(raw$gameWeek)) {
      for (day in raw$gameWeek %||% list()) {
        for (g in day$games %||% list()) {
          state <- tryCatch(g$gameState %||% "", error = function(e) "")
          gtype <- tryCatch(as.integer(g$gameType %||% 0), error = function(e) 0L)
          if (state %in% c("OFF", "FINAL") && gtype %in% c(2L, 3L)) ids <- c(ids, as.character(g$id))
        }
      }
    }
    d <- d + 7
  }
  unique(ids)
}

all_candidate_games <- if (MODE == "backfill") find_season_games(season_year) else find_recent_games(4L)
new_games <- setdiff(all_candidate_games, processed_ids)
new_games <- as.character(sort(as.numeric(new_games)))
cat("Found", length(all_candidate_games), "completed games in scope (mode:", MODE, ");",
    length(new_games), "not yet processed.\n")

if (length(new_games) == 0) {
  cat("Nothing new to process. Exiting.\n")
  quit(save = "no", status = 0)
}

if (length(new_games) > MAX_GAMES_PER_RUN) {
  cat("Processing the oldest", MAX_GAMES_PER_RUN, "of these this run",
      "(unusual for daily mode — check for a gap in runs).\n")
  new_games <- new_games[seq_len(MAX_GAMES_PER_RUN)]
}

# ── 2. Helpers: time parsing, situationCode parsing ──────────────────────────
parse_mmss <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_real_)
  p <- strsplit(as.character(x), ":")[[1]]
  if (length(p) != 2) return(NA_real_)
  suppressWarnings(as.numeric(p[1]) * 60 + as.numeric(p[2]))
}

# situationCode: 4-char string, [awayGoalieIn][awaySkaters][homeSkaters][homeGoalieIn]
parse_situation <- function(code) {
  if (is.null(code) || is.na(code) || nchar(as.character(code)) != 4)
    return(list(label = "other"))
  ch <- strsplit(as.character(code), "")[[1]]
  away_g <- suppressWarnings(as.integer(ch[1])); away_sk <- suppressWarnings(as.integer(ch[2]))
  home_sk <- suppressWarnings(as.integer(ch[3])); home_g <- suppressWarnings(as.integer(ch[4]))
  if (any(is.na(c(away_g, away_sk, home_sk, home_g)))) return(list(label = "other"))
  label <- if (away_g == 1 && home_g == 1 && away_sk == 5 && home_sk == 5) "5v5"
    else if (away_g == 1 && home_g == 1 && away_sk == 4 && home_sk == 4) "4v4"
    else if (away_g == 1 && home_g == 1 && away_sk == 3 && home_sk == 3) "3v3"
    else if (away_g == 1 && home_g == 1 && home_sk > away_sk) "home_pp"
    else if (away_g == 1 && home_g == 1 && away_sk > home_sk) "away_pp"
    else "other"
  list(label = label)
}

bump1 <- function(lst, key, by = 1) { if (!is.null(key) && !is.na(key)) lst[[key]] <- (lst[[key]] %||% 0) + by; lst }
bump  <- function(lst, ids, by = 1) { for (pid in ids) if (!is.na(pid)) lst[[pid]] <- (lst[[pid]] %||% 0) + by; lst }
to_named_list <- function(ids, vals) { if (length(ids) == 0) return(list()); setNames(as.list(vals), as.character(ids)) }
merge_add <- function(base, add) { for (k in names(add)) base[[k]] <- (base[[k]] %||% 0) + add[[k]]; base }

# ── 3. Skater on-ice reconstruction (ONLY runs when shift data exists) ──────
process_game_skaters <- function(pbp, shifts_raw, home_id, away_id, sit_df, shots_xg_lookup = numeric(0)) {
  shifts <- lapply(shifts_raw$data, function(s) {
    tryCatch({
      per <- suppressWarnings(as.integer(s$period %||% NA))
      st  <- parse_mmss(s$startTime %||% NA)
      en  <- parse_mmss(s$endTime %||% NA)
      if (is.na(per) || is.na(st) || is.na(en)) return(NULL)
      data.frame(player_id = as.character(s$playerId %||% NA), team_id = as.character(s$teamId %||% NA),
                 start_abs = (per - 1) * 1200 + st, end_abs = (per - 1) * 1200 + en,
                 stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  })
  shifts_df <- bind_rows(Filter(Negate(is.null), shifts))
  if (nrow(shifts_df) == 0) return(NULL)

  on_ice_at <- function(t_abs, team_id) {
    if (is.na(t_abs) || is.na(team_id)) return(character(0))
    rows <- shifts_df[shifts_df$team_id == team_id & shifts_df$start_abs <= t_abs & shifts_df$end_abs >= t_abs, ]
    unique(rows$player_id)
  }

  cf <- list(); ca <- list(); gf <- list(); ga <- list()
  ev_goals <- list(); ev_assists <- list()
  pp_goals <- list(); pp_assists <- list()
  pk_ga <- list()
  # Individual on-ice PP-shots-for / PK-shots-against — same shift-matching
  # approach as 5v5 CF/CA above, just filtered to power-play/penalty-kill
  # situations instead. This is what makes real per-player PK/PP shot
  # volume possible (not just goals), which wasn't tracked before.
  pp_shots_ind <- list(); pk_shots_against_ind <- list()
  # On-ice (not individual-credit) PP-goals-for / PK-goals-against — needed
  # for true WOWY on PP Offense/PK Defense, same reasoning as CF/CA vs.
  # ev_goals above: on-ice credits EVERYONE who was out there, not just
  # the scorer, which is what a "team rate with/without this player"
  # comparison actually needs.
  pp_gf_onice_ind <- list(); pk_ga_onice_ind <- list()
  # On-ice xG-for/against — needed for xG-based WOWY (a less noisy signal
  # than actual goals, since it isolates shot-generation/shot-suppression
  # skill from shooting/goaltending luck). Same shared-credit reasoning as
  # CF/CA/goals above: whoever was on the ice for a shot gets credited,
  # not just the shooter/goalie.
  xg_for_5v5 <- list(); xg_against_5v5 <- list()
  toi_5v5 <- list(); toi_pp <- list(); toi_pk <- list()
  player_team_id <- list()

  if (nrow(sit_df) > 0) {
    game_end_abs <- suppressWarnings(max(shifts_df$end_abs, na.rm = TRUE))
    seg_start <- sit_df$t_abs; seg_end <- c(sit_df$t_abs[-1], game_end_abs); seg_label <- sit_df$label
    all_players <- unique(shifts_df$player_id)
    for (pid in all_players) {
      psh <- shifts_df[shifts_df$player_id == pid, ]
      pteam <- psh$team_id[1]
      player_team_id[[pid]] <- pteam
      t5 <- 0; tpp <- 0; tpk <- 0
      for (i in seq_along(seg_start)) {
        ov <- pmin(psh$end_abs, seg_end[i]) - pmax(psh$start_abs, seg_start[i])
        ov <- sum(ov[ov > 0])
        if (ov <= 0) next
        lbl <- seg_label[i]
        if (lbl == "5v5") t5 <- t5 + ov
        else if ((lbl == "home_pp" && pteam == home_id) || (lbl == "away_pp" && pteam == away_id)) tpp <- tpp + ov
        else if ((lbl == "home_pp" && pteam == away_id) || (lbl == "away_pp" && pteam == home_id)) tpk <- tpk + ov
      }
      toi_5v5[[pid]] <- t5; toi_pp[[pid]] <- tpp; toi_pk[[pid]] <- tpk
    }
  }

  for (play_i in seq_along(pbp$plays)) {
    pl <- pbp$plays[[play_i]]
    typ  <- tryCatch(pl$typeDescKey %||% "", error = function(e) "")
    code <- tryCatch(as.character(pl$situationCode %||% NA), error = function(e) NA_character_)
    sit  <- parse_situation(code)
    det  <- pl$details %||% list()
    owner_team <- tryCatch(as.character(det$eventOwnerTeamId %||% NA), error = function(e) NA_character_)
    per <- tryCatch(as.integer(pl$periodDescriptor$number %||% NA), error = function(e) NA_integer_)
    tip <- tryCatch(parse_mmss(pl$timeInPeriod %||% NA), error = function(e) NA_real_)
    t_abs <- if (!is.na(per) && !is.na(tip)) (per - 1) * 1200 + tip else NA_real_

    is_shot_evt <- typ %in% c("shot-on-goal", "missed-shot", "blocked-shot", "goal")
    if (is_shot_evt && sit$label == "5v5" && !is.na(t_abs) && !is.na(owner_team)) {
      against_team <- if (identical(owner_team, home_id)) away_id else home_id
      for_on <- on_ice_at(t_abs, owner_team); against_on <- on_ice_at(t_abs, against_team)
      cf <- bump(cf, for_on); ca <- bump(ca, against_on)
      if (typ == "goal") { gf <- bump(gf, for_on); ga <- bump(ga, against_on) }
      # On-ice xG credit — same shift-matching as CF/CA above, just weighted
      # by this specific shot's scored xG value instead of a flat +1 count.
      # Single-bracket indexing (not [[ ) so a missing key returns NA
      # rather than erroring — expected for any shot the model couldn't
      # score (e.g. missing coordinates) or if no trained model exists yet.
      shot_xg <- unname(shots_xg_lookup[as.character(play_i)])
      if (!is.na(shot_xg)) {
        xg_for_5v5 <- bump(xg_for_5v5, for_on, by = shot_xg)
        xg_against_5v5 <- bump(xg_against_5v5, against_on, by = shot_xg)
      }
    }
    if (is_shot_evt && sit$label %in% c("home_pp", "away_pp") && !is.na(t_abs) && !is.na(owner_team)) {
      owner_on_pp <- (identical(owner_team, home_id) && sit$label == "home_pp") ||
                     (identical(owner_team, away_id) && sit$label == "away_pp")
      against_team <- if (identical(owner_team, home_id)) away_id else home_id
      if (isTRUE(owner_on_pp)) {
        # owner_team has the power play; against_team is killing the penalty
        pp_shooters_on <- on_ice_at(t_abs, owner_team)
        pk_defenders_on <- on_ice_at(t_abs, against_team)
        pp_shots_ind <- bump(pp_shots_ind, pp_shooters_on)
        pk_shots_against_ind <- bump(pk_shots_against_ind, pk_defenders_on)
        if (typ == "goal") {
          pp_gf_onice_ind <- bump(pp_gf_onice_ind, pp_shooters_on)
          pk_ga_onice_ind <- bump(pk_ga_onice_ind, pk_defenders_on)
        }
      }
      # Shorthanded shot BY the penalty-killing team (rare) isn't tracked
      # here — matches the existing team-level handling, which also treats
      # this as a minor edge case rather than precisely modeling it.
    }

    if (typ == "goal") {
      scorer <- tryCatch(as.character(det$scoringPlayerId %||% NA), error = function(e) NA_character_)
      a1 <- tryCatch(as.character(det$assist1PlayerId %||% NA), error = function(e) NA_character_)
      a2 <- tryCatch(as.character(det$assist2PlayerId %||% NA), error = function(e) NA_character_)

      if (sit$label == "5v5") {
        if (!is.na(scorer)) ev_goals <- bump(ev_goals, scorer)
        if (!is.na(a1)) ev_assists <- bump(ev_assists, a1)
        if (!is.na(a2)) ev_assists <- bump(ev_assists, a2)
      } else if (sit$label %in% c("home_pp", "away_pp") && !is.na(owner_team)) {
        scorer_is_home <- identical(owner_team, home_id)
        scorer_on_pp <- (scorer_is_home && sit$label == "home_pp") || (!scorer_is_home && sit$label == "away_pp")
        if (isTRUE(scorer_on_pp)) {
          if (!is.na(scorer)) pp_goals <- bump(pp_goals, scorer)
          if (!is.na(a1)) pp_assists <- bump(pp_assists, a1)
          if (!is.na(a2)) pp_assists <- bump(pp_assists, a2)
        } else if (!is.na(t_abs)) {
          pk_team <- if (identical(owner_team, home_id)) away_id else home_id
          pk_on <- on_ice_at(t_abs, pk_team)
          pk_ga <- bump(pk_ga, pk_on)
        }
      }
    }
  }

  list(cf = cf, ca = ca, gf = gf, ga = ga, ev_goals = ev_goals, ev_assists = ev_assists,
       pp_goals = pp_goals, pp_assists = pp_assists, pk_ga = pk_ga,
       pp_shots_ind = pp_shots_ind, pk_shots_against_ind = pk_shots_against_ind,
       pp_gf_onice_ind = pp_gf_onice_ind, pk_ga_onice_ind = pk_ga_onice_ind,
       xg_for_5v5 = xg_for_5v5, xg_against_5v5 = xg_against_5v5,
       player_team_id = player_team_id,
       toi_5v5 = toi_5v5, toi_pp = toi_pp, toi_pk = toi_pk)
}

# ── 4. Per-game processing ───────────────────────────────────────────────────
process_game <- function(game_id) {
  pbp <- nhl_get(paste0("https://api-web.nhle.com/v1/gamecenter/", game_id, "/play-by-play"))
  if (is.null(pbp) || is.null(pbp$plays) || length(pbp$plays) == 0) return(NULL)

  home_id <- tryCatch(as.character(pbp$homeTeam$id), error = function(e) NA_character_)
  away_id <- tryCatch(as.character(pbp$awayTeam$id), error = function(e) NA_character_)
  home_abbrev <- tryCatch(as.character(pbp$homeTeam$abbrev %||% NA), error = function(e) NA_character_)
  away_abbrev <- tryCatch(as.character(pbp$awayTeam$abbrev %||% NA), error = function(e) NA_character_)
  if (is.na(home_id) || is.na(away_id) || is.na(home_abbrev) || is.na(away_abbrev)) return(NULL)

  sit_events <- lapply(pbp$plays, function(pl) {
    tryCatch({
      per <- suppressWarnings(as.integer(pl$periodDescriptor$number %||% NA))
      tip <- parse_mmss(pl$timeInPeriod %||% NA)
      if (is.na(per) || is.na(tip)) return(NULL)
      data.frame(t_abs = (per - 1) * 1200 + tip, code = as.character(pl$situationCode %||% NA), stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  })
  sit_df <- bind_rows(Filter(Negate(is.null), sit_events))
  team_toi <- list(home_5v5 = 0, home_pp = 0, home_pk = 0, away_5v5 = 0, away_pp = 0, away_pk = 0)
  if (nrow(sit_df) > 0) {
    sit_df <- sit_df %>% arrange(t_abs) %>% distinct(t_abs, .keep_all = TRUE)
    sit_df$label <- sapply(sit_df$code, function(c) parse_situation(c)$label)
    game_end_abs <- suppressWarnings(max(sit_df$t_abs, na.rm = TRUE))
    seg_start <- sit_df$t_abs; seg_end <- c(sit_df$t_abs[-1], game_end_abs)
    seg_dur <- pmax(0, seg_end - seg_start)
    for (i in seq_along(seg_start)) {
      dur <- seg_dur[i]; if (dur <= 0) next
      lbl <- sit_df$label[i]
      if (lbl == "5v5") { team_toi$home_5v5 <- team_toi$home_5v5 + dur; team_toi$away_5v5 <- team_toi$away_5v5 + dur }
      else if (lbl == "home_pp") { team_toi$home_pp <- team_toi$home_pp + dur; team_toi$away_pk <- team_toi$away_pk + dur }
      else if (lbl == "away_pp") { team_toi$away_pp <- team_toi$away_pp + dur; team_toi$home_pk <- team_toi$home_pk + dur }
    }
  }

  t_cf <- list(); t_ca <- list(); t_gf <- list(); t_ga <- list()
  t_pp_goals <- list(); t_pp_shots <- list(); t_pk_ga <- list(); t_pk_shots <- list()
  g_shots5 <- list(); g_ga5 <- list(); g_shotspk <- list(); g_gapk <- list()
  # Raw shot-level rows for future xG training — NOT aggregated here, just
  # collected as individual rows and written to a separate per-season file.
  # Coordinates/shot type were always present in the play-by-play feed;
  # this is the first time we're actually keeping them instead of only
  # checking whether a shot happened.
  shot_rows <- list()
  # Penalties taken/drawn — new, needed for the WAR "Penalties" component.
  # FIELD NAMES BELOW ARE BEST-GUESS based on the NHL API's general
  # naming convention (committedByPlayerId / drawnByPlayerId), consistent
  # with how other event types in this same feed name their fields
  # (eventOwnerTeamId, scoringPlayerId, assist1PlayerId, etc.) — but this
  # is the one part of this extension that should be verified against a
  # real penalty event from the live feed before trusting the output,
  # the same way every other field-name assumption in this pipeline has
  # needed a real-data check at some point.
  pen_taken <- list(); pen_drawn <- list()

  for (play_i in seq_along(pbp$plays)) {
    pl <- pbp$plays[[play_i]]
    typ  <- tryCatch(pl$typeDescKey %||% "", error = function(e) "")
    code <- tryCatch(as.character(pl$situationCode %||% NA), error = function(e) NA_character_)
    sit  <- parse_situation(code)
    det  <- pl$details %||% list()
    owner_team <- tryCatch(as.character(det$eventOwnerTeamId %||% NA), error = function(e) NA_character_)
    goalie_id  <- tryCatch(as.character(det$goalieInNetId %||% NA), error = function(e) NA_character_)
    is_shot_evt <- typ %in% c("shot-on-goal", "missed-shot", "blocked-shot", "goal")

    if (typ == "penalty") {
      taker <- tryCatch(as.character(det$committedByPlayerId %||% NA), error = function(e) NA_character_)
      drawer <- tryCatch(as.character(det$drawnByPlayerId %||% NA), error = function(e) NA_character_)
      pen_desc <- tryCatch(det$descKey %||% NA_character_, error = function(e) NA_character_)
      pen_type_code <- tryCatch(det$typeCode %||% NA_character_, error = function(e) NA_character_)
      pen_dur <- tryCatch(suppressWarnings(as.integer(det$duration %||% NA)), error = function(e) NA_integer_)
      # Fights/misconducts/matches shouldn't count toward normal penalty
      # impact the way a minor does — exclude anything not a standard
      # minor from this WAR component (majors/misconducts still get
      # written to the raw output below if you want them later, they just
      # aren't counted in the simple taken/drawn tallies).
      # NOTE: duration is reported in MINUTES, not seconds (confirmed from
      # a real event: a standard minor shows duration=2, not 120) — the
      # first version of this check assumed seconds and silently excluded
      # every single penalty as a result. typeCode == "MIN" is the more
      # explicit, robust signal and is checked first; the duration check
      # is kept as a secondary guard in case typeCode is ever missing.
      is_std_minor <- (!is.na(pen_type_code) && pen_type_code == "MIN") ||
                      (is.na(pen_type_code) && !is.na(pen_dur) && pen_dur == 2)
      if (is_std_minor) {
        if (!is.na(taker)) pen_taken <- bump(pen_taken, taker)
        if (!is.na(drawer)) pen_drawn <- bump(pen_drawn, drawer)
      }
    }

    if (is_shot_evt && !is.na(owner_team)) {
      per <- tryCatch(as.integer(pl$periodDescriptor$number %||% NA), error = function(e) NA_integer_)
      tip <- tryCatch(parse_mmss(pl$timeInPeriod %||% NA), error = function(e) NA_real_)
      t_abs_shot <- if (!is.na(per) && !is.na(tip)) (per - 1) * 1200 + tip else NA_real_
      shot_rows[[length(shot_rows) + 1]] <- data.frame(
        game_id = game_id, event_idx = play_i, period = per, time_in_period = tip, t_abs = t_abs_shot,
        situation_code = code, situation_label = sit$label,
        event_type = typ, owner_team_id = owner_team, home_id = home_id,
        # Which side the HOME team defends this period — needed to know
        # which net (x=+89 or x=-89) a given shot was actually aimed at,
        # since teams switch ends every period. Without this, x_coord/
        # y_coord alone can't give a correct distance/angle to net.
        home_defending_side = tryCatch(pl$homeTeamDefendingSide %||% NA_character_, error = function(e) NA_character_),
        shooter_id = tryCatch(as.character(det$shootingPlayerId %||% det$scoringPlayerId %||% NA), error = function(e) NA_character_),
        goalie_id = goalie_id,
        x_coord = tryCatch(suppressWarnings(as.numeric(det$xCoord %||% NA)), error = function(e) NA_real_),
        y_coord = tryCatch(suppressWarnings(as.numeric(det$yCoord %||% NA)), error = function(e) NA_real_),
        zone_code = tryCatch(det$zoneCode %||% NA_character_, error = function(e) NA_character_),
        shot_type = tryCatch(det$shotType %||% NA_character_, error = function(e) NA_character_),
        is_goal = (typ == "goal"),
        stringsAsFactors = FALSE
      )
    }

    if (!is_shot_evt || is.na(owner_team)) next
    against_team <- if (identical(owner_team, home_id)) away_id else home_id

    if (sit$label == "5v5") {
      t_cf <- bump1(t_cf, owner_team); t_ca <- bump1(t_ca, against_team)
      if (typ == "goal") { t_gf <- bump1(t_gf, owner_team); t_ga <- bump1(t_ga, against_team) }
      if (!is.na(goalie_id)) {
        g_shots5 <- bump1(g_shots5, goalie_id)
        if (typ == "goal") g_ga5 <- bump1(g_ga5, goalie_id)
      }
    } else if (sit$label %in% c("home_pp", "away_pp")) {
      owner_on_pp <- (identical(owner_team, home_id) && sit$label == "home_pp") ||
                     (identical(owner_team, away_id) && sit$label == "away_pp")
      if (isTRUE(owner_on_pp)) {
        t_pp_shots <- bump1(t_pp_shots, owner_team)
        if (typ == "goal") { t_pp_goals <- bump1(t_pp_goals, owner_team); t_pk_ga <- bump1(t_pk_ga, against_team) }
        if (!is.na(goalie_id)) {
          g_shotspk <- bump1(g_shotspk, goalie_id)
          if (typ == "goal") g_gapk <- bump1(g_gapk, goalie_id)
        }
      } else {
        t_pk_shots <- bump1(t_pk_shots, owner_team)
      }
    }
  }

  shots_raw_df <- if (length(shot_rows) > 0) bind_rows(shot_rows) else NULL
  # Score shots HERE, before process_game_skaters() runs, so on-ice xG
  # credit can use the exact same scored values as everything else
  # (shots_raw.csv output, goalie xg_faced) — never re-derived or
  # recomputed a second time, which would risk the two computations
  # drifting out of sync with each other.
  shots_raw_df <- score_shots_with_xg(shots_raw_df, xg_model_obj)
  shots_xg_lookup <- if (!is.null(shots_raw_df) && "xg" %in% names(shots_raw_df) && "event_idx" %in% names(shots_raw_df)) {
    setNames(shots_raw_df$xg, as.character(shots_raw_df$event_idx))
  } else numeric(0)

  # Team-level xG-for/against — aggregated from the already-scored shots
  # (can't be done inline during the main loop above, since scoring itself
  # needs the FULL shot sequence first for rebound detection).
  # MUST be filtered to 5v5-only, matching gf_5v5/ga_5v5's scope and the
  # player-level xg_for_5v5/xg_against_5v5 — without this filter, the team
  # baseline included PP/PK shots while the player-level number was
  # 5v5-only, a scope mismatch that produced a systematic bias (every
  # player looking like they contribute far less than their "fair share"
  # of an inflated all-situation team total).
  t_xg_for <- list(); t_xg_against <- list()
  if (!is.null(shots_raw_df) && "xg" %in% names(shots_raw_df)) {
    valid_xg <- shots_raw_df[!is.na(shots_raw_df$xg) & shots_raw_df$situation_label == "5v5", ]
    if (nrow(valid_xg) > 0) {
      valid_xg$against_team_id <- ifelse(valid_xg$owner_team_id == home_id, away_id, home_id)
      xg_for_agg <- valid_xg %>% group_by(owner_team_id) %>% summarise(xg = sum(xg), .groups = "drop")
      xg_against_agg <- valid_xg %>% group_by(against_team_id) %>% summarise(xg = sum(xg), .groups = "drop")
      for (i in seq_len(nrow(xg_for_agg))) t_xg_for[[xg_for_agg$owner_team_id[i]]] <- xg_for_agg$xg[i]
      for (i in seq_len(nrow(xg_against_agg))) t_xg_against[[xg_against_agg$against_team_id[i]]] <- xg_against_agg$xg[i]
    }
  }

  team_result <- list(home_id = home_id, away_id = away_id, home_abbrev = home_abbrev, away_abbrev = away_abbrev,
                       cf = t_cf, ca = t_ca, gf = t_gf, ga = t_ga,
                       pp_goals = t_pp_goals, pp_shots = t_pp_shots, pk_ga = t_pk_ga, pk_shots = t_pk_shots,
                       xg_for = t_xg_for, xg_against = t_xg_against,
                       toi = team_toi)
  goalie_result <- list(shots_5v5 = g_shots5, ga_5v5 = g_ga5, shots_pk = g_shotspk, ga_pk = g_gapk)
  penalty_result <- list(pen_taken = pen_taken, pen_drawn = pen_drawn)

  skater_result <- NULL
  shifts_raw <- nhl_get(paste0("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId=", game_id))
  if (!is.null(shifts_raw) && !is.null(shifts_raw$data) && length(shifts_raw$data) > 0) {
    skater_result <- tryCatch(process_game_skaters(pbp, shifts_raw, home_id, away_id, sit_df, shots_xg_lookup), error = function(e) NULL)
    if (!is.null(skater_result)) {
      skater_result$player_team_abbrev <- lapply(skater_result$player_team_id, function(tid) {
        if (identical(as.character(tid), as.character(home_id))) home_abbrev
        else if (identical(as.character(tid), as.character(away_id))) away_abbrev
        else NA_character_
      })
    }
  }

  list(team = team_result, goalie = goalie_result, skater = skater_result,
       shots_raw = shots_raw_df, penalty = penalty_result)
}

# ── 5. Load existing totals, process new games, merge ───────────────────────
existing_skater <- if (file.exists(SKATER_OUT)) tryCatch(read.csv(SKATER_OUT, stringsAsFactors = FALSE), error = function(e) NULL) else NULL
existing_team   <- if (file.exists(TEAM_OUT))   tryCatch(read.csv(TEAM_OUT,   stringsAsFactors = FALSE), error = function(e) NULL) else NULL
existing_goalie <- if (file.exists(GOALIE_OUT)) tryCatch(read.csv(GOALIE_OUT, stringsAsFactors = FALSE), error = function(e) NULL) else NULL

blank_skater <- function() list(cf=list(),ca=list(),gf=list(),ga=list(),ev_goals=list(),ev_assists=list(),
                                 pp_goals=list(),pp_assists=list(),pk_ga=list(),pp_shots=list(),pk_shots_against=list(),
                                 pp_gf_onice=list(),pk_ga_onice=list(),pen_taken=list(),pen_drawn=list(),
                                 xg_for_5v5=list(),xg_against_5v5=list(),
                                 toi_5v5=list(),toi_pp=list(),toi_pk=list(),gp=list())
# Guard against older skater_onice.csv files from before pp_shots/
# pk_shots_against/pp_gf_onice/pk_ga_onice/pen_taken/pen_drawn/xg_for_5v5/
# xg_against_5v5 existed — default to 0 rather than erroring, meaning
# those already-processed games just won't contribute to the new columns
# until reprocessed (not a correctness issue, just a coverage gap that
# closes naturally as new games get processed).
if (!is.null(existing_skater) && nrow(existing_skater) > 0) {
  if (!"pp_shots" %in% names(existing_skater)) existing_skater$pp_shots <- 0
  if (!"pk_shots_against" %in% names(existing_skater)) existing_skater$pk_shots_against <- 0
  if (!"pp_gf_onice" %in% names(existing_skater)) existing_skater$pp_gf_onice <- 0
  if (!"pk_ga_onice" %in% names(existing_skater)) existing_skater$pk_ga_onice <- 0
  if (!"pen_taken" %in% names(existing_skater)) existing_skater$pen_taken <- 0
  if (!"pen_drawn" %in% names(existing_skater)) existing_skater$pen_drawn <- 0
  if (!"xg_for_5v5" %in% names(existing_skater)) existing_skater$xg_for_5v5 <- 0
  if (!"xg_against_5v5" %in% names(existing_skater)) existing_skater$xg_against_5v5 <- 0
}
skater_totals <- if (is.null(existing_skater) || nrow(existing_skater) == 0) blank_skater() else list(
  cf = to_named_list(existing_skater$player_id, existing_skater$cf_5v5), ca = to_named_list(existing_skater$player_id, existing_skater$ca_5v5),
  gf = to_named_list(existing_skater$player_id, existing_skater$gf_5v5), ga = to_named_list(existing_skater$player_id, existing_skater$ga_5v5),
  ev_goals = to_named_list(existing_skater$player_id, existing_skater$ev_goals), ev_assists = to_named_list(existing_skater$player_id, existing_skater$ev_assists),
  pp_goals = to_named_list(existing_skater$player_id, existing_skater$pp_goals), pp_assists = to_named_list(existing_skater$player_id, existing_skater$pp_assists),
  pk_ga = to_named_list(existing_skater$player_id, existing_skater$pk_ga),
  pp_shots = to_named_list(existing_skater$player_id, existing_skater$pp_shots), pk_shots_against = to_named_list(existing_skater$player_id, existing_skater$pk_shots_against),
  pp_gf_onice = to_named_list(existing_skater$player_id, existing_skater$pp_gf_onice), pk_ga_onice = to_named_list(existing_skater$player_id, existing_skater$pk_ga_onice),
  pen_taken = to_named_list(existing_skater$player_id, existing_skater$pen_taken), pen_drawn = to_named_list(existing_skater$player_id, existing_skater$pen_drawn),
  xg_for_5v5 = to_named_list(existing_skater$player_id, existing_skater$xg_for_5v5), xg_against_5v5 = to_named_list(existing_skater$player_id, existing_skater$xg_against_5v5),
  toi_5v5 = to_named_list(existing_skater$player_id, existing_skater$toi_5v5_sec), toi_pp = to_named_list(existing_skater$player_id, existing_skater$toi_pp_sec),
  toi_pk = to_named_list(existing_skater$player_id, existing_skater$toi_pk_sec), gp = to_named_list(existing_skater$player_id, existing_skater$gp_onice)
)

blank_team <- function() list(cf=list(),ca=list(),gf=list(),ga=list(),pp_goals=list(),pp_shots=list(),
                               pk_ga=list(),pk_shots=list(),xg_for=list(),xg_against=list(),
                               toi_5v5=list(),toi_pp=list(),toi_pk=list(),gp=list())
if (!is.null(existing_team) && nrow(existing_team) > 0) {
  if (!"xg_for" %in% names(existing_team)) existing_team$xg_for <- 0
  if (!"xg_against" %in% names(existing_team)) existing_team$xg_against <- 0
}
team_totals <- if (is.null(existing_team) || nrow(existing_team) == 0) blank_team() else list(
  cf = to_named_list(existing_team$team_abbrev, existing_team$cf_5v5), ca = to_named_list(existing_team$team_abbrev, existing_team$ca_5v5),
  gf = to_named_list(existing_team$team_abbrev, existing_team$gf_5v5), ga = to_named_list(existing_team$team_abbrev, existing_team$ga_5v5),
  pp_goals = to_named_list(existing_team$team_abbrev, existing_team$pp_goals), pp_shots = to_named_list(existing_team$team_abbrev, existing_team$pp_shots),
  pk_ga = to_named_list(existing_team$team_abbrev, existing_team$pk_goals_against), pk_shots = to_named_list(existing_team$team_abbrev, existing_team$pk_shots_against),
  xg_for = to_named_list(existing_team$team_abbrev, existing_team$xg_for), xg_against = to_named_list(existing_team$team_abbrev, existing_team$xg_against),
  toi_5v5 = to_named_list(existing_team$team_abbrev, existing_team$toi_5v5_sec), toi_pp = to_named_list(existing_team$team_abbrev, existing_team$toi_pp_sec),
  toi_pk = to_named_list(existing_team$team_abbrev, existing_team$toi_pk_sec), gp = to_named_list(existing_team$team_abbrev, existing_team$gp_onice)
)

blank_goalie <- function() list(shots_5v5=list(),ga_5v5=list(),shots_pk=list(),ga_pk=list(),xg_faced=list(),gp=list())
if (!is.null(existing_goalie) && nrow(existing_goalie) > 0 && !"xg_faced" %in% names(existing_goalie)) existing_goalie$xg_faced <- 0  # guard for pre-xG-model CSVs
goalie_totals <- if (is.null(existing_goalie) || nrow(existing_goalie) == 0) blank_goalie() else list(
  shots_5v5 = to_named_list(existing_goalie$player_id, existing_goalie$shots_5v5), ga_5v5 = to_named_list(existing_goalie$player_id, existing_goalie$ga_5v5),
  shots_pk = to_named_list(existing_goalie$player_id, existing_goalie$shots_pk), ga_pk = to_named_list(existing_goalie$player_id, existing_goalie$ga_pk),
  xg_faced = to_named_list(existing_goalie$player_id, existing_goalie$xg_faced),
  gp = to_named_list(existing_goalie$player_id, existing_goalie$gp_onice)
)

remap_team_keys <- function(lst, home_id, away_id, home_abbrev, away_abbrev) {
  out <- list()
  for (k in names(lst)) {
    ab <- if (identical(k, home_id)) home_abbrev else if (identical(k, away_id)) away_abbrev else NA
    if (!is.na(ab)) out[[ab]] <- lst[[k]]
  }
  out
}

processed_this_run <- character(0)
skater_games_ok <- 0L
team_games_ok <- 0L
# One row per player per game — used at the end to determine each player's
# primary team for the season (most games played), same simplification
# most public tools use for a season-total row rather than splitting
# traded players' stats across multiple team rows.
player_team_rows <- list()
# Raw shots accumulate as a list of per-game data.frames, combined and
# written once at the end — same "append to existing CSV" pattern as
# everything else in this script, so backfill runs can resume/extend.
shots_raw_new <- list()

for (gid in new_games) {
  cat("Processing game", gid, "...\n")
  res <- tryCatch(process_game(gid), error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL })
  if (is.null(res)) { cat("  Skipped entirely (no usable play-by-play).\n"); next }

  tr <- res$team
  team_totals$cf       <- merge_add(team_totals$cf,       remap_team_keys(tr$cf, tr$home_id, tr$away_id, tr$home_abbrev, tr$away_abbrev))
  team_totals$ca       <- merge_add(team_totals$ca,       remap_team_keys(tr$ca, tr$home_id, tr$away_id, tr$home_abbrev, tr$away_abbrev))
  team_totals$gf       <- merge_add(team_totals$gf,       remap_team_keys(tr$gf, tr$home_id, tr$away_id, tr$home_abbrev, tr$away_abbrev))
  team_totals$ga       <- merge_add(team_totals$ga,       remap_team_keys(tr$ga, tr$home_id, tr$away_id, tr$home_abbrev, tr$away_abbrev))
  team_totals$pp_goals <- merge_add(team_totals$pp_goals, remap_team_keys(tr$pp_goals, tr$home_id, tr$away_id, tr$home_abbrev, tr$away_abbrev))
  team_totals$pp_shots <- merge_add(team_totals$pp_shots, remap_team_keys(tr$pp_shots, tr$home_id, tr$away_id, tr$home_abbrev, tr$away_abbrev))
  team_totals$pk_ga    <- merge_add(team_totals$pk_ga,    remap_team_keys(tr$pk_ga, tr$home_id, tr$away_id, tr$home_abbrev, tr$away_abbrev))
  team_totals$pk_shots <- merge_add(team_totals$pk_shots, remap_team_keys(tr$pk_shots, tr$home_id, tr$away_id, tr$home_abbrev, tr$away_abbrev))
  team_totals$xg_for     <- merge_add(team_totals$xg_for,     remap_team_keys(tr$xg_for, tr$home_id, tr$away_id, tr$home_abbrev, tr$away_abbrev))
  team_totals$xg_against <- merge_add(team_totals$xg_against, remap_team_keys(tr$xg_against, tr$home_id, tr$away_id, tr$home_abbrev, tr$away_abbrev))
  team_totals$toi_5v5  <- merge_add(team_totals$toi_5v5, setNames(list(tr$toi$home_5v5, tr$toi$away_5v5), c(tr$home_abbrev, tr$away_abbrev)))
  team_totals$toi_pp   <- merge_add(team_totals$toi_pp,  setNames(list(tr$toi$home_pp,  tr$toi$away_pp),  c(tr$home_abbrev, tr$away_abbrev)))
  team_totals$toi_pk   <- merge_add(team_totals$toi_pk,  setNames(list(tr$toi$home_pk,  tr$toi$away_pk),  c(tr$home_abbrev, tr$away_abbrev)))
  team_totals$gp       <- merge_add(team_totals$gp,      setNames(list(1, 1), c(tr$home_abbrev, tr$away_abbrev)))
  team_games_ok <- team_games_ok + 1L

  gr <- res$goalie
  goalie_totals$shots_5v5 <- merge_add(goalie_totals$shots_5v5, gr$shots_5v5)
  goalie_totals$ga_5v5    <- merge_add(goalie_totals$ga_5v5,    gr$ga_5v5)
  goalie_totals$shots_pk  <- merge_add(goalie_totals$shots_pk,  gr$shots_pk)
  goalie_totals$ga_pk     <- merge_add(goalie_totals$ga_pk,     gr$ga_pk)
  goalie_game_players <- unique(c(names(gr$shots_5v5), names(gr$shots_pk)))
  goalie_totals$gp <- merge_add(goalie_totals$gp, to_named_list(goalie_game_players, rep(1, length(goalie_game_players))))

  if (!is.null(res$shots_raw) && nrow(res$shots_raw) > 0) {
    scored_shots <- res$shots_raw  # already scored inside process_game()
    shots_raw_new[[length(shots_raw_new) + 1]] <- scored_shots
    # Goalie xG-faced — matches the SAME scope as the existing goalie
    # shots_5v5/shots_pk tracking above (5v5 shots faced + shots faced
    # while this goalie's team is shorthanded). The rarer "shorthanded
    # goal against a power-play team" situation isn't tracked for goalies
    # anywhere in this pipeline yet (a pre-existing scope limitation, not
    # something newly introduced here), so it's excluded here too for
    # consistency rather than partially covering it.
    if (!is.null(scored_shots) && "xg" %in% names(scored_shots)) {
      xg_tracked <- scored_shots %>%
        filter(!is.na(goalie_id), !is.na(xg),
               situation_label == "5v5" | (situation_label %in% c("home_pp","away_pp") &
                 ((situation_label == "home_pp" & owner_team_id != home_id) |
                  (situation_label == "away_pp" & owner_team_id == home_id))))
      if (nrow(xg_tracked) > 0) {
        xg_by_goalie <- xg_tracked %>% group_by(goalie_id) %>% summarise(xg = sum(xg), .groups = "drop")
        goalie_totals$xg_faced <- merge_add(goalie_totals$xg_faced, to_named_list(xg_by_goalie$goalie_id, xg_by_goalie$xg))
      }
    }
  }

  # Penalties taken/drawn come from play-by-play parsing alone (same as
  # team/goalie stats) — NOT gated behind shift-data availability the way
  # skater on-ice stats below are, so this has better coverage even for
  # games with missing shift charts.
  skater_totals$pen_taken <- merge_add(skater_totals$pen_taken, res$penalty$pen_taken)
  skater_totals$pen_drawn <- merge_add(skater_totals$pen_drawn, res$penalty$pen_drawn)

  if (!is.null(res$skater)) {
    sk <- res$skater
    skater_totals$cf <- merge_add(skater_totals$cf, sk$cf); skater_totals$ca <- merge_add(skater_totals$ca, sk$ca)
    skater_totals$gf <- merge_add(skater_totals$gf, sk$gf); skater_totals$ga <- merge_add(skater_totals$ga, sk$ga)
    skater_totals$ev_goals <- merge_add(skater_totals$ev_goals, sk$ev_goals); skater_totals$ev_assists <- merge_add(skater_totals$ev_assists, sk$ev_assists)
    skater_totals$pp_goals <- merge_add(skater_totals$pp_goals, sk$pp_goals); skater_totals$pp_assists <- merge_add(skater_totals$pp_assists, sk$pp_assists)
    skater_totals$pk_ga <- merge_add(skater_totals$pk_ga, sk$pk_ga)
    skater_totals$pp_shots <- merge_add(skater_totals$pp_shots, sk$pp_shots_ind)
    skater_totals$pk_shots_against <- merge_add(skater_totals$pk_shots_against, sk$pk_shots_against_ind)
    skater_totals$pp_gf_onice <- merge_add(skater_totals$pp_gf_onice, sk$pp_gf_onice_ind)
    skater_totals$pk_ga_onice <- merge_add(skater_totals$pk_ga_onice, sk$pk_ga_onice_ind)
    skater_totals$xg_for_5v5 <- merge_add(skater_totals$xg_for_5v5, sk$xg_for_5v5)
    skater_totals$xg_against_5v5 <- merge_add(skater_totals$xg_against_5v5, sk$xg_against_5v5)
    skater_totals$toi_5v5 <- merge_add(skater_totals$toi_5v5, sk$toi_5v5)
    skater_totals$toi_pp  <- merge_add(skater_totals$toi_pp,  sk$toi_pp)
    skater_totals$toi_pk  <- merge_add(skater_totals$toi_pk,  sk$toi_pk)
    game_players <- unique(names(sk$toi_5v5))
    skater_totals$gp <- merge_add(skater_totals$gp, to_named_list(game_players, rep(1, length(game_players))))
    if (!is.null(sk$player_team_abbrev) && length(sk$player_team_abbrev) > 0) {
      player_team_rows[[length(player_team_rows) + 1]] <- data.frame(
        player_id = names(sk$player_team_abbrev),
        team_abbrev = unlist(sk$player_team_abbrev, use.names = FALSE),
        stringsAsFactors = FALSE
      )
    }
    skater_games_ok <- skater_games_ok + 1L
  }
  processed_this_run <- c(processed_this_run, gid)
  Sys.sleep(0.2)
}

if (length(processed_this_run) == 0) {
  cat("No games successfully processed this run. Exiting without writing.\n")
  quit(save = "no", status = 0)
}

gv <- function(lst, pid) lst[[pid]] %||% 0

# ── Team output ───────────────────────────────────────────────────────────
all_teams <- unique(names(team_totals$gp))
team_df <- data.frame(
  team_abbrev = all_teams,
  gp_onice    = sapply(all_teams, gv, lst = team_totals$gp),
  cf_5v5      = sapply(all_teams, gv, lst = team_totals$cf),
  ca_5v5      = sapply(all_teams, gv, lst = team_totals$ca),
  gf_5v5      = sapply(all_teams, gv, lst = team_totals$gf),
  ga_5v5      = sapply(all_teams, gv, lst = team_totals$ga),
  pp_goals    = sapply(all_teams, gv, lst = team_totals$pp_goals),
  pp_shots    = sapply(all_teams, gv, lst = team_totals$pp_shots),
  pk_goals_against = sapply(all_teams, gv, lst = team_totals$pk_ga),
  pk_shots_against = sapply(all_teams, gv, lst = team_totals$pk_shots),
  # NOTE: despite the name not saying "5v5" the way gf_5v5/ga_5v5 does,
  # these ARE 5v5-only (filtered at the aggregation step above) — that
  # ambiguous naming is exactly what let a real scope-mismatch bug slip
  # through once already (team-level was all-situation while player-level
  # xg_for_5v5 was 5v5-only). Keep these two scoped together if this ever
  # gets extended to also track an all-situation version.
  xg_for     = sapply(all_teams, gv, lst = team_totals$xg_for),
  xg_against = sapply(all_teams, gv, lst = team_totals$xg_against),
  toi_5v5_sec = sapply(all_teams, gv, lst = team_totals$toi_5v5),
  toi_pp_sec  = sapply(all_teams, gv, lst = team_totals$toi_pp),
  toi_pk_sec  = sapply(all_teams, gv, lst = team_totals$toi_pk),
  stringsAsFactors = FALSE
) %>% mutate(
  cf_pct_5v5     = ifelse((cf_5v5 + ca_5v5) > 0, round(100 * cf_5v5 / (cf_5v5 + ca_5v5), 1), NA_real_),
  gf_per60_5v5   = ifelse(toi_5v5_sec > 0, round(gf_5v5 / (toi_5v5_sec / 3600), 2), NA_real_),
  ga_per60_5v5   = ifelse(toi_5v5_sec > 0, round(ga_5v5 / (toi_5v5_sec / 3600), 2), NA_real_),
  pp_goals_per60 = ifelse(toi_pp_sec > 0, round(pp_goals / (toi_pp_sec / 3600), 2), NA_real_),
  pk_ga_per60    = ifelse(toi_pk_sec > 0, round(pk_goals_against / (toi_pk_sec / 3600), 2), NA_real_)
)
write.csv(team_df, TEAM_OUT, row.names = FALSE)

# ── Goalie output ─────────────────────────────────────────────────────────
all_goalies <- unique(names(goalie_totals$gp))
goalie_df <- data.frame(
  player_id = all_goalies,
  gp_onice  = sapply(all_goalies, gv, lst = goalie_totals$gp),
  shots_5v5 = sapply(all_goalies, gv, lst = goalie_totals$shots_5v5),
  ga_5v5    = sapply(all_goalies, gv, lst = goalie_totals$ga_5v5),
  shots_pk  = sapply(all_goalies, gv, lst = goalie_totals$shots_pk),
  ga_pk     = sapply(all_goalies, gv, lst = goalie_totals$ga_pk),
  xg_faced  = sapply(all_goalies, gv, lst = goalie_totals$xg_faced),
  stringsAsFactors = FALSE
) %>% mutate(
  sv_pct_5v5 = ifelse(shots_5v5 > 0, round(1 - ga_5v5 / shots_5v5, 4), NA_real_),
  sv_pct_pk  = ifelse(shots_pk  > 0, round(1 - ga_pk  / shots_pk,  4), NA_real_),
  # Goals Saved Above Expected — real goaltending skill independent of shot
  # quality, using the same 5v5+PK-against scope as sv_pct_5v5/sv_pct_pk
  # above (see the scope note where xg_faced gets accumulated). Positive
  # GSAx = allowed fewer goals than the shots they faced would suggest an
  # average goalie allows; negative = allowed more.
  actual_ga_tracked = ga_5v5 + ga_pk,
  gsax = ifelse(xg_faced > 0, round(xg_faced - actual_ga_tracked, 2), NA_real_)
)
write.csv(goalie_df, GOALIE_OUT, row.names = FALSE)

# ── Determine each player's team for the season ─────────────────────────────
# Mode (most frequent) of the team_abbrev seen across this run's newly-
# processed games. For players with no new games this run, fall back to
# whatever was already recorded — this is a "current team" heuristic
# consistent with how the rest of this pipeline treats team association
# (current roster is the source of truth), not a full multi-team season
# split for traded players.
team_lookup <- list()
if (length(player_team_rows) > 0) {
  ptr <- bind_rows(player_team_rows)
  ptr <- ptr[!is.na(ptr$team_abbrev), ]
  if (nrow(ptr) > 0) {
    modes <- ptr %>% count(player_id, team_abbrev) %>% group_by(player_id) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup()
    team_lookup <- setNames(as.list(modes$team_abbrev), modes$player_id)
  }
}
existing_team_lookup <- if (!is.null(existing_skater) && "team_abbrev" %in% names(existing_skater)) {
  setNames(as.list(existing_skater$team_abbrev), as.character(existing_skater$player_id))
} else list()

# ── Skater output (only if we have any shift-dependent data at all) ────────
all_pids <- unique(names(skater_totals$gp))
if (length(all_pids) > 0) {
  skater_df <- data.frame(
    player_id   = all_pids,
    team_abbrev = sapply(all_pids, function(p) {
      if (!is.null(team_lookup[[p]])) team_lookup[[p]]
      else if (!is.null(existing_team_lookup[[p]])) existing_team_lookup[[p]]
      else NA_character_
    }),
    gp_onice    = sapply(all_pids, gv, lst = skater_totals$gp),
    cf_5v5      = sapply(all_pids, gv, lst = skater_totals$cf),
    ca_5v5      = sapply(all_pids, gv, lst = skater_totals$ca),
    gf_5v5      = sapply(all_pids, gv, lst = skater_totals$gf),
    ga_5v5      = sapply(all_pids, gv, lst = skater_totals$ga),
    ev_goals    = sapply(all_pids, gv, lst = skater_totals$ev_goals),
    ev_assists  = sapply(all_pids, gv, lst = skater_totals$ev_assists),
    pp_goals    = sapply(all_pids, gv, lst = skater_totals$pp_goals),
    pp_assists  = sapply(all_pids, gv, lst = skater_totals$pp_assists),
    pk_ga       = sapply(all_pids, gv, lst = skater_totals$pk_ga),
    pp_shots         = sapply(all_pids, gv, lst = skater_totals$pp_shots),
    pk_shots_against = sapply(all_pids, gv, lst = skater_totals$pk_shots_against),
    pp_gf_onice = sapply(all_pids, gv, lst = skater_totals$pp_gf_onice),
    pk_ga_onice = sapply(all_pids, gv, lst = skater_totals$pk_ga_onice),
    xg_for_5v5     = sapply(all_pids, gv, lst = skater_totals$xg_for_5v5),
    xg_against_5v5 = sapply(all_pids, gv, lst = skater_totals$xg_against_5v5),
    pen_taken   = sapply(all_pids, gv, lst = skater_totals$pen_taken),
    pen_drawn   = sapply(all_pids, gv, lst = skater_totals$pen_drawn),
    toi_5v5_sec = sapply(all_pids, gv, lst = skater_totals$toi_5v5),
    toi_pp_sec  = sapply(all_pids, gv, lst = skater_totals$toi_pp),
    toi_pk_sec  = sapply(all_pids, gv, lst = skater_totals$toi_pk),
    stringsAsFactors = FALSE
  ) %>% mutate(
    cf_pct_5v5      = ifelse((cf_5v5 + ca_5v5) > 0, round(100 * cf_5v5 / (cf_5v5 + ca_5v5), 1), NA_real_),
    gf_per60_5v5    = ifelse(toi_5v5_sec > 0, round(gf_5v5 / (toi_5v5_sec / 3600), 2), NA_real_),
    ga_per60_5v5    = ifelse(toi_5v5_sec > 0, round(ga_5v5 / (toi_5v5_sec / 3600), 2), NA_real_),
    ev_points_per60 = ifelse(toi_5v5_sec > 0, round((ev_goals + ev_assists) / (toi_5v5_sec / 3600), 2), NA_real_),
    pp_points_per60 = ifelse(toi_pp_sec > 0, round((pp_goals + pp_assists) / (toi_pp_sec / 3600), 2), NA_real_),
    pk_ga_per60     = ifelse(toi_pk_sec > 0, round(pk_ga / (toi_pk_sec / 3600), 2), NA_real_),
    # Real on-ice PK save rate — how often shots against were stopped while
    # THIS player was on the ice killing a penalty. Previously impossible
    # (no per-player PK shot volume existed); pk_ga_per60 alone can't tell
    # you whether a low goals-against rate reflects genuinely good penalty
    # killing or just facing very few shots.
    pk_save_pct     = ifelse(pk_shots_against > 0, round(1 - pk_ga / pk_shots_against, 4), NA_real_),
    # On-ice (not individual-credit) PP-GF/60 and PK-GA/60 — these are what
    # WOWY actually needs to compare against team totals, same idea as
    # gf_per60_5v5/ga_per60_5v5 above but for special teams.
    pp_gf_onice_per60 = ifelse(toi_pp_sec > 0, round(pp_gf_onice / (toi_pp_sec / 3600), 2), NA_real_),
    pk_ga_onice_per60 = ifelse(toi_pk_sec > 0, round(pk_ga_onice / (toi_pk_sec / 3600), 2), NA_real_),
    # On-ice xG-for/against per 60 — a less noisy version of gf_per60_5v5/
    # ga_per60_5v5 above, since it reflects shot-generation/shot-suppression
    # quality directly rather than actual goals (which include shooting/
    # goaltending luck on top of the underlying shot quality).
    xg_for_per60_5v5     = ifelse(toi_5v5_sec > 0, round(xg_for_5v5 / (toi_5v5_sec / 3600), 3), NA_real_),
    xg_against_per60_5v5 = ifelse(toi_5v5_sec > 0, round(xg_against_5v5 / (toi_5v5_sec / 3600), 3), NA_real_),
    # Penalties can happen in any strength state, so total tracked TOI
    # (5v5+PP+PK) is the denominator here rather than one specific state.
    # This slightly undercounts true all-situation TOI (garbage-time/
    # empty-net play isn't tracked), which is a minor, acceptable
    # imprecision rather than a real error.
    toi_all_tracked_sec = toi_5v5_sec + toi_pp_sec + toi_pk_sec,
    pen_taken_per60 = ifelse(toi_all_tracked_sec > 0, round(pen_taken / (toi_all_tracked_sec / 3600), 2), NA_real_),
    pen_drawn_per60 = ifelse(toi_all_tracked_sec > 0, round(pen_drawn / (toi_all_tracked_sec / 3600), 2), NA_real_),
    penalty_diff_per60 = ifelse(!is.na(pen_drawn_per60) & !is.na(pen_taken_per60), round(pen_drawn_per60 - pen_taken_per60, 2), NA_real_)
  )
  write.csv(skater_df, SKATER_OUT, row.names = FALSE)
}

# ── Raw shots output (event-level, for future xG training) ─────────────────
if (length(shots_raw_new) > 0) {
  new_shots_df <- bind_rows(shots_raw_new)
  existing_shots_raw <- if (file.exists(SHOTS_RAW_OUT)) tryCatch(read.csv(SHOTS_RAW_OUT, stringsAsFactors = FALSE), error = function(e) NULL) else NULL
  combined_shots_df <- if (!is.null(existing_shots_raw) && nrow(existing_shots_raw) > 0) {
    # De-dupe on game_id in case a game somehow got reprocessed — keep the
    # existing rows and only add shots from genuinely new games.
    new_shots_df <- new_shots_df[!(new_shots_df$game_id %in% existing_shots_raw$game_id), ]
    bind_rows(existing_shots_raw, new_shots_df)
  } else new_shots_df
  write.csv(combined_shots_df, SHOTS_RAW_OUT, row.names = FALSE)
  cat("Wrote", SHOTS_RAW_OUT, "-", nrow(combined_shots_df), "shot events total (", nrow(new_shots_df), "new)\n")
}

writeLines(unique(c(processed_ids, processed_this_run)), STATE_FILE)
cat("Wrote", TEAM_OUT, "-", nrow(team_df), "teams |", GOALIE_OUT, "-", nrow(goalie_df), "goalies", "\n")
if (length(all_pids) > 0) cat("Wrote", SKATER_OUT, "-", length(all_pids), "players\n")
cat(length(processed_this_run), "games processed this run (",
    team_games_ok, "contributed team/goalie stats,", skater_games_ok, "also had usable shift data ).\n")
