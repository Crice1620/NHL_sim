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
LINEUP_OUT    <- file.path(OUT_DIR, "shot_lineups.csv")  # full on-ice lineups per 5v5 shot event — RAPM's design-matrix input, joinable to shots_raw.csv via game_id+event_idx
STINTS_OUT    <- file.path(OUT_DIR, "stints.csv")  # 5v5 stint-level lineups + shot counts + duration — shot-VOLUME RAPM's design-matrix input (see build_stints())

# ── xG model loading and scoring ────────────────────────────────────────────
XG_MODEL_PATH <- file.path("data", "xg_model", "xg_model.rds")
xg_model_obj <- if (file.exists(XG_MODEL_PATH)) tryCatch(readRDS(XG_MODEL_PATH), error = function(e) NULL) else NULL
if (is.null(xg_model_obj)) {
  cat("No trained xG model found at", XG_MODEL_PATH, "— shots will be written without xg scores, goalie GSAx will be NA.\n")
} else {
  cat("Loaded xG model (trained", as.character(xg_model_obj$trained_at), "| seasons:", paste(xg_model_obj$seasons, collapse=", "), ")\n")
}

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

  s$shot_type_clean  <- factor(s$shot_type_clean,  levels = xg_obj$shot_type_levels)
  s$shooter_strength <- factor(s$shooter_strength, levels = xg_obj$shooter_strength_levels)

  valid <- !is.na(s$dist_to_net) & !is.na(s$angle_to_net) & !is.na(s$is_rebound) &
           !is.na(s$shot_type_clean) & !is.na(s$shooter_strength) & !is.na(s$target_side) & !is.na(s$goalie_id)
  xg_vals <- rep(NA_real_, nrow(s))
  if (any(valid)) {
    X_raw <- model.matrix(~ dist_to_net + angle_to_net + is_rebound + shot_type_clean + shooter_strength - 1,
                            data = s[valid, c("dist_to_net","angle_to_net","is_rebound","shot_type_clean","shooter_strength")] %>%
                              mutate(is_rebound = as.integer(is_rebound)))
    train_cols <- xg_obj$model$feature_names
    X_new <- matrix(0, nrow = nrow(X_raw), ncol = length(train_cols), dimnames = list(NULL, train_cols))
    common_cols <- intersect(train_cols, colnames(X_raw))
    X_new[, common_cols] <- X_raw[, common_cols]
    X_new <- matrix(as.numeric(X_new), nrow = nrow(X_new), ncol = ncol(X_new), dimnames = dimnames(X_new))
    xg_vals[valid] <- predict(xg_obj$model, xgb.DMatrix(data = X_new))
  }
  shots_df_scored <- s %>% select(-is_home_shooter, -target_side, -norm_x, -norm_y, -dist_to_net, -angle_to_net,
                                    -shooter_strength, -shot_type_clean, -time_since_own_last_shot, -is_rebound)
  shots_df_scored$xg <- xg_vals
  shots_df_scored$is_rebound_at_scoring <- s$is_rebound
  shots_df_scored
}

# Fallback for when home_defending_side is missing (confirmed: the NHL API
# doesn't include this field for older seasons — 100% missing for 2011-2016
# tested, 0% missing for 2023/2025). Infers it from shot coordinates
# instead: teams switch ends every period (a fixed rule, not a tendency),
# so the home team's own shots should cluster toward whichever net they're
# ATTACKING that period — the opposite of what they defend. VALIDATED
# against 2023 (real ground truth available): 100% agreement at both the
# row level (164,336 shots) and the game-period level (4,593 periods),
# zero disagreements even in the lowest-sample-size bucket — a strong,
# checked result, not an unverified guess. Logged when it fires (not
# silent) — if this triggers during NORMAL daily processing of a current
# season, that would be surprising and worth a second look, unlike its
# expected use backfilling old seasons.
infer_defending_side <- function(shots_df) {
  per_period <- shots_df %>%
    filter(owner_team_id == home_id, !is.na(x_coord)) %>%
    group_by(game_id, period) %>%
    summarise(home_median_x = median(x_coord, na.rm = TRUE), .groups = "drop") %>%
    mutate(inferred_side = ifelse(home_median_x > 0, "left", "right")) %>%
    select(game_id, period, inferred_side)
  shots_df %>% left_join(per_period, by = c("game_id", "period"))
}

MAX_GAMES_PER_RUN <- if (MODE == "backfill") Inf else 50L

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

nhl_get <- function(url, timeout_s = 25) {
  resp <- tryCatch(GET(url, timeout(timeout_s)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE),
           error = function(e) NULL)
}

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

parse_mmss <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_real_)
  p <- strsplit(as.character(x), ":")[[1]]
  if (length(p) != 2) return(NA_real_)
  suppressWarnings(as.numeric(p[1]) * 60 + as.numeric(p[2]))
}

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

process_game_skaters <- function(pbp, shifts_raw, home_id, away_id, sit_df, shots_xg_lookup = numeric(0), game_id = NA_character_) {
  shifts <- lapply(shifts_raw$data, function(s) {
    tryCatch({
      tc <- suppressWarnings(as.integer(s$typeCode %||% NA))
      if (is.na(tc) || tc != 517) return(NULL)
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
    rows <- shifts_df[shifts_df$team_id == team_id & shifts_df$start_abs < t_abs & shifts_df$end_abs >= t_abs, ]
    unique(rows$player_id)
  }

  cf <- list(); ca <- list(); gf <- list(); ga <- list()
  ev_goals <- list(); ev_assists <- list()
  pp_goals <- list(); pp_assists <- list()
  pk_ga <- list()
  pp_shots_ind <- list(); pk_shots_against_ind <- list()
  pp_gf_onice_ind <- list(); pk_ga_onice_ind <- list()
  xg_for_5v5 <- list(); xg_against_5v5 <- list()
  toi_5v5 <- list(); toi_pp <- list(); toi_pk <- list()
  player_team_id <- list()
  # RAPM needs the FULL lineup combination for every event (who was on the
  # ice TOGETHER), not just each player's own marginal total the way
  # for_on/against_on (and their PP/PK equivalents) get used everywhere
  # else below (via bump()). This is genuinely new — everything else in
  # this function already computed these on-ice lists for every shot, at
  # both 5v5 and PP/PK, they just never got kept. One row per shot event,
  # both sides' full rosters as semicolon-joined strings (compact — full
  # long-format one-row-per-player-per-event would run ~10x larger across
  # 15 seasons' worth of games) — matches shots_raw.csv's granularity
  # exactly (one row per shot, joinable via game_id+event_idx).
  # situation_code is preserved on every row so the exact strength state
  # (5v5, 5v4, 5v3, 4v3, etc.) can be recovered later — PP/PK RAPM needs
  # to handle asymmetric strength states differently from 5v5's uniform
  # 5-a-side case, and that's a modeling decision for later, not something
  # this capture step should pre-judge by discarding the raw code.
  lineup_rows <- list()

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
      shot_xg <- unname(shots_xg_lookup[as.character(play_i)])
      if (!is.na(shot_xg)) {
        xg_for_5v5 <- bump(xg_for_5v5, for_on, by = shot_xg)
        xg_against_5v5 <- bump(xg_against_5v5, against_on, by = shot_xg)
      }
      # NEW — RAPM capture: same for_on/against_on already computed above
      # for the counter-bumping, just also written down as a row instead
      # of discarded. Only kept when both sides have a plausible full
      # complement (>=3 skaters) — a shift-chart gap or mid-transition
      # artifact producing an obviously incomplete lineup would poison a
      # RAPM regression far more than it would a simple per-player counter,
      # so this is a stricter bar than bump() above needs.
      if (length(for_on) >= 3 && length(against_on) >= 3) {
        lineup_rows[[length(lineup_rows) + 1]] <- data.frame(
          game_id = game_id, event_idx = play_i, situation_code = code,
          for_team_id = owner_team, against_team_id = against_team,
          for_players = paste(for_on, collapse = ";"),
          against_players = paste(against_on, collapse = ";"),
          stringsAsFactors = FALSE
        )
      }
    }
    if (is_shot_evt && sit$label %in% c("home_pp", "away_pp") && !is.na(t_abs) && !is.na(owner_team)) {
      owner_on_pp <- (identical(owner_team, home_id) && sit$label == "home_pp") ||
                     (identical(owner_team, away_id) && sit$label == "away_pp")
      against_team <- if (identical(owner_team, home_id)) away_id else home_id
      if (isTRUE(owner_on_pp)) {
        pp_shooters_on <- on_ice_at(t_abs, owner_team)
        pk_defenders_on <- on_ice_at(t_abs, against_team)
        pp_shots_ind <- bump(pp_shots_ind, pp_shooters_on)
        pk_shots_against_ind <- bump(pk_shots_against_ind, pk_defenders_on)
        if (typ == "goal") {
          pp_gf_onice_ind <- bump(pp_gf_onice_ind, pp_shooters_on)
          pk_ga_onice_ind <- bump(pk_ga_onice_ind, pk_defenders_on)
        }
        # NEW — RAPM capture for PP/PK, same principle as the 5v5 capture
        # above: pp_shooters_on/pk_defenders_on are already computed right
        # here for the counter bumps, just also written down instead of
        # discarded. Deliberately a LOOSER gate than 5v5's >=3 (just
        # non-empty on both sides) rather than a hardcoded skater count —
        # PP/PK strength states aren't uniform (5v4, 5v3, 4v3 all have
        # different natural counts), so raw situation_code is preserved
        # instead, letting the actual modeling step decide how to bucket
        # by exact strength state rather than this capture step guessing.
        if (length(pp_shooters_on) > 0 && length(pk_defenders_on) > 0) {
          lineup_rows[[length(lineup_rows) + 1]] <- data.frame(
            game_id = game_id, event_idx = play_i, situation_code = code,
            for_team_id = owner_team, against_team_id = against_team,
            for_players = paste(pp_shooters_on, collapse = ";"),
            against_players = paste(pk_defenders_on, collapse = ";"),
            stringsAsFactors = FALSE
          )
        }
      }
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
       toi_5v5 = toi_5v5, toi_pp = toi_pp, toi_pk = toi_pk,
       lineup = if (length(lineup_rows) > 0) bind_rows(lineup_rows) else NULL,
       # Returned so process_game() can build shot-volume RAPM stints
       # without duplicating this function's own shift-parsing logic —
       # process_game() already independently builds sit_df's 5v5 segment
       # boundaries and shot_rows' timestamps in its own scope, so this is
       # the one missing piece needed there.
       shifts_df = shifts_df)
}

# ── Shot-volume RAPM stint reconstruction ────────────────────────────────────
# Unlike xG-RAPM (one row per SHOT EVENT — shot_lineups.csv already covers
# this), shot-VOLUME RAPM needs to model shots-per-minute-of-ice-time,
# which requires knowing every continuous interval of unchanged on-ice
# personnel — "stints" — whether or not a shot happened during them. A
# 5v5 segment (from sit_df, already built for team-level TOI tracking)
# can span many individual player shifts (line changes don't change the
# STRENGTH STATE, just who's on the ice) — each shift-change point WITHIN
# a 5v5 segment splits it into a separate stint.
#
# Reuses three things that already exist elsewhere in this script, rather
# than fetching or computing anything new: sit_df's 5v5 segment
# boundaries (built for team_toi tracking), shifts_df's individual player
# shift start/end times (returned from process_game_skaters — see its own
# return statement for why), and shots_raw_df's shot timestamps (built by
# process_game()'s own play-by-play loop). This function just combines
# those three, it doesn't re-derive any of them from scratch.
build_stints <- function(sit_df, shifts_df, shots_df, home_id, away_id, game_id) {
  if (is.null(shifts_df) || nrow(shifts_df) == 0 || is.null(sit_df) || nrow(sit_df) == 0) return(NULL)
  game_end_abs <- suppressWarnings(max(shifts_df$end_abs, na.rm = TRUE))
  seg_start <- sit_df$t_abs; seg_end <- c(sit_df$t_abs[-1], game_end_abs); seg_label <- sit_df$label

  on_ice_at_local <- function(t_abs, team_id) {
    rows <- shifts_df[shifts_df$team_id == team_id & shifts_df$start_abs < t_abs & shifts_df$end_abs >= t_abs, ]
    unique(rows$player_id)
  }

  stint_rows <- list()
  for (i in seq_along(seg_start)) {
    if (is.na(seg_label[i]) || seg_label[i] != "5v5") next  # 5v5-only, matching xG-RAPM's own scope decision
    s_start <- seg_start[i]; s_end <- seg_end[i]
    if (is.na(s_start) || is.na(s_end) || s_end <= s_start) next
    # Shift-change points STRICTLY inside this 5v5 segment split it into
    # separate stints — a line change partway through a 5v5 segment means
    # the on-ice personnel changed even though the strength state didn't.
    boundaries <- unique(c(
      shifts_df$start_abs[shifts_df$start_abs > s_start & shifts_df$start_abs < s_end],
      shifts_df$end_abs[shifts_df$end_abs > s_start & shifts_df$end_abs < s_end]
    ))
    cut_points <- sort(unique(c(s_start, boundaries, s_end)))
    if (length(cut_points) < 2) next
    for (j in seq_len(length(cut_points) - 1)) {
      stint_start <- cut_points[j]; stint_end <- cut_points[j + 1]
      if (stint_end <= stint_start) next
      # Probe strictly inside the stint (not exactly at a boundary) to
      # avoid ambiguity in on_ice_at_local's own strict-inequality check.
      probe_t <- stint_start + min(0.5, (stint_end - stint_start) / 2)
      home_on <- on_ice_at_local(probe_t, home_id)
      away_on <- on_ice_at_local(probe_t, away_id)
      # Guard: only keep genuine, complete 5-on-5 stints — same quality
      # gate philosophy as shot_lineups.csv, so a shift-data gap doesn't
      # silently masquerade as a real (but incomplete) stint.
      if (length(home_on) != 5 || length(away_on) != 5) next
      duration <- stint_end - stint_start
      shots_home <- if (!is.null(shots_df)) sum(shots_df$owner_team_id == home_id & shots_df$t_abs >= stint_start & shots_df$t_abs < stint_end, na.rm = TRUE) else 0
      shots_away <- if (!is.null(shots_df)) sum(shots_df$owner_team_id == away_id & shots_df$t_abs >= stint_start & shots_df$t_abs < stint_end, na.rm = TRUE) else 0
      stint_rows[[length(stint_rows) + 1]] <- data.frame(
        game_id = game_id, stint_start = stint_start, stint_end = stint_end, duration_sec = duration,
        home_players = paste(sort(home_on), collapse = ";"), away_players = paste(sort(away_on), collapse = ";"),
        shots_home = shots_home, shots_away = shots_away,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(stint_rows) == 0) return(NULL)
  bind_rows(stint_rows)
}

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
  shot_rows <- list()
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
  if (!is.null(shots_raw_df) && "home_defending_side" %in% names(shots_raw_df) &&
      any(is.na(shots_raw_df$home_defending_side))) {
    n_missing <- sum(is.na(shots_raw_df$home_defending_side))
    cat("  home_defending_side missing for", n_missing, "of", nrow(shots_raw_df),
        "shots in game", game_id, "— inferring from shot coordinates",
        "(validated 100% agreement against known-good seasons; see onice_stats.R notes).\n")
    shots_raw_df <- infer_defending_side(shots_raw_df) %>%
      mutate(home_defending_side = coalesce(home_defending_side, inferred_side)) %>%
      select(-inferred_side)
  }
  shots_raw_df <- score_shots_with_xg(shots_raw_df, xg_model_obj)
  shots_xg_lookup <- if (!is.null(shots_raw_df) && "xg" %in% names(shots_raw_df) && "event_idx" %in% names(shots_raw_df)) {
    setNames(shots_raw_df$xg, as.character(shots_raw_df$event_idx))
  } else numeric(0)

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
    skater_result <- tryCatch(process_game_skaters(pbp, shifts_raw, home_id, away_id, sit_df, shots_xg_lookup, game_id), error = function(e) NULL)
    if (!is.null(skater_result)) {
      skater_result$player_team_abbrev <- lapply(skater_result$player_team_id, function(tid) {
        if (identical(as.character(tid), as.character(home_id))) home_abbrev
        else if (identical(as.character(tid), as.character(away_id))) away_abbrev
        else NA_character_
      })
    }
  }

  stints_result <- if (!is.null(skater_result) && !is.null(skater_result$shifts_df)) {
    tryCatch(build_stints(sit_df, skater_result$shifts_df, shots_raw_df, home_id, away_id, game_id),
             error = function(e) NULL)
  } else NULL

  list(team = team_result, goalie = goalie_result, skater = skater_result,
       shots_raw = shots_raw_df, penalty = penalty_result, stints = stints_result)
}

existing_skater <- if (file.exists(SKATER_OUT)) tryCatch(read.csv(SKATER_OUT, stringsAsFactors = FALSE), error = function(e) NULL) else NULL
existing_team   <- if (file.exists(TEAM_OUT))   tryCatch(read.csv(TEAM_OUT,   stringsAsFactors = FALSE), error = function(e) NULL) else NULL
existing_goalie <- if (file.exists(GOALIE_OUT)) tryCatch(read.csv(GOALIE_OUT, stringsAsFactors = FALSE), error = function(e) NULL) else NULL

blank_skater <- function() list(cf=list(),ca=list(),gf=list(),ga=list(),ev_goals=list(),ev_assists=list(),
                                 pp_goals=list(),pp_assists=list(),pk_ga=list(),pp_shots=list(),pk_shots_against=list(),
                                 pp_gf_onice=list(),pk_ga_onice=list(),pen_taken=list(),pen_drawn=list(),
                                 xg_for_5v5=list(),xg_against_5v5=list(),
                                 toi_5v5=list(),toi_pp=list(),toi_pk=list(),gp=list())
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
if (!is.null(existing_goalie) && nrow(existing_goalie) > 0 && !"xg_faced" %in% names(existing_goalie)) existing_goalie$xg_faced <- 0
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
player_team_rows <- list()
shots_raw_new <- list()
lineup_rows_new <- list()  # accumulates across games, same append pattern as shots_raw_new
stint_rows_new <- list()  # shot-volume RAPM's design-matrix input — same append pattern as lineup_rows_new

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
    scored_shots <- res$shots_raw
    shots_raw_new[[length(shots_raw_new) + 1]] <- scored_shots
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

  if (!is.null(res$stints) && nrow(res$stints) > 0) {
    st <- res$stints
    # home_id/away_id aren't in scope here the way tr$home_id is (tr is
    # this game's team_result) — reusing that instead of re-deriving them.
    st$home_abbrev <- tr$home_abbrev
    st$away_abbrev <- tr$away_abbrev
    stint_rows_new[[length(stint_rows_new) + 1]] <- st
  }

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
    if (!is.null(sk$lineup) && nrow(sk$lineup) > 0) {
      lu <- sk$lineup
      lu$for_team_abbrev <- ifelse(lu$for_team_id == tr$home_id, tr$home_abbrev,
                                    ifelse(lu$for_team_id == tr$away_id, tr$away_abbrev, NA_character_))
      lu$against_team_abbrev <- ifelse(lu$against_team_id == tr$home_id, tr$home_abbrev,
                                        ifelse(lu$against_team_id == tr$away_id, tr$away_abbrev, NA_character_))
      lineup_rows_new[[length(lineup_rows_new) + 1]] <- lu
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
  actual_ga_tracked = ga_5v5 + ga_pk,
  gsax = ifelse(xg_faced > 0, round(xg_faced - actual_ga_tracked, 2), NA_real_)
)
write.csv(goalie_df, GOALIE_OUT, row.names = FALSE)

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
    pk_save_pct     = ifelse(pk_shots_against > 0, round(1 - pk_ga / pk_shots_against, 4), NA_real_),
    pp_gf_onice_per60 = ifelse(toi_pp_sec > 0, round(pp_gf_onice / (toi_pp_sec / 3600), 2), NA_real_),
    pk_ga_onice_per60 = ifelse(toi_pk_sec > 0, round(pk_ga_onice / (toi_pk_sec / 3600), 2), NA_real_),
    xg_for_per60_5v5     = ifelse(toi_5v5_sec > 0, round(xg_for_5v5 / (toi_5v5_sec / 3600), 3), NA_real_),
    xg_against_per60_5v5 = ifelse(toi_5v5_sec > 0, round(xg_against_5v5 / (toi_5v5_sec / 3600), 3), NA_real_),
    toi_all_tracked_sec = toi_5v5_sec + toi_pp_sec + toi_pk_sec,
    pen_taken_per60 = ifelse(toi_all_tracked_sec > 0, round(pen_taken / (toi_all_tracked_sec / 3600), 2), NA_real_),
    pen_drawn_per60 = ifelse(toi_all_tracked_sec > 0, round(pen_drawn / (toi_all_tracked_sec / 3600), 2), NA_real_),
    penalty_diff_per60 = ifelse(!is.na(pen_drawn_per60) & !is.na(pen_taken_per60), round(pen_drawn_per60 - pen_taken_per60, 2), NA_real_)
  )
  write.csv(skater_df, SKATER_OUT, row.names = FALSE)
}

if (length(shots_raw_new) > 0) {
  new_shots_df <- bind_rows(shots_raw_new)
  existing_shots_raw <- if (file.exists(SHOTS_RAW_OUT)) tryCatch(read.csv(SHOTS_RAW_OUT, stringsAsFactors = FALSE), error = function(e) NULL) else NULL
  combined_shots_df <- if (!is.null(existing_shots_raw) && nrow(existing_shots_raw) > 0) {
    new_shots_df <- new_shots_df[!(new_shots_df$game_id %in% existing_shots_raw$game_id), ]
    bind_rows(existing_shots_raw, new_shots_df)
  } else new_shots_df
  write.csv(combined_shots_df, SHOTS_RAW_OUT, row.names = FALSE)
  cat("Wrote", SHOTS_RAW_OUT, "-", nrow(combined_shots_df), "shot events total (", nrow(new_shots_df), "new)\n")
}

if (length(lineup_rows_new) > 0) {
  new_lineup_df <- bind_rows(lineup_rows_new)
  existing_lineup <- if (file.exists(LINEUP_OUT)) tryCatch(read.csv(LINEUP_OUT, stringsAsFactors = FALSE), error = function(e) NULL) else NULL
  combined_lineup_df <- if (!is.null(existing_lineup) && nrow(existing_lineup) > 0) {
    new_lineup_df <- new_lineup_df[!(new_lineup_df$game_id %in% existing_lineup$game_id), ]
    bind_rows(existing_lineup, new_lineup_df)
  } else new_lineup_df
  write.csv(combined_lineup_df, LINEUP_OUT, row.names = FALSE)
  cat("Wrote", LINEUP_OUT, "-", nrow(combined_lineup_df), "shot-lineup rows total (", nrow(new_lineup_df), "new)\n")
}

if (length(stint_rows_new) > 0) {
  new_stints_df <- bind_rows(stint_rows_new)
  existing_stints <- if (file.exists(STINTS_OUT)) tryCatch(read.csv(STINTS_OUT, stringsAsFactors = FALSE), error = function(e) NULL) else NULL
  combined_stints_df <- if (!is.null(existing_stints) && nrow(existing_stints) > 0) {
    new_stints_df <- new_stints_df[!(new_stints_df$game_id %in% existing_stints$game_id), ]
    bind_rows(existing_stints, new_stints_df)
  } else new_stints_df
  write.csv(combined_stints_df, STINTS_OUT, row.names = FALSE)
  cat("Wrote", STINTS_OUT, "-", nrow(combined_stints_df), "stint rows total (", nrow(new_stints_df), "new)\n")
}

writeLines(unique(c(processed_ids, processed_this_run)), STATE_FILE)
cat("Wrote", TEAM_OUT, "-", nrow(team_df), "teams |", GOALIE_OUT, "-", nrow(goalie_df), "goalies", "\n")
if (length(all_pids) > 0) cat("Wrote", SKATER_OUT, "-", length(all_pids), "players\n")
cat(length(processed_this_run), "games processed this run (",
    team_games_ok, "contributed team/goalie stats,", skater_games_ok, "also had usable shift data ).\n")
