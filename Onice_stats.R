# =============================================================================
# onice_stats.R — Daily incremental on-ice reconstruction from NHL
# play-by-play + shift-chart data.
#
# WHAT THIS DOES:
#   For every newly-completed game since the last run, this script:
#     1. Fetches play-by-play events (shots, goals, etc.), each tagged with
#        a situationCode that tells us the strength state (5v5, PP, PK, ...).
#     2. Fetches shift-chart data (every player's exact shift start/end
#        times) so we can reconstruct who was actually ON THE ICE for each
#        event — not just who did the shooting/scoring.
#     3. Accumulates, per skater: on-ice Corsi For/Against and Goals For/
#        Against at 5v5 (real "EV Offence"/"EV Defence", not a box-score
#        proxy), individual goals/assists at 5v5 and on the power play, and
#        on-ice goals-against while shorthanded (real Penalty-Kill defense).
#     4. Merges these into a running SEASON-TO-DATE CSV and commits it —
#        each run only processes games completed since the last run, using
#        a small "already processed" state file, so this stays fast even
#        run daily across an 84-game season.
#
# HONEST CAVEATS (read these before debugging a discrepancy):
#   - situationCode format ("1551" etc: [awayGoalie][awaySkaters][homeSkaters]
#     [homeGoalie]) and the shift-chart endpoint/field names below are based
#     on well-documented public NHL API conventions, NOT verified against a
#     live call from this environment. If the script errors on real data,
#     the most likely culprit is a field name or endpoint path that's
#     drifted — check the actual JSON shape via the diagnostic block near
#     the top of the game-processing loop.
#   - On-ice reconstruction assumes standard 20-minute periods for computing
#     absolute game-seconds (period-1)*1200 + timeInPeriod. This is correct
#     for regulation and 3-on-3 OT in the way it affects WITHIN-period shift
#     overlap math, but shootouts have no meaningful shifts and are skipped.
#   - Delayed penalties, own-goals, and penalty-shot goals are not
#     specially handled — they'll fall into "other" situationCode buckets
#     and simply won't count toward 5v5/PP/PK totals, which is the safe
#     (if slightly conservative) behavior.
#   - Goalies are excluded from all on-ice accumulation (only skaters).
#
# BACKFILL BEHAVIOR: this script scans the WHOLE season so far (not just a
# few recent days), so the very first run against a season with a backlog
# will find hundreds of unprocessed games. It caps itself to
# MAX_GAMES_PER_RUN per invocation and relies on the "already processed"
# state file to resume — re-run the workflow (workflow_dispatch) a few
# times to power through a large backlog quickly, or just let the daily
# cron grind through it. Once caught up, a normal day only has ~5-15 new
# games, so this becomes a fast, ordinary incremental update.
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

# ── Mode: "daily" (default) only looks back a few days and targets the
# CURRENT season — this is what the scheduled workflow runs, and it never
# re-scans a whole season. "backfill" scans an entire season's schedule and
# is meant to be triggered manually (optionally for a past season via
# --season=YYYY) when you deliberately want to fill in history. These are
# two separate GitHub Actions workflows calling the same script with
# different arguments — see onice_daily.yml and onice_backfill.yml.
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^--", name, "="), "", hit[1])
}
MODE <- get_arg("mode", "daily")               # "daily" or "backfill"
SEASON_ARG <- get_arg("season", NA_character_)  # e.g. "2026" — only meaningful in backfill mode

current_year  <- as.integer(format(Sys.Date(), "%Y"))
current_month <- as.integer(format(Sys.Date(), "%m"))
# Daily mode always targets the CURRENT season (rolls forward in September,
# same convention as the main app). Backfill mode can target any season via
# --season=YYYY, defaulting to the current one if not specified.
season_year <- if (!is.na(SEASON_ARG)) as.integer(SEASON_ARG) else
  ifelse(current_month >= 9, current_year + 1L, current_year)

cat("Mode:", MODE, "| Target season:", season_year, "\n")

OUT_DIR     <- file.path("data", "onice", as.character(season_year))
STATE_FILE  <- file.path(OUT_DIR, "processed_games.txt")
SKATER_OUT  <- file.path(OUT_DIR, "skater_onice.csv")
# Caps how many NEW games get processed in a single run.
# - daily mode: small cap, just a safety net for an occasional multi-day gap
#   (a normal day only has ~5-15 new games).
# - backfill mode: NO cap — processes the entire remaining backlog for the
#   season in one run. A full season is ~1,300 games (2 API calls each), so
#   this can genuinely take 30-90+ minutes; the workflow's timeout-minutes
#   is set high to accommodate that.
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

# DAILY mode: only looks back a handful of days — this is what keeps the
# scheduled job lean and never re-scans a whole season.
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

# BACKFILL mode: scans the WHOLE season. /v1/schedule/{date} returns a full
# week per call (same trick app.R's fetch_remaining_schedule uses), so
# stepping by 7 days keeps this to ~35 calls to cover an entire season.
find_season_games <- function(season_end_year) {
  ids <- character(0)
  start_date <- as.Date(paste0(season_end_year - 1L, "-09-01"))
  end_date   <- Sys.Date()
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
# Game IDs are chronologically sequential within a season, so sorting
# numerically processes oldest-first — makes partial backfill progress
# sensible run-to-run.
new_games <- as.character(sort(as.numeric(new_games)))
cat("Found", length(all_candidate_games), "completed games in scope (mode:", MODE, ");",
    length(new_games), "not yet processed.\n")

if (length(new_games) == 0) {
  cat("Nothing new to process. Exiting.\n")
  quit(save = "no", status = 0)
}

if (length(new_games) > MAX_GAMES_PER_RUN) {
  cat("Processing the oldest", MAX_GAMES_PER_RUN, "of these this run",
      if (MODE == "backfill") "(re-run the backfill workflow to continue)." else "(unusual for daily mode — check for a gap in runs).", "\n")
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
# e.g. "1551" = both goalies in, 5 skaters each side = 5v5.
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

# ── 3. Per-game processing ───────────────────────────────────────────────────
process_game <- function(game_id) {
  pbp <- nhl_get(paste0("https://api-web.nhle.com/v1/gamecenter/", game_id, "/play-by-play"))
  if (is.null(pbp) || is.null(pbp$plays) || length(pbp$plays) == 0) return(NULL)
  shifts_raw <- nhl_get(paste0("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId=", game_id))
  if (is.null(shifts_raw) || is.null(shifts_raw$data) || length(shifts_raw$data) == 0) return(NULL)
  
  home_id <- tryCatch(as.character(pbp$homeTeam$id), error = function(e) NA_character_)
  away_id <- tryCatch(as.character(pbp$awayTeam$id), error = function(e) NA_character_)
  if (is.na(home_id) || is.na(away_id)) return(NULL)
  
  # ---- Shift intervals in absolute game-seconds ((period-1)*1200 + t) -----
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
  bump <- function(lst, ids, by = 1) { for (pid in ids) if (!is.na(pid)) lst[[pid]] <- (lst[[pid]] %||% 0) + by; lst }
  
  # ---- TOI by strength: build a piecewise strength timeline from the
  # sequence of situationCode values across plays, then intersect each
  # player's shifts against it. ------------------------------------------
  toi_5v5 <- list(); toi_pp <- list(); toi_pk <- list()
  sit_events <- lapply(pbp$plays, function(pl) {
    tryCatch({
      per <- suppressWarnings(as.integer(pl$periodDescriptor$number %||% NA))
      tip <- parse_mmss(pl$timeInPeriod %||% NA)
      if (is.na(per) || is.na(tip)) return(NULL)
      data.frame(t_abs = (per - 1) * 1200 + tip, code = as.character(pl$situationCode %||% NA), stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  })
  sit_df <- bind_rows(Filter(Negate(is.null), sit_events))
  if (nrow(sit_df) > 0) {
    sit_df <- sit_df %>% arrange(t_abs) %>% distinct(t_abs, .keep_all = TRUE)
    sit_df$label <- sapply(sit_df$code, function(c) parse_situation(c)$label)
    game_end_abs <- suppressWarnings(max(shifts_df$end_abs, na.rm = TRUE))
    seg_start <- sit_df$t_abs; seg_end <- c(sit_df$t_abs[-1], game_end_abs); seg_label <- sit_df$label
    all_players <- unique(shifts_df$player_id)
    for (pid in all_players) {
      psh <- shifts_df[shifts_df$player_id == pid, ]
      pteam <- psh$team_id[1]
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
  
  # ---- Walk plays: Corsi/Goals at 5v5, individual EV/PP scoring, PK GA ----
  for (pl in pbp$plays) {
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
          # Shorthanded goal AGAINST the PK unit — charge on-ice PK skaters.
          pk_team <- if (identical(owner_team, home_id)) away_id else home_id
          pk_on <- on_ice_at(t_abs, pk_team)
          pk_ga <- bump(pk_ga, pk_on)
        }
      }
    }
  }
  
  list(cf = cf, ca = ca, gf = gf, ga = ga, ev_goals = ev_goals, ev_assists = ev_assists,
       pp_goals = pp_goals, pp_assists = pp_assists, pk_ga = pk_ga,
       toi_5v5 = toi_5v5, toi_pp = toi_pp, toi_pk = toi_pk)
}

# ── 4. Load existing totals, process new games, merge ───────────────────────
existing <- if (file.exists(SKATER_OUT)) tryCatch(read.csv(SKATER_OUT, stringsAsFactors = FALSE), error = function(e) NULL) else NULL

blank_totals <- function() list(cf=list(),ca=list(),gf=list(),ga=list(),ev_goals=list(),ev_assists=list(),
                                pp_goals=list(),pp_assists=list(),pk_ga=list(),
                                toi_5v5=list(),toi_pp=list(),toi_pk=list(),gp=list())

to_named_list <- function(ids, vals) { if (length(ids)==0) return(list()); setNames(as.list(vals), as.character(ids)) }

totals <- if (is.null(existing) || nrow(existing) == 0) blank_totals() else list(
  cf         = to_named_list(existing$player_id, existing$cf_5v5),
  ca         = to_named_list(existing$player_id, existing$ca_5v5),
  gf         = to_named_list(existing$player_id, existing$gf_5v5),
  ga         = to_named_list(existing$player_id, existing$ga_5v5),
  ev_goals   = to_named_list(existing$player_id, existing$ev_goals),
  ev_assists = to_named_list(existing$player_id, existing$ev_assists),
  pp_goals   = to_named_list(existing$player_id, existing$pp_goals),
  pp_assists = to_named_list(existing$player_id, existing$pp_assists),
  pk_ga      = to_named_list(existing$player_id, existing$pk_ga),
  toi_5v5    = to_named_list(existing$player_id, existing$toi_5v5_sec),
  toi_pp     = to_named_list(existing$player_id, existing$toi_pp_sec),
  toi_pk     = to_named_list(existing$player_id, existing$toi_pk_sec),
  gp         = to_named_list(existing$player_id, existing$gp_onice)
)

merge_add <- function(base, add) { for (pid in names(add)) base[[pid]] <- (base[[pid]] %||% 0) + add[[pid]]; base }

processed_this_run <- character(0)
for (gid in new_games) {
  cat("Processing game", gid, "...\n")
  res <- tryCatch(process_game(gid), error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL })
  if (is.null(res)) { cat("  Skipped (no usable play-by-play/shift data).\n"); next }
  totals$cf <- merge_add(totals$cf, res$cf); totals$ca <- merge_add(totals$ca, res$ca)
  totals$gf <- merge_add(totals$gf, res$gf); totals$ga <- merge_add(totals$ga, res$ga)
  totals$ev_goals <- merge_add(totals$ev_goals, res$ev_goals); totals$ev_assists <- merge_add(totals$ev_assists, res$ev_assists)
  totals$pp_goals <- merge_add(totals$pp_goals, res$pp_goals); totals$pp_assists <- merge_add(totals$pp_assists, res$pp_assists)
  totals$pk_ga <- merge_add(totals$pk_ga, res$pk_ga)
  totals$toi_5v5 <- merge_add(totals$toi_5v5, res$toi_5v5)
  totals$toi_pp  <- merge_add(totals$toi_pp,  res$toi_pp)
  totals$toi_pk  <- merge_add(totals$toi_pk,  res$toi_pk)
  game_players <- unique(names(res$toi_5v5))
  totals$gp <- merge_add(totals$gp, to_named_list(game_players, rep(1, length(game_players))))
  processed_this_run <- c(processed_this_run, gid)
  Sys.sleep(0.2)
}

if (length(processed_this_run) == 0) {
  cat("No games successfully processed this run. Exiting without writing.\n")
  quit(save = "no", status = 0)
}

# ── 5. Write output ───────────────────────────────────────────────────────
all_pids <- unique(names(totals$gp))
gv <- function(lst, pid) lst[[pid]] %||% 0

out_df <- data.frame(
  player_id   = all_pids,
  gp_onice    = sapply(all_pids, gv, lst = totals$gp),
  cf_5v5      = sapply(all_pids, gv, lst = totals$cf),
  ca_5v5      = sapply(all_pids, gv, lst = totals$ca),
  gf_5v5      = sapply(all_pids, gv, lst = totals$gf),
  ga_5v5      = sapply(all_pids, gv, lst = totals$ga),
  ev_goals    = sapply(all_pids, gv, lst = totals$ev_goals),
  ev_assists  = sapply(all_pids, gv, lst = totals$ev_assists),
  pp_goals    = sapply(all_pids, gv, lst = totals$pp_goals),
  pp_assists  = sapply(all_pids, gv, lst = totals$pp_assists),
  pk_ga       = sapply(all_pids, gv, lst = totals$pk_ga),
  toi_5v5_sec = sapply(all_pids, gv, lst = totals$toi_5v5),
  toi_pp_sec  = sapply(all_pids, gv, lst = totals$toi_pp),
  toi_pk_sec  = sapply(all_pids, gv, lst = totals$toi_pk),
  stringsAsFactors = FALSE
) %>%
  mutate(
    cf_pct_5v5      = ifelse((cf_5v5 + ca_5v5) > 0, round(100 * cf_5v5 / (cf_5v5 + ca_5v5), 1), NA_real_),
    gf_per60_5v5    = ifelse(toi_5v5_sec > 0, round(gf_5v5 / (toi_5v5_sec / 3600), 2), NA_real_),
    ga_per60_5v5    = ifelse(toi_5v5_sec > 0, round(ga_5v5 / (toi_5v5_sec / 3600), 2), NA_real_),
    ev_points_per60 = ifelse(toi_5v5_sec > 0, round((ev_goals + ev_assists) / (toi_5v5_sec / 3600), 2), NA_real_),
    pp_points_per60 = ifelse(toi_pp_sec > 0, round((pp_goals + pp_assists) / (toi_pp_sec / 3600), 2), NA_real_),
    pk_ga_per60     = ifelse(toi_pk_sec > 0, round(pk_ga / (toi_pk_sec / 3600), 2), NA_real_)
  )

write.csv(out_df, SKATER_OUT, row.names = FALSE)
writeLines(unique(c(processed_ids, processed_this_run)), STATE_FILE)
cat("Wrote", SKATER_OUT, "-", nrow(out_df), "players tracked.",
    length(processed_this_run), "new games processed this run.\n")