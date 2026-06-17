# =============================================================================
# NHL BASIC STATS SCRAPER — Team + Skater + Goalie + Game Results, 2021-22 on
# =============================================================================
#
# USAGE
# ──────────────────────────────────────────────────────────────────────────────
#   source("nhl_stats_scraper.R")                          # full history
#   run_stats_scraper(seasons_override = c(2026))           # current season only
#   run_stats_scraper(debug = TRUE, push = FALSE)           # test without uploading
#
# FILES UPLOADED TO GITHUB  (one folder per season, e.g. data/stats/2025-26/)
# ──────────────────────────────────────────────────────────────────────────────
#   team_summary.csv    — GF/GA per game, PP%, PK%, pts...
#   team_realtime.csv   — hits, blocks, takeaways, giveaways, corsi
#   skater_summary.csv  — goals, assists, pts, TOI, sh%, +/-, PP, SH...
#   goalie_summary.csv  — wins, GAA, sv%, saves, shutouts...
#   data/game_results/2025-26.csv  — one row per home game: scores, home/away abbrev
# =============================================================================

# ── CONFIG ────────────────────────────────────────────────────────────────────
GITHUB_USERNAME <- "Crice1620"
GITHUB_REPO     <- "NHL_sim"
GITHUB_BRANCH   <- "main"
GITHUB_PAT      <- Sys.getenv("GITHUB_PAT")
GAME_TYPE       <- 2L
# ── END CONFIG ────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(readr)
  library(base64enc)
})

log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S]"), ..., "\n")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

season_id        <- function(end_yr) paste0(end_yr - 1L, end_yr)
current_end_year <- function() {
  yr <- as.integer(format(Sys.Date(), "%Y"))
  mo <- as.integer(format(Sys.Date(), "%m"))
  if (mo >= 9L) yr + 1L else yr
}
all_stats_seasons <- function() seq(2022L, current_end_year())

szn_folder <- function(s) paste0("data/stats/", s - 1L, "-", substr(as.character(s), 3, 4))

safe_num <- function(x) tryCatch(as.numeric(x %||% NA),   error = function(e) NA_real_)
safe_int <- function(x) tryCatch(as.integer(x %||% NA),   error = function(e) NA_integer_)
safe_chr <- function(x) tryCatch(as.character(x %||% NA), error = function(e) NA_character_)

# =============================================================================
# SECTION 1 — Generic paginated fetcher
# =============================================================================

STATS_BASE <- "https://api.nhle.com/stats/rest/en"

fetch_all_pages <- function(endpoint, cayenne_exp, sort_json,
                            timeout_s = 30, debug = FALSE) {
  page_size <- 100L; start <- 0L; all_data <- list()
  repeat {
    resp <- tryCatch(
      GET(paste0(STATS_BASE, "/", endpoint),
          query = list(isAggregate="false", isGame="false",
                       sort=sort_json, start=start, limit=page_size,
                       cayenneExp=cayenne_exp),
          timeout(timeout_s)),
      error = function(e) NULL)
    if (is.null(resp) || status_code(resp) != 200) break
    raw <- tryCatch(fromJSON(content(resp,"text",encoding="UTF-8"),simplifyVector=FALSE), error=function(e) NULL)
    if (is.null(raw) || length(raw$data) == 0) break
    all_data <- c(all_data, raw$data)
    total <- as.integer(raw$total %||% 0L); start <- start + page_size
    if (start >= total) break
    Sys.sleep(0.05)
  }
  all_data
}

# =============================================================================
# SECTION 2 — Team summary
# =============================================================================

fetch_team_summary_scraper <- function(season_end_yr, game_type, debug=FALSE) {
  sid  <- season_id(season_end_yr)
  ce   <- paste0("seasonId=", sid, " and gameTypeId=", game_type)
  sort <- '[{"property":"points","direction":"DESC"}]'
  data <- fetch_all_pages("team/summary", ce, sort, debug=debug)
  if (length(data) == 0) return(NULL)
  rows <- lapply(data, function(r) tryCatch(data.frame(
    season=season_end_yr, game_type=game_type,
    team_id              = safe_int(r$teamId),
    team_full            = safe_chr(r$teamFullName),
    gp                   = safe_int(r$gamesPlayed),
    wins                 = safe_int(r$wins),
    losses               = safe_int(r$losses),
    ot_losses            = safe_int(r$otLosses),
    ties                 = safe_int(r$ties),
    points               = safe_int(r$points),
    point_pct            = safe_num(r$pointPct),
    wins_regulation      = safe_int(r$winsInRegulation),
    wins_regulation_ot   = safe_int(r$regulationAndOtWins),
    wins_shootout        = safe_int(r$winsInShootout),
    goals_for            = safe_int(r$goalsFor),
    goals_against        = safe_int(r$goalsAgainst),
    gf_per_game          = safe_num(r$goalsForPerGame),
    ga_per_game          = safe_num(r$goalsAgainstPerGame),
    shots_for_per_game   = safe_num(r$shotsForPerGame),
    shots_against_per_game = safe_num(r$shotsAgainstPerGame),
    pp_pct               = safe_num(r$powerPlayPct),
    pp_net_pct           = safe_num(r$powerPlayNetPct),
    pk_pct               = safe_num(r$penaltyKillPct),
    pk_net_pct           = safe_num(r$penaltyKillNetPct),
    fo_win_pct           = safe_num(r$faceoffWinPct),
    shutouts             = safe_int(r$teamShutouts),
    stringsAsFactors=FALSE
  ), error=function(e) NULL))
  do.call(rbind, Filter(Negate(is.null), rows))
}

# =============================================================================
# SECTION 3 — Team realtime
# =============================================================================

fetch_team_realtime_scraper <- function(season_end_yr, game_type, debug=FALSE) {
  sid  <- season_id(season_end_yr)
  ce   <- paste0("seasonId=", sid, " and gameTypeId=", game_type)
  sort <- '[{"property":"hits","direction":"DESC"}]'
  data <- fetch_all_pages("team/realtime", ce, sort, debug=debug)
  if (length(data) == 0) return(NULL)
  rows <- lapply(data, function(r) tryCatch(data.frame(
    season=season_end_yr, game_type=game_type,
    team_id               = safe_int(r$teamId),
    team_full             = safe_chr(r$teamFullName),
    gp                    = safe_int(r$gamesPlayed),
    hits                  = safe_int(r$hits),
    hits_per60            = safe_num(r$hitsPer60),
    blocked_shots         = safe_int(r$blockedShots),
    blocked_shots_per60   = safe_num(r$blockedShotsPer60),
    takeaways             = safe_int(r$takeaways),
    takeaways_per60       = safe_num(r$takeawaysPer60),
    giveaways             = safe_int(r$giveaways),
    giveaways_per60       = safe_num(r$giveawaysPer60),
    missed_shots          = safe_int(r$missedShots),
    total_shot_attempts   = safe_int(r$totalShotAttempts),
    shot_attempts_blocked = safe_int(r$shotAttemptsBlocked),
    corsi_pct             = safe_num(r$satPct),
    empty_net_goals       = safe_int(r$emptyNetGoals),
    toi_per_game_5v5      = safe_num(r$timeOnIcePerGame5v5),
    stringsAsFactors=FALSE
  ), error=function(e) NULL))
  do.call(rbind, Filter(Negate(is.null), rows))
}

# =============================================================================
# SECTION 4 — Skater summary
# =============================================================================

fetch_skater_summary_scraper <- function(season_end_yr, game_type, debug=FALSE) {
  sid  <- season_id(season_end_yr)
  ce   <- paste0("seasonId=", sid, " and gameTypeId=", game_type)
  sort <- '[{"property":"points","direction":"DESC"},{"property":"goals","direction":"DESC"},{"property":"playerId","direction":"ASC"}]'
  data <- fetch_all_pages("skater/summary", ce, sort, debug=debug)
  if (length(data) == 0) return(NULL)
  rows <- lapply(data, function(r) tryCatch(data.frame(
    season=season_end_yr, game_type=game_type,
    player_id        = safe_chr(r$playerId),
    player_name      = safe_chr(r$skaterFullName),
    last_name        = safe_chr(r$lastName),
    team_abbrev      = safe_chr(r$teamAbbrevs),
    position         = safe_chr(r$positionCode),
    shoots           = safe_chr(r$shootsCatches),
    gp               = safe_int(r$gamesPlayed),
    goals            = safe_int(r$goals),
    assists          = safe_int(r$assists),
    points           = safe_int(r$points),
    points_per_game  = safe_num(r$pointsPerGame),
    plus_minus       = safe_int(r$plusMinus),
    pim              = safe_int(r$penaltyMinutes),
    shots            = safe_int(r$shots),
    shooting_pct     = safe_num(r$shootingPct),
    toi_per_game     = safe_chr(r$timeOnIcePerGame),
    ev_goals         = safe_int(r$evGoals),
    ev_points        = safe_int(r$evPoints),
    pp_goals         = safe_int(r$ppGoals),
    pp_points        = safe_int(r$ppPoints),
    sh_goals         = safe_int(r$shGoals),
    sh_points        = safe_int(r$shPoints),
    ot_goals         = safe_int(r$otGoals),
    gw_goals         = safe_int(r$gameWinningGoals),
    fo_win_pct       = safe_num(r$faceoffWinPct),
    stringsAsFactors=FALSE
  ), error=function(e) NULL))
  do.call(rbind, Filter(Negate(is.null), rows))
}

# =============================================================================
# SECTION 5 — Goalie summary
# =============================================================================

fetch_goalie_summary_scraper <- function(season_end_yr, game_type, debug=FALSE) {
  sid  <- season_id(season_end_yr)
  ce   <- paste0("seasonId=", sid, " and gameTypeId=", game_type)
  sort <- '[{"property":"wins","direction":"DESC"},{"property":"playerId","direction":"ASC"}]'
  data <- fetch_all_pages("goalie/summary", ce, sort, debug=debug)
  if (length(data) == 0) return(NULL)
  rows <- lapply(data, function(r) tryCatch(data.frame(
    season=season_end_yr, game_type=game_type,
    player_id        = safe_chr(r$playerId),
    player_name      = safe_chr(r$goalieFullName),
    last_name        = safe_chr(r$lastName),
    team_abbrev      = safe_chr(r$teamAbbrevs),
    shoots_catches   = safe_chr(r$shootsCatches),
    gp               = safe_int(r$gamesPlayed),
    gs               = safe_int(r$gamesStarted),
    wins             = safe_int(r$wins),
    losses           = safe_int(r$losses),
    ot_losses        = safe_int(r$otLosses),
    ties             = safe_int(r$ties),
    goals_against    = safe_int(r$goalsAgainst),
    gaa              = safe_num(r$goalsAgainstAverage),
    saves            = safe_int(r$saves),
    shots_against    = safe_int(r$shotsAgainst),
    sv_pct           = safe_num(r$savePct),
    shutouts         = safe_int(r$shutouts),
    toi_sec          = safe_int(r$timeOnIce),
    goals            = safe_int(r$goals),
    assists          = safe_int(r$assists),
    points           = safe_int(r$points),
    pim              = safe_int(r$penaltyMinutes),
    stringsAsFactors=FALSE
  ), error=function(e) NULL))
  do.call(rbind, Filter(Negate(is.null), rows))
}

# =============================================================================
# SECTION 6 — Game results (home-team view, one row per game)
# =============================================================================

fetch_game_results_scraper <- function(season_end_yr, game_type, debug=FALSE) {
  sid    <- season_id(season_end_yr)
  ce     <- paste0("seasonId=", sid, " and gameTypeId=", game_type)
  sort_j <- '[{"property":"gameDate","direction":"ASC"}]'
  all_rows <- list(); start <- 0L
  
  repeat {
    resp <- tryCatch(
      GET(paste0(STATS_BASE, "/team/summary"),
          query=list(isAggregate="false", isGame="true",
                     sort=sort_j, start=start, limit=100L, cayenneExp=ce),
          timeout(45)),
      error=function(e) NULL)
    if (is.null(resp) || status_code(resp) != 200) break
    raw <- tryCatch(fromJSON(content(resp,"text",encoding="UTF-8"),simplifyVector=TRUE), error=function(e) NULL)
    if (is.null(raw) || is.null(raw$data) || nrow(raw$data) == 0) break
    all_rows <- c(all_rows, list(raw$data))
    total <- as.integer(raw$total %||% 0L); start <- start + 100L
    if (start >= total) break
    Sys.sleep(0.05)
  }
  
  if (length(all_rows) == 0) return(NULL)
  df <- bind_rows(all_rows)
  
  required <- c("gameId","gameDate","homeRoad","teamFullName",
                "opponentTeamAbbrev","goalsFor","goalsAgainst")
  if (!all(required %in% names(df))) return(NULL)
  
  # Keep only home-team rows so each game appears once
  df_h <- df[df$homeRoad == "H" & !is.na(df$homeRoad), ]
  if (nrow(df_h) == 0) return(NULL)
  
  out <- data.frame(
    season      = season_end_yr,
    game_type   = game_type,
    game_id     = as.character(df_h$gameId),
    game_date   = as.character(substr(as.character(df_h$gameDate), 1, 10)),
    home_full   = as.character(df_h$teamFullName),
    away_abbrev = as.character(df_h$opponentTeamAbbrev),
    home_score  = as.integer(df_h$goalsFor),
    away_score  = as.integer(df_h$goalsAgainst),
    period_type = "REG",
    stringsAsFactors = FALSE
  )
  
  # Remove duplicates
  out[!duplicated(out$game_id) &
        !is.na(out$home_score) &
        !is.na(out$away_score), ]
}

# =============================================================================
# SECTION 7 — GitHub upload
# =============================================================================

github_upload_file <- function(file_path, local_path) {
  api_url  <- sprintf("https://api.github.com/repos/%s/%s/contents/%s",
                      GITHUB_USERNAME, GITHUB_REPO, file_path)
  auth     <- authenticate(GITHUB_USERNAME, GITHUB_PAT)
  b64      <- base64encode(readBin(local_path, "raw", file.size(local_path)))
  existing <- tryCatch(GET(api_url, auth, timeout(15)), error=function(e) NULL)
  sha <- if (!is.null(existing) && status_code(existing) == 200)
    fromJSON(content(existing,"text",encoding="UTF-8"))$sha else NULL
  body <- list(message=paste0("update ", basename(file_path), " — ",
                              format(Sys.time(),"%Y-%m-%d %H:%M UTC")),
               content=b64, branch=GITHUB_BRANCH)
  if (!is.null(sha)) body$sha <- sha
  resp <- PUT(api_url, auth, body=toJSON(body, auto_unbox=TRUE),
              add_headers("Content-Type"="application/json"), timeout(60))
  sc <- status_code(resp)
  if (sc %in% c(200L, 201L)) {
    log_msg(sprintf("  ✓ %s", file_path))
  } else {
    msg <- tryCatch(fromJSON(content(resp,"text",encoding="UTF-8"))$message,
                    error=function(e) as.character(sc))
    log_msg(sprintf("  ✗ FAILED %s : %s", file_path, msg))
  }
  invisible(sc)
}

github_upload_df <- function(df, repo_path) {
  tmp <- tempfile(fileext=".csv"); on.exit(unlink(tmp))
  write_csv(df, tmp)
  github_upload_file(repo_path, tmp)
}

# =============================================================================
# SECTION 8 — Main entry point
# =============================================================================

run_stats_scraper <- function(seasons_override = NULL,
                              push             = TRUE,
                              debug            = FALSE,
                              game_type        = GAME_TYPE) {
  
  if (push && nchar(trimws(GITHUB_PAT)) == 0)
    stop("GITHUB_PAT is empty — add to ~/.Renviron and run readRenviron('~/.Renviron').")
  
  seasons <- if (!is.null(seasons_override)) as.integer(seasons_override)
  else all_stats_seasons()
  
  log_msg(sprintf("Seasons : %s", paste(seasons, collapse=", ")))
  
  for (szn in seasons) {
    log_msg(sprintf("── Season %d-%d ──────────────────────────────", szn-1L, szn))
    results <- list()
    
    log_msg("  Team summary...")
    df <- fetch_team_summary_scraper(szn, game_type, debug)
    if (!is.null(df) && nrow(df) > 0) { results$team_summary <- df; log_msg(sprintf("    -> %d teams", nrow(df))) }
    
    log_msg("  Team realtime...")
    df <- fetch_team_realtime_scraper(szn, game_type, debug)
    if (!is.null(df) && nrow(df) > 0) { results$team_realtime <- df; log_msg(sprintf("    -> %d teams", nrow(df))) }
    
    log_msg("  Skater summary...")
    df <- fetch_skater_summary_scraper(szn, game_type, debug)
    if (!is.null(df) && nrow(df) > 0) { results$skater_summary <- df; log_msg(sprintf("    -> %d skaters", nrow(df))) }
    
    log_msg("  Goalie summary...")
    df <- fetch_goalie_summary_scraper(szn, game_type, debug)
    if (!is.null(df) && nrow(df) > 0) { results$goalie_summary <- df; log_msg(sprintf("    -> %d goalies", nrow(df))) }
    
    log_msg("  Game results...")
    df <- fetch_game_results_scraper(szn, game_type, debug)
    if (!is.null(df) && nrow(df) > 0) { results$game_results <- df; log_msg(sprintf("    -> %d games", nrow(df))) }
    
    if (!push) next
    
    log_msg("  Uploading to GitHub...")
    for (nm in names(results)) {
      if (nm == "game_results") {
        # Game results go in their own top-level folder, one file per season
        repo_path <- paste0("data/game_results/", szn - 1L, "-",
                            substr(as.character(szn), 3, 4), ".csv")
      } else {
        repo_path <- paste0(szn_folder(szn), "/", nm, ".csv")
      }
      github_upload_df(results[[nm]], repo_path)
    }
  }
  
  log_msg("Done.")
  invisible(NULL)
}

# =============================================================================
# Run when sourced — only current season by default
# Full history:           run_stats_scraper()
# Current season only:    run_stats_scraper(seasons_override = c(2026))
# Test without uploading: run_stats_scraper(debug=TRUE, push=FALSE)
# =============================================================================
#run_stats_scraper()
