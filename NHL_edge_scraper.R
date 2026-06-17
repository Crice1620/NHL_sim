# =============================================================================
# NHL EDGE STATS SCRAPER  —  Team + Skater + Goalie, Seasons 2021-22 onward
# =============================================================================
#
# USAGE
# ──────────────────────────────────────────────────────────────────────────────
#   source("nhl_edge_scraper.R")                           # full history
#   run_edge_scraper(seasons_override = c(2026))            # current season only
#   run_edge_scraper(debug = TRUE, push = FALSE)            # test without uploading
#
# FILES UPLOADED TO GITHUB  (e.g. data/edge/2025-26/)
# ──────────────────────────────────────────────────────────────────────────────
#   team_comparison.csv        — per team, flat summary
#   team_skating_distance.csv  — per team, distance detail
#   team_skating_speed.csv     — per team, speed/burst detail
#   team_zone_time.csv         — per team, zone time %
#   team_shot_location.csv     — per team, shot location by zone
#   team_shot_speed.csv        — per team, shot speed detail
#   skater_summary.csv         — per skater, flat summary (main model input)
#   skater_shot_location.csv   — per skater, SOG+goals by zone (17 areas)
#   skater_last10_distance.csv — per skater, distance per game (last 10)
#   goalie_summary.csv         — per goalie: dist skated, shot speed faced, zone time
# =============================================================================

# ── CONFIG ────────────────────────────────────────────────────────────────────
GITHUB_USERNAME <- "Crice1620"
GITHUB_REPO     <- "NHL_sim"
GITHUB_BRANCH   <- "main"
GITHUB_PAT      <- Sys.getenv("GITHUB_PAT")
GAME_TYPE       <- 2L
# ── END CONFIG ────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(dplyr)
  library(readr); library(base64enc)
})

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S]"), ..., "\n")

# =============================================================================
# SECTION 1 — NHL API helper
# =============================================================================

nhl_get <- function(url, timeout_s = 20, debug = FALSE) {
  if (debug) log_msg("  GET", url)
  resp <- tryCatch(GET(url, timeout(timeout_s)), error = function(e) NULL)
  if (is.null(resp)) return(NULL)
  sc <- status_code(resp)
  if (debug) log_msg("  HTTP", sc)
  if (sc != 200) return(NULL)
  tryCatch(
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE),
    error = function(e) NULL
  )
}

season_id        <- function(end_yr) paste0(end_yr - 1L, end_yr)
current_end_year <- function() {
  yr <- as.integer(format(Sys.Date(), "%Y"))
  mo <- as.integer(format(Sys.Date(), "%m"))
  if (mo >= 9L) yr + 1L else yr
}
all_edge_seasons <- function() seq(2022L, current_end_year())

# =============================================================================
# SECTION 2 — Team list
# =============================================================================

ABBREV_TO_ID <- c(
  NJD=1L,  NYI=2L,  NYR=3L,  PHI=4L,  PIT=5L,
  BOS=6L,  BUF=7L,  MTL=8L,  OTT=9L,  TOR=10L,
  CAR=12L, FLA=13L, TBL=14L, WSH=15L, CHI=16L,
  DET=17L, NSH=18L, STL=19L, CGY=20L, COL=21L,
  EDM=22L, VAN=23L, ANA=24L, DAL=25L, LAK=26L,
  SJS=28L, CBJ=29L, MIN=30L, WPG=52L, PHX=53L,
  VGK=54L, SEA=55L, UTA=59L
)

HISTORICAL_TEAMS <- list(
  list(team_id=53L, team_abbrev="PHX", last_season=2024L)
)

fetch_team_list <- function(debug = FALSE) {
  hist_df <- do.call(rbind, lapply(HISTORICAL_TEAMS, function(h)
    data.frame(team_id=h$team_id, team_abbrev=h$team_abbrev, stringsAsFactors=FALSE)))
  raw <- nhl_get("https://api-web.nhle.com/v1/standings/now", 20, debug)
  if (!is.null(raw) && !is.null(raw$standings)) {
    rows <- lapply(raw$standings, function(s) tryCatch({
      abbr <- as.character(s$teamAbbrev$default %||% s$teamAbbrev %||% NA)
      tid  <- ABBREV_TO_ID[abbr]; if (is.na(tid)) return(NULL)
      data.frame(team_id=as.integer(tid), team_abbrev=abbr, stringsAsFactors=FALSE)
    }, error=function(e) NULL))
    out <- do.call(rbind, Filter(Negate(is.null), rows))
    out <- out[!duplicated(out$team_id), ]
    if (nrow(out) > 0) {
      out <- rbind(out, hist_df[!hist_df$team_id %in% out$team_id, ])
      return(out)
    }
  }
  log_msg("Using hard-coded team list.")
  data.frame(team_id=as.integer(ABBREV_TO_ID), team_abbrev=names(ABBREV_TO_ID),
             stringsAsFactors=FALSE)
}

# =============================================================================
# SECTION 3 — Player list (skaters)
# =============================================================================

fetch_player_list <- function(season_end_yr, debug = FALSE) {
  sid    <- season_id(season_end_yr)
  ce     <- paste0("seasonId=", sid, " and gameTypeId=2")
  sort_j <- '[{"property":"points","direction":"DESC"},{"property":"playerId","direction":"ASC"}]'
  all_data <- list(); start <- 0L
  repeat {
    resp <- tryCatch(
      GET("https://api.nhle.com/stats/rest/en/skater/summary",
          query=list(isAggregate="false",isGame="false",sort=sort_j,start=start,limit=100L,cayenneExp=ce),
          timeout(30)), error=function(e) NULL)
    if (is.null(resp)||status_code(resp)!=200) break
    raw <- tryCatch(fromJSON(content(resp,"text",encoding="UTF-8"),simplifyVector=FALSE),error=function(e) NULL)
    if (is.null(raw)||length(raw$data)==0) break
    all_data <- c(all_data, raw$data)
    total <- as.integer(raw$total %||% 0L); start <- start+100L
    if (start>=total) break; Sys.sleep(0.05)
  }
  if (length(all_data)==0) return(NULL)
  rows <- lapply(all_data, function(r) tryCatch(data.frame(
    player_id=as.character(r$playerId %||% NA),
    team_abbrev=as.character(r$teamAbbrevs %||% NA),
    position=as.character(r$positionCode %||% NA),
    stringsAsFactors=FALSE), error=function(e) NULL))
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  out[!is.na(out$player_id), ]
}

# =============================================================================
# SECTION 3b — Goalie list
# =============================================================================

fetch_goalie_list <- function(season_end_yr, debug = FALSE) {
  sid    <- season_id(season_end_yr)
  ce     <- paste0("seasonId=", sid, " and gameTypeId=2")
  sort_j <- '[{"property":"wins","direction":"DESC"},{"property":"playerId","direction":"ASC"}]'
  all_data <- list(); start <- 0L
  repeat {
    resp <- tryCatch(
      GET("https://api.nhle.com/stats/rest/en/goalie/summary",
          query=list(isAggregate="false",isGame="false",sort=sort_j,start=start,limit=100L,cayenneExp=ce),
          timeout(30)), error=function(e) NULL)
    if (is.null(resp)||status_code(resp)!=200) break
    raw <- tryCatch(fromJSON(content(resp,"text",encoding="UTF-8"),simplifyVector=FALSE),error=function(e) NULL)
    if (is.null(raw)||length(raw$data)==0) break
    all_data <- c(all_data, raw$data)
    total <- as.integer(raw$total %||% 0L); start <- start+100L
    if (start>=total) break; Sys.sleep(0.05)
  }
  if (length(all_data)==0) return(NULL)
  rows <- lapply(all_data, function(r) tryCatch(data.frame(
    player_id=as.character(r$playerId %||% NA),
    team_abbrev=as.character(r$teamAbbrevs %||% NA),
    position="G", stringsAsFactors=FALSE), error=function(e) NULL))
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  out[!is.na(out$player_id), ]
}

# =============================================================================
# SECTION 4 — Primitive helpers
# =============================================================================

imp <- function(node) {
  if (is.null(node)) return(NA_real_)
  v <- node$imperial %||% node$metric %||% NULL
  if (is.null(v)) return(NA_real_)
  tryCatch(as.numeric(v), error=function(e) NA_real_)
}
sc  <- function(x) { if(is.null(x)||length(x)==0) return(NA_real_); tryCatch(as.numeric(x[[1]]),error=function(e) NA_real_) }
sci <- function(x) { v<-sc(x); if(is.na(v)) NA_integer_ else as.integer(v) }
scc <- function(x) { if(is.null(x)||length(x)==0) return(NA_character_); tryCatch(as.character(x[[1]]),error=function(e) NA_character_) }

# =============================================================================
# SECTION 5 — TEAM parsers (unchanged from original)
# =============================================================================

BASE <- "https://api-web.nhle.com/v1/edge"

parse_team_comparison <- function(raw, abbr, tid, szn, gt) {
  if (is.null(raw)) return(NULL)
  tm   <- raw$team                   %||% list()
  sd   <- raw$skatingDistanceDetails %||% list()
  ss   <- raw$skatingSpeedDetails    %||% list()
  zt   <- raw$zoneTimeDetails        %||% list()
  spd  <- raw$shotSpeedDetails       %||% list()
  sdif <- raw$shotDifferential       %||% list()
  zone_val <- function(field) { v <- zt[[field]] %||% NULL; if(is.null(v)) NA_real_ else tryCatch(as.numeric(v),error=function(e) NA_real_) }
  data.frame(
    season=szn, game_type=gt, team_id=tid, team_abbrev=abbr,
    gp=sci(tm$gamesPlayed), wins=sci(tm$wins), losses=sci(tm$losses),
    ot_losses=sci(tm$otLosses), points=sci(tm$points),
    conference=scc(tm$conference), division=scc(tm$division),
    dist_total_mi=imp(sd$distanceTotal), dist_per60_mi=imp(sd$distancePer60),
    dist_max_game_mi=imp(sd$distanceMaxGame),
    max_skate_speed_mph=imp(ss$maxSkatingSpeed),
    bursts_over22=sc(ss$burstsOver22), bursts_20_22=sc(ss$bursts20To22),
    bursts_18_20=sc(ss$bursts18To20),
    oz_pct=zone_val("offensiveZonePctg"), nz_pct=zone_val("neutralZonePctg"),
    dz_pct=zone_val("defensiveZonePctg"), oz_league_avg=zone_val("offensiveZoneLeagueAvg"),
    dz_league_avg=zone_val("defensiveZoneLeagueAvg"),
    top_shot_speed_mph=imp(spd$topShotSpeed), avg_shot_speed_mph=imp(spd$avgShotSpeed),
    shot_attempts_over100=sc(spd$shotAttemptsOver100),
    shot_attempts_90_100=sc(spd$shotAttempts90To100),
    shot_attempts_80_90=sc(spd$shotAttempts80To90),
    shot_diff=sc(sdif$shotDifferential %||% sdif$differential),
    shots_for=sc(sdif$shotsFor), shots_against=sc(sdif$shotsAgainst),
    stringsAsFactors=FALSE)
}

parse_team_skating_distance <- function(raw, abbr, tid, szn, gt) {
  if (is.null(raw)) return(NULL)
  sd <- raw$skatingDistanceDetails %||% list(); if(length(sd)==0) return(NULL)
  data.frame(season=szn,game_type=gt,team_id=tid,team_abbrev=abbr,
             dist_total_mi=imp(sd$distanceTotal),dist_per60_mi=imp(sd$distancePer60),
             dist_max_game_mi=imp(sd$distanceMaxGame),dist_max_period_mi=imp(sd$distanceMaxPeriod),
             stringsAsFactors=FALSE)
}

parse_team_skating_speed <- function(raw, abbr, tid, szn, gt) {
  if (is.null(raw)) return(NULL)
  ss <- raw$skatingSpeedDetails %||% list(); if(length(ss)==0) return(NULL)
  data.frame(season=szn,game_type=gt,team_id=tid,team_abbrev=abbr,
             max_skate_speed_mph=imp(ss$maxSkatingSpeed),bursts_over22=sc(ss$burstsOver22),
             bursts_20_22=sc(ss$bursts20To22),bursts_18_20=sc(ss$bursts18To20),
             stringsAsFactors=FALSE)
}

parse_team_zone_time <- function(raw, abbr, tid, szn, gt) {
  if (is.null(raw)) return(NULL)
  zt <- raw$zoneTimeDetails %||% list(); if(length(zt)==0) return(NULL)
  zv <- function(f) { v <- zt[[f]] %||% NULL; if(is.null(v)) NA_real_ else tryCatch(as.numeric(v),error=function(e) NA_real_) }
  data.frame(season=szn,game_type=gt,team_id=tid,team_abbrev=abbr,
             oz_pct=zv("offensiveZonePctg"),nz_pct=zv("neutralZonePctg"),dz_pct=zv("defensiveZonePctg"),
             oz_league_avg=zv("offensiveZoneLeagueAvg"),dz_league_avg=zv("defensiveZoneLeagueAvg"),
             stringsAsFactors=FALSE)
}

parse_team_shot_location <- function(raw, abbr, tid, szn, gt) {
  if (is.null(raw)) return(NULL)
  sl <- raw$shotLocationDetails %||% list(); if(length(sl)==0) return(NULL)
  rows <- lapply(sl, function(z) tryCatch(data.frame(
    season=szn,game_type=gt,team_id=tid,team_abbrev=abbr,
    zone=scc(z$zone %||% z$area %||% z$name),goals=sc(z$goals),
    sog=sc(z$shotsOnGoal %||% z$sog),sh_pct=sc(z$shootingPct %||% z$shootingPctg),
    stringsAsFactors=FALSE),error=function(e) NULL))
  do.call(rbind, Filter(Negate(is.null), rows))
}

parse_team_shot_speed <- function(raw, abbr, tid, szn, gt) {
  if (is.null(raw)) return(NULL)
  spd <- raw$shotSpeedDetails %||% list(); if(length(spd)==0) return(NULL)
  data.frame(season=szn,game_type=gt,team_id=tid,team_abbrev=abbr,
             top_shot_speed_mph=imp(spd$topShotSpeed),avg_shot_speed_mph=imp(spd$avgShotSpeed),
             shot_attempts_over100=sc(spd$shotAttemptsOver100),shot_attempts_90_100=sc(spd$shotAttempts90To100),
             shot_attempts_80_90=sc(spd$shotAttempts80To90),shot_attempts_70_80=sc(spd$shotAttempts70To80),
             stringsAsFactors=FALSE)
}

TEAM_ENDPOINTS <- list(
  list(name="team_comparison",       url_fn=function(tid,szn,gt) sprintf("%s/team-comparison/%d/%s/%d",              BASE,tid,szn,gt), parse_fn=parse_team_comparison),
  list(name="team_skating_distance", url_fn=function(tid,szn,gt) sprintf("%s/team-skating-distance-detail/%d/%s/%d", BASE,tid,szn,gt), parse_fn=parse_team_skating_distance),
  list(name="team_skating_speed",    url_fn=function(tid,szn,gt) sprintf("%s/team-skating-speed-detail/%d/%s/%d",    BASE,tid,szn,gt), parse_fn=parse_team_skating_speed),
  list(name="team_zone_time",        url_fn=function(tid,szn,gt) sprintf("%s/team-zone-time-detail/%d/%s/%d",        BASE,tid,szn,gt), parse_fn=parse_team_zone_time),
  list(name="team_shot_location",    url_fn=function(tid,szn,gt) sprintf("%s/team-shot-location-detail/%d/%s/%d",    BASE,tid,szn,gt), parse_fn=parse_team_shot_location),
  list(name="team_shot_speed",       url_fn=function(tid,szn,gt) sprintf("%s/team-shot-speed-detail/%d/%s/%d",       BASE,tid,szn,gt), parse_fn=parse_team_shot_speed)
)

# =============================================================================
# SECTION 6 — PLAYER parsers (unchanged from original)
# =============================================================================

PBASE <- "https://api-web.nhle.com/v1/edge"

player_meta <- function(p) list(
  player_id=scc(p$id), first_name=scc(p$firstName$default %||% p$firstName),
  last_name=scc(p$lastName$default %||% p$lastName),
  position=scc(p$position), team_abbrev=scc(p$team$abbrev %||% NA))

parse_skater_summary <- function(raw, pid, abbr, szn, gt) {
  if (is.null(raw)) return(NULL)
  p   <- raw$player               %||% list()
  spd <- raw$shotSpeedDetails     %||% list()
  ss  <- raw$skatingSpeedDetails  %||% list()
  sd  <- raw$skatingDistanceDetails %||% list()
  zt  <- raw$zoneTimeDetails      %||% list()
  zs  <- raw$zoneStarts           %||% list()
  zv  <- function(f) { v <- zt[[f]] %||% NULL; if(is.null(v)) NA_real_ else tryCatch(as.numeric(v),error=function(e) NA_real_) }
  meta <- player_meta(p)
  data.frame(
    season=szn,game_type=gt,player_id=pid,
    first_name=meta$first_name,last_name=meta$last_name,
    position=meta$position,
    team_abbrev=if(!is.na(meta$team_abbrev)) meta$team_abbrev else abbr,
    gp=sci(p$gamesPlayed),goals=sci(p$goals),assists=sci(p$assists),points=sci(p$points),
    top_shot_speed_mph=imp(spd$topShotSpeed),avg_shot_speed_mph=imp(spd$avgShotSpeed),
    shot_attempts_over100=sc(spd$shotAttemptsOver100),shot_attempts_90_100=sc(spd$shotAttempts90To100),
    shot_attempts_80_90=sc(spd$shotAttempts80To90),shot_attempts_70_80=sc(spd$shotAttempts70To80),
    max_skate_speed_mph=imp(ss$maxSkatingSpeed),bursts_over22=sc(ss$burstsOver22),
    bursts_20_22=sc(ss$bursts20To22),bursts_18_20=sc(ss$bursts18To20),
    dist_total_mi=imp(sd$distanceTotal),dist_per60_mi=imp(sd$distancePer60),
    dist_max_game_mi=imp(sd$distanceMaxGame),
    oz_pct=zv("offensiveZonePctg"),nz_pct=zv("neutralZonePctg"),dz_pct=zv("defensiveZonePctg"),
    oz_league_avg=zv("offensiveZoneLeagueAvg"),dz_league_avg=zv("defensiveZoneLeagueAvg"),
    oz_starts_pct=sc(zs$offensiveZoneStarts),nz_starts_pct=sc(zs$neutralZoneStarts),
    dz_starts_pct=sc(zs$defensiveZoneStarts),stringsAsFactors=FALSE)
}

parse_skater_shot_location <- function(raw, pid, abbr, szn, gt) {
  if (is.null(raw)) return(NULL)
  sl <- raw$shotLocationDetails %||% list(); if(length(sl)==0) return(NULL)
  rows <- lapply(sl, function(z) tryCatch(data.frame(
    season=szn,game_type=gt,player_id=pid,team_abbrev=abbr,
    zone=scc(z$area %||% z$zone %||% z$name),sog=sc(z$sog %||% z$shots),
    goals=sc(z$goals),sh_pct=sc(z$shootingPctg %||% z$shootingPct),
    stringsAsFactors=FALSE),error=function(e) NULL))
  do.call(rbind, Filter(Negate(is.null), rows))
}

parse_skater_last10 <- function(raw, pid, abbr, szn, gt) {
  if (is.null(raw)) return(NULL)
  games <- raw$skatingDistanceLast10 %||% list(); if(length(games)==0) return(NULL)
  rows <- lapply(games, function(g) tryCatch(data.frame(
    season=szn,game_type=gt,player_id=pid,team_abbrev=abbr,
    game_date=scc(g$gameDate),dist_all_mi=imp(g$distanceSkatedAll),
    toi_all_sec=sc(g$toiAll),dist_even_mi=imp(g$distanceSkatedEven),
    toi_even_sec=sc(g$toiEven),dist_pp_mi=imp(g$distanceSkatedPP),
    toi_pp_sec=sc(g$toiPP),home=as.integer(isTRUE(g$playerOnHomeTeam)),
    stringsAsFactors=FALSE),error=function(e) NULL))
  do.call(rbind, Filter(Negate(is.null), rows))
}

# =============================================================================
# SECTION 6b — GOALIE Edge parser (new)
# =============================================================================

parse_goalie_edge <- function(raw, pid, abbr, szn, gt) {
  if (is.null(raw)) return(NULL)
  p   <- raw$player                  %||% list()
  sd  <- raw$skatingDistanceDetails  %||% list()
  spd <- raw$shotSpeedDetails        %||% list()
  zt  <- raw$zoneTimeDetails         %||% list()
  zv  <- function(f) { v <- zt[[f]] %||% NULL; if(is.null(v)) NA_real_ else tryCatch(as.numeric(v),error=function(e) NA_real_) }
  meta <- player_meta(p)
  data.frame(
    season=szn, game_type=gt, player_id=pid, position="G",
    first_name=meta$first_name, last_name=meta$last_name,
    team_abbrev=if(!is.na(meta$team_abbrev)) meta$team_abbrev else abbr,
    gp=sci(p$gamesPlayed),
    # How much the goalie skates
    dist_per60_mi        = imp(sd$distancePer60),
    dist_total_mi        = imp(sd$distanceTotal),
    dist_max_game_mi     = imp(sd$distanceMaxGame),
    # Shot speed they faced
    avg_shot_speed_faced_mph = imp(spd$avgShotSpeed),
    top_shot_speed_faced_mph = imp(spd$topShotSpeed),
    shots_faced_over100      = sc(spd$shotAttemptsOver100),
    shots_faced_90_100       = sc(spd$shotAttempts90To100),
    shots_faced_80_90        = sc(spd$shotAttempts80To90),
    # Zone time
    dz_pct = zv("defensiveZonePctg"),
    oz_pct = zv("offensiveZonePctg"),
    stringsAsFactors=FALSE)
}

# =============================================================================
# SECTION 7 — Scrape functions
# =============================================================================

scrape_teams <- function(teams, season_end_yr, game_type, debug=FALSE) {
  szn     <- season_id(season_end_yr)
  results <- setNames(lapply(TEAM_ENDPOINTS, function(e) list()), sapply(TEAM_ENDPOINTS, `[[`, "name"))
  for (i in seq_len(nrow(teams))) {
    tid  <- teams$team_id[i]; abbr <- teams$team_abbrev[i]
    hist <- Filter(function(h) h$team_id==tid, HISTORICAL_TEAMS)
    if (length(hist)>0 && season_end_yr>hist[[1]]$last_season) next
    for (ep in TEAM_ENDPOINTS) {
      url <- ep$url_fn(tid, szn, game_type)
      raw <- nhl_get(url, 20, debug)
      df  <- ep$parse_fn(raw, abbr, tid, season_end_yr, game_type)
      if (!is.null(df) && nrow(df)>0) results[[ep$name]][[length(results[[ep$name]])+1]] <- df
      Sys.sleep(0.06)
    }
  }
  lapply(results, function(rows) {
    if (length(rows)==0) return(NULL)
    tryCatch(bind_rows(rows), error=function(e) do.call(rbind, rows))
  })
}

scrape_players <- function(players, season_end_yr, game_type, debug=FALSE) {
  szn <- season_id(season_end_yr)
  summaries <- list(); shot_locs <- list(); last10s <- list()
  n <- nrow(players)
  for (i in seq_len(n)) {
    pid <- players$player_id[i]; abbr <- players$team_abbrev[i]
    if (i %% 50==0) log_msg(sprintf("    Player %d / %d", i, n))
    # Cache raw_comp to avoid double-hitting the API
    url_comp <- sprintf("%s/skater-comparison/%s/%s/%d", PBASE, pid, szn, game_type)
    raw_comp <- nhl_get(url_comp)
    df_sum <- parse_skater_summary(raw_comp, pid, abbr, season_end_yr, game_type)
    if (!is.null(df_sum)&&nrow(df_sum)>0) summaries[[length(summaries)+1]] <- df_sum
    df_sl <- parse_skater_shot_location(raw_comp, pid, abbr, season_end_yr, game_type)
    if (!is.null(df_sl)&&nrow(df_sl)>0) shot_locs[[length(shot_locs)+1]] <- df_sl
    Sys.sleep(0.06)
    url_dist <- sprintf("%s/skater-skating-distance-detail/%s/%s/%d", PBASE, pid, szn, game_type)
    raw_dist <- nhl_get(url_dist)
    df_l10 <- parse_skater_last10(raw_dist, pid, abbr, season_end_yr, game_type)
    if (!is.null(df_l10)&&nrow(df_l10)>0) last10s[[length(last10s)+1]] <- df_l10
    Sys.sleep(0.06)
  }
  bind_safe <- function(lst) { if(length(lst)==0) return(NULL); tryCatch(bind_rows(lst),error=function(e) do.call(rbind,lst)) }
  list(skater_summary=bind_safe(summaries), skater_shot_location=bind_safe(shot_locs),
       skater_last10_distance=bind_safe(last10s))
}

scrape_goalies <- function(goalies, season_end_yr, game_type, debug=FALSE) {
  szn <- season_id(season_end_yr); rows <- list(); n <- nrow(goalies)
  for (i in seq_len(n)) {
    pid <- goalies$player_id[i]; abbr <- goalies$team_abbrev[i]
    if (i %% 20==0) log_msg(sprintf("    Goalie %d / %d", i, n))
    url <- sprintf("%s/goalie-comparison/%s/%s/%d", PBASE, pid, szn, game_type)
    raw <- nhl_get(url, 20, debug)
    df  <- parse_goalie_edge(raw, pid, abbr, season_end_yr, game_type)
    if (!is.null(df)) rows[[length(rows)+1]] <- df
    Sys.sleep(0.06)
  }
  if (length(rows)==0) return(NULL)
  tryCatch(bind_rows(rows), error=function(e) do.call(rbind, rows))
}

# =============================================================================
# SECTION 8 — GitHub upload
# =============================================================================

raw_github_url <- function(file_path)
  sprintf("https://raw.githubusercontent.com/%s/%s/%s/%s",
          GITHUB_USERNAME, GITHUB_REPO, GITHUB_BRANCH, file_path)

github_upload_file <- function(file_path, local_path) {
  api_url  <- sprintf("https://api.github.com/repos/%s/%s/contents/%s",
                      GITHUB_USERNAME, GITHUB_REPO, file_path)
  auth     <- authenticate(GITHUB_USERNAME, GITHUB_PAT)
  b64      <- base64encode(readBin(local_path, "raw", file.size(local_path)))
  existing <- tryCatch(GET(api_url, auth, timeout(15)), error=function(e) NULL)
  sha <- if (!is.null(existing)&&status_code(existing)==200)
    fromJSON(content(existing,"text",encoding="UTF-8"))$sha else NULL
  body <- list(message=paste0("update ",basename(file_path)," — ",
                              format(Sys.time(),"%Y-%m-%d %H:%M UTC")),
               content=b64, branch=GITHUB_BRANCH)
  if (!is.null(sha)) body$sha <- sha
  resp <- PUT(api_url, auth, body=toJSON(body,auto_unbox=TRUE),
              add_headers("Content-Type"="application/json"), timeout(60))
  sc_r <- status_code(resp)
  if (sc_r %in% c(200L,201L)) { log_msg(sprintf("  ✓ %s", file_path))
  } else {
    msg <- tryCatch(fromJSON(content(resp,"text",encoding="UTF-8"))$message,
                    error=function(e) as.character(sc_r))
    log_msg(sprintf("  ✗ FAILED %s : %s", file_path, msg))
  }
  invisible(sc_r)
}

github_upload_df <- function(df, repo_path) {
  tmp <- tempfile(fileext=".csv"); on.exit(unlink(tmp))
  write_csv(df, tmp); github_upload_file(repo_path, tmp)
}

github_upload_text <- function(text, repo_path) {
  tmp <- tempfile(); on.exit(unlink(tmp))
  writeLines(text, tmp); github_upload_file(repo_path, tmp)
}

# =============================================================================
# SECTION 9 — Main entry point
# =============================================================================

run_edge_scraper <- function(seasons_override       = NULL,
                             push                   = TRUE,
                             debug                  = FALSE,
                             scrape_players_flag    = TRUE,
                             scrape_goalies_flag    = TRUE,
                             game_type              = GAME_TYPE) {
  
  if (push && nchar(trimws(GITHUB_PAT))==0)
    stop("GITHUB_PAT is empty — add to ~/.Renviron and run readRenviron('~/.Renviron').")
  
  seasons <- if (!is.null(seasons_override)) as.integer(seasons_override)
  else all_edge_seasons()
  
  log_msg(sprintf("Seasons : %s", paste(seasons, collapse=", ")))
  log_msg(sprintf("Repo    : github.com/%s/%s  (branch: %s)",
                  GITHUB_USERNAME, GITHUB_REPO, GITHUB_BRANCH))
  
  teams <- fetch_team_list(debug)
  log_msg(sprintf("Teams   : %d", nrow(teams)))
  
  all_results <- list()
  
  for (szn in seasons) {
    log_msg(sprintf("── Season %d-%d ──────────────────────────────", szn-1L, szn))
    
    # Teams
    log_msg("  Scraping team endpoints...")
    team_res <- scrape_teams(teams, szn, game_type, debug)
    for (nm in names(team_res)) {
      if (!is.null(team_res[[nm]])) {
        all_results[[nm]] <- bind_rows(all_results[[nm]], team_res[[nm]])
        log_msg(sprintf("    %s: %d rows", nm, nrow(team_res[[nm]])))
      }
    }
    
    # Skaters
    if (scrape_players_flag) {
      log_msg("  Fetching player list...")
      players <- fetch_player_list(szn, debug)
      if (is.null(players)||nrow(players)==0) {
        log_msg("  No players found — skipping.")
      } else {
        log_msg(sprintf("  Players: %d — scraping Edge data...", nrow(players)))
        player_res <- scrape_players(players, szn, game_type, debug)
        for (nm in names(player_res)) {
          if (!is.null(player_res[[nm]])) {
            all_results[[nm]] <- bind_rows(all_results[[nm]], player_res[[nm]])
            log_msg(sprintf("    %s: %d rows", nm, nrow(player_res[[nm]])))
          }
        }
      }
    }
    
    # Goalies
    if (scrape_goalies_flag) {
      log_msg("  Fetching goalie list...")
      goalies <- fetch_goalie_list(szn, debug)
      if (is.null(goalies)||nrow(goalies)==0) {
        log_msg("  No goalies found — skipping.")
      } else {
        log_msg(sprintf("  Goalies: %d — scraping Edge data...", nrow(goalies)))
        goalie_res <- scrape_goalies(goalies, szn, game_type, debug)
        if (!is.null(goalie_res)) {
          all_results[["goalie_summary"]] <- bind_rows(all_results[["goalie_summary"]], goalie_res)
          log_msg(sprintf("    goalie_summary: %d rows", nrow(goalie_res)))
        }
      }
    }
  }
  
  if (length(all_results)==0) {
    log_msg("No data scraped. Try: run_edge_scraper(debug=TRUE, push=FALSE)")
    return(invisible(NULL))
  }
  
  if (!push) {
    log_msg("push=FALSE — skipping upload.")
    return(invisible(all_results))
  }
  
  log_msg("Uploading to GitHub...")
  szn_folder <- function(s) paste0("data/edge/", s-1L, "-", substr(as.character(s),3,4))
  for (nm in names(all_results)) {
    df <- all_results[[nm]]; if (is.null(df)) next
    for (szn in unique(df$season)) {
      df_szn    <- df[df$season==szn, ]
      repo_path <- paste0(szn_folder(szn), "/", nm, ".csv")
      github_upload_df(df_szn, repo_path)
    }
  }
  
  github_upload_text(paste0("# NHL Edge Stats\n\nAuto-generated: ",
                            format(Sys.time(),"%Y-%m-%d %H:%M UTC")), "README.md")
  
  log_msg("Done.")
  invisible(all_results)
}

# =============================================================================
# Run when sourced.
# Full history:           run_edge_scraper()
# Current season only:    run_edge_scraper(seasons_override = c(2026))
# Teams only (fast):      run_edge_scraper(scrape_players_flag=FALSE, scrape_goalies_flag=FALSE)
# Test without uploading: run_edge_scraper(debug=TRUE, push=FALSE)
# =============================================================================
run_edge_scraper()