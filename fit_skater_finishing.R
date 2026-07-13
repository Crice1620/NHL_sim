# ==============================================================================
# fit_skater_finishing.R
# ==============================================================================
# Fits skater "finishing skill" (Goals Above Expected, GAx) — the one
# component identified as genuinely missing when comparing this pipeline
# against HockeyStats.com's published 6-component team model:
#   1. Offensive impact on shots-for            — not modeled separately here
#      (see note below on why this is a deliberate simplification, not a gap)
#   2. Offensive impact on xG-for                — off_rapm_per60 (fit_rapm.R)
#   3. Defensive impact on shots-against          — not modeled separately here
#   4. Defensive impact on xG-against             — def_rapm_per60 (fit_rapm.R)
#   5. Shooting/finishing impact                  — THIS SCRIPT
#   6. Goaltending impact                         — already exists (GSAx in
#                                                    season_sim.R, confirmed
#                                                    still live this session)
#
# On (1)/(3): HockeyStats' game engine is a genuine two-step stochastic
# process (does a shot happen this second, THEN does it score), which is
# WHY they need shot volume as its own separate component. This pipeline's
# engine is simpler — goals are generated directly via Poisson(team xG
# total), skipping the shot-by-shot step entirely. That's a valid
# simplification on its own (xG's definition already bakes in shots ×
# per-shot probability), so shot volume isn't rebuilt separately here —
# but it IS a real, deliberate difference from HockeyStats' architecture,
# not something to pretend doesn't exist.
#
# METHOD: for each skater, GAx = (their own actual goals scored) minus (the
# sum of xG on their own shots) — i.e. did they score more or fewer goals
# than their own shot quality alone would predict. This is the EXACT same
# logic already used and validated for goaltending in season_sim.R (GSAx =
# xG faced minus goals allowed), just from the shooter's side instead of
# the goalie's.
#
# POOLING, NOT RECENCY-WEIGHTING: raw counts (goals, xG, TOI) are POOLED
# across the season window first, then ONE rate is computed from the
# pooled totals — deliberately mirroring goalie GSAx's own established
# approach in this codebase ("more robust for small samples than averaging
# three separate ratios together"), not the recency-weighted-blend-of-
# per-season-values approach used for RAPM/WOWY. Shooting talent is also
# generally a more stable, slowly-changing individual trait than team
# context, which further supports pooling over recency-weighting here.
#
# SCOPE: 5v5 shots PLUS shots taken while the shooter's own team is on the
# power play (not shorthanded shots against, which are rare and not
# relevant to a shooter's own finishing skill). Pooling both together
# (rather than fitting them separately) is deliberate: the xG value on
# each shot ALREADY accounts for the strength-state baseline (the xG
# model conditions on shooter_strength as a factor — see onice_stats.R's
# score_shots_with_xg()), so GAx computed from xG-versus-actual is already
# strength-state-normalized without needing a separate PP-specific fit.
# Pooling more shots per player also gives a more stable estimate than
# splitting an already-thin sample further.
#
# HONEST CAVEAT: this hasn't been run against real data as of writing —
# shooter_id was confirmed present in onice_stats.R's shot capture and
# shots_raw.csv's columns, but this specific aggregation is new and
# untested. Expect a debugging pass may be needed, particularly around
# column-name assumptions.

suppressMessages({
  library(dplyr)
  library(httr)
})

# ── Mode: identical convention to fit_rapm.R, so this can slot into the
# same periodic workflow without reinventing the pattern.
#
# RSTUDIO-NATIVE OVERRIDE: commandArgs() only reflects real command-line
# arguments, i.e. Rscript fit_skater_finishing.R --season=2015 --window=1
# run from an actual shell — a plain source("fit_skater_finishing.R") from
# an RStudio console never sets these at all. To loop through many seasons
# directly in RStudio without spawning separate Rscript processes, set
# SEASON_OVERRIDE/WINDOW_OVERRIDE/MODE_OVERRIDE in the console BEFORE each
# source() call — if present, they take priority over commandArgs(); if
# absent (the normal command-line-invocation case), nothing changes here.
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^--", name, "="), "", hit[1])
}
MODE <- if (exists("MODE_OVERRIDE")) MODE_OVERRIDE else get_arg("mode", "backfill")
SEASON_ARG <- if (exists("SEASON_OVERRIDE")) as.character(SEASON_OVERRIDE) else get_arg("season", NA_character_)
WINDOW_ARG <- if (exists("WINDOW_OVERRIDE")) as.character(WINDOW_OVERRIDE) else get_arg("window", NA_character_)  # explicit override — see below

GH_ONICE <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/onice"
# LOAD_LOCAL is tied ONLY to --mode=current (which runs via the weekly
# workflow against a freshly-checked-out repo) — NOT to --season being set.
# A specific historical --season on its own should default to GitHub,
# matching backfill mode's own convention, since the whole point of
# computing single-season values for PAST seasons (see the --window
# override below) is comparing them against the already-uploaded,
# GitHub-hosted pooled data — the user running this locally won't
# generally have every historical season's onice data sitting on disk.
LOAD_LOCAL <- MODE == "current"

# Pooling window — 1 (genuinely single-season, no pooling at all) for
# --mode=current, matching how RAPM's own --mode=current already behaves
# (a true single-season fit, no multi-year blending). This matters because
# app.R specifically needs current-season-ONLY data (cards, Game Sim,
# Series Sim, Playoff Bracket were all deliberately switched away from any
# multi-year blend) — a "current" file secretly built from 3 pooled years
# would silently violate that. 3 years ONLY for manual/backfill runs,
# which is what feeds season_sim.R's own 3-year blend + aging curve.
# HONEST TRADEOFF: a true single-season GAx will be noisier than the
# pooled version — that instability is exactly why pooling existed in the
# first place (same reasoning goalie GSAx already established) — but
# that's the real cost of actually matching app.R's current-season-only
# design goal, not something to paper over.
#
# --window=N is a SEPARATE, explicit override on top of the mode-based
# default above — needed for computing a genuine SINGLE-SEASON value for
# a SPECIFIC HISTORICAL season (via --season=YYYY --window=1), which is
# neither "current" (that season already happened) nor the normal 3-year
# backfill (the whole point is comparing against the pooled value, not
# reproducing it). Without this, there was no way to get a single-season
# historical value at all outside of --mode=current's own auto-detected
# "whatever season is live right now."
POOL_WINDOW <- if (!is.na(WINDOW_ARG)) as.integer(WINDOW_ARG) else if (MODE == "current") 1L else 3L

load_season_data <- function(season) {
  if (LOAD_LOCAL) {
    shots_path <- file.path("data", "onice", as.character(season), "shots_raw.csv")
    onice_path <- file.path("data", "onice", as.character(season), "skater_onice.csv")
    shots <- if (file.exists(shots_path)) read.csv(shots_path, stringsAsFactors = FALSE) else NULL
    onice <- if (file.exists(onice_path)) read.csv(onice_path, stringsAsFactors = FALSE) else NULL
  } else {
    # Timeout + retry — base R's read.csv() reading directly from a URL
    # has neither, and shots_raw.csv files are large enough (163,818 rows
    # for 2026 alone) that an occasional timeout is ordinary network
    # flakiness, not a broken file — same fix already applied in
    # check_stints.R and fit_shot_volume_rapm.R after hitting this
    # exact issue in practice.
    gh_fetch <- function(url) {
      for (attempt in 1:3) {
        resp <- tryCatch(GET(url, timeout(90)), error = function(e) NULL)
        if (!is.null(resp) && status_code(resp) == 200) {
          return(tryCatch(read.csv(text = content(resp, "text", encoding = "UTF-8"), stringsAsFactors = FALSE),
                           error = function(e) NULL))
        }
        if (!is.null(resp) && status_code(resp) == 404) return(NULL)  # genuinely missing — retrying won't help
        if (attempt < 3) Sys.sleep(2 * attempt)
      }
      NULL
    }
    shots <- gh_fetch(paste0(GH_ONICE, "/", season, "/shots_raw.csv"))
    onice <- gh_fetch(paste0(GH_ONICE, "/", season, "/skater_onice.csv"))
  }
  if (is.null(shots)) {
    cat("  Missing shots_raw.csv for season", season, "— skipping.\n")
    return(NULL)
  }
  list(shots = shots, onice = onice)
}

MIN_GAX_SHOTS_POOLED <- 150  # pooled across the full window — see header note on why this is higher than a single-season threshold would need to be

fit_finishing_for_season <- function(target_season) {
  cat("\n=== Fitting skater finishing (GAx) for target season", target_season, "===\n")
  window_seasons <- (target_season - POOL_WINDOW + 1L):target_season
  cat("  Pooling raw counts across seasons:", paste(window_seasons, collapse = ", "), "\n")

  all_shots <- list()
  all_onice <- list()
  for (s in window_seasons) {
    d <- load_season_data(s)
    if (is.null(d)) next
    if (!is.null(d$shots)) { d$shots$season <- s; all_shots[[as.character(s)]] <- d$shots }
    if (!is.null(d$onice)) { d$onice$season <- s; all_onice[[as.character(s)]] <- d$onice }
  }
  if (length(all_shots) == 0) {
    cat("  No shot data available for any season in this window — skipping.\n")
    return(NULL)
  }
  shots <- bind_rows(all_shots)
  onice <- if (length(all_onice) > 0) bind_rows(all_onice) else NULL

  if (!"shooter_id" %in% names(shots) || !"xg" %in% names(shots)) {
    cat("  shots_raw.csv missing shooter_id or xg column — cannot compute finishing skill for this window.\n")
    return(NULL)
  }
  shots$shooter_id <- as.character(shots$shooter_id)

  # Scope: 5v5 + shots taken by the shooter's own team while on the power
  # play (see header note on why these are pooled together, not split).
  own_team_pp <- (shots$situation_label == "home_pp" & shots$owner_team_id == shots$home_id) |
                  (shots$situation_label == "away_pp" & shots$owner_team_id != shots$home_id)
  scoped <- shots %>%
    filter(!is.na(shooter_id), shooter_id != "", !is.na(xg),
           situation_label == "5v5" | own_team_pp)

  cat("  Scoped to", nrow(scoped), "shots (5v5 + own-team PP) across", nrow(shots), "total shot rows in window.\n")
  if (nrow(scoped) == 0) {
    cat("  No usable scoped shots — skipping.\n")
    return(NULL)
  }

  pooled <- scoped %>%
    group_by(shooter_id) %>%
    summarise(
      shots_taken   = dplyr::n(),
      actual_goals  = sum(coalesce(is_goal, FALSE), na.rm = TRUE),
      expected_goals = sum(coalesce(xg, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(gax_pooled = actual_goals - expected_goals)

  cat("  ", nrow(pooled), "unique shooters found in scoped shots.\n")
  pooled <- pooled %>% filter(shots_taken >= MIN_GAX_SHOTS_POOLED)
  cat("  ", nrow(pooled), "shooters clear the", MIN_GAX_SHOTS_POOLED, "pooled-shot minimum.\n")
  if (nrow(pooled) == 0) {
    cat("  No shooters cleared the minimum sample threshold — skipping.\n")
    return(NULL)
  }

  # Per-60 conversion needs each player's own pooled ice time across the
  # SAME window (5v5 + PP, matching the shot scope above) — pulled from
  # skater_onice.csv, summed across the window's seasons per player,
  # exactly the same pooling principle as the shot counts above.
  if (is.null(onice) || !all(c("player_id", "toi_5v5_sec") %in% names(onice))) {
    cat("  No on-ice TOI data available for this window — cannot compute a per-60 rate.\n")
    return(NULL)
  }
  onice$player_id <- as.character(onice$player_id)
  if (!"toi_pp_sec" %in% names(onice)) onice$toi_pp_sec <- 0
  toi_pooled <- onice %>%
    group_by(player_id) %>%
    summarise(toi_total_sec = sum(coalesce(toi_5v5_sec, 0), na.rm = TRUE) + sum(coalesce(toi_pp_sec, 0), na.rm = TRUE), .groups = "drop")

  pooled <- pooled %>%
    left_join(toi_pooled, by = c("shooter_id" = "player_id")) %>%
    filter(!is.na(toi_total_sec), toi_total_sec > 0) %>%
    mutate(gax_per60 = gax_pooled / (toi_total_sec / 3600))

  cat("  ", nrow(pooled), "shooters have both a qualifying shot sample and usable TOI.\n")

  result <- pooled %>%
    transmute(player_id = shooter_id, season = target_season,
              shots_taken, actual_goals, expected_goals, gax_pooled,
              toi_total_sec, gax_per60)

  cat("  Done. Sample coefficients (top 5 by gax_per60):\n")
  print(head(result %>% arrange(desc(gax_per60)), 5))
  cat("  Bottom 5 by gax_per60 (worst finishers relative to shot quality):\n")
  print(head(result %>% arrange(gax_per60), 5))

  result
}

save_finishing_output <- function(df, season) {
  # data/finishing/{season}/ — a new, separate folder from data/rapm/, since
  # this is a genuinely different methodology (pooled goals-vs-expected,
  # not a ridge regression), not a RAPM variant.
  # THREE distinct filenames, none of which may ever collide:
  #   skater_gax.csv          — 3-year pooled (default backfill), feeds season_sim.R
  #   skater_gax_current.csv  — single-season, --mode=current, feeds app.R
  #   skater_gax_single_season.csv — single-season for a SPECIFIC HISTORICAL
  #     season (--season=YYYY --window=1), used ONLY to compare against
  #     that same season's own pooled value when fitting finishing skill's
  #     shrinkage regression — this is neither of the other two cases and
  #     would silently corrupt one of them if it shared a filename.
  out_dir <- file.path("data", "finishing", as.character(season))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  filename <- if (MODE == "current") {
    "skater_gax_current.csv"
  } else if (!is.na(WINDOW_ARG) && as.integer(WINDOW_ARG) == 1 && !is.na(SEASON_ARG)) {
    "skater_gax_single_season.csv"
  } else {
    "skater_gax.csv"
  }
  out_path <- file.path(out_dir, filename)
  write.csv(df, out_path, row.names = FALSE)
  out_path
}

# ── Driver ────────────────────────────────────────────────────────────────
SEASONS_TO_FIT <- if (MODE == "current") {
  cy <- as.integer(format(Sys.Date(), "%Y"))
  cm <- as.integer(format(Sys.Date(), "%m"))
  ifelse(cm >= 9, cy + 1L, cy)
} else if (!is.na(SEASON_ARG)) {
  as.integer(SEASON_ARG)
} else {
  2013:2026  # POOL_WINDOW=3 means the earliest usable TARGET season is 2013 (pooling 2011-2013), given the known 2010 on-ice data gap
}
cat("Mode:", MODE, "| Target seasons:", paste(SEASONS_TO_FIT, collapse = ", "), "\n")

all_results <- list()
for (s in SEASONS_TO_FIT) {
  res <- tryCatch(fit_finishing_for_season(s), error = function(e) {
    cat("Season", s, "failed entirely:", conditionMessage(e), "\n"); NULL
  })
  if (!is.null(res)) {
    all_results[[as.character(s)]] <- res
    out_path <- save_finishing_output(res, s)
    cat("  Saved to", out_path, "\n")
  }
}

USING_SINGLE_SEASON <- MODE == "current" || !is.na(SEASON_ARG)
if (!USING_SINGLE_SEASON && length(all_results) > 0) {
  combined <- bind_rows(all_results)
  write.csv(combined, "skater_gax_all_seasons.csv", row.names = FALSE)
  cat("\nSaved combined output to skater_gax_all_seasons.csv (", nrow(combined), "player-seasons across",
      length(all_results), "target seasons).\n")
} else if (!USING_SINGLE_SEASON) {
  cat("\nNo seasons produced usable finishing-skill output.\n")
}
