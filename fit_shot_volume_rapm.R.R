# ==============================================================================
# fit_shot_volume_rapm.R
# ==============================================================================
# Fits shot-VOLUME RAPM — isolated player impact on shots-for/shots-against
# per unit ice time, the component identified as missing when comparing
# this pipeline against HockeyStats.com's 6-part team model (xG-RAPM,
# already built via fit_rapm.R, only covers shot QUALITY, not volume).
#
# METHOD: for every 5v5 stint (see onice_stats.R's build_stints()), each
# team's own 5 skaters get credited/blamed for the shots that happened
# during that stint, with the stint's duration as a Poisson exposure
# offset — the standard way to model a COUNT that occurs at some rate
# over a variable amount of time. This is a genuinely different
# regression TYPE than fit_rapm.R's xG-RAPM (which fits a linear ridge
# regression on a continuous xG target, one row per shot) — this fits a
# Poisson-family ridge regression on a count target, one row per
# (stint, team) pair, exactly mirroring the offense/defense split
# structure fit_rapm.R already established, just with stints instead of
# shots as the unit of observation.
#
# SCALE, HONESTLY: this design matrix is roughly 10x larger than xG-RAPM's
# — two rows per STINT rather than one row per SHOT, and stints
# outnumber shots by a wide margin (a season's stints.csv already ran
# 400,000-670,000 rows on its own). Expect real, noticeably longer
# fitting time than fit_rapm.R, not an instant drop-in. nfolds is
# deliberately reduced from cv.glmnet's default of 10 to 5 specifically
# to manage this — a real speed/precision tradeoff, not a free lunch.
#
# CONVERTING POISSON COEFFICIENTS TO THIS PROJECT'S ADDITIVE CONVENTION:
# Poisson regression coefficients are on the LOG scale — exp(coefficient)
# is a MULTIPLICATIVE rate ratio, not directly comparable to RAPM/
# finishing skill's additive per-60 values. Converting via
# (exp(coef) - 1) * league_avg_shots_per60 turns this into an additive
# per-60 deviation with the same zero-centered convention used
# everywhere else in this project — a coefficient of exactly 0 (no
# effect) correctly maps to 0 additive shots.
#
# HONEST, UNRESOLVED RISK — NOT ADDRESSED BY THIS SCRIPT: shot volume and
# xG are correlated by construction (more shots directly means more
# cumulative xG, all else equal). Combining this output with xG-RAPM's
# output at the team level without checking that correlation first risks
# double-counting the same underlying skill twice. This script only
# PRODUCES the shot-volume RAPM values — it does not address how they
# should be combined with xG-RAPM when wiring into season_sim.R/app.R.
# That's a separate, later step.

suppressMessages({
  library(dplyr)
  library(Matrix)
  library(glmnet)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^--", name, "="), "", hit[1])
}
MODE <- get_arg("mode", "backfill")
SEASON_ARG <- get_arg("season", NA_character_)

GH_ONICE <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/onice"
LOAD_LOCAL <- MODE == "current" || !is.na(SEASON_ARG)

load_stints <- function(season) {
  if (LOAD_LOCAL) {
    path <- file.path("data", "onice", as.character(season), "stints.csv")
    if (!file.exists(path)) return(NULL)
    tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  } else {
    tryCatch(read.csv(paste0(GH_ONICE, "/", season, "/stints.csv"), stringsAsFactors = FALSE), error = function(e) NULL)
  }
}

fit_shot_volume_for_season <- function(season) {
  cat("\n=== Fitting shot-volume RAPM for season", season, "===\n")
  stints <- load_stints(season)
  if (is.null(stints) || nrow(stints) == 0) {
    cat("  No stints.csv for season", season, "— skipping.\n")
    return(NULL)
  }
  cat("  Loaded", nrow(stints), "stints.\n")
  
  # Long format: TWO rows per stint, one per team's own perspective —
  # same offense/defense split structure as fit_rapm.R, just with stints
  # (and a Poisson count target) instead of shots (and a linear xG target).
  home_rows <- data.frame(
    players = stints$home_players, shots_for = stints$shots_home,
    shots_against = stints$shots_away, duration_sec = stints$duration_sec,
    stringsAsFactors = FALSE
  )
  away_rows <- data.frame(
    players = stints$away_players, shots_for = stints$shots_away,
    shots_against = stints$shots_home, duration_sec = stints$duration_sec,
    stringsAsFactors = FALSE
  )
  long_df <- bind_rows(home_rows, away_rows)
  long_df <- long_df[!is.na(long_df$duration_sec) & long_df$duration_sec > 0, ]
  cat("  ", nrow(long_df), "team-stint rows after combining both perspectives.\n")
  
  # ── Sparse design matrix — same construction principle as fit_rapm.R,
  # just built from semicolon-joined player lists instead of a lineup
  # data frame's own columns.
  player_lists <- strsplit(long_df$players, ";")
  all_players <- sort(unique(unlist(player_lists)))
  cat("  ", length(all_players), "unique players across", nrow(long_df), "team-stint rows.\n")
  if (length(all_players) == 0) { cat("  No usable players found — skipping.\n"); return(NULL) }
  
  player_idx <- setNames(seq_along(all_players), all_players)
  n_per_row <- sapply(player_lists, length)
  row_i <- rep(seq_len(nrow(long_df)), times = n_per_row)
  col_j <- player_idx[unlist(player_lists)]
  X <- sparseMatrix(i = row_i, j = col_j, x = 1, dims = c(nrow(long_df), length(all_players)),
                    dimnames = list(NULL, all_players))
  
  offset_vec <- log(long_df$duration_sec / 3600)  # per-hour Poisson exposure
  
  # ── Offense model: shots THIS team's own players generated ─────────────
  cat("  Fitting offense (shots-for) Poisson ridge regression (nfolds=5 — see header note on why)...\n")
  off_fit <- cv.glmnet(X, long_df$shots_for, family = "poisson", offset = offset_vec,
                       alpha = 0, standardize = FALSE, nfolds = 5)
  off_coef <- as.numeric(coef(off_fit, s = "lambda.min"))[-1]  # drop intercept
  names(off_coef) <- all_players
  
  # ── Defense model: shots THIS team's own players ALLOWED ───────────────
  cat("  Fitting defense (shots-against) Poisson ridge regression...\n")
  def_fit <- cv.glmnet(X, long_df$shots_against, family = "poisson", offset = offset_vec,
                       alpha = 0, standardize = FALSE, nfolds = 5)
  def_coef <- as.numeric(coef(def_fit, s = "lambda.min"))[-1]
  names(def_coef) <- all_players
  
  # ── Convert log-scale Poisson coefficients to this project's additive
  # per-60 convention (see header note for the full reasoning).
  league_avg_shots_per60 <- sum(long_df$shots_for) / sum(long_df$duration_sec / 3600)
  cat("  League-average shots-for per 60 (this season):", round(league_avg_shots_per60, 2), "\n")
  
  shot_vol_off_per60 <- round((exp(off_coef) - 1) * league_avg_shots_per60, 4)
  # DEFENSE sign: negated, same convention as xG-RAPM — higher should mean
  # BETTER defense (fewer shots allowed), not "the raw model's predicted
  # shot count," which runs the opposite direction without this flip.
  shot_vol_def_per60 <- round(-(exp(def_coef) - 1) * league_avg_shots_per60, 4)
  
  result <- data.frame(
    player_id = all_players, season = season,
    shot_vol_off_per60 = shot_vol_off_per60,
    shot_vol_def_per60 = shot_vol_def_per60,
    stringsAsFactors = FALSE
  )
  
  cat("  Done. Top 5 offense (shot-volume):\n")
  print(head(result[order(-result$shot_vol_off_per60), c("player_id", "shot_vol_off_per60")], 5))
  cat("  Top 5 defense (shot-suppression):\n")
  print(head(result[order(-result$shot_vol_def_per60), c("player_id", "shot_vol_def_per60")], 5))
  cat("  Means (both should be close to 0, matching RAPM's own zero-centered convention) — offense:",
      round(mean(result$shot_vol_off_per60), 4), "| defense:", round(mean(result$shot_vol_def_per60), 4), "\n")
  
  result
}

save_output <- function(df, season) {
  # data/shot_volume_rapm/{season}/ — new, separate folder from data/rapm/,
  # since this is a genuinely different regression type (Poisson, not
  # linear ridge) fit on different data (stints, not individual shots).
  out_dir <- file.path("data", "shot_volume_rapm", as.character(season))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  # DISTINCT FILENAME for --mode=current, same reasoning as
  # fit_skater_finishing.R's identical pattern: the weekly workflow's
  # current-season refresh must never collide with or overwrite a
  # differently-scoped backfill file for the same season.
  filename <- if (MODE == "current") "shot_volume_rapm_current.csv" else "shot_volume_rapm.csv"
  out_path <- file.path(out_dir, filename)
  write.csv(df, out_path, row.names = FALSE)
  out_path
}

SEASONS_TO_FIT <- if (MODE == "current") {
  cy <- as.integer(format(Sys.Date(), "%Y")); cm <- as.integer(format(Sys.Date(), "%m"))
  ifelse(cm >= 9, cy + 1L, cy)
} else if (!is.na(SEASON_ARG)) {
  as.integer(SEASON_ARG)
} else {
  2011:2026  # matches the range check_stints.R already validated stints.csv across
}
cat("Mode:", MODE, "| Seasons to fit:", paste(SEASONS_TO_FIT, collapse = ", "), "\n")

for (s in SEASONS_TO_FIT) {
  res <- tryCatch(fit_shot_volume_for_season(s), error = function(e) {
    cat("Season", s, "failed entirely:", conditionMessage(e), "\n"); NULL
  })
  if (!is.null(res)) {
    out_path <- save_output(res, s)
    cat("  Saved to", out_path, "\n")
  }
}