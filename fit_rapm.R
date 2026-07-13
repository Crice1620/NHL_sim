# ==============================================================================
# fit_rapm.R
# ==============================================================================
# Fits RAPM (Regularized Adjusted Plus-Minus) for 5v5 Offence and Defence,
# per season, from the shot_lineups.csv + shots_raw.csv data produced by the
# updated onice_stats.R. Output is designed to slot into the EXISTING
# per-season CSV / recency-weighting / percentile pipeline as a new,
# preferred input — no changes needed to season_sim.R/app.R's downstream
# blending logic, which already knows how to fall back gracefully
# (coalesce(rapm, xgwowy, wowy, box_score_proxy)).
#
# DESIGN CHOICE — why two SEPARATE regressions, not one combined +1/-1 model:
# The classic single-regression RAPM design (shooting team's players = +1,
# defending team's players = -1, one shared coefficient per player) produces
# ONE blended number per player — it can't cleanly separate "this player is
# a great shot generator" from "this player is a great shot suppressor,"
# since both effects load onto the same coefficient from opposite sides.
# That's fine for a single "value" number, but this app's cards need
# genuinely separate Offence and Defence categories. So this script instead
# fits:
#   - OFFENCE model: design matrix codes the SHOOTING team's players as +1
#     and zeroes out the defending team entirely (not -1) for that row.
#     Coefficient = this player's average contribution to shot/xG
#     generation when their team has the puck, independent of who they're
#     defending against on other shifts.
#   - DEFENCE model: mirror image — defending team's players = +1, shooting
#     team zeroed out. Target is still the shot's xG value, so a HIGHER
#     coefficient here means MORE xG allowed = WORSE defense; this is
#     negated before output so higher-is-better matches every other
#     category on the cards (same convention already used for
#     disc_pg = -(pim/gp)).
# This is a genuine, considered design choice — not necessarily identical
# to any specific public model's exact methodology — and should be treated
# as a first draft to validate once real output exists, not a guaranteed-
# correct implementation.
#
# SCOPE: 5v5 ONLY in this first pass. shot_lineups.csv already captures
# PP/PK lineups too (situation_code preserved on every row), but PP/PK's
# asymmetric strength states (5v4 vs 5v3 vs 4v3, each a genuinely different
# game situation) need their own careful design decision about whether to
# pool them or fit separately — deliberately not rushed here. Extending to
# PP/PK is a real, separate follow-up once 5v5 output looks sound.
#
# REGULARIZATION: ridge regression (glmnet, alpha=0) via cv.glmnet with
# proper cross-validation to select the penalty — NOT a plain OLS
# regression, which would badly overfit given many players have relatively
# few shifts together. Ridge's shrinkage is specifically what should help
# with thin-sample cases (like PP/PK will be, once attempted) far better
# than WOWY's raw with/without comparison could.
#
# HONEST CAVEAT: this hasn't been run against real data (shot_lineups.csv
# doesn't exist yet as of writing this — your backfill is what creates it).
# Expect a debugging pass once real output comes back, particularly around
# column-name assumptions and the per-60 scaling step, which is a reasonable
# but unverified approximation.

suppressMessages({
  library(dplyr)
  library(Matrix)
  library(glmnet)
})

# ── Mode: matches onice_stats.R's own --mode/--season conventions.
#   --mode=current : auto-detects the CURRENT season (same "before Sept =
#                    this year, after Sept = next year" logic used
#                    throughout this codebase) and fits ONLY that season —
#                    what the periodic workflow uses, since re-fitting all
#                    16 historical seasons every week would be wasteful.
#                    Assumes it's running inside a repo checkout (local
#                    files exist, faster than re-fetching).
#   --season=YYYY  : fits one specific season (any season, not just current).
#   (neither)      : full historical range, fetched from GitHub — the
#                    default for manual/interactive use, matching
#                    everything used so far in this conversation.
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^--", name, "="), "", hit[1])
}
MODE <- get_arg("mode", "backfill")
SEASON_ARG <- get_arg("season", NA_character_)

GH_ONICE <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/onice"
# If running this in the same repo checkout as onice_stats.R, local file
# paths (data/onice/{season}/...) will be faster than re-fetching over the
# network — auto-enabled for --mode=current or an explicit --season,
# both of which assume a repo checkout context.
LOAD_LOCAL <- MODE == "current" || !is.na(SEASON_ARG)

load_season_lineup_data <- function(season) {
  if (LOAD_LOCAL) {
    lineup_path <- file.path("data", "onice", as.character(season), "shot_lineups.csv")
    shots_path  <- file.path("data", "onice", as.character(season), "shots_raw.csv")
    onice_path  <- file.path("data", "onice", as.character(season), "skater_onice.csv")
    lineup <- if (file.exists(lineup_path)) read.csv(lineup_path, stringsAsFactors = FALSE, colClasses = c(situation_code = "character")) else NULL
    shots  <- if (file.exists(shots_path))  read.csv(shots_path,  stringsAsFactors = FALSE) else NULL
    onice  <- if (file.exists(onice_path))  read.csv(onice_path,  stringsAsFactors = FALSE) else NULL
  } else {
    lineup <- tryCatch(read.csv(paste0(GH_ONICE, "/", season, "/shot_lineups.csv"), stringsAsFactors = FALSE, colClasses = c(situation_code = "character")), error = function(e) NULL)
    shots  <- tryCatch(read.csv(paste0(GH_ONICE, "/", season, "/shots_raw.csv"),   stringsAsFactors = FALSE), error = function(e) NULL)
    onice  <- tryCatch(read.csv(paste0(GH_ONICE, "/", season, "/skater_onice.csv"), stringsAsFactors = FALSE), error = function(e) NULL)
  }
  if (is.null(lineup) || is.null(shots)) {
    cat("  Missing shot_lineups.csv or shots_raw.csv for season", season, "— skipping.\n")
    return(NULL)
  }
  list(lineup = lineup, shots = shots, onice = onice)
}

# ── Build the sparse design matrix + response vector for one side (offense
# OR defense) of one season's 5v5 shots ─────────────────────────────────────
# side: "for" (offense — shooting team's players coded +1) or "against"
# (defense — defending team's players coded +1). The OTHER side is zeroed
# out entirely for that row — this is the actual mechanism that keeps
# offense and defense signals separate (see the header note above).
build_design_matrix <- function(df, side, player_index) {
  player_col <- if (side == "for") "for_players" else "against_players"
  # Split each row's semicolon-joined player list into individual
  # (row_index, player_id) pairs — this expansion is what turns the compact
  # one-row-per-shot storage back into the long format a sparse matrix needs.
  split_list <- strsplit(df[[player_col]], ";", fixed = TRUE)
  n_per_row <- lengths(split_list)
  valid_rows <- which(n_per_row > 0)
  if (length(valid_rows) == 0) return(NULL)

  row_idx <- rep(valid_rows, n_per_row[valid_rows])
  player_ids <- unlist(split_list[valid_rows], use.names = FALSE)
  col_idx <- player_index[player_ids]
  keep <- !is.na(col_idx)  # a player_id not in player_index shouldn't happen, but guard anyway
  if (!all(keep)) {
    row_idx <- row_idx[keep]; col_idx <- col_idx[keep]
  }

  X <- sparseMatrix(i = row_idx, j = col_idx, x = 1,
                     dims = c(nrow(df), length(player_index)),
                     dimnames = list(NULL, names(player_index)))
  X
}

fit_rapm_for_season <- function(season) {
  cat("\n=== Fitting RAPM for season", season, "===\n")
  d <- load_season_lineup_data(season)
  if (is.null(d)) return(NULL)

  lineup <- d$lineup
  shots  <- d$shots

  # situation_label (the authoritative field used everywhere else in this
  # pipeline for strength-state filtering) only exists on shots_raw.csv, not
  # shot_lineups.csv directly — so the actual 5v5 filter happens right after
  # the join below, not before it.
  merged <- lineup %>%
    inner_join(shots %>% select(game_id, event_idx, xg, situation_label, is_goal),
               by = c("game_id", "event_idx")) %>%
    filter(situation_label == "5v5", !is.na(xg))

  cat("  Merged", nrow(merged), "usable 5v5 shot-lineup rows (of", nrow(lineup), "total lineup rows).\n")
  if (nrow(merged) < 500) {
    cat("  Too few usable rows to fit a meaningful model — skipping season", season, ".\n")
    return(NULL)
  }

  # Unique player universe for this season, indexed for the sparse matrix.
  all_players <- unique(c(
    unlist(strsplit(merged$for_players, ";", fixed = TRUE)),
    unlist(strsplit(merged$against_players, ";", fixed = TRUE))
  ))
  all_players <- all_players[!is.na(all_players) & all_players != ""]
  player_index <- setNames(seq_along(all_players), all_players)
  cat("  ", length(all_players), "unique players in this season's 5v5 shot data.\n")

  y <- merged$xg

  # ── Offense model: shooting team's players = +1, defending team zeroed ──
  cat("  Building offense design matrix...\n")
  X_off <- build_design_matrix(merged, "for", player_index)
  cat("  Fitting offense ridge regression (cv.glmnet, alpha=0)...\n")
  fit_off <- tryCatch(cv.glmnet(X_off, y, alpha = 0, standardize = FALSE), error = function(e) {
    cat("    Offense model fit failed:", conditionMessage(e), "\n"); NULL
  })

  # ── Defense model: defending team's players = +1, shooting team zeroed ──
  cat("  Building defense design matrix...\n")
  X_def <- build_design_matrix(merged, "against", player_index)
  cat("  Fitting defense ridge regression (cv.glmnet, alpha=0)...\n")
  fit_def <- tryCatch(cv.glmnet(X_def, y, alpha = 0, standardize = FALSE), error = function(e) {
    cat("    Defense model fit failed:", conditionMessage(e), "\n"); NULL
  })

  if (is.null(fit_off) || is.null(fit_def)) {
    cat("  Could not fit both models for season", season, "— skipping.\n")
    return(NULL)
  }

  off_coefs <- as.matrix(coef(fit_off, s = "lambda.min"))[-1, 1]  # drop intercept
  def_coefs <- as.matrix(coef(fit_def, s = "lambda.min"))[-1, 1]

  result <- data.frame(
    player_id = names(player_index),
    season = season,
    off_rapm_raw = off_coefs[names(player_index)],
    # Negated — higher coefficient in the raw defense model means MORE xG
    # allowed (worse defense), so this flips it to match every other
    # category's higher-is-better convention.
    def_rapm_raw = -def_coefs[names(player_index)],
    stringsAsFactors = FALSE
  )

  # Per-60 conversion: a RAPM coefficient represents the extra xG added to
  # a shot every time this player is on the ice for one. If they were on
  # the ice for N of their team's shots, their total added xG across those
  # N shots is coefficient * N — dividing by their own 5v5 ice time (in
  # hours) converts that into a per-60 RATE, in the same units WOWY uses.
  # Uses each player's OWN on-ice shot rate (cf_5v5/ca_5v5 divided by their
  # own toi_5v5_sec), not a league-average rate — a shutdown, low-event
  # player and a high-event top-liner get correctly different conversions
  # this way, which a single league-wide multiplier would get wrong.
  if (!is.null(d$onice) && all(c("toi_5v5_sec", "cf_5v5", "ca_5v5") %in% names(d$onice))) {
    onice_lookup <- d$onice %>%
      mutate(
        own_cf_per60 = ifelse(toi_5v5_sec > 0, cf_5v5 / (toi_5v5_sec / 3600), NA_real_),
        own_ca_per60 = ifelse(toi_5v5_sec > 0, ca_5v5 / (toi_5v5_sec / 3600), NA_real_)
      ) %>%
      select(player_id, toi_5v5_sec, own_cf_per60, own_ca_per60)
    onice_lookup$player_id <- as.character(onice_lookup$player_id)
    result <- result %>% left_join(onice_lookup, by = "player_id")
    result$off_rapm_per60 <- result$off_rapm_raw * result$own_cf_per60
    # Defense uses the SAME sign convention already applied to def_rapm_raw
    # (already negated so higher = better) — own_ca_per60 (their own
    # on-ice shots-against rate) is always positive, so multiplying
    # preserves that convention correctly.
    result$def_rapm_per60 <- result$def_rapm_raw * result$own_ca_per60
  } else {
    result$off_rapm_per60 <- NA_real_
    result$def_rapm_per60 <- NA_real_
    result$toi_5v5_sec <- NA_real_
  }

  cat("  Done. Sample coefficients (top 5 by offense, per-60):\n")
  print(head(result %>% arrange(desc(off_rapm_per60)), 5))

  result
}

# ── PP/PK RAPM ───────────────────────────────────────────────────────────────
# Same for/against +1-vs-zeroed design as 5v5 (see fit_rapm_for_season), with
# ONE real, necessary addition: strength-state control. A 5v3 power play
# scores at a meaningfully higher rate than a plain 5v4, purely as a
# function of the situation — and coaches naturally deploy their best
# players in the rarer, higher-leverage 5v3 chances. Without controlling
# for this, some of "5v3 is just an easier situation to score in" would
# get misattributed to whichever players happen to see more 5v3 time,
# inflating their coefficients for reasons unrelated to real skill.
#
# Fix: include strength-state (5v4/5v3/4v3) as additional design-matrix
# columns, but UNPENALIZED (penalty.factor=0) — ridge's shrinkage should
# focus on the many, individually-uncertain PLAYER coefficients, while the
# known, real strength-state effect gets estimated at full strength every
# time rather than blended away by the same penalty.

# Derives exact strength state (e.g. "5v4", "5v3") from situationCode
# (format: [awayGoalie][awaySkaters][homeSkaters][homeGoalie], preserved on
# every shot_lineups.csv row specifically for this purpose). Expressed as
# attacker-skaters v defender-skaters regardless of home/away, matching
# for_players/against_players' own PP-team-perspective convention.
derive_strength_state <- function(situation_code) {
  situation_code <- as.character(situation_code)  # defensive — protects against this exact bug even if the read-in type wasn't fixed at the source for some reason
  ch <- strsplit(situation_code, "")
  sapply(ch, function(x) {
    if (length(x) != 4) return(NA_character_)
    away_sk <- suppressWarnings(as.integer(x[2]))
    home_sk <- suppressWarnings(as.integer(x[3]))
    if (is.na(away_sk) || is.na(home_sk)) return(NA_character_)
    paste0(max(away_sk, home_sk), "v", min(away_sk, home_sk))
  })
}

fit_pppk_rapm_for_season <- function(season) {
  cat("\n=== Fitting PP/PK RAPM for season", season, "===\n")
  d <- load_season_lineup_data(season)
  if (is.null(d)) return(NULL)

  lineup <- d$lineup
  shots  <- d$shots

  merged <- lineup %>%
    inner_join(shots %>% select(game_id, event_idx, xg, situation_label), by = c("game_id", "event_idx")) %>%
    filter(situation_label %in% c("home_pp", "away_pp"), !is.na(xg), !is.na(situation_code))

  cat("  Merged", nrow(merged), "usable PP/PK shot-lineup rows.\n")
  if (nrow(merged) < 300) {
    cat("  Too few usable rows to fit a meaningful model — skipping season", season, ".\n")
    return(NULL)
  }

  merged$strength_state <- derive_strength_state(merged$situation_code)
  merged <- merged %>% filter(!is.na(strength_state))
  state_counts <- table(merged$strength_state)
  cat("  Strength-state breakdown:\n")
  print(state_counts)
  # Drop any strength state with too few shots to estimate its own dummy
  # coefficient reliably (rare double-minor situations especially) —
  # excludes those rows rather than fitting a near-empty column.
  valid_states <- names(state_counts)[state_counts >= 30]
  merged <- merged %>% filter(strength_state %in% valid_states)
  if (nrow(merged) < 300) {
    cat("  Too few rows after strength-state filtering — skipping season", season, ".\n")
    return(NULL)
  }
  merged$strength_state <- factor(merged$strength_state)

  all_players <- unique(c(
    unlist(strsplit(merged$for_players, ";", fixed = TRUE)),
    unlist(strsplit(merged$against_players, ";", fixed = TRUE))
  ))
  all_players <- all_players[!is.na(all_players) & all_players != ""]
  player_index <- setNames(seq_along(all_players), all_players)
  cat("  ", length(all_players), "unique players in this season's PP/PK shot data.\n")

  y <- merged$xg
  strength_dummies <- Matrix(model.matrix(~ strength_state - 1, data = merged), sparse = TRUE)
  n_state_cols <- ncol(strength_dummies)

  cat("  Building PP offense design matrix...\n")
  X_off <- cbind(build_design_matrix(merged, "for", player_index), strength_dummies)
  penalty_vec <- c(rep(1, length(player_index)), rep(0, n_state_cols))
  cat("  Fitting PP offense ridge regression (player coefficients penalized, strength-state not)...\n")
  fit_off <- tryCatch(cv.glmnet(X_off, y, alpha = 0, standardize = FALSE, penalty.factor = penalty_vec),
                       error = function(e) { cat("    PP offense fit failed:", conditionMessage(e), "\n"); NULL })

  cat("  Building PK defense design matrix...\n")
  X_def <- cbind(build_design_matrix(merged, "against", player_index), strength_dummies)
  cat("  Fitting PK defense ridge regression (player coefficients penalized, strength-state not)...\n")
  fit_def <- tryCatch(cv.glmnet(X_def, y, alpha = 0, standardize = FALSE, penalty.factor = penalty_vec),
                       error = function(e) { cat("    PK defense fit failed:", conditionMessage(e), "\n"); NULL })

  if (is.null(fit_off) || is.null(fit_def)) {
    cat("  Could not fit both PP/PK models for season", season, "— skipping.\n")
    return(NULL)
  }

  off_coefs_all <- as.matrix(coef(fit_off, s = "lambda.min"))[-1, 1]
  def_coefs_all <- as.matrix(coef(fit_def, s = "lambda.min"))[-1, 1]
  # Only the PLAYER portion of the coefficients — the strength-state dummy
  # coefficients aren't per-player output, they existed only to absorb
  # that confound out of the player coefficients.
  result <- data.frame(
    player_id = names(player_index),
    season = season,
    pp_rapm_raw = off_coefs_all[names(player_index)],
    pk_rapm_raw = -def_coefs_all[names(player_index)],  # negated — same higher-is-better convention as 5v5
    stringsAsFactors = FALSE
  )

  # Same per-60 derivation as 5v5 (see fit_rapm_for_season), using each
  # player's own on-ice PP-shots-for / PK-shots-against rate rather than a
  # league-average rate.
  if (!is.null(d$onice) && all(c("toi_pp_sec", "toi_pk_sec", "pp_shots", "pk_shots_against") %in% names(d$onice))) {
    onice_lookup <- d$onice %>%
      mutate(
        own_pp_shots_per60 = ifelse(toi_pp_sec > 0, pp_shots / (toi_pp_sec / 3600), NA_real_),
        own_pk_sa_per60    = ifelse(toi_pk_sec > 0, pk_shots_against / (toi_pk_sec / 3600), NA_real_)
      ) %>%
      select(player_id, own_pp_shots_per60, own_pk_sa_per60)
    onice_lookup$player_id <- as.character(onice_lookup$player_id)
    result <- result %>% left_join(onice_lookup, by = "player_id")
    result$pp_rapm_per60 <- result$pp_rapm_raw * result$own_pp_shots_per60
    result$pk_rapm_per60 <- result$pk_rapm_raw * result$own_pk_sa_per60
  } else {
    result$pp_rapm_per60 <- NA_real_
    result$pk_rapm_per60 <- NA_real_
  }

  cat("  Done. Sample coefficients (top 5 by PP offense, per-60):\n")
  print(head(result %>% arrange(desc(pp_rapm_per60)), 5))
  result
}

# ── Driver — fit for each requested season, save each to its own file ───────
SEASONS_TO_FIT <- if (MODE == "current") {
  # Same current-season logic used throughout this codebase (season_sim.R,
  # app.R, onice_stats.R): before September, the upcoming season hasn't
  # started yet and "current" still means the one that just ended; from
  # September on, the new season is underway.
  cy <- as.integer(format(Sys.Date(), "%Y"))
  cm <- as.integer(format(Sys.Date(), "%m"))
  ifelse(cm >= 9, cy + 1L, cy)
} else if (!is.na(SEASON_ARG)) {
  as.integer(SEASON_ARG)
} else {
  2010:2026
}
cat("Mode:", MODE, "| Seasons to fit:", paste(SEASONS_TO_FIT, collapse = ", "), "\n")

USING_SINGLE_SEASON <- MODE == "current" || !is.na(SEASON_ARG)

save_rapm_output <- function(df, season, filename) {
  # data/rapm/{season}/ — matches the same per-season nested structure
  # already used everywhere else (data/onice/{season}/, data/stats/{season}/),
  # so season_sim.R/app.R can read this the same way they already read
  # everything else. dir.create() works fine whether running inside an
  # actual repo checkout or standalone locally — creates the folder either way.
  out_dir <- file.path("data", "rapm", as.character(season))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, filename)
  write.csv(df, out_path, row.names = FALSE)
  out_path
}

all_results <- list()
for (s in SEASONS_TO_FIT) {
  res <- tryCatch(fit_rapm_for_season(s), error = function(e) {
    cat("Season", s, "failed entirely:", conditionMessage(e), "\n"); NULL
  })
  if (!is.null(res)) {
    all_results[[as.character(s)]] <- res
    out_path <- save_rapm_output(res, s, "rapm.csv")
    cat("  Saved to", out_path, "\n")
  }
}

# Combined-across-seasons file: only useful/needed when doing a full
# historical run — in single-season workflow mode this would just
# duplicate that one season's data under a different name, so skip it.
if (!USING_SINGLE_SEASON && length(all_results) > 0) {
  combined <- bind_rows(all_results)
  write.csv(combined, "rapm_all_seasons.csv", row.names = FALSE)
  cat("\nSaved combined 5v5 output to rapm_all_seasons.csv (", nrow(combined), "player-seasons across",
      length(all_results), "seasons).\n")
} else if (!USING_SINGLE_SEASON) {
  cat("\nNo seasons produced usable 5v5 RAPM output — check shot_lineups.csv exists and has data.\n")
}

# ── PP/PK driver — same pattern, separate output files (different columns
# than 5v5's, kept distinct rather than joined to avoid any join-related
# risk; combine downstream if wanted) ───────────────────────────────────────
all_pppk_results <- list()
for (s in SEASONS_TO_FIT) {
  res <- tryCatch(fit_pppk_rapm_for_season(s), error = function(e) {
    cat("PP/PK season", s, "failed entirely:", conditionMessage(e), "\n"); NULL
  })
  if (!is.null(res)) {
    all_pppk_results[[as.character(s)]] <- res
    out_path <- save_rapm_output(res, s, "rapm_pppk.csv")
    cat("  Saved to", out_path, "\n")
  }
}

if (!USING_SINGLE_SEASON && length(all_pppk_results) > 0) {
  combined_pppk <- bind_rows(all_pppk_results)
  write.csv(combined_pppk, "rapm_pppk_all_seasons.csv", row.names = FALSE)
  cat("\nSaved combined PP/PK output to rapm_pppk_all_seasons.csv (", nrow(combined_pppk), "player-seasons across",
      length(all_pppk_results), "seasons).\n")
} else if (!USING_SINGLE_SEASON) {
  cat("\nNo seasons produced usable PP/PK RAPM output.\n")
} else {
  cat("\nNo seasons produced usable PP/PK RAPM output.\n")
}
