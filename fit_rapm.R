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

# ── Team-effect redistribution — TOI-weighted, per-player lineup stability ──
# Rather than leave the team-effect term entirely separate from player
# output (which would mean every downstream consumer — season_sim.R AND
# app.R's Player Cards — has to separately fetch and add it back in), or
# split it evenly across the whole roster (which would wrongly credit
# low-usage, frequently-rotated players for a static top line's success),
# this distributes each team's effect coefficient across ITS OWN players,
# weighted by how STATIC each individual player's own deployment was, not
# just by ice time alone.
#
# For a given player, "stability" is measured the same way as the team-wide
# Herfindahl check that first confirmed this problem — but scoped to just
# this ONE player: across every shift they were on the ice for, how
# concentrated was the specific set of other teammates they shared it
# with? A player who's almost always paired with the same teammates gets
# a HIGH stability score — exactly the case where the regression had the
# least ability to isolate their own individual signal, and where a share
# of the team effect legitimately belongs. A player who rotated through
# many different combinations gets a LOW score, since RAPM already had
# enough variation to identify their own contribution without the team
# term's help — they don't need (and shouldn't receive) a slice of it.
#
# Weighted by TOI as well as stability, so a low-minute player who
# happened to always share the ice with the same teammates (purely
# because they had so few shifts to begin with) doesn't get an outsized
# share off a tiny, noisy sample.
#
# HONEST LIMITATION, stated plainly rather than hidden: this is still an
# approximation. It doesn't resolve which SPECIFIC player within a static
# group deserves credit (nothing can, given the underlying data) — it
# distributes proportional to deployment pattern, not proportional to
# some independently-verified measure of individual skill. A player's
# resulting share still travels with them if traded, which assumes that
# share was genuinely portable individual skill rather than chemistry
# specific to the exact teammates they're leaving behind.
redistribute_team_effect <- function(lineup_df, team_abbrev, side, team_effect_value, toi_lookup) {
  # side: "for" (offense-side redistribution) or "against" (defense-side)
  team_col <- if (side == "for") "for_team_abbrev" else "against_team_abbrev"
  players_col <- if (side == "for") "for_players" else "against_players"
  team_rows <- lineup_df[lineup_df[[team_col]] == team_abbrev & !is.na(lineup_df[[team_col]]), ]
  if (nrow(team_rows) == 0 || is.na(team_effect_value) || team_effect_value == 0) return(NULL)

  # NOTE: does NOT require exactly 5 players per row. Confirmed via direct
  # debug trace (Carolina, 2026 season) that for_players/against_players
  # consistently list 6 real skaters, not 5 — verified all 6 IDs are
  # genuine skaters (present in skater_onice.csv), ruling out a goalie
  # explanation. Most likely a shift-overlap artifact from line changes in
  # the underlying shift-chart data, upstream of this file. Requiring
  # EXACTLY 5 (an earlier version of this function did) silently rejected
  # effectively 100% of real shot events for at least one team/season —
  # build_design_matrix(), the actual RAPM-fitting code, never made this
  # assumption in the first place. Rejecting only an implausible count
  # (fewer than 3, or more than 8) here instead, which almost certainly
  # indicates genuinely broken data, not a normal line-change artifact.
  player_lists <- strsplit(team_rows[[players_col]], ";", fixed = TRUE)
  n_per_row <- lengths(player_lists)
  player_lists <- player_lists[n_per_row >= 3 & n_per_row <= 8]
  if (length(player_lists) == 0) return(NULL)

  # PERFORMANCE FIX: the original version built ONE data.frame() per
  # player per shot event (potentially tens of thousands of calls per
  # team/side) — data.frame() construction overhead, not the actual
  # string work, was the real bottleneck, making a full historical
  # backfill take an impractically long time. This builds plain
  # character-vector lists across all rows first (cheap, no data.frame
  # overhead per iteration) and constructs exactly ONE data.frame() at
  # the very end. Same result, dramatically faster.
  player_id_list <- vector("list", length(player_lists))
  others_key_list <- vector("list", length(player_lists))
  for (i in seq_along(player_lists)) {
    plist <- sort(player_lists[[i]])
    np <- length(plist)
    player_id_list[[i]] <- plist
    others_key_list[[i]] <- if (np <= 1) rep("", np) else vapply(seq_len(np), function(j) paste(plist[-j], collapse = ";"), character(1))
  }
  psd <- data.frame(
    player_id = unlist(player_id_list, use.names = FALSE),
    others_key = unlist(others_key_list, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  if (nrow(psd) == 0) return(NULL)

  stability <- psd %>%
    group_by(player_id) %>%
    summarise(
      n_shifts = dplyr::n(),
      hhi = { tab <- table(others_key); sum((as.numeric(tab) / sum(tab))^2) },
      .groups = "drop"
    )
  stability$player_id <- as.character(stability$player_id)
  stability <- stability %>% left_join(toi_lookup, by = "player_id") %>%
    mutate(toi_5v5_sec = coalesce(toi_5v5_sec, 0))

  stability$raw_weight <- stability$hhi * stability$toi_5v5_sec
  total_weight <- sum(stability$raw_weight, na.rm = TRUE)
  if (total_weight <= 0) return(NULL)  # no usable stability signal — leave the team effect undistributed for this team/side rather than guess
  stability$player_share <- (stability$raw_weight / total_weight) * team_effect_value
  stability %>% select(player_id, player_share)
}

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
    filter(situation_label == "5v5", !is.na(xg), !is.na(for_team_abbrev), !is.na(against_team_abbrev))

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

  # ── JOINT offense+defense model — REPLACES the previous two separate,
  # opponent-blind regressions. Each shot event now gets TWO sets of
  # columns: the shooting team's 5 players lit up in their own "OFF_"
  # columns, and the defending team's 5 players lit up in their own "DEF_"
  # columns — fit together, in one regression, rather than two blind ones.
  #
  # WHY THIS CHANGED: the previous design (shooting team +1, defending
  # team ZEROED OUT for the offense model, and the mirror image for
  # defense) meant NEITHER model ever saw who else was on the ice on the
  # opposing side. A player's defense coefficient had no way to
  # distinguish "this player got worse" from "this player faced tougher
  # competition" — both would show up identically as more xG allowed.
  # Confirmed as a real, live problem: a well-regarded, heavily-deployed
  # top-pairing defenseman (K'Andre Miller, CAR, 2025-26) showed an
  # extreme, implausible defense value under the old design — the
  # cleanest explanation is that his tough-competition matchup duty
  # (something coaches choose FOR him, not a reflection of his own
  # declining skill) was getting misattributed entirely onto his own
  # coefficient with no way for the model to route any of that credit
  # toward the players who were actually shooting on him.
  #
  # This design still produces genuinely separate offense/defense numbers
  # per player (the original goal that motivated two models in the first
  # place) — it just estimates both simultaneously, in a single model,
  # so each can properly control for the other rather than being blind to
  # the opposing team entirely.
  #
  # ── TEAM-EFFECT TERM — a team that runs very static, repeated 5-man
  # combinations (little variation in who plays with whom) gives RAPM's
  # regression very little to work with when trying to isolate INDIVIDUAL
  # credit — ridge's response to that kind of collinearity is to shrink
  # all of those correlated players toward zero together, even when the
  # group's real, measured output was excellent. Confirmed directly, not
  # assumed: Carolina's real 2025-26 team xG (from team_onice.csv, no
  # shift/lineup dependency at all) was +0.573/game, 2nd-best in the
  # league — but summing RAPM across their real roster produced a
  # strongly negative result, and Carolina independently ranked 2nd of 32
  # teams in lineup-combination concentration (Herfindahl index), nearly
  # double the league median.
  #
  # FIX: one additional column per team on each side (shooting-team-effect,
  # defending-team-effect), PENALIZED THE SAME as the player coefficients
  # (same ridge lambda, same penalty group — not left unpenalized like the
  # PP/PK strength-state dummies elsewhere in this file, which exist for a
  # different reason). This matters: ridge's L2 penalty makes it cheaper to
  # explain a signal shared by an entire, static lineup group through ONE
  # team coefficient than by forcing that same signal onto ~18 individual,
  # highly-correlated player coefficients — so this naturally absorbs
  # exactly the "team system/structure" effect a static-lines team produces,
  # leaving player coefficients to reflect only each player's OWN,
  # above-team-baseline contribution. A team with normal lineup variation
  # (most of the league) should see this term shrink close to zero, since
  # there's nothing shared left for it to usefully explain once individual
  # players are already accounting for the variation.
  cat("  Building joint offense+defense design matrix (both teams' players, every shot)...\n")
  X_off_side <- build_design_matrix(merged, "for", player_index)
  X_def_side <- build_design_matrix(merged, "against", player_index)
  colnames(X_off_side) <- paste0("OFF_", colnames(X_off_side))
  colnames(X_def_side) <- paste0("DEF_", colnames(X_def_side))
  team_off_dummies <- Matrix(model.matrix(~ for_team_abbrev - 1, data = merged), sparse = TRUE)
  team_def_dummies <- Matrix(model.matrix(~ against_team_abbrev - 1, data = merged), sparse = TRUE)
  colnames(team_off_dummies) <- paste0("TEAMOFF_", sub("^for_team_abbrev", "", colnames(team_off_dummies)))
  colnames(team_def_dummies) <- paste0("TEAMDEF_", sub("^against_team_abbrev", "", colnames(team_def_dummies)))
  X_joint <- cbind(X_off_side, X_def_side, team_off_dummies, team_def_dummies)
  cat("  Fitting joint ridge regression (cv.glmnet, alpha=0,", ncol(X_joint), "columns —",
      length(player_index), "players x 2 sides + team-effect terms, all equally penalized)...\n")
  fit_joint <- tryCatch(cv.glmnet(X_joint, y, alpha = 0, standardize = FALSE), error = function(e) {
    cat("    Joint model fit failed:", conditionMessage(e), "\n"); NULL
  })

  if (is.null(fit_joint)) {
    cat("  Could not fit the joint model for season", season, "— skipping.\n")
    return(NULL)
  }

  joint_coefs <- as.matrix(coef(fit_joint, s = "lambda.min"))[-1, 1]  # drop intercept
  off_coefs <- joint_coefs[paste0("OFF_", names(player_index))]
  def_coefs <- joint_coefs[paste0("DEF_", names(player_index))]

  # Team-effect coefficients — separate, team-level granularity (not per-
  # player), saved to its own file below rather than joined into `result`.
  team_names_off <- sub("^TEAMOFF_", "", grep("^TEAMOFF_", names(joint_coefs), value = TRUE))
  team_off_effect_raw <- setNames(joint_coefs[paste0("TEAMOFF_", team_names_off)], team_names_off)
  team_def_effect_raw <- setNames(-joint_coefs[paste0("TEAMDEF_", team_names_off)], team_names_off)  # negated, same higher-is-better convention
  team_effects_result <- data.frame(
    team_abbrev = team_names_off, season = season,
    team_off_effect_raw = team_off_effect_raw[team_names_off],
    team_def_effect_raw = team_def_effect_raw[team_names_off],
    stringsAsFactors = FALSE
  ) %>% arrange(desc(team_off_effect_raw + team_def_effect_raw))
  cat("  Team-effect coefficients (all teams, sorted best to worst combined):\n")
  print(team_effects_result %>% mutate(across(where(is.numeric), ~round(.x, 5))))
  team_effects_out_path <- save_rapm_output(team_effects_result, season, "team_effects.csv")
  cat("  Saved team-effect output to", team_effects_out_path, "\n")

  # ── Redistribute each team's effect across its own players, weighted by
  # each player's own TOI-weighted lineup stability. See
  # redistribute_team_effect()'s own header for the full reasoning. This
  # means EVERY consumer of rapm.csv (season_sim.R AND app.R's Player
  # Cards) gets the fix automatically — the team's system effect is
  # already folded into each player's own coefficient, proportional to
  # how much that specific player's own signal was entangled with a
  # static deployment.
  cat("  Redistributing team effects across players (TOI-weighted lineup stability)...\n")
  toi_lookup_for_redist <- if (!is.null(d$onice) && "toi_5v5_sec" %in% names(d$onice)) {
    d$onice %>% mutate(player_id = as.character(player_id)) %>% select(player_id, toi_5v5_sec)
  } else data.frame(player_id = character(0), toi_5v5_sec = numeric(0))

  off_shares_all <- list(); def_shares_all <- list()
  for (tm in team_names_off) {
    off_share <- redistribute_team_effect(merged, tm, "for", team_off_effect_raw[tm], toi_lookup_for_redist)
    def_share <- redistribute_team_effect(merged, tm, "against", team_def_effect_raw[tm], toi_lookup_for_redist)
    if (!is.null(off_share)) off_shares_all[[tm]] <- off_share
    if (!is.null(def_share)) def_shares_all[[tm]] <- def_share
  }
  off_shares_combined <- if (length(off_shares_all) > 0) bind_rows(off_shares_all) %>% rename(off_share = player_share) else data.frame(player_id = character(0), off_share = numeric(0))
  def_shares_combined <- if (length(def_shares_all) > 0) bind_rows(def_shares_all) %>% rename(def_share = player_share) else data.frame(player_id = character(0), def_share = numeric(0))

  off_coefs_df <- data.frame(player_id = names(off_coefs), off_rapm_raw = as.numeric(off_coefs), stringsAsFactors = FALSE) %>%
    left_join(off_shares_combined, by = "player_id") %>%
    mutate(off_rapm_raw = off_rapm_raw + coalesce(off_share, 0))
  # NOTE the SUBTRACTION here, not addition — def_coefs at this point is
  # still in the RAW, higher-is-worse convention (it only gets negated
  # once, below, in result's own construction), while def_share (from
  # team_def_effect_raw, passed into redistribute_team_effect() above) is
  # already in the higher-is-better convention. Subtracting here means
  # the eventual, single negation below produces the correct, final sum:
  # -(def_coefs_raw - def_share) = -def_coefs_raw + def_share, i.e. the
  # player's own (correctly-signed) coefficient PLUS their higher-is-
  # better team share, added the way it should be. Adding instead of
  # subtracting here would have silently flipped the sign of every
  # player's own share of the team effect.
  def_coefs_df <- data.frame(player_id = names(def_coefs), def_rapm_raw_pre = as.numeric(def_coefs), stringsAsFactors = FALSE) %>%
    left_join(def_shares_combined, by = "player_id") %>%
    mutate(def_rapm_raw_pre = def_rapm_raw_pre - coalesce(def_share, 0))
  cat("  Redistribution complete —", sum(!is.na(off_shares_combined$off_share[match(names(off_coefs), off_shares_combined$player_id)])),
      "players received an offense-side share,",
      nrow(def_shares_combined), "players received a defense-side share.\n")

  off_coefs <- setNames(off_coefs_df$off_rapm_raw, off_coefs_df$player_id)[names(off_coefs)]
  def_coefs <- setNames(def_coefs_df$def_rapm_raw_pre, def_coefs_df$player_id)[names(def_coefs)]

  result <- data.frame(
    player_id = names(player_index),
    season = season,
    off_rapm_raw = off_coefs,
    # Negated — higher coefficient in the raw defense model means MORE xG
    # allowed (worse defense), so this flips it to match every other
    # category's higher-is-better convention.
    def_rapm_raw = -def_coefs,
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
  # No winsorization needed here anymore — the joint model above fixes
  # the actual root cause (competition quality getting misattributed onto
  # the defender/attacker with no outlet), rather than clipping the
  # resulting symptom after the fact.
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
    filter(situation_label %in% c("home_pp", "away_pp"), !is.na(xg), !is.na(situation_code),
           !is.na(for_team_abbrev), !is.na(against_team_abbrev))

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

  # ── JOINT PP+PK model — same fix and reasoning as 5v5's identical
  # redesign (see fit_rapm_for_season() for the full explanation): both
  # sides' players now enter a single regression together, so a PK
  # defender's coefficient can properly account for facing a tougher-
  # than-average power play, rather than misattributing all of that
  # difficulty onto the defender alone. Strength-state dummies (5v4/5v3/
  # 4v3) stay unpenalized, same as before, appended once to the combined
  # matrix rather than duplicated per side.
  cat("  Building joint PP+PK design matrix (both teams' players, every shot)...\n")
  X_off_side <- build_design_matrix(merged, "for", player_index)
  X_def_side <- build_design_matrix(merged, "against", player_index)
  colnames(X_off_side) <- paste0("OFF_", colnames(X_off_side))
  colnames(X_def_side) <- paste0("DEF_", colnames(X_def_side))
  # Team-effect terms — same fix and reasoning as 5v5's identical addition
  # (see fit_rapm_for_season() for the full explanation and the direct,
  # measured confirmation via Carolina's lineup-concentration numbers).
  # Penalized the SAME as player coefficients (part of the penalized
  # group, penalty=1) — kept separate from strength_dummies, which stay
  # unpenalized (penalty=0) for their own, different reason.
  team_off_dummies <- Matrix(model.matrix(~ for_team_abbrev - 1, data = merged), sparse = TRUE)
  team_def_dummies <- Matrix(model.matrix(~ against_team_abbrev - 1, data = merged), sparse = TRUE)
  colnames(team_off_dummies) <- paste0("TEAMOFF_", sub("^for_team_abbrev", "", colnames(team_off_dummies)))
  colnames(team_def_dummies) <- paste0("TEAMDEF_", sub("^against_team_abbrev", "", colnames(team_def_dummies)))
  n_team_cols <- ncol(team_off_dummies) + ncol(team_def_dummies)
  X_joint <- cbind(X_off_side, X_def_side, team_off_dummies, team_def_dummies, strength_dummies)
  penalty_vec <- c(rep(1, 2 * length(player_index)), rep(1, n_team_cols), rep(0, n_state_cols))
  cat("  Fitting joint PP+PK ridge regression (player + team-effect coefficients penalized, strength-state not,",
      ncol(X_joint), "columns)...\n")
  fit_joint <- tryCatch(cv.glmnet(X_joint, y, alpha = 0, standardize = FALSE, penalty.factor = penalty_vec),
                        error = function(e) { cat("    Joint PP/PK fit failed:", conditionMessage(e), "\n"); NULL })

  if (is.null(fit_joint)) {
    cat("  Could not fit the joint PP/PK model for season", season, "— skipping.\n")
    return(NULL)
  }

  joint_coefs_all <- as.matrix(coef(fit_joint, s = "lambda.min"))[-1, 1]
  # Team-effect coefficients — saved separately, same as 5v5's version.
  team_names_pppk <- sub("^TEAMOFF_", "", grep("^TEAMOFF_", names(joint_coefs_all), value = TRUE))
  team_pp_effect_raw <- setNames(joint_coefs_all[paste0("TEAMOFF_", team_names_pppk)], team_names_pppk)
  team_pk_effect_raw <- setNames(-joint_coefs_all[paste0("TEAMDEF_", team_names_pppk)], team_names_pppk)
  team_effects_pppk_result <- data.frame(
    team_abbrev = team_names_pppk, season = season,
    team_pp_effect_raw = team_pp_effect_raw[team_names_pppk],
    team_pk_effect_raw = team_pk_effect_raw[team_names_pppk],
    stringsAsFactors = FALSE
  ) %>% arrange(desc(team_pp_effect_raw + team_pk_effect_raw))
  team_effects_pppk_out_path <- save_rapm_output(team_effects_pppk_result, season, "team_effects_pppk.csv")
  cat("  Saved PP/PK team-effect output to", team_effects_pppk_out_path, "\n")

  # Same redistribution as 5v5 (see redistribute_team_effect()'s header) —
  # PP needs no sign correction (neither side is negated for PP), PK needs
  # the same subtract-before-negation handling as 5v5's defense side.
  cat("  Redistributing PP/PK team effects across players...\n")
  toi_lookup_pppk <- if (!is.null(d$onice) && all(c("toi_pp_sec", "toi_pk_sec") %in% names(d$onice))) {
    d$onice %>% mutate(player_id = as.character(player_id), toi_5v5_sec = coalesce(toi_pp_sec, 0) + coalesce(toi_pk_sec, 0)) %>%
      select(player_id, toi_5v5_sec)  # reusing the same column name redistribute_team_effect() expects
  } else data.frame(player_id = character(0), toi_5v5_sec = numeric(0))

  pp_shares_all <- list(); pk_shares_all <- list()
  for (tm in team_names_pppk) {
    pp_share <- redistribute_team_effect(merged, tm, "for", team_pp_effect_raw[tm], toi_lookup_pppk)
    pk_share <- redistribute_team_effect(merged, tm, "against", team_pk_effect_raw[tm], toi_lookup_pppk)
    if (!is.null(pp_share)) pp_shares_all[[tm]] <- pp_share
    if (!is.null(pk_share)) pk_shares_all[[tm]] <- pk_share
  }
  pp_shares_combined <- if (length(pp_shares_all) > 0) bind_rows(pp_shares_all) %>% rename(pp_share = player_share) else data.frame(player_id = character(0), pp_share = numeric(0))
  pk_shares_combined <- if (length(pk_shares_all) > 0) bind_rows(pk_shares_all) %>% rename(pk_share = player_share) else data.frame(player_id = character(0), pk_share = numeric(0))

  pp_coefs_raw <- joint_coefs_all[paste0("OFF_", names(player_index))]
  pk_coefs_raw <- joint_coefs_all[paste0("DEF_", names(player_index))]  # still un-negated here, matching 5v5's def_coefs convention at this same stage
  pp_coefs_df <- data.frame(player_id = names(pp_coefs_raw), pp_rapm_raw = as.numeric(pp_coefs_raw), stringsAsFactors = FALSE) %>%
    left_join(pp_shares_combined, by = "player_id") %>%
    mutate(pp_rapm_raw = pp_rapm_raw + coalesce(pp_share, 0))
  pk_coefs_df <- data.frame(player_id = names(pk_coefs_raw), pk_rapm_raw_pre = as.numeric(pk_coefs_raw), stringsAsFactors = FALSE) %>%
    left_join(pk_shares_combined, by = "player_id") %>%
    mutate(pk_rapm_raw_pre = pk_rapm_raw_pre - coalesce(pk_share, 0))  # subtract, same reasoning as 5v5's def side
  pp_coefs_final <- setNames(pp_coefs_df$pp_rapm_raw, pp_coefs_df$player_id)[names(pp_coefs_raw)]
  pk_coefs_final <- setNames(pk_coefs_df$pk_rapm_raw_pre, pk_coefs_df$player_id)[names(pk_coefs_raw)]

  # Only the PLAYER portion of the coefficients — the strength-state dummy
  # coefficients aren't per-player output, they existed only to absorb
  # that confound out of the player coefficients.
  result <- data.frame(
    player_id = names(player_index),
    season = season,
    pp_rapm_raw = pp_coefs_final,
    pk_rapm_raw = -pk_coefs_final,  # negated — same higher-is-better convention as 5v5
    stringsAsFactors = FALSE
  )


  # Same per-60 derivation as 5v5 (see fit_rapm_for_season), using each
  # player's own on-ice PP-shots-for / PK-shots-against rate rather than a
  # league-average rate. No winsorization needed here either, for the
  # same reason as 5v5 — the joint model above addresses the actual root
  # cause directly.
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
