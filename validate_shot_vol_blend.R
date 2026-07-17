# ==============================================================================
# validate_shot_vol_blend.R
# ==============================================================================
# fit_shot_volume_rapm.R's own header already flagged the risk this
# validates directly: "shot volume and xG are correlated by construction...
# combining this output with xG-RAPM's output at the team level without
# checking that correlation first risks double-counting the same
# underlying skill twice." Confirmed as a real, live problem tonight —
# adding both signals at full, unadjusted (weight=1) strength blew the
# league's xG range out from a realistic ~1.6-2.7 to an impossible 0.5-3.8,
# with some teams projecting 150+ point seasons.
#
# Rather than guess at a dampening factor, this fits the actual, correct
# blend empirically: for every team, in every season, computing BOTH their
# roster-summed rapm_xgf/xga_delta (shot QUALITY) and shot_vol_off/def_delta
# (shot VOLUME) — using their REAL, actual roster and ice time that season,
# same method as every other roster-reconstruction check tonight — then
# regressing their REAL, measured team_onice.csv xG against both signals
# together. The fitted coefficients ARE the empirically-correct weights;
# comparing R² with vs without shot-volume tells us whether it adds
# genuine, new information or is mostly redundant with quality RAPM.

suppressMessages({ library(dplyr); library(httr) })

GH_ONICE <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/onice"
GH_RAPM <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/rapm"
GH_SHOTVOL <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/shot_volume_rapm"
SEASONS <- c(2021, 2022, 2023, 2024, 2025, 2026)

gh_read <- function(url, timeout_s = 90, max_retries = 3) {
  for (attempt in seq_len(max_retries)) {
    resp <- tryCatch(GET(url, timeout(timeout_s)), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200) {
      return(tryCatch(read.csv(text = content(resp, "text", encoding = "UTF-8"), stringsAsFactors = FALSE),
                      error = function(e) NULL))
    }
    if (!is.null(resp) && status_code(resp) == 404) return(NULL)
    if (attempt < max_retries) Sys.sleep(2 * attempt)
  }
  NULL
}

compute_team_season_row <- function(team, season, skater_onice, team_onice, rapm, shot_vol) {
  team_row <- team_onice %>% filter(team_abbrev == team)
  if (nrow(team_row) == 0 || is.na(team_row$gp_onice[1]) || team_row$gp_onice[1] <= 0) return(NULL)
  
  roster <- skater_onice %>%
    filter(team_abbrev == team, coalesce(toi_5v5_sec, 0) > 0) %>%
    left_join(rapm %>% select(player_id, off_rapm_per60, def_rapm_per60), by = "player_id") %>%
    left_join(shot_vol %>% select(player_id, shot_vol_off_per60, shot_vol_def_per60), by = "player_id")
  if (nrow(roster) == 0) return(NULL)
  
  roster <- roster %>% mutate(toi_pg_min = (toi_5v5_sec / team_row$gp_onice[1]) / 60)
  
  data.frame(
    team = team, season = season,
    rapm_xgf_delta = sum(coalesce(roster$off_rapm_per60, 0) * roster$toi_pg_min / 60, na.rm = TRUE),
    rapm_xga_delta = sum(coalesce(roster$def_rapm_per60, 0) * roster$toi_pg_min / 60, na.rm = TRUE),
    shot_vol_off_delta = sum(coalesce(roster$shot_vol_off_per60, 0) * roster$toi_pg_min / 60, na.rm = TRUE),
    shot_vol_def_delta = sum(coalesce(roster$shot_vol_def_per60, 0) * roster$toi_pg_min / 60, na.rm = TRUE),
    real_xgf_pg = team_row$xg_for[1] / team_row$gp_onice[1],
    real_xga_pg = team_row$xg_against[1] / team_row$gp_onice[1],
    stringsAsFactors = FALSE
  )
}

all_rows <- list()
for (s in SEASONS) {
  cat("=== Loading season", s, "===\n")
  skater_onice <- gh_read(paste0(GH_ONICE, "/", s, "/skater_onice.csv"))
  team_onice   <- gh_read(paste0(GH_ONICE, "/", s, "/team_onice.csv"))
  rapm         <- gh_read(paste0(GH_RAPM, "/", s, "/rapm.csv"))
  shot_vol     <- gh_read(paste0(GH_SHOTVOL, "/", s, "/shot_volume_rapm.csv"))
  if (is.null(shot_vol)) shot_vol <- gh_read(paste0(GH_SHOTVOL, "/", s, "/shot_volume_rapm_current.csv"))
  if (is.null(skater_onice) || is.null(team_onice) || is.null(rapm) || is.null(shot_vol)) {
    cat("  Missing a required file for season", s, "— skipping.\n"); next
  }
  skater_onice$player_id <- as.character(skater_onice$player_id)
  rapm$player_id <- as.character(rapm$player_id)
  shot_vol$player_id <- as.character(shot_vol$player_id)
  
  teams_this_season <- unique(team_onice$team_abbrev)
  cat("  ", length(teams_this_season), "teams found.\n")
  for (tm in teams_this_season) {
    row <- tryCatch(compute_team_season_row(tm, s, skater_onice, team_onice, rapm, shot_vol), error = function(e) NULL)
    if (!is.null(row)) all_rows[[paste(tm, s)]] <- row
  }
}

validation_df <- bind_rows(all_rows)
cat("\n=== Combined dataset:", nrow(validation_df), "team-seasons across", length(SEASONS), "seasons ===\n\n")

# ── OFFENSE: does shot volume add real, incremental predictive power? ──────
cat("── OFFENSE MODEL ──\n")
fit_quality_only_off <- lm(real_xgf_pg ~ rapm_xgf_delta, data = validation_df)
fit_blend_off <- lm(real_xgf_pg ~ rapm_xgf_delta + shot_vol_off_delta, data = validation_df)
cat("Quality-only R-squared:", round(summary(fit_quality_only_off)$r.squared, 4), "\n")
cat("Quality+Volume blend R-squared:", round(summary(fit_blend_off)$r.squared, 4), "\n")
cat("Blend model coefficients:\n")
print(round(coef(fit_blend_off), 4))
cat("\n(If the blend's R-squared is meaningfully higher than quality-only, shot volume adds\n")
cat(" real information. The coefficients ARE the correct weights to use — NOT 1.0 for both,\n")
cat(" which is what tonight's broken fix assumed.)\n\n")

# ── DEFENSE: same check ─────────────────────────────────────────────────────
cat("── DEFENSE MODEL ──\n")
fit_quality_only_def <- lm(real_xga_pg ~ rapm_xga_delta, data = validation_df)
fit_blend_def <- lm(real_xga_pg ~ rapm_xga_delta + shot_vol_def_delta, data = validation_df)
cat("Quality-only R-squared:", round(summary(fit_quality_only_def)$r.squared, 4), "\n")
cat("Quality+Volume blend R-squared:", round(summary(fit_blend_def)$r.squared, 4), "\n")
cat("Blend model coefficients:\n")
print(round(coef(fit_blend_def), 4))
cat("\n(Coefficients on rapm_xga_delta/shot_vol_def_delta should come out NEGATIVE here —\n")
cat(" higher values of both mean BETTER defense, i.e. LESS real xG allowed.)\n\n")

# ── Direct spot-check: what would CAR's real 2025-26 season look like under
# the EMPIRICALLY FITTED blend, instead of tonight's broken weight=1 version? ──
car_row <- validation_df %>% filter(team == "CAR", season == 2026)
if (nrow(car_row) == 1) {
  cat("── CAR 2025-26 spot-check ──\n")
  cat("Real, measured xgf_pg:", round(car_row$real_xgf_pg, 3), "| real xga_pg:", round(car_row$real_xga_pg, 3), "\n")
  predicted_xgf <- predict(fit_blend_off, newdata = car_row)
  predicted_xga <- predict(fit_blend_def, newdata = car_row)
  cat("Blend-predicted xgf_pg:", round(predicted_xgf, 3), "| blend-predicted xga_pg:", round(predicted_xga, 3), "\n")
  cat("(Compare against tonight's broken, weight=1 result: xgf=3.756, xga=0.531 — if the\n")
  cat(" properly-fitted blend lands much closer to the REAL values above, that confirms\n")
  cat(" this is the right way to combine the two signals.)\n")
} else {
  cat("CAR/2026 row not found in the validation dataset — check season list/team abbrev.\n")
}

write.csv(validation_df, "shot_vol_validation_dataset.csv", row.names = FALSE)
cat("\nSaved full validation dataset to shot_vol_validation_dataset.csv (for further inspection).\n")