# train_xg_model.R
#
# Trains an expected-goals (xG) model from historical shot-level data
# (shots_raw.csv, backfilled per season by onice_stats.R). Run manually or
# via GitHub Actions — NOT on a daily schedule. Unlike season_sim.R (which
# needs fresh numbers every day), the shot-to-goal relationship doesn't
# meaningfully drift day to day, so there's no reason to retrain constantly.
#
# Output: a saved model file (data/xg_model/xg_model.rds) that the live app
# and season_sim.R can load and score new shots against — this script only
# TRAINS, it never runs inside the user-facing app itself.

suppressPackageStartupMessages({
  library(dplyr)
  library(xgboost)
})

# ── Config ───────────────────────────────────────────────────────────────
# Edit this list as more seasons get backfilled. Starting with ~5-6 recent
# seasons balances having enough volume to fit a stable model against the
# risk of pulling in older, less consistent shot-tracking data or outdated
# scoring-rate context from a very different NHL era.
SEASONS <- c(2021, 2022, 2023, 2024, 2025)

OUT_DIR      <- file.path("data", "xg_model")
MODEL_OUT    <- file.path(OUT_DIR, "xg_model.rds")
METRICS_OUT  <- file.path(OUT_DIR, "xg_model_metrics.txt")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

gh_read <- function(url) tryCatch(read.csv(url(url), stringsAsFactors = FALSE), error = function(e) NULL)

# ── 1. Load all seasons' raw shot data ──────────────────────────────────
cat("Loading shots_raw.csv for seasons:", paste(SEASONS, collapse = ", "), "\n")
pieces <- lapply(SEASONS, function(s) {
  d <- gh_read(paste0("https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/onice/", s, "/shots_raw.csv"))
  if (is.null(d) || nrow(d) == 0) { cat("  ", s, ": no data found\n"); return(NULL) }
  d$season <- s
  cat("  ", s, ":", nrow(d), "shot events\n")
  d
})
shots <- bind_rows(Filter(Negate(is.null), pieces))
if (nrow(shots) == 0) stop("No shot data found for any requested season — check that shots_raw.csv has actually been backfilled for at least one season in SEASONS.")
cat("Total shot events loaded:", nrow(shots), "\n\n")

# ── 2. Filter to usable events ───────────────────────────────────────────
# Blocked shots are excluded — they never reach the goalie, so "did it
# become a goal" isn't a meaningful outcome for them the way it is for
# shots-on-goal/missed-shots/goals, which is consistent with how public xG
# models (e.g. MoneyPuck) typically scope their training data.
#
# Empty-net shots are ALSO excluded — goalie_id is NA exactly when there's
# no goalie in net, and an empty net is essentially guaranteed to score
# regardless of distance/angle. Left in, this would flatten (or invert) the
# expected distance-vs-goal-rate relationship for far-away shots and
# inflate conversion rates for whatever situational bucket empty-net shots
# happen to fall into — which is exactly what the first training run's
# sanity checks showed (the two farthest distance buckets came back nearly
# identical instead of continuing to drop, and "other" situations converted
# unusually high). This isn't measuring shot quality, it's measuring
# "no goalie," so it doesn't belong in an xG model the same way.
before_n <- nrow(shots)
shots <- shots %>%
  filter(event_type %in% c("shot-on-goal", "missed-shot", "goal")) %>%
  filter(!is.na(x_coord), !is.na(y_coord), !is.na(home_defending_side), !is.na(home_id)) %>%
  filter(!is.na(goalie_id))
cat("After filtering to shot-on-goal/missed/goal events with valid coordinates and a goalie in net:", nrow(shots), "of", before_n, "\n\n")
if (nrow(shots) < 5000) {
  cat("WARNING: fewer than 5,000 usable shot events — this is not enough volume to train a stable model.\n")
  cat("Back off and backfill more seasons before trusting anything this script produces.\n")
}

# ── 3. Feature engineering ───────────────────────────────────────────────
# Coordinate normalization: NHL rinks have the away net at one end and home
# net at the other, and teams switch ends every period, so raw x_coord/
# y_coord alone can't tell you distance/angle to the TARGET net without
# knowing which way the home team is defending that period.
#   - home_defending_side = the side where the home team's OWN net sits.
#   - A team's ATTACKING (target) net is on the OPPOSITE side from
#     whichever net that team defends.
shots <- shots %>%
  mutate(
    is_home_shooter = (owner_team_id == home_id),
    target_side = case_when(
      is_home_shooter  & home_defending_side == "left"  ~ "right",
      is_home_shooter  & home_defending_side == "right" ~ "left",
      !is_home_shooter & home_defending_side == "left"  ~ "left",
      !is_home_shooter & home_defending_side == "right" ~ "right",
      TRUE ~ NA_character_
    ),
    # Flip so every shot is normalized as if attacking the net at x=+89, y=0.
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
  filter(!is.na(target_side), !is.na(dist_to_net), dist_to_net < 200)  # drop rows with obviously bad coordinates

# Rebound detection: a shot attempt within 3 seconds of the SAME team's
# previous shot attempt in the same game. Rebounds convert at a much
# higher rate than the average shot — a well-established, real xG feature.
shots <- shots %>%
  arrange(game_id, t_abs) %>%
  group_by(game_id, owner_team_id) %>%
  mutate(time_since_own_last_shot = t_abs - lag(t_abs),
         is_rebound = !is.na(time_since_own_last_shot) & time_since_own_last_shot <= 3) %>%
  ungroup()

# ── Sanity checks — DO NOT SKIP THESE ────────────────────────────────────
# If the coordinate normalization above is flipped or otherwise wrong, the
# model will still "train" without erroring, it'll just be quietly
# meaningless — exactly the kind of failure mode that's bitten this project
# before (units/scope mismatches that produced a number, just the wrong
# one). Check these prints BEFORE trusting anything downstream:
cat("\n── Sanity checks (verify these look like real hockey before trusting anything below) ──\n")
cat("  Median distance to net:", round(median(shots$dist_to_net, na.rm = TRUE), 1), "ft (expect roughly 25-35)\n")
cat("  Goal rate by distance bucket (MUST decrease as distance increases):\n")
print(shots %>% mutate(dist_bucket = cut(dist_to_net, breaks = c(0,10,20,30,40,60,200))) %>%
  group_by(dist_bucket) %>% summarise(n = dplyr::n(), goal_rate = round(mean(is_goal), 4), .groups = "drop"))
cat("  Goal rate, rebound vs not (rebounds MUST convert higher):\n")
print(shots %>% group_by(is_rebound) %>% summarise(n = dplyr::n(), goal_rate = round(mean(is_goal), 4), .groups = "drop"))
cat("  Goal rate, PP vs even vs PK (PP MUST convert higher, PK lower):\n")
print(shots %>% group_by(shooter_strength) %>% summarise(n = dplyr::n(), goal_rate = round(mean(is_goal), 4), .groups = "drop"))
cat("\n")

# ── 4. Build model matrix ────────────────────────────────────────────────
shots$shot_type_clean  <- factor(shots$shot_type_clean)
shots$shooter_strength <- factor(shots$shooter_strength)

model_df <- shots %>%
  transmute(is_goal = as.integer(is_goal), dist_to_net, angle_to_net,
            is_rebound = as.integer(is_rebound), shot_type_clean, shooter_strength) %>%
  na.omit()

X <- model.matrix(is_goal ~ dist_to_net + angle_to_net + is_rebound + shot_type_clean + shooter_strength - 1, data = model_df)
y <- model_df$is_goal
cat("Training rows:", nrow(X), "| overall goal rate:", round(mean(y), 4), "\n")

set.seed(42)
train_idx <- sample(seq_len(nrow(X)), size = floor(0.8 * nrow(X)))
dtrain <- xgb.DMatrix(data = X[train_idx, ], label = y[train_idx])
dtest  <- xgb.DMatrix(data = X[-train_idx, ], label = y[-train_idx])

# ── 5. Train ──────────────────────────────────────────────────────────────
params <- list(objective = "binary:logistic", eval_metric = "logloss",
                max_depth = 4, eta = 0.05, subsample = 0.8, colsample_bytree = 0.8)
cat("\nTraining xG model...\n")
xg_model <- xgb.train(params = params, data = dtrain, nrounds = 500,
                       watchlist = list(train = dtrain, test = dtest),
                       early_stopping_rounds = 20, print_every_n = 25, verbose = 1)

# ── 6. Validate ───────────────────────────────────────────────────────────
preds <- predict(xg_model, dtest)
y_test <- y[-train_idx]
logloss <- -mean(y_test * log(pmax(preds, 1e-15)) + (1 - y_test) * log(pmax(1 - preds, 1e-15)))
auc <- { r <- rank(preds); n1 <- sum(y_test == 1); n0 <- sum(y_test == 0); (sum(r[y_test == 1]) - n1*(n1+1)/2) / (n1*n0) }

cat("\n── Validation ──\n")
cat("Test log loss:", round(logloss, 4), "(lower is better; a league-average-only baseline would be roughly 0.30-0.33)\n")
cat("Test AUC:", round(auc, 4), "(0.5 = random, 1.0 = perfect; public xG models typically land around 0.78-0.82)\n")
cat("Calibration — mean predicted xG:", round(mean(preds), 4), "vs actual goal rate:", round(mean(y_test), 4), "(should be close)\n")

writeLines(c(
  paste("Trained:", Sys.time()),
  paste("Seasons:", paste(SEASONS, collapse = ", ")),
  paste("Training rows:", nrow(X)),
  paste("Test log loss:", round(logloss, 4)),
  paste("Test AUC:", round(auc, 4)),
  paste("Mean predicted xG:", round(mean(preds), 4)),
  paste("Actual goal rate:", round(mean(y_test), 4))
), METRICS_OUT)

# ── 7. Save model + everything needed to score new shots later ─────────────
# Factor levels are saved alongside the model because model.matrix() needs
# the SAME columns at prediction time as it had during training — a shot
# type or strength state that didn't appear in training data would
# otherwise silently break scoring later.
saveRDS(list(
  model = xg_model,
  shot_type_levels = levels(shots$shot_type_clean),
  shooter_strength_levels = levels(shots$shooter_strength),
  trained_at = Sys.time(),
  seasons = SEASONS
), MODEL_OUT)

cat("\nSaved model to", MODEL_OUT, "\n")
cat("Saved metrics to", METRICS_OUT, "\n")
