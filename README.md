# NHL Edge Stats

Auto-generated — last updated: 2026-06-16 13:25 UTC

## Read in R / Shiny
```r
BASE <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/edge"
skaters <- read.csv(paste0(BASE, "/skater_summary.csv"))
teams   <- read.csv(paste0(BASE, "/team_comparison.csv"))
# season = end year: 2022=2021-22, 2026=2025-26
```

## Files
| File | Description |
|------|-------------|
| `data/edge/team_comparison.csv` | Per-team flat summary: distance, speed, zone time, shot speed, shot differential |
| `data/edge/team_skating_distance.csv` | Per-team skating distance total/per60/max |
| `data/edge/team_skating_speed.csv` | Per-team max speed + burst counts |
| `data/edge/team_zone_time.csv` | Per-team zone time % vs league avg |
| `data/edge/team_shot_location.csv` | Per-team SOG/goals/sh% by zone (17 zones) |
| `data/edge/team_shot_speed.csv` | Per-team shot speed distribution |
| `data/edge/skater_summary.csv` | Per-player flat summary: speed, distance, zone time, zone starts, shot speed |
| `data/edge/skater_shot_location.csv` | Per-player SOG/goals/sh% by zone (17 zones) |
| `data/edge/skater_last10_distance.csv` | Per-player distance per game for last 10 games |
