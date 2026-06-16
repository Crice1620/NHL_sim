# NHL Edge Stats

Auto-generated — last updated: 2026-06-16 10:49 UTC

## Read in R / Shiny
```r
BASE <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/edge"
edge <- read.csv(paste0(BASE, "/team_comparison.csv"))
# season = end year: 2022=2021-22, 2026=2025-26
cur  <- edge[edge$season == 2026, ]
```

## Files
| File | Description |
|------|-------------|
| `data/edge/team_comparison.csv` | Flat summary per team: skating distance, speed bursts, zone time, shot location, shot differential |
| `data/edge/team_skating_distance.csv` | Skating distance totals and per-60 rates |
| `data/edge/team_zone_time.csv` | Zone time percentages vs league average |
| `data/edge/team_shot_location.csv` | Shot location goals/SOG/sh% broken out by zone |
