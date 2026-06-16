# NHL Basic Stats

Auto-generated — last updated: 2026-06-16 13:38 UTC

## Read in R / Shiny
```r
BASE <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/stats"
# Each season lives in its own folder: 2021-22, 2022-23, ..., 2025-26
BASE_SZN  <- paste0(BASE, "/2025-26")
team_sum  <- read.csv(paste0(BASE_SZN, "/team_summary.csv"))
team_rt   <- read.csv(paste0(BASE_SZN, "/team_realtime.csv"))
skaters   <- read.csv(paste0(BASE_SZN, "/skater_summary.csv"))
goalies   <- read.csv(paste0(BASE_SZN, "/goalie_summary.csv"))

# Load all seasons and combine:
all_skaters <- bind_rows(lapply(
  c("2021-22","2022-23","2023-24","2024-25","2025-26"),
  function(s) read.csv(paste0(BASE, "/", s, "/skater_summary.csv"))
))
```

## Files
| File | Key columns |
|------|-------------|
| `data/stats/team_summary_{season}.csv` | gp, wins, losses, otl, pts, gf/ga per game, pp%, pk%, fo%, shutouts |
| `data/stats/team_realtime_{season}.csv` | hits, blocks, takeaways, giveaways, corsi% (satPct), shot attempts |
| `data/stats/skater_summary_{season}.csv` | goals, assists, pts, +/-, pim, shots, sh%, toi, ev/pp/sh splits |
| `data/stats/goalie_summary_{season}.csv` | wins, gaa, sv%, saves, shots against, shutouts, toi |

## Season column convention
`season` = end year of the season.  2022 = 2021-22,  2026 = 2025-26.
