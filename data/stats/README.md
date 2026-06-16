# NHL Basic Stats

Auto-generated — last updated: 2026-06-16 11:35 UTC

## Read in R / Shiny
```r
BASE <- "https://raw.githubusercontent.com/Crice1620/NHL_sim/main/data/stats"
team_sum <- read.csv(paste0(BASE, "/team_summary.csv"))
team_rt  <- read.csv(paste0(BASE, "/team_realtime.csv"))
skaters  <- read.csv(paste0(BASE, "/skater_summary.csv"))
goalies  <- read.csv(paste0(BASE, "/goalie_summary.csv"))
# season = end year: 2022=2021-22, 2026=2025-26
cur_skaters <- skaters[skaters$season == 2026, ]
```

## Files
| File | Key columns |
|------|-------------|
| `data/stats/team_summary.csv` | gp, wins, losses, otl, pts, gf/ga per game, pp%, pk%, fo%, shutouts |
| `data/stats/team_realtime.csv` | hits, blocks, takeaways, giveaways, corsi% (satPct), shot attempts |
| `data/stats/skater_summary.csv` | goals, assists, pts, +/-, pim, shots, sh%, toi, ev/pp/sh splits |
| `data/stats/goalie_summary.csv` | wins, gaa, sv%, saves, shots against, shutouts, toi |

## Season column convention
`season` = end year of the season.  2022 = 2021-22,  2026 = 2025-26.
