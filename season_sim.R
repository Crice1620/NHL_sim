Checking whether the 2027 season has already started...
Averaging player/team values over seasons: 2024, 2025, 2026 
Computing skater values...
  skater_hist rows: 2784 
  skater_vals_by_season rows: 2478 
  proj_skater_vals rows: 1075 
Computing goalie values...
  goalie_hist rows: 299 
  goalie_vals_by_season rows: 241 
  proj_goalie_vals rows: 104 
Computing skater stat-line projections...
  proj_skater_stats rows: 1076 
Computing goalie stat-line projections...
  proj_goalie_stats rows: 104 
── Roster endpoint diagnostic for COL ──
  URL: https://api-web.nhle.com/v1/roster/COL/current 
  Status: 200 | Body snippet: {"forwards":[{"id":8482947,"headshot":"https://assets.nhle.com/mugs/nhl/20262027/COL/8482947.png","firstName":{"default":"Zakhar"},"lastName":{"default":"Bardakov"},"sweaterNumber":93,"positionCode":"C","shootsCatches":"L","heightInInches":74,"weightInPounds":198,"heightInCentimeters":188,"weightInK 
  URL: https://api-web.nhle.com/v1/roster/COL/20262027 
  Status: 200 | Body snippet: {"forwards":[{"id":8482947,"headshot":"https://assets.nhle.com/mugs/nhl/20262027/COL/8482947.png","firstName":{"default":"Zakhar"},"lastName":{"default":"Bardakov"},"sweaterNumber":93,"positionCode":"C","shootsCatches":"L","heightInInches":74,"weightInPounds":198,"heightInCentimeters":188,"weightInK 
  URL: https://api-web.nhle.com/v1/roster/COL/20252026 
  Status: 200 | Body snippet: {"forwards":[{"id":8482947,"headshot":"https://assets.nhle.com/mugs/nhl/20252026/COL/8482947.png","firstName":{"default":"Zakhar"},"lastName":{"default":"Bardakov"},"sweaterNumber":93,"positionCode":"C","shootsCatches":"L","heightInInches":74,"weightInPounds":198,"heightInCentimeters":188,"weightInK 
──────────────────────────────────────────
Fetching rosters for 32 teams (target season 2027 )...
  Method breakdown: current=32 
  all_rosters rows: 795  (teams: 32 )
  Players with usable birth date: 795 of 795 
  Team net rating range (100% roster-driven): -1.083 to 1.18 
  skater_output rows: 714 ( 682 with history )
  goalie_output rows: 81 ( 75 with history )
Computing skater on-ice defense projections...
  WOWY computed for 1313 player-seasons across 3 seasons.
  proj_skater_onice rows: 1313 
  League-average WOWY (recentering baseline) — offense: -0.019 | defense: -0.002 
  SOG-to-Corsi conversion ratio (self-calibrated, scope-matched): 1.798 
  EDM raw on-ice (5v5-only, pre-conversion): onice_ca_pg_wtd= 13.63 onice_cf_pg_wtd= 14.52 -> after conversion: 24.51 | real all-situation shots-against was 26.7
  Teams with a full top-18 of on-ice defense data: 32 of 32 (partial coverage falls back to the blocks/hits proxy for those teams)
  Teams with enough WOWY coverage — offense: 32 defense: 32 of 32 
  Of which using real xG-based WOWY (vs. goals-based fallback) — offense: 32 defense: 32 of 32 
  League-average 5v5 xGA-per-shot (xG-based goalie-adjustment baseline): 0.153 
   VGK raw on-ice: onice_ca_pg_wtd= 13.77 onice_cf_pg_wtd= 14.33 -> shots_for_pg= 30.72 shooting_pct= 0.1066 final shots_against_pg= 28.3 
   MIN raw on-ice: onice_ca_pg_wtd= 14.57 onice_cf_pg_wtd= 14.42 -> shots_for_pg= 31.69 shooting_pct= 0.1062 final shots_against_pg= 29.3 
   NSH raw on-ice: onice_ca_pg_wtd= 13.5 onice_cf_pg_wtd= 13.57 -> shots_for_pg= 29.27 shooting_pct= 0.1045 final shots_against_pg= 27.29 
   SEA raw on-ice: onice_ca_pg_wtd= 14.59 onice_cf_pg_wtd= 13.46 -> shots_for_pg= 27.65 shooting_pct= 0.1113 final shots_against_pg= 29.32 

── Deep check: VGK — every stage of the offense/defense pipeline ──
  OFFENSE
    goals_for_pg (raw, pre-WOWY)      = 3.265 
    wowy_off_adj_per_game (EV)        = 0.008 | roster coverage: 18 / 18
    goals_for_pg_wowy_adj (final)     = 3.274 
    shots_for_pg                      = 30.72 
    shooting_pct (final)              = 0.1066 | league avg was 0.1112 
  ACTUAL SIMULATION INPUTS (what simulate_games() really uses now)
    onice_xgf_pg_wtd (5v5 xG-for)     = 2.23 | onice_xga_pg_wtd (5v5 xG-against) = 1.954 
    pp_goals_pg (team-level PP-for)   = 0.667 | league avg = 0.591 
    pk_ga_pg (team-level PK-against)  = 0.467 | league avg = 0.595 
  DEFENSE
    onice_ca_pg_wtd (5v5, pre-convert)= 13.772 
    onice_pk_sa_pg_wtd                = 2.204 
    shots_against_onice (pre-WOWY)    = 28.73 | source: onice ( 18 / 18 with on-ice history)
    wowy_def_adj_per_game (EV)        = 0.048 | roster coverage: 18 / 18
    shots_against_pg (final)          = 28.296 | league avg range was ~15-35
  DEFENSE — player-level breakdown (same 12F/6D roster feeding the average above)
     Jack Eichel          | toi_pg_min= 21 onice_ca_pg= 14.2 onice_cf_pg= 15.23 
     Mitch Marner         | toi_pg_min= 20.4 onice_ca_pg= 12.95 onice_cf_pg= 14.06 
     Mark Stone           | toi_pg_min= 19.2 onice_ca_pg= 12.85 onice_cf_pg= 13.99 
     Tomas Hertl          | toi_pg_min= 17.2 onice_ca_pg= 10.91 onice_cf_pg= 12.88 
     William Karlsson     | toi_pg_min= 16.5 onice_ca_pg= 12.37 onice_cf_pg= 13.14 
     Ivan Barbashev       | toi_pg_min= 16.4 onice_ca_pg= 14.86 onice_cf_pg= 15.27 
     Nic Dowd             | toi_pg_min= 14.8 onice_ca_pg= 11.32 onice_cf_pg= 10.21 
     Brett Howden         | toi_pg_min= 14.8 onice_ca_pg= 12.17 onice_cf_pg= 11.87 
     Braeden Bowman       | toi_pg_min= 14.1 onice_ca_pg= 11.63 onice_cf_pg= 11.43 
     Victor Olofsson      | toi_pg_min= 13.7 onice_ca_pg= 10.27 onice_cf_pg= 11.45 
     Alexander Holtz      | toi_pg_min= 11.5 onice_ca_pg= 10.84 onice_cf_pg= 9.34 
     Tanner Laczynski     | toi_pg_min= 10.4 onice_ca_pg= 7.68 onice_cf_pg= 6.18 
     Rasmus Andersson     | toi_pg_min= 23.5 onice_ca_pg= 18.62 onice_cf_pg= 17.98 
     Shea Theodore        | toi_pg_min= 22.6 onice_ca_pg= 16.48 onice_cf_pg= 18.8 
     Noah Hanifin         | toi_pg_min= 22.4 onice_ca_pg= 17.42 onice_cf_pg= 18.18 
     Brayden McNabb       | toi_pg_min= 20.3 onice_ca_pg= 15.28 onice_cf_pg= 16.63 
     Parker Wotherspoon   | toi_pg_min= 19.6 onice_ca_pg= 15.71 onice_cf_pg= 16.26 
     Jeremy Lauzon        | toi_pg_min= 17.4 onice_ca_pg= 13.21 onice_cf_pg= 13.42 

── Deep check: SEA — every stage of the offense/defense pipeline ──
  OFFENSE
    goals_for_pg (raw, pre-WOWY)      = 2.97 
    wowy_off_adj_per_game (EV)        = 0.107 | roster coverage: 18 / 18
    goals_for_pg_wowy_adj (final)     = 3.077 
    shots_for_pg                      = 27.646 
    shooting_pct (final)              = 0.1113 | league avg was 0.1112 
  ACTUAL SIMULATION INPUTS (what simulate_games() really uses now)
    onice_xgf_pg_wtd (5v5 xG-for)     = 1.957 | onice_xga_pg_wtd (5v5 xG-against) = 2.148 
    pp_goals_pg (team-level PP-for)   = 0.508 | league avg = 0.591 
    pk_ga_pg (team-level PK-against)  = 0.646 | league avg = 0.595 
  DEFENSE
    onice_ca_pg_wtd (5v5, pre-convert)= 14.588 
    onice_pk_sa_pg_wtd                = 1.688 
    shots_against_onice (pre-WOWY)    = 29.27 | source: onice ( 18 / 18 with on-ice history)
    wowy_def_adj_per_game (EV)        = -0.006 | roster coverage: 18 / 18
    shots_against_pg (final)          = 29.32 | league avg range was ~15-35
  DEFENSE — player-level breakdown (same 12F/6D roster feeding the average above)
     Chandler Stephenson  | toi_pg_min= 19.4 onice_ca_pg= 16.03 onice_cf_pg= 11.98 
     Matty Beniers        | toi_pg_min= 19 onice_ca_pg= 14.59 onice_cf_pg= 13.79 
     Jordan Eberle        | toi_pg_min= 18 onice_ca_pg= 13.65 onice_cf_pg= 13.26 
     Jared McCann         | toi_pg_min= 16.8 onice_ca_pg= 12.32 onice_cf_pg= 11.92 
     Freddy Gaudreau      | toi_pg_min= 15.8 onice_ca_pg= 13.81 onice_cf_pg= 10.79 
     Bobby McMann         | toi_pg_min= 15.3 onice_ca_pg= 13.91 onice_cf_pg= 12.75 
     Kaapo Kakko          | toi_pg_min= 14.6 onice_ca_pg= 12.85 onice_cf_pg= 12.29 
     Mackie Samoskevich   | toi_pg_min= 14.1 onice_ca_pg= 9.8 onice_cf_pg= 12.31 
     Shane Wright         | toi_pg_min= 13.9 onice_ca_pg= 11.99 onice_cf_pg= 11.04 
     Berkly Catton        | toi_pg_min= 12.9 onice_ca_pg= 12.09 onice_cf_pg= 10.94 
     Ryan Winterton       | toi_pg_min= 11.9 onice_ca_pg= 10.9 onice_cf_pg= 10 
     Ben Meyers           | toi_pg_min= 11.6 onice_ca_pg= 9.06 onice_cf_pg= 8.07 
     Brandon Montour      | toi_pg_min= 22.8 onice_ca_pg= 18.99 onice_cf_pg= 18.53 
     Vince Dunn           | toi_pg_min= 21.8 onice_ca_pg= 18.82 onice_cf_pg= 17.68 
     Adam Larsson         | toi_pg_min= 21.4 onice_ca_pg= 18.97 onice_cf_pg= 17.16 
     Ryan Lindgren        | toi_pg_min= 18.8 onice_ca_pg= 16.13 onice_cf_pg= 14.87 
     Ryker Evans          | toi_pg_min= 18.4 onice_ca_pg= 16.32 onice_cf_pg= 14.44 
     Cale Fleury          | toi_pg_min= 15.8 onice_ca_pg= 12.82 onice_cf_pg= 11.93 

── Deep check: SJS — every stage of the offense/defense pipeline ──
  OFFENSE
    goals_for_pg (raw, pre-WOWY)      = 3.032 
    wowy_off_adj_per_game (EV)        = 0.016 | roster coverage: 17 / 18
    goals_for_pg_wowy_adj (final)     = 3.048 
    shots_for_pg                      = 27.739 
    shooting_pct (final)              = 0.1099 | league avg was 0.1112 
  ACTUAL SIMULATION INPUTS (what simulate_games() really uses now)
    onice_xgf_pg_wtd (5v5 xG-for)     = 1.893 | onice_xga_pg_wtd (5v5 xG-against) = 2.026 
    pp_goals_pg (team-level PP-for)   = 0.585 | league avg = 0.591 
    pk_ga_pg (team-level PK-against)  = 0.738 | league avg = 0.595 
  DEFENSE
    onice_ca_pg_wtd (5v5, pre-convert)= 13.78 
    onice_pk_sa_pg_wtd                = 1.877 
    shots_against_onice (pre-WOWY)    = 28.156 | source: onice ( 17 / 18 with on-ice history)
    wowy_def_adj_per_game (EV)        = -0.013 | roster coverage: 17 / 18
    shots_against_pg (final)          = 28.27 | league avg range was ~15-35
  DEFENSE — player-level breakdown (same 12F/6D roster feeding the average above)
     Macklin Celebrini    | toi_pg_min= 21 onice_ca_pg= 15.34 onice_cf_pg= 14.76 
     Alexander Wennberg   | toi_pg_min= 19.8 onice_ca_pg= 13.17 onice_cf_pg= 12.15 
     Will Smith           | toi_pg_min= 17.7 onice_ca_pg= 13.5 onice_cf_pg= 12.42 
     Mason Marchment      | toi_pg_min= 16.6 onice_ca_pg= 13.17 onice_cf_pg= 13.72 
     Collin Graf          | toi_pg_min= 16.4 onice_ca_pg= 12.81 onice_cf_pg= 10.88 
     Kiefer Sherwood      | toi_pg_min= 16.2 onice_ca_pg= 11.69 onice_cf_pg= 12.49 
     Tyler Toffoli        | toi_pg_min= 15.7 onice_ca_pg= 12.31 onice_cf_pg= 11.74 
     Ty Dellandrea        | toi_pg_min= 13.2 onice_ca_pg= 12.09 onice_cf_pg= 8.4 
     Michael Misa         | toi_pg_min= 12.8 onice_ca_pg= 11.74 onice_cf_pg= 10.9 
     Barclay Goodrow      | toi_pg_min= 12.1 onice_ca_pg= 10.55 onice_cf_pg= 7.59 
     Adam Gaudette        | toi_pg_min= 11.2 onice_ca_pg= 9.58 onice_cf_pg= 8.4 
     Zack Ostapchuk       | toi_pg_min= 9.9 onice_ca_pg= 8.24 onice_cf_pg= 6.75 
     Jacob Trouba         | toi_pg_min= 22.1 onice_ca_pg= 18.65 onice_cf_pg= 18.95 
     Darnell Nurse        | toi_pg_min= 21.4 onice_ca_pg= 18.62 onice_cf_pg= 17.88 
     Dmitry Orlov         | toi_pg_min= 20.6 onice_ca_pg= 14.82 onice_cf_pg= 17.85 
     Sam Dickinson        | toi_pg_min= 16.8 onice_ca_pg= 15.3 onice_cf_pg= 13.73 
     Michael Kesselring   | toi_pg_min= 15.6 onice_ca_pg= 13.68 onice_cf_pg= 15.09 

── Deep check: EDM — every stage of the offense/defense pipeline ──
  OFFENSE
    goals_for_pg (raw, pre-WOWY)      = 3.492 
    wowy_off_adj_per_game (EV)        = 0.203 | roster coverage: 18 / 18
    goals_for_pg_wowy_adj (final)     = 3.695 
    shots_for_pg                      = 30.101 
    shooting_pct (final)              = 0.1228 | league avg was 0.1112 
  ACTUAL SIMULATION INPUTS (what simulate_games() really uses now)
    onice_xgf_pg_wtd (5v5 xG-for)     = 2.349 | onice_xga_pg_wtd (5v5 xG-against) = 2.138 
    pp_goals_pg (team-level PP-for)   = 0.759 | league avg = 0.591 
    pk_ga_pg (team-level PK-against)  = 0.637 | league avg = 0.595 
  DEFENSE
    onice_ca_pg_wtd (5v5, pre-convert)= 13.628 
    onice_pk_sa_pg_wtd                = 2.029 
    shots_against_onice (pre-WOWY)    = 28.157 | source: onice ( 18 / 18 with on-ice history)
    wowy_def_adj_per_game (EV)        = 0.076 | roster coverage: 18 / 18
    shots_against_pg (final)          = 27.472 | league avg range was ~15-35
  DEFENSE — player-level breakdown (same 12F/6D roster feeding the average above)
     Connor McDavid       | toi_pg_min= 22.6 onice_ca_pg= 15.05 onice_cf_pg= 19.32 
     Leon Draisaitl       | toi_pg_min= 21.5 onice_ca_pg= 14.61 onice_cf_pg= 18.1 
     Zach Hyman           | toi_pg_min= 19.6 onice_ca_pg= 14.12 onice_cf_pg= 17.12 
     Ryan Nugent-Hopkins  | toi_pg_min= 19.1 onice_ca_pg= 13.05 onice_cf_pg= 13.28 
     Jason Dickinson      | toi_pg_min= 15.7 onice_ca_pg= 11.67 onice_cf_pg= 9.95 
     Matt Savoie          | toi_pg_min= 14.8 onice_ca_pg= 11.92 onice_cf_pg= 11.86 
     Vasily Podkolzin     | toi_pg_min= 14.6 onice_ca_pg= 11.1 onice_cf_pg= 13.31 
     Kasperi Kapanen      | toi_pg_min= 13.6 onice_ca_pg= 11.87 onice_cf_pg= 10.73 
     Mattias Janmark      | toi_pg_min= 12.5 onice_ca_pg= 10.22 onice_cf_pg= 8.76 
     Mathieu Joseph       | toi_pg_min= 12.4 onice_ca_pg= 10.07 onice_cf_pg= 9.58 
     Trent Frederic       | toi_pg_min= 11.9 onice_ca_pg= 11.21 onice_cf_pg= 9.86 
     Colton Dach          | toi_pg_min= 11.5 onice_ca_pg= 9.13 onice_cf_pg= 8.08 
     Evan Bouchard        | toi_pg_min= 24.2 onice_ca_pg= 15.54 onice_cf_pg= 20.45 
     Mattias Ekholm       | toi_pg_min= 21 onice_ca_pg= 14.66 onice_cf_pg= 19.66 
     Jake Walman          | toi_pg_min= 20.1 onice_ca_pg= 17.4 onice_cf_pg= 16.44 
     Connor Murphy        | toi_pg_min= 18.4 onice_ca_pg= 16.03 onice_cf_pg= 12.95 
     Ryan Shea            | toi_pg_min= 18.3 onice_ca_pg= 14.91 onice_cf_pg= 14.58 
     Shakir Mukhamadullin | toi_pg_min= 17.3 onice_ca_pg= 14.8 onice_cf_pg= 12.58 

── Deep check: PIT — every stage of the offense/defense pipeline ──
  OFFENSE
    goals_for_pg (raw, pre-WOWY)      = 3.611 
    wowy_off_adj_per_game (EV)        = 0.106 | roster coverage: 18 / 18
    goals_for_pg_wowy_adj (final)     = 3.717 
    shots_for_pg                      = 29.844 
    shooting_pct (final)              = 0.1245 | league avg was 0.1112 
  ACTUAL SIMULATION INPUTS (what simulate_games() really uses now)
    onice_xgf_pg_wtd (5v5 xG-for)     = 2.237 | onice_xga_pg_wtd (5v5 xG-against) = 2.135 
    pp_goals_pg (team-level PP-for)   = 0.651 | league avg = 0.591 
    pk_ga_pg (team-level PK-against)  = 0.539 | league avg = 0.595 
  DEFENSE
    onice_ca_pg_wtd (5v5, pre-convert)= 13.53 
    onice_pk_sa_pg_wtd                = 1.393 
    shots_against_onice (pre-WOWY)    = 26.838 | source: onice ( 18 / 18 with on-ice history)
    wowy_def_adj_per_game (EV)        = -0.061 | roster coverage: 18 / 18
    shots_against_pg (final)          = 27.388 | league avg range was ~15-35
  DEFENSE — player-level breakdown (same 12F/6D roster feeding the average above)
     Bryan Rust           | toi_pg_min= 19.9 onice_ca_pg= 14.45 onice_cf_pg= 14.69 
     Sidney Crosby        | toi_pg_min= 19.7 onice_ca_pg= 15.2 onice_cf_pg= 16.57 
     Rickard Rakell       | toi_pg_min= 18.9 onice_ca_pg= 14.13 onice_cf_pg= 15.64 
     Evgeni Malkin        | toi_pg_min= 17.7 onice_ca_pg= 13.24 onice_cf_pg= 13.77 
     Andrei Kuzmenko      | toi_pg_min= 15.1 onice_ca_pg= 11.24 onice_cf_pg= 11.83 
     Ben Kindel           | toi_pg_min= 15.1 onice_ca_pg= 10.55 onice_cf_pg= 11.57 
     Thomas Novak         | toi_pg_min= 14.1 onice_ca_pg= 11.62 onice_cf_pg= 12.71 
     Egor Chinakhov       | toi_pg_min= 13.9 onice_ca_pg= 12.13 onice_cf_pg= 11.89 
     Blake Lizotte        | toi_pg_min= 13.4 onice_ca_pg= 10.06 onice_cf_pg= 9.81 
     Connor Dewar         | toi_pg_min= 13.3 onice_ca_pg= 9.92 onice_cf_pg= 9.04 
     Nicholas Robertson   | toi_pg_min= 12.4 onice_ca_pg= 11.4 onice_cf_pg= 10.3 
     Justin Brazeau       | toi_pg_min= 12.4 onice_ca_pg= 10.43 onice_cf_pg= 10.17 
     Erik Karlsson        | toi_pg_min= 23.5 onice_ca_pg= 17.23 onice_cf_pg= 19.7 
     Kris Letang          | toi_pg_min= 22.4 onice_ca_pg= 17.91 onice_cf_pg= 17.4 
     Samuel Girard        | toi_pg_min= 19.1 onice_ca_pg= 15.44 onice_cf_pg= 18.45 
     Trevor van Riemsdyk  | toi_pg_min= 16.9 onice_ca_pg= 15.19 onice_cf_pg= 13.73 
     Kaedan Korczak       | toi_pg_min= 15.8 onice_ca_pg= 12.41 onice_cf_pg= 13.33 
     Ryan Graves          | toi_pg_min= 15.6 onice_ca_pg= 13.25 onice_cf_pg= 12.88 
  GSAx goaltending: league-avg tracked sv% = 0.9345 | goalies with enough sample: 101 
  GOALTENDING ( VGK )
     Adin Hill | proj_gp= 37 proj_sv_pct (box-score)= 0.888 gsax_adj_sv_pct (used in sim)= 0.9219 
         shots_tracked= 4706 (needs >= 200 ) gsax_pooled= -59.24 
     Carter Hart | proj_gp= 20 proj_sv_pct (box-score)= 0.895 gsax_adj_sv_pct (used in sim)= 0.9196 
         shots_tracked= 2477 (needs >= 200 ) gsax_pooled= -36.95 
     Carl Lindbom | proj_gp= 8 proj_sv_pct (box-score)= 0.873 gsax_adj_sv_pct (used in sim)= 0.9052 
         shots_tracked= 283 (needs >= 200 ) gsax_pooled= -8.28 
    -> team_goaltending$goalie_sv_pct = 0.9192 
  GOALTENDING ( SEA )
     Joey Daccord | proj_gp= 50 proj_sv_pct (box-score)= 0.902 gsax_adj_sv_pct (used in sim)= 0.9258 
         shots_tracked= 6203 (needs >= 200 ) gsax_pooled= -54.05 
     Philipp Grubauer | proj_gp= 31 proj_sv_pct (box-score)= 0.9 gsax_adj_sv_pct (used in sim)= 0.9174 
         shots_tracked= 3569 (needs >= 200 ) gsax_pooled= -60.94 
    -> team_goaltending$goalie_sv_pct = 0.9226 
  GOALTENDING ( SJS )
     Yaroslav Askarov | proj_gp= 45 proj_sv_pct (box-score)= 0.884 gsax_adj_sv_pct (used in sim)= 0.9152 
         shots_tracked= 2423 (needs >= 200 ) gsax_pooled= -46.78 
     Alex Nedeljkovic | proj_gp= 39 proj_sv_pct (box-score)= 0.896 gsax_adj_sv_pct (used in sim)= 0.9201 
         shots_tracked= 4310 (needs >= 200 ) gsax_pooled= -62.24 
     Eric Comrie | proj_gp= 23 proj_sv_pct (box-score)= 0.896 gsax_adj_sv_pct (used in sim)= 0.9205 
         shots_tracked= 2118 (needs >= 200 ) gsax_pooled= -29.6 
    -> team_goaltending$goalie_sv_pct = 0.9181 
  GOALTENDING ( EDM )
     Tristan Jarry | proj_gp= 36 proj_sv_pct (box-score)= 0.887 gsax_adj_sv_pct (used in sim)= 0.9217 
         shots_tracked= 4531 (needs >= 200 ) gsax_pooled= -57.92 
     Frederik Andersen | proj_gp= 32 proj_sv_pct (box-score)= 0.881 gsax_adj_sv_pct (used in sim)= 0.9276 
         shots_tracked= 3863 (needs >= 200 ) gsax_pooled= -26.56 
     Devon Levi | proj_gp= 14 proj_sv_pct (box-score)= 0.883 gsax_adj_sv_pct (used in sim)= 0.9156 
         shots_tracked= 1218 (needs >= 200 ) gsax_pooled= -22.98 
    -> team_goaltending$goalie_sv_pct = 0.923 
  GOALTENDING ( PIT )
     Arturs Silovs | proj_gp= 37 proj_sv_pct (box-score)= 0.887 gsax_adj_sv_pct (used in sim)= 0.9201 
         shots_tracked= 2431 (needs >= 200 ) gsax_pooled= -34.98 
     Sergei Murashov | proj_gp= 5 proj_sv_pct (box-score)= 0.897 gsax_adj_sv_pct (used in sim)= NA 
         shots_tracked= 185 (needs >= 200 ) gsax_pooled= -2.68 
    -> team_goaltending$goalie_sv_pct = 0.9174 
  Team shots/game range: 25 - 35.7 | shots-against range: 23 - 31.6 | Sv% range: 0.915 - 0.928 

  ── League-wide goaltending ranking (goalie_sv_pct — what the sim actually uses) ──
  WSH  | goalie_sv_pct=0.9279 (rank 1/32)
  MTL  | goalie_sv_pct=0.9271 (rank 2/32)
  BOS  | goalie_sv_pct=0.9263 (rank 3/32)
  COL  | goalie_sv_pct=0.9258 (rank 4/32)
  CBJ  | goalie_sv_pct=0.9258 (rank 5/32)
  DAL  | goalie_sv_pct=0.9252 (rank 6/32)
  WPG  | goalie_sv_pct=0.9250 (rank 7/32)
  NJD  | goalie_sv_pct=0.9248 (rank 8/32)
  NYR  | goalie_sv_pct=0.9244 (rank 9/32)
  TOR  | goalie_sv_pct=0.9240 (rank 10/32)
  BUF  | goalie_sv_pct=0.9236 (rank 11/32)
  TBL  | goalie_sv_pct=0.9234 (rank 12/32)
  NYI  | goalie_sv_pct=0.9233 (rank 13/32)
  STL  | goalie_sv_pct=0.9232 (rank 14/32)
  CHI  | goalie_sv_pct=0.9231 (rank 15/32)
  EDM  | goalie_sv_pct=0.9230 (rank 16/32)
  FLA  | goalie_sv_pct=0.9229 (rank 17/32)
  CGY  | goalie_sv_pct=0.9227 (rank 18/32)
  SEA  | goalie_sv_pct=0.9226 (rank 19/32)
  CAR  | goalie_sv_pct=0.9223 (rank 20/32)
  MIN  | goalie_sv_pct=0.9218 (rank 21/32)
  LAK  | goalie_sv_pct=0.9214 (rank 22/32)
  ANA  | goalie_sv_pct=0.9212 (rank 23/32)
  DET  | goalie_sv_pct=0.9211 (rank 24/32)
  UTA  | goalie_sv_pct=0.9208 (rank 25/32)
  NSH  | goalie_sv_pct=0.9205 (rank 26/32)
  VAN  | goalie_sv_pct=0.9204 (rank 27/32)
  PHI  | goalie_sv_pct=0.9196 (rank 28/32)
  VGK  | goalie_sv_pct=0.9192 (rank 29/32)
  SJS  | goalie_sv_pct=0.9181 (rank 30/32)
  PIT  | goalie_sv_pct=0.9174 (rank 31/32)
  OTT  | goalie_sv_pct=0.9154 (rank 32/32)
  Spread (best - worst): 0.0125 

  Model league-avg shots_against_pg: 28.38 vs. real league average ~27.83 ( model runs HIGH )

  ── Player-level diagnostic: EDM, VGK, MIN, NSH, SEA — actual 12F/6D roster by rate_toi_min ──

  -- EDM --
    Connor McDavid       | proj_pts=137.7 rate_goals=0.525 rate_shots=3.513 rate_toi_min=22.6 n_seasons=3
    Leon Draisaitl       | proj_pts=124.1 rate_goals=0.594 rate_shots=3.001 rate_toi_min=21.5 n_seasons=3
    Evan Bouchard        | proj_pts=88.2 rate_goals=0.229 rate_shots=2.745 rate_toi_min=24.2 n_seasons=3
    Zach Hyman           | proj_pts=67.7 rate_goals=0.493 rate_shots=2.865 rate_toi_min=19.6 n_seasons=3
    Ryan Nugent-Hopkins  | proj_pts=62.0 rate_goals=0.267 rate_shots=1.958 rate_toi_min=19.1 n_seasons=3
    Mattias Ekholm       | proj_pts=42.6 rate_goals=0.094 rate_shots=1.790 rate_toi_min=21.0 n_seasons=3
    Matt Savoie          | proj_pts=37.9 rate_goals=0.220 rate_shots=1.683 rate_toi_min=14.8 n_seasons=1
    Jake Walman          | proj_pts=37.9 rate_goals=0.140 rate_shots=2.035 rate_toi_min=20.1 n_seasons=3
    Vasily Podkolzin     | proj_pts=33.4 rate_goals=0.187 rate_shots=1.592 rate_toi_min=14.6 n_seasons=3
    Ryan Shea            | proj_pts=31.1 rate_goals=0.069 rate_shots=0.817 rate_toi_min=18.3 n_seasons=3
    Kasperi Kapanen      | proj_pts=27.3 rate_goals=0.144 rate_shots=1.335 rate_toi_min=13.6 n_seasons=3
    Jason Dickinson      | proj_pts=23.7 rate_goals=0.126 rate_shots=1.126 rate_toi_min=15.7 n_seasons=3
    Shakir Mukhamadullin | proj_pts=20.8 rate_goals=0.096 rate_shots=0.983 rate_toi_min=17.3 n_seasons=2
    Mathieu Joseph       | proj_pts=20.7 rate_goals=0.058 rate_shots=0.939 rate_toi_min=12.4 n_seasons=3
    Connor Murphy        | proj_pts=19.2 rate_goals=0.053 rate_shots=0.980 rate_toi_min=18.4 n_seasons=3
    Colton Dach          | proj_pts=18.4 rate_goals=0.082 rate_shots=0.963 rate_toi_min=11.5 n_seasons=2
    Mattias Janmark      | proj_pts=16.8 rate_goals=0.027 rate_shots=0.626 rate_toi_min=12.5 n_seasons=3
    Trent Frederic       | proj_pts=14.0 rate_goals=0.088 rate_shots=1.151 rate_toi_min=11.9 n_seasons=3

  -- VGK --
    Jack Eichel          | proj_pts=101.6 rate_goals=0.372 rate_shots=3.423 rate_toi_min=21.0 n_seasons=3
    Mark Stone           | proj_pts=95.5 rate_goals=0.400 rate_shots=2.106 rate_toi_min=19.2 n_seasons=3
    Mitch Marner         | proj_pts=90.8 rate_goals=0.312 rate_shots=2.081 rate_toi_min=20.4 n_seasons=3
    Tomas Hertl          | proj_pts=62.3 rate_goals=0.333 rate_shots=2.445 rate_toi_min=17.2 n_seasons=3
    Ivan Barbashev       | proj_pts=60.9 rate_goals=0.289 rate_shots=1.603 rate_toi_min=16.4 n_seasons=3
    Shea Theodore        | proj_pts=55.2 rate_goals=0.130 rate_shots=1.891 rate_toi_min=22.6 n_seasons=3
    William Karlsson     | proj_pts=49.2 rate_goals=0.250 rate_shots=2.257 rate_toi_min=16.5 n_seasons=3
    Rasmus Andersson     | proj_pts=43.5 rate_goals=0.182 rate_shots=2.134 rate_toi_min=23.5 n_seasons=3
    Braeden Bowman       | proj_pts=40.4 rate_goals=0.148 rate_shots=1.389 rate_toi_min=14.1 n_seasons=1
    Tanner Laczynski     | proj_pts=36.8 rate_goals=0.021 rate_shots=1.042 rate_toi_min=10.4 n_seasons=2
    Noah Hanifin         | proj_pts=36.8 rate_goals=0.077 rate_shots=1.885 rate_toi_min=22.4 n_seasons=3
    Victor Olofsson      | proj_pts=35.3 rate_goals=0.188 rate_shots=1.868 rate_toi_min=13.7 n_seasons=3
    Brett Howden         | proj_pts=34.6 rate_goals=0.228 rate_shots=1.280 rate_toi_min=14.8 n_seasons=3
    Parker Wotherspoon   | proj_pts=26.2 rate_goals=0.032 rate_shots=0.854 rate_toi_min=19.6 n_seasons=3
    Nic Dowd             | proj_pts=25.1 rate_goals=0.106 rate_shots=0.936 rate_toi_min=14.8 n_seasons=3
    Alexander Holtz      | proj_pts=24.2 rate_goals=0.108 rate_shots=1.223 rate_toi_min=11.5 n_seasons=3
    Brayden McNabb       | proj_pts=18.4 rate_goals=0.071 rate_shots=1.231 rate_toi_min=20.3 n_seasons=3
    Jeremy Lauzon        | proj_pts=14.1 rate_goals=0.019 rate_shots=1.071 rate_toi_min=17.4 n_seasons=3

  -- MIN --
    Kirill Kaprizov      | proj_pts=100.1 rate_goals=0.586 rate_shots=3.484 rate_toi_min=22.2 n_seasons=3
    Quinn Hughes         | proj_pts=88.9 rate_goals=0.141 rate_shots=2.598 rate_toi_min=27.0 n_seasons=3
    Matt Boldy           | proj_pts=87.0 rate_goals=0.474 rate_shots=3.309 rate_toi_min=20.4 n_seasons=3
    Joel Eriksson Ek     | proj_pts=60.2 rate_goals=0.288 rate_shots=2.888 rate_toi_min=19.3 n_seasons=3
    Brock Faber          | proj_pts=46.9 rate_goals=0.164 rate_shots=1.965 rate_toi_min=24.9 n_seasons=3
    Ryan Hartman         | proj_pts=43.5 rate_goals=0.263 rate_shots=2.232 rate_toi_min=16.3 n_seasons=3
    Blake Coleman        | proj_pts=42.9 rate_goals=0.263 rate_shots=2.484 rate_toi_min=17.4 n_seasons=3
    Bobby Brink          | proj_pts=39.0 rate_goals=0.197 rate_shots=1.459 rate_toi_min=15.0 n_seasons=3
    Maksim Shabanov      | proj_pts=34.4 rate_goals=0.114 rate_shots=1.273 rate_toi_min=13.7 n_seasons=1
    Danila Yurov         | proj_pts=31.1 rate_goals=0.164 rate_shots=1.096 rate_toi_min=13.3 n_seasons=1
    Nick Foligno         | proj_pts=30.1 rate_goals=0.130 rate_shots=1.150 rate_toi_min=14.3 n_seasons=3
    Jared Spurgeon       | proj_pts=28.1 rate_goals=0.083 rate_shots=1.270 rate_toi_min=19.9 n_seasons=3
    Jonas Brodin         | proj_pts=27.6 rate_goals=0.072 rate_shots=1.323 rate_toi_min=21.2 n_seasons=3
    Marcus Foligno       | proj_pts=24.7 rate_goals=0.159 rate_shots=1.159 rate_toi_min=13.5 n_seasons=3
    Olli Maatta          | proj_pts=24.6 rate_goals=0.039 rate_shots=0.842 rate_toi_min=18.0 n_seasons=3
    Yakov Trenin         | proj_pts=21.3 rate_goals=0.084 rate_shots=1.197 rate_toi_min=13.2 n_seasons=3
    Michael McCarron     | proj_pts=18.0 rate_goals=0.097 rate_shots=1.295 rate_toi_min=13.9 n_seasons=3
    Matt Kiersted        | proj_pts=14.0 rate_goals=0.000 rate_shots=0.667 rate_toi_min=15.7 n_seasons=1

  -- NSH --
    Filip Forsberg       | proj_pts=78.5 rate_goals=0.463 rate_shots=3.210 rate_toi_min=18.8 n_seasons=3
    Ryan O'Reilly        | proj_pts=70.6 rate_goals=0.297 rate_shots=1.925 rate_toi_min=20.0 n_seasons=3
    Roman Josi           | proj_pts=67.9 rate_goals=0.194 rate_shots=2.756 rate_toi_min=24.9 n_seasons=3
    Steven Stamkos       | proj_pts=65.1 rate_goals=0.459 rate_shots=2.456 rate_toi_min=17.9 n_seasons=3
    Luke Evangelista     | proj_pts=52.1 rate_goals=0.152 rate_shots=2.120 rate_toi_min=15.7 n_seasons=3
    Jonathan Marchessaul | proj_pts=50.5 rate_goals=0.246 rate_shots=2.393 rate_toi_min=17.2 n_seasons=3
    Mavrik Bourque       | proj_pts=39.6 rate_goals=0.227 rate_shots=1.666 rate_toi_min=15.0 n_seasons=2
    Matthew Wood         | proj_pts=35.0 rate_goals=0.234 rate_shots=1.448 rate_toi_min=12.3 n_seasons=2
    Alexander Kerfoot    | proj_pts=32.3 rate_goals=0.169 rate_shots=1.066 rate_toi_min=14.4 n_seasons=3
    Ross Colton          | proj_pts=31.9 rate_goals=0.165 rate_shots=2.062 rate_toi_min=13.1 n_seasons=3
    Brady Skjei          | proj_pts=30.3 rate_goals=0.070 rate_shots=1.578 rate_toi_min=22.3 n_seasons=3
    Jack Drury           | proj_pts=26.1 rate_goals=0.118 rate_shots=1.245 rate_toi_min=13.9 n_seasons=3
    Adam Wilsby          | proj_pts=22.7 rate_goals=0.020 rate_shots=1.055 rate_toi_min=17.1 n_seasons=2
    Nils Hoglander       | proj_pts=21.4 rate_goals=0.104 rate_shots=1.080 rate_toi_min=11.8 n_seasons=3
    Nicolas Hague        | proj_pts=18.1 rate_goals=0.054 rate_shots=1.002 rate_toi_min=18.7 n_seasons=3
    Nick Perbix          | proj_pts=16.8 rate_goals=0.049 rate_shots=0.952 rate_toi_min=17.8 n_seasons=3
    Ilya Lyubushkin      | proj_pts=14.0 rate_goals=0.015 rate_shots=0.629 rate_toi_min=16.5 n_seasons=3
    Ozzy Wiesblatt       | proj_pts=10.7 rate_goals=0.024 rate_shots=0.630 rate_toi_min=10.2 n_seasons=2

  -- SEA --
    Jared McCann         | proj_pts=63.9 rate_goals=0.339 rate_shots=2.371 rate_toi_min=16.8 n_seasons=3
    Jordan Eberle        | proj_pts=55.9 rate_goals=0.297 rate_shots=2.012 rate_toi_min=18.0 n_seasons=3
    Chandler Stephenson  | proj_pts=52.8 rate_goals=0.192 rate_shots=1.107 rate_toi_min=19.4 n_seasons=3
    Kaapo Kakko          | proj_pts=48.5 rate_goals=0.193 rate_shots=1.311 rate_toi_min=14.6 n_seasons=3
    Vince Dunn           | proj_pts=48.5 rate_goals=0.149 rate_shots=2.200 rate_toi_min=21.8 n_seasons=3
    Matty Beniers        | proj_pts=48.4 rate_goals=0.241 rate_shots=1.804 rate_toi_min=19.0 n_seasons=3
    Bobby McMann         | proj_pts=45.7 rate_goals=0.338 rate_shots=2.316 rate_toi_min=15.3 n_seasons=3
    Brandon Montour      | proj_pts=42.2 rate_goals=0.185 rate_shots=2.752 rate_toi_min=22.8 n_seasons=3
    Shane Wright         | proj_pts=36.0 rate_goals=0.190 rate_shots=1.262 rate_toi_min=13.9 n_seasons=3
    Mackie Samoskevich   | proj_pts=35.0 rate_goals=0.170 rate_shots=2.000 rate_toi_min=14.1 n_seasons=3
    Freddy Gaudreau      | proj_pts=31.6 rate_goals=0.139 rate_shots=1.167 rate_toi_min=15.8 n_seasons=3
    Adam Larsson         | proj_pts=26.0 rate_goals=0.083 rate_shots=1.336 rate_toi_min=21.4 n_seasons=3
    Ryker Evans          | proj_pts=23.6 rate_goals=0.100 rate_shots=0.745 rate_toi_min=18.4 n_seasons=3
    Ben Meyers           | proj_pts=22.2 rate_goals=0.122 rate_shots=1.103 rate_toi_min=11.6 n_seasons=3
    Berkly Catton        | proj_pts=21.6 rate_goals=0.106 rate_shots=1.182 rate_toi_min=12.9 n_seasons=1
    Ryan Winterton       | proj_pts=20.8 rate_goals=0.054 rate_shots=1.275 rate_toi_min=11.9 n_seasons=3
    Ryan Lindgren        | proj_pts=14.9 rate_goals=0.035 rate_shots=0.722 rate_toi_min=18.8 n_seasons=3
    Cale Fleury          | proj_pts=10.3 rate_goals=0.038 rate_shots=0.981 rate_toi_min=15.8 n_seasons=2

  ── Goalie-level diagnostic: EDM, VGK, MIN, NSH ──

  -- EDM goalies --
    Tristan Jarry        | has_history=TRUE proj_gp=36.0 proj_wins=17.9 proj_sv_pct=0.8870
    Frederik Andersen    | has_history=TRUE proj_gp=32.0 proj_wins=16.0 proj_sv_pct=0.8810
    Devon Levi           | has_history=TRUE proj_gp=14.0 proj_wins=4.3 proj_sv_pct=0.8830

  -- VGK goalies --
    Adin Hill            | has_history=TRUE proj_gp=37.0 proj_wins=18.3 proj_sv_pct=0.8880
    Carter Hart          | has_history=TRUE proj_gp=20.0 proj_wins=11.4 proj_sv_pct=0.8950
    Carl Lindbom         | has_history=TRUE proj_gp=8.0 proj_wins=2.0 proj_sv_pct=0.8730

  -- MIN goalies --
    Filip Gustavsson     | has_history=TRUE proj_gp=52.0 proj_wins=28.3 proj_sv_pct=0.9070
    Jesper Wallstedt     | has_history=TRUE proj_gp=35.0 proj_wins=18.0 proj_sv_pct=0.9160
    Calvin Pickard       | has_history=TRUE proj_gp=26.0 proj_wins=12.1 proj_sv_pct=0.8870
    Riley Mercer         | has_history=FALSE proj_gp=NA proj_wins=NA proj_sv_pct=NA
    Chase Wutzke         | has_history=FALSE proj_gp=NA proj_wins=NA proj_sv_pct=NA

  -- NSH goalies --
    Juuse Saros          | has_history=TRUE proj_gp=59.0 proj_wins=26.2 proj_sv_pct=0.8950
    Justus Annunen       | has_history=TRUE proj_gp=30.0 proj_wins=11.8 proj_sv_pct=0.9000

  -- SEA goalies --
    Joey Daccord         | has_history=TRUE proj_gp=50.0 proj_wins=21.9 proj_sv_pct=0.9020
    Philipp Grubauer     | has_history=TRUE proj_gp=31.0 proj_wins=11.8 proj_sv_pct=0.9000


  ── Full team inputs (diagnostic) ──
  WSH  | shots_for=33.2 shooting_pct=0.1272 shots_against=29.6 (onice) goalie_sv=0.9279 | wowy_off=+0.059 wowy_def=-0.059 | approx_quality=2.098
  CAR  | shots_for=33.1 shooting_pct=0.1138 shots_against=23.0 (onice) goalie_sv=0.9223 | wowy_off=+0.094 wowy_def=+0.043 | approx_quality=1.980
  COL  | shots_for=35.7 shooting_pct=0.1095 shots_against=27.7 (onice) goalie_sv=0.9258 | wowy_off=+0.095 wowy_def=-0.026 | approx_quality=1.858
  TBL  | shots_for=30.3 shooting_pct=0.1262 shots_against=27.7 (onice) goalie_sv=0.9234 | wowy_off=+0.094 wowy_def=+0.023 | approx_quality=1.694
  EDM  | shots_for=30.1 shooting_pct=0.1228 shots_against=27.5 (onice) goalie_sv=0.9230 | wowy_off=+0.203 wowy_def=+0.076 | approx_quality=1.580
  FLA  | shots_for=34.4 shooting_pct=0.1082 shots_against=28.2 (onice) goalie_sv=0.9229 | wowy_off=+0.073 wowy_def=-0.003 | approx_quality=1.548
  BUF  | shots_for=30.3 shooting_pct=0.1191 shots_against=27.6 (onice) goalie_sv=0.9236 | wowy_off=+0.069 wowy_def=-0.023 | approx_quality=1.500
  PIT  | shots_for=29.8 shooting_pct=0.1245 shots_against=27.4 (onice) goalie_sv=0.9174 | wowy_off=+0.106 wowy_def=-0.061 | approx_quality=1.454
  DAL  | shots_for=27.4 shooting_pct=0.1295 shots_against=28.1 (onice) goalie_sv=0.9252 | wowy_off=+0.019 wowy_def=+0.016 | approx_quality=1.448
  MTL  | shots_for=27.8 shooting_pct=0.1314 shots_against=30.8 (onice) goalie_sv=0.9271 | wowy_off=+0.120 wowy_def=-0.080 | approx_quality=1.403
  TOR  | shots_for=29.3 shooting_pct=0.1227 shots_against=29.8 (onice) goalie_sv=0.9240 | wowy_off=+0.063 wowy_def=-0.034 | approx_quality=1.327
  BOS  | shots_for=27.0 shooting_pct=0.1217 shots_against=27.4 (onice) goalie_sv=0.9263 | wowy_off=+0.006 wowy_def=+0.036 | approx_quality=1.263
  ANA  | shots_for=29.9 shooting_pct=0.1102 shots_against=25.9 (onice) goalie_sv=0.9212 | wowy_off=+0.074 wowy_def=+0.028 | approx_quality=1.261
  UTA  | shots_for=31.4 shooting_pct=0.1086 shots_against=27.6 (onice) goalie_sv=0.9208 | wowy_off=-0.031 wowy_def=+0.022 | approx_quality=1.223
  NYR  | shots_for=27.5 shooting_pct=0.1205 shots_against=27.8 (onice) goalie_sv=0.9244 | wowy_off=+0.049 wowy_def=+0.058 | approx_quality=1.219
  CBJ  | shots_for=31.7 shooting_pct=0.1081 shots_against=29.9 (onice) goalie_sv=0.9258 | wowy_off=+0.040 wowy_def=+0.036 | approx_quality=1.209
  PHI  | shots_for=30.2 shooting_pct=0.1185 shots_against=29.6 (onice) goalie_sv=0.9196 | wowy_off=+0.057 wowy_def=-0.053 | approx_quality=1.197
  LAK  | shots_for=31.9 shooting_pct=0.1080 shots_against=28.9 (onice) goalie_sv=0.9214 | wowy_off=+0.042 wowy_def=-0.018 | approx_quality=1.174
  NJD  | shots_for=33.7 shooting_pct=0.0962 shots_against=27.9 (onice) goalie_sv=0.9248 | wowy_off=+0.086 wowy_def=+0.031 | approx_quality=1.149
  OTT  | shots_for=30.5 shooting_pct=0.1106 shots_against=26.6 (onice) goalie_sv=0.9154 | wowy_off=+0.099 wowy_def=+0.040 | approx_quality=1.116
  MIN  | shots_for=31.7 shooting_pct=0.1062 shots_against=29.3 (onice) goalie_sv=0.9218 | wowy_off=+0.048 wowy_def=+0.096 | approx_quality=1.074
  VGK  | shots_for=30.7 shooting_pct=0.1066 shots_against=28.3 (onice) goalie_sv=0.9192 | wowy_off=+0.008 wowy_def=+0.048 | approx_quality=0.986
  STL  | shots_for=27.1 shooting_pct=0.1201 shots_against=30.1 (onice) goalie_sv=0.9232 | wowy_off=+0.146 wowy_def=-0.130 | approx_quality=0.937
  NSH  | shots_for=29.3 shooting_pct=0.1045 shots_against=27.3 (onice) goalie_sv=0.9205 | wowy_off=-0.003 wowy_def=-0.046 | approx_quality=0.889
  WPG  | shots_for=27.1 shooting_pct=0.1085 shots_against=28.3 (onice) goalie_sv=0.9250 | wowy_off=+0.017 wowy_def=+0.072 | approx_quality=0.816
  SEA  | shots_for=27.6 shooting_pct=0.1113 shots_against=29.3 (onice) goalie_sv=0.9226 | wowy_off=+0.107 wowy_def=-0.006 | approx_quality=0.808
  SJS  | shots_for=27.7 shooting_pct=0.1099 shots_against=28.3 (onice) goalie_sv=0.9181 | wowy_off=+0.016 wowy_def=-0.013 | approx_quality=0.733
  NYI  | shots_for=29.4 shooting_pct=0.1064 shots_against=31.6 (onice) goalie_sv=0.9233 | wowy_off=+0.099 wowy_def=-0.128 | approx_quality=0.704
  VAN  | shots_for=25.0 shooting_pct=0.1099 shots_against=26.5 (onice) goalie_sv=0.9204 | wowy_off=+0.019 wowy_def=-0.010 | approx_quality=0.633
  DET  | shots_for=27.9 shooting_pct=0.1060 shots_against=29.8 (onice) goalie_sv=0.9211 | wowy_off=-0.003 wowy_def=-0.007 | approx_quality=0.596
  CHI  | shots_for=26.9 shooting_pct=0.1035 shots_against=31.4 (onice) goalie_sv=0.9231 | wowy_off=-0.013 wowy_def=-0.187 | approx_quality=0.366
  CGY  | shots_for=26.4 shooting_pct=0.0932 shots_against=29.2 (onice) goalie_sv=0.9227 | wowy_off=-0.012 wowy_def=-0.072 | approx_quality=0.204


  ── Roster-weighted xG sanity check (current-roster, trade-aware) ──
  CAR  | onice_xgf_pg_wtd=2.510 onice_xga_pg_wtd=1.946 xg_diff_pg=+0.563 | roster coverage: 18 / 18
  COL  | onice_xgf_pg_wtd=2.456 onice_xga_pg_wtd=2.080 xg_diff_pg=+0.377 | roster coverage: 18 / 18
  VGK  | onice_xgf_pg_wtd=2.230 onice_xga_pg_wtd=1.954 xg_diff_pg=+0.276 | roster coverage: 18 / 18
  TBL  | onice_xgf_pg_wtd=2.285 onice_xga_pg_wtd=2.047 xg_diff_pg=+0.239 | roster coverage: 18 / 18
  OTT  | onice_xgf_pg_wtd=2.178 onice_xga_pg_wtd=1.958 xg_diff_pg=+0.220 | roster coverage: 18 / 18
  EDM  | onice_xgf_pg_wtd=2.349 onice_xga_pg_wtd=2.138 xg_diff_pg=+0.211 | roster coverage: 18 / 18
  FLA  | onice_xgf_pg_wtd=2.204 onice_xga_pg_wtd=2.035 xg_diff_pg=+0.169 | roster coverage: 18 / 18
  NJD  | onice_xgf_pg_wtd=2.289 onice_xga_pg_wtd=2.158 xg_diff_pg=+0.132 | roster coverage: 18 / 18
  LAK  | onice_xgf_pg_wtd=2.171 onice_xga_pg_wtd=2.040 xg_diff_pg=+0.131 | roster coverage: 18 / 18
  PIT  | onice_xgf_pg_wtd=2.237 onice_xga_pg_wtd=2.135 xg_diff_pg=+0.102 | roster coverage: 18 / 18
  UTA  | onice_xgf_pg_wtd=2.174 onice_xga_pg_wtd=2.074 xg_diff_pg=+0.099 | roster coverage: 18 / 18
  DAL  | onice_xgf_pg_wtd=2.096 onice_xga_pg_wtd=2.019 xg_diff_pg=+0.077 | roster coverage: 18 / 18
  CBJ  | onice_xgf_pg_wtd=2.243 onice_xga_pg_wtd=2.178 xg_diff_pg=+0.065 | roster coverage: 18 / 18
  MIN  | onice_xgf_pg_wtd=2.140 onice_xga_pg_wtd=2.096 xg_diff_pg=+0.044 | roster coverage: 18 / 18
  PHI  | onice_xgf_pg_wtd=2.080 onice_xga_pg_wtd=2.040 xg_diff_pg=+0.040 | roster coverage: 18 / 18
  ANA  | onice_xgf_pg_wtd=2.076 onice_xga_pg_wtd=2.069 xg_diff_pg=+0.007 | roster coverage: 18 / 18
  WPG  | onice_xgf_pg_wtd=2.028 onice_xga_pg_wtd=2.034 xg_diff_pg=-0.006 | roster coverage: 18 / 18
  NYR  | onice_xgf_pg_wtd=2.035 onice_xga_pg_wtd=2.059 xg_diff_pg=-0.024 | roster coverage: 18 / 18
  WSH  | onice_xgf_pg_wtd=2.261 onice_xga_pg_wtd=2.289 xg_diff_pg=-0.028 | roster coverage: 18 / 18
  STL  | onice_xgf_pg_wtd=2.131 onice_xga_pg_wtd=2.161 xg_diff_pg=-0.030 | roster coverage: 18 / 18
  NSH  | onice_xgf_pg_wtd=2.057 onice_xga_pg_wtd=2.107 xg_diff_pg=-0.050 | roster coverage: 18 / 18
  BUF  | onice_xgf_pg_wtd=2.143 onice_xga_pg_wtd=2.200 xg_diff_pg=-0.057 | roster coverage: 18 / 18
  TOR  | onice_xgf_pg_wtd=2.100 onice_xga_pg_wtd=2.208 xg_diff_pg=-0.108 | roster coverage: 18 / 18
  SJS  | onice_xgf_pg_wtd=1.893 onice_xga_pg_wtd=2.026 xg_diff_pg=-0.133 | roster coverage: 17 / 18
  BOS  | onice_xgf_pg_wtd=1.950 onice_xga_pg_wtd=2.088 xg_diff_pg=-0.139 | roster coverage: 18 / 18
  DET  | onice_xgf_pg_wtd=1.951 onice_xga_pg_wtd=2.092 xg_diff_pg=-0.140 | roster coverage: 17 / 18
  NYI  | onice_xgf_pg_wtd=2.209 onice_xga_pg_wtd=2.356 xg_diff_pg=-0.147 | roster coverage: 18 / 18
  MTL  | onice_xgf_pg_wtd=2.206 onice_xga_pg_wtd=2.373 xg_diff_pg=-0.166 | roster coverage: 18 / 18
  SEA  | onice_xgf_pg_wtd=1.957 onice_xga_pg_wtd=2.148 xg_diff_pg=-0.191 | roster coverage: 18 / 18
  CGY  | onice_xgf_pg_wtd=1.978 onice_xga_pg_wtd=2.209 xg_diff_pg=-0.231 | roster coverage: 18 / 18
  VAN  | onice_xgf_pg_wtd=1.835 onice_xga_pg_wtd=2.203 xg_diff_pg=-0.368 | roster coverage: 18 / 18
  CHI  | onice_xgf_pg_wtd=1.911 onice_xga_pg_wtd=2.513 xg_diff_pg=-0.602 | roster coverage: 18 / 18
  Range — onice_xgf_pg_wtd: 1.835 - 2.51 | onice_xga_pg_wtd: 1.946 - 2.513 
  Spread (best - worst) xg_diff_pg: 1.166 


  ── League-wide PP/PK ranking (recency-weighted, 3yr — what the sim actually uses) ──
  DAL  | pp_goals_pg=0.814 (rank 1/32) | pk_ga_pg=0.574
  EDM  | pp_goals_pg=0.759 (rank 2/32) | pk_ga_pg=0.637
  OTT  | pp_goals_pg=0.720 (rank 3/32) | pk_ga_pg=0.664
  DET  | pp_goals_pg=0.705 (rank 4/32) | pk_ga_pg=0.610
  MIN  | pp_goals_pg=0.685 (rank 5/32) | pk_ga_pg=0.634
  CAR  | pp_goals_pg=0.671 (rank 6/32) | pk_ga_pg=0.491
  VGK  | pp_goals_pg=0.667 (rank 7/32) | pk_ga_pg=0.467
  MTL  | pp_goals_pg=0.665 (rank 8/32) | pk_ga_pg=0.654
  FLA  | pp_goals_pg=0.659 (rank 9/32) | pk_ga_pg=0.592
  PIT  | pp_goals_pg=0.651 (rank 10/32) | pk_ga_pg=0.539
  TBL  | pp_goals_pg=0.647 (rank 11/32) | pk_ga_pg=0.526
  NSH  | pp_goals_pg=0.643 (rank 12/32) | pk_ga_pg=0.538
  VAN  | pp_goals_pg=0.618 (rank 13/32) | pk_ga_pg=0.655
  NYR  | pp_goals_pg=0.599 (rank 14/32) | pk_ga_pg=0.583
  WPG  | pp_goals_pg=0.588 (rank 15/32) | pk_ga_pg=0.582
  NJD  | pp_goals_pg=0.586 (rank 16/32) | pk_ga_pg=0.520
  SJS  | pp_goals_pg=0.585 (rank 17/32) | pk_ga_pg=0.738
  UTA  | pp_goals_pg=0.585 (rank 18/32) | pk_ga_pg=0.645
  COL  | pp_goals_pg=0.583 (rank 19/32) | pk_ga_pg=0.481
  BUF  | pp_goals_pg=0.567 (rank 20/32) | pk_ga_pg=0.560
  BOS  | pp_goals_pg=0.564 (rank 21/32) | pk_ga_pg=0.691
  WSH  | pp_goals_pg=0.549 (rank 22/32) | pk_ga_pg=0.584
  TOR  | pp_goals_pg=0.538 (rank 23/32) | pk_ga_pg=0.554
  ANA  | pp_goals_pg=0.535 (rank 24/32) | pk_ga_pg=0.775
  CHI  | pp_goals_pg=0.511 (rank 25/32) | pk_ga_pg=0.499
  SEA  | pp_goals_pg=0.508 (rank 26/32) | pk_ga_pg=0.646
  CGY  | pp_goals_pg=0.488 (rank 27/32) | pk_ga_pg=0.583
  STL  | pp_goals_pg=0.480 (rank 28/32) | pk_ga_pg=0.616
  LAK  | pp_goals_pg=0.470 (rank 29/32) | pk_ga_pg=0.640
  CBJ  | pp_goals_pg=0.448 (rank 30/32) | pk_ga_pg=0.615
  NYI  | pp_goals_pg=0.433 (rank 31/32) | pk_ga_pg=0.540
  PHI  | pp_goals_pg=0.403 (rank 32/32) | pk_ga_pg=0.604
  Range — pp_goals_pg: 0.403 - 0.814 | pk_ga_pg: 0.467 - 0.775 


  ── Direct team-level xG comparison (most recent season only, no roster reconstruction) ──
  COL  | xgf_pg=2.474 xga_pg=1.875 xg_diff_pg=+0.599
  CAR  | xgf_pg=2.525 xga_pg=1.952 xg_diff_pg=+0.573
  OTT  | xgf_pg=2.178 xga_pg=1.812 xg_diff_pg=+0.366
  TBL  | xgf_pg=2.197 xga_pg=1.881 xg_diff_pg=+0.316
  VGK  | xgf_pg=2.190 xga_pg=1.878 xg_diff_pg=+0.312
  UTA  | xgf_pg=2.237 xga_pg=2.019 xg_diff_pg=+0.218
  LAK  | xgf_pg=2.086 xga_pg=1.901 xg_diff_pg=+0.185
  CBJ  | xgf_pg=2.259 xga_pg=2.101 xg_diff_pg=+0.159
  PIT  | xgf_pg=2.260 xga_pg=2.133 xg_diff_pg=+0.128
  EDM  | xgf_pg=2.278 xga_pg=2.158 xg_diff_pg=+0.120
  DAL  | xgf_pg=2.015 xga_pg=1.928 xg_diff_pg=+0.087
  ANA  | xgf_pg=2.271 xga_pg=2.206 xg_diff_pg=+0.066
  BUF  | xgf_pg=2.164 xga_pg=2.108 xg_diff_pg=+0.057
  PHI  | xgf_pg=1.917 xga_pg=1.874 xg_diff_pg=+0.042
  STL  | xgf_pg=2.043 xga_pg=2.006 xg_diff_pg=+0.037
  FLA  | xgf_pg=2.050 xga_pg=2.026 xg_diff_pg=+0.024
  WSH  | xgf_pg=2.199 xga_pg=2.197 xg_diff_pg=+0.003
  MIN  | xgf_pg=2.107 xga_pg=2.116 xg_diff_pg=-0.009
  NJD  | xgf_pg=2.105 xga_pg=2.176 xg_diff_pg=-0.070
  NYR  | xgf_pg=1.927 xga_pg=2.034 xg_diff_pg=-0.107
  NSH  | xgf_pg=2.064 xga_pg=2.185 xg_diff_pg=-0.120
  DET  | xgf_pg=2.048 xga_pg=2.184 xg_diff_pg=-0.136
  NYI  | xgf_pg=2.100 xga_pg=2.256 xg_diff_pg=-0.156
  WPG  | xgf_pg=1.958 xga_pg=2.138 xg_diff_pg=-0.180
  MTL  | xgf_pg=2.057 xga_pg=2.241 xg_diff_pg=-0.184
  SJS  | xgf_pg=1.948 xga_pg=2.167 xg_diff_pg=-0.219
  BOS  | xgf_pg=1.944 xga_pg=2.194 xg_diff_pg=-0.250
  CGY  | xgf_pg=1.918 xga_pg=2.199 xg_diff_pg=-0.281
  SEA  | xgf_pg=1.832 xga_pg=2.209 xg_diff_pg=-0.378
  TOR  | xgf_pg=1.947 xga_pg=2.343 xg_diff_pg=-0.396
  CHI  | xgf_pg=1.820 xga_pg=2.406 xg_diff_pg=-0.585
  VAN  | xgf_pg=1.877 xga_pg=2.464 xg_diff_pg=-0.588
  Spread (best - worst) xg_diff_pg: 1.187 

  last_schedule rows: 1312 
Running 10000 season simulations (shot-based engine)...
  OT/SO rate check (single sample, 1312 games): 18.8 % | real NHL rate is roughly 20-23%
  Win-prob sanity check — CAR (best 5v5 xG) vs CHI (worst 5v5 xG):
    CAR at home: 64.6 % CAR win | 3.31 vs 2.5 avg goals
    CHI at home: 60.4 % CAR win | 2.6 vs 3.16 avg goals
  Win-prob sanity check — league-AVERAGE team vs CHI (worst 5v5 xG):
    AVG at home: 58.4 % AVG win | 3.08 vs 2.66 avg goals
    CHI at home: 55.5 % AVG win | 2.74 vs 2.96 avg goals
    CHI's average win rate vs AVG (both directions): 43 % -> rough implied points (ignoring OTL bonus): 70.6 
  CHI's real schedule — opponent quality check:
    Games scheduled: 82 | avg opponent xg_diff_pg: 0.0261 (league avg is ~0 by construction; negative here would mean CHI's real schedule is softer than average)
    CHI's own full-schedule simulated points (avg over 2000 sims, before the 84-game scaling): 79.7 | scaled to 84 games: 81.6 
  Schedule template games/team: 82 | scaling proj_points to 84 -game season (x 1.0244 )
Wrote data/season_sim/2026.json (projecting season 2027 )
