# Data pipeline (R, no API key)

Weekly update, in the RStudio Console:

```r
setwd("~/Desktop/Sports Data/APPollModel")
source("pipeline/run_weekly.R")
```

| Script | What it does |
|---|---|
| `01_get_data.R` | Scrapes AP poll votes (collegepolltracker.com), builds game results from cfbfastR play-by-play, fills gaps from ESPN's scoreboard. Cached one file per season in `cache/`: finished seasons download once, the current season re-downloads every run. |
| `02_build_features.R` | Records, streaks, opponent info, bias, poll targets. Lines up poll-week numbering per season. |
| `03_validate.R` | Checks columns, duplicates, team names, week alignment. If everything passes, copies the CSV to `data/CFBPollDataFinal.csv`. |
| `run_weekly.R` | Runs all three. |

**Current season:** only weeks whose games are all final are kept. The newest finished week
whose next poll isn't out yet is the live prediction (`next_week_points` blank).

**New season:** change `LAST_SEASON` in `01_get_data.R`.

**Fixes:** team name mismatches go in `NAME_MAP`, FBS conferences in `FBS_CONFERENCES`
(both in `02_build_features.R`).
