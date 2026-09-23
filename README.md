# AP Poll Predictor

FSU Sports Analytics Club model that predicts each week's AP Top 25 college football poll.
A LightGBM model predicts every team's next-week AP poll points from this week's result,
opponent, record and streak. A Streamlit app shows the predictions:
**[appoll-predictor.streamlit.app](https://appoll-predictor.streamlit.app)**

## Project layout

```
APPollApp.py          Streamlit app (Streamlit Cloud runs this file)
requirements.txt      Python packages for the app
data/                 CFBPollDataFinal.csv - one row per team per week (what the app reads)
model/                train_model.ipynb, CFBLightGBM.txt, feature_cols.json, evaluate_model.py
pipeline/             R scripts that build the data (no API key needed)
archive/              original R scripts + their descriptions, kept for reference
```

## Weekly update (in season, Sunday or later)

1. RStudio Console:
   ```r
   setwd("~/Desktop/Sports Data/APPollModel")
   source("pipeline/run_weekly.R")
   ```
   If it ends with `ALL CHECKS PASSED`, `data/CFBPollDataFinal.csv` is updated.
   If not, nothing is changed. Read the problems it lists.
2. Terminal:
   ```bash
   cd ~/Desktop/Sports\ Data/APPollModel
   git commit -am "Week update"
   git push mine main
   ```
   Streamlit Cloud redeploys in a couple of minutes.

The model is **not** retrained weekly, so every week of the current season is a real
prediction it has never seen.

## Once a season (offseason)

1. `pipeline/01_get_data.R`: set `LAST_SEASON` to the new season.
2. `model/train_model.ipynb`: set `TRAIN_THROUGH` to the season that just ended.
3. Run the weekly update, then retrain:
   ```bash
   source venv/bin/activate
   python -m jupyter nbconvert --to notebook --execute --inplace model/train_model.ipynb
   python model/evaluate_model.py     # optional: honest test on unseen seasons
   ```
4. Commit and push. `CFBLightGBM.txt` and `feature_cols.json` always go together.

## Running the app locally

```bash
cd ~/Desktop/Sports\ Data/APPollModel
source venv/bin/activate
streamlit run APPollApp.py
```

## Model inputs

16 features (`model/feature_cols.json`): conference, poll_points, result, pts_scored,
pts_allowed, pt_diff, opponent, opponent_conference, opponent_wins, opponent_losses,
opponent_points, bias, undefeated, total_wins_after, total_losses_after, streak.
Target: `next_week_points`. Categorical: conference, opponent_conference, result, bias, opponent.
