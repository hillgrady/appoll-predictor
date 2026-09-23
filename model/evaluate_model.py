"""
evaluate_model.py - honest test of the AP Poll model (nothing is saved or changed)

For each test season (2022, 2023, 2024) it trains the exact same LightGBM model
the notebook uses on every EARLIER season only, then predicts every week of the
test season it has never seen. It does this for:
  * NEW data - the cfbfastR 3.0 pipeline CSV (CFBPollDataFinal.csv on this branch)
  * OLD data - the CSV currently live on GitHub (read from the main branch)
  * BASELINE - "nobody moves": next week's poll = this week's poll

Metrics are per poll week (weeks 3-14, like the app), averaged over the season:
  top25_hit   how many of the real next-week Top 25 the model had in its Top 25
  exact       how many teams it put at exactly the right rank
  avg_miss    average ranks off, for teams in the real Top 25 (lower = better)
  spearman    rank correlation within the real Top 25 (1.0 = perfect order)
  rmse        error in predicted poll points, for teams in the real Top 25

Run from the project folder with the venv on:
    source venv/bin/activate
    python model/evaluate_model.py
"""
import subprocess
import sys
from io import StringIO

import numpy as np
import pandas as pd
import lightgbm as lgb

TEST_YEARS = [2022, 2023, 2024]
WEEKS = range(3, 15)
DROP = ["total_wins_before", "total_losses_before", "team", "rank", "poll_point_diff"]
CATS = ["conference", "opponent_conference", "result", "bias", "opponent"]
PARAMS = dict(learning_rate=0.01, n_estimators=1500, max_depth=6,
              random_state=42, verbose=-1)          # same as APPollLightGBM2.ipynb


def load_new():
    return pd.read_csv("data/CFBPollDataFinal.csv")


def load_old():
    try:
        txt = None
        for path in ("main:data/CFBPollDataFinal.csv", "main:CFBPollDataFinal.csv"):
            r = subprocess.run(["git", "show", path], capture_output=True, text=True)
            if r.returncode == 0:
                txt = r.stdout
                break
        if txt is None:
            raise RuntimeError("not found on main")
    except Exception as e:
        print("Could not read the old CSV from the main branch:", e)
        return None
    return pd.read_csv(StringIO(txt))


def features(d):
    return [c for c in d.columns if c not in DROP + ["next_week_points", "year", "week"]]


def to_X(d, cols, cat_levels=None):
    X = d[cols].copy()
    for c in CATS:
        if cat_levels is None:
            X[c] = X[c].astype("category")
        else:
            v = X[c].where(X[c].isin(cat_levels[c]))   # unseen teams -> missing
            X[c] = pd.Categorical(v, categories=cat_levels[c])
    return X


def rank_desc(s):
    return s.rank(method="min", ascending=False)


def week_metrics(wk, pred_col):
    """wk: one year-week of rows with next_week_points and a prediction column."""
    actual = wk[wk.next_week_points > 0].nlargest(25, "next_week_points")
    if len(actual) < 20:
        return None
    wk = wk.assign(model_rank=rank_desc(wk[pred_col]),
                   actual_rank=rank_desc(wk.next_week_points))
    pred_top = set(wk.nsmallest(25, "model_rank").team)
    a = wk[wk.team.isin(actual.team)]
    return dict(
        top25_hit=len(pred_top & set(actual.team)),
        exact=int((a.model_rank == a.actual_rank).sum()),
        avg_miss=float((a.model_rank - a.actual_rank).abs().mean()),
        spearman=float(a[["model_rank", "actual_rank"]].corr(method="spearman").iloc[0, 1]),
        rmse=float(np.sqrt(((a[pred_col] - a.next_week_points) ** 2).mean())),
    )


def season_metrics(test, pred_col):
    rows = []
    for (y, w), wk in test.groupby(["year", "week"]):
        if w in WEEKS:
            m = week_metrics(wk, pred_col)
            if m:
                rows.append(m)
    return pd.DataFrame(rows).mean().round(2)


def evaluate(d, label):
    d = d.drop_duplicates(["team", "year", "week"])
    d = d[d.next_week_points.notna()].copy()     # skip the live week
    cols = features(d)
    out = []
    for ty in TEST_YEARS:
        train, test = d[d.year < ty], d[d.year == ty].copy()
        if test.empty:
            continue
        Xtr = to_X(train, cols)
        levels = {c: Xtr[c].cat.categories for c in CATS}
        model = lgb.LGBMRegressor(**PARAMS)
        model.fit(Xtr, train.next_week_points, categorical_feature=CATS)
        test["pred"] = model.predict(to_X(test, cols, levels))
        out.append(season_metrics(test, "pred").rename(f"{label} {ty}"))
        if label == "NEW":   # baseline once, on the new data's test season
            out.append(season_metrics(test.assign(base=test.poll_points), "base")
                       .rename(f"BASELINE {ty}"))
        print(f"  done: {label} {ty}", file=sys.stderr)
    return out


if __name__ == "__main__":
    print("Training and testing (a few minutes)...", file=sys.stderr)
    results = evaluate(load_new(), "NEW")
    old = load_old()
    if old is not None:
        results += evaluate(old, "OLD")
    table = pd.DataFrame(results)
    order = [f"{k} {y}" for y in TEST_YEARS for k in ("NEW", "OLD", "BASELINE")]
    table = table.loc[[r for r in order if r in table.index]]
    pd.set_option("display.width", 120)
    print("\nAverage per poll week (weeks 3-14) in a season the model never saw:\n")
    print(table.to_string())
    print("\ntop25_hit / exact: out of 25, higher is better | avg_miss / rmse: lower is better")
