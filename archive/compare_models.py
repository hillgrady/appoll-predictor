"""
compare_models.py - old model file vs retrained model file, scored the app's way

Both models predict the SAME data (the current CFBPollDataFinal.csv), and each
week is scored exactly like APPollApp.py does:
  correct    "Correctly Ranked Teams"  (model rank == actual rank)
  avg_diff   "Avg Rank Difference"     (over teams that got votes next week)
  top25      how many of the real next-week Top 25 are in the model's Top 25
Averaged over weeks 3-14 of each season. Nothing is saved or changed.

The OLD model file is read from the main branch (what's live on GitHub now).

    source venv/bin/activate
    python compare_models.py
"""
import subprocess
import tempfile

import pandas as pd
import lightgbm as lgb

DROP_COLS = ["total_wins_before", "total_losses_before", "team", "rank", "poll_point_diff"]
EXCLUDE = ["next_week_points", "year", "week"] + DROP_COLS
CATS = ["conference", "opponent_conference", "result", "bias", "opponent"]


def load_old_model():
    txt = subprocess.run(["git", "show", "main:CFBLightGBM.txt"],
                         capture_output=True, text=True, check=True).stdout
    f = tempfile.NamedTemporaryFile("w", suffix=".txt", delete=False)
    f.write(txt)
    f.close()
    return lgb.Booster(model_file=f.name)


def score_week(model, current, feature_cols):
    current = current.copy()
    for c in CATS:                                   # same as the app
        current[c] = current[c].astype("category")
    current["pred"] = model.predict(current[feature_cols])
    current["model_rank"] = current["pred"].rank(method="min", ascending=False)
    actual = current["next_week_points"].rank(method="min", ascending=False)
    current["actual_rank"] = actual.where(current["next_week_points"] > 0)
    real_top = set(current.nlargest(25, "next_week_points").team)
    pred_top = set(current.nsmallest(25, "model_rank").team)
    return dict(
        correct=int((current["model_rank"] == current["actual_rank"]).sum()),
        avg_diff=(current["model_rank"] - current["actual_rank"]).abs().mean(),
        diff_top25=(current["model_rank"] - current["actual_rank"])[current["actual_rank"] <= 25].abs().mean(),
        diff_26plus=(current["model_rank"] - current["actual_rank"])[current["actual_rank"] > 25].abs().mean(),
        top25=len(real_top & pred_top),
    )


def main():
    df = pd.read_csv("CFBPollDataFinal.csv")
    feature_cols = [c for c in df.columns if c not in EXCLUDE]
    models = {"OLD": load_old_model(), "NEW": lgb.Booster(model_file="CFBLightGBM.txt")}
    rows = []
    for (y, w), cur in df.groupby(["year", "week"]):
        if not 3 <= w <= 14 or cur["next_week_points"].isna().all():
            continue
        for name, m in models.items():
            rows.append(dict(year=y, model=name, **score_week(m, cur, feature_cols)))
    res = (pd.DataFrame(rows).groupby(["year", "model"]).mean()
           .unstack("model").round(2))
    res.columns = [f"{metric}_{model}" for metric, model in res.columns]
    order = [f"{m}_{k}" for m in ("correct", "top25", "avg_diff", "diff_top25", "diff_26plus")
             for k in ("OLD", "NEW")]
    pd.set_option("display.width", 140)
    print("\nAverage per week (weeks 3-14), same data, scored like the app:\n")
    print(res[order].to_string())
    print("\ncorrect / top25: higher is better | avg_diff: lower is better")
    print("diff_top25 = avg rank miss for the real Top 25; diff_26plus = teams ranked 26+ (a few votes)")
    print("OLD model was trained on 2014-2024 (old CSV); NEW on 2014-2026 (new CSV).")

    # Biggest NEW-model misses in the most recent week, with OLD for comparison
    last = df[df.next_week_points.notna() & df.week.between(3, 14)] \
        .sort_values(["year", "week"]).iloc[-1]
    y, w = int(last.year), int(last.week)
    cur = df[(df.year == y) & (df.week == w)].copy()
    for c in CATS:
        cur[c] = cur[c].astype("category")
    for name, m in models.items():
        cur[f"rank_{name}"] = pd.Series(m.predict(cur[feature_cols]), index=cur.index) \
            .rank(method="min", ascending=False)
    cur["actual"] = cur.next_week_points.rank(method="min", ascending=False) \
        .where(cur.next_week_points > 0)
    cur["miss_NEW"] = (cur.rank_NEW - cur.actual).abs()
    print(f"\nBiggest misses, {y} week {w} (NEW model):")
    print(cur[cur.actual.notna()].nlargest(10, "miss_NEW")[["team", "poll_points", "result", "opponent",
          "next_week_points", "actual", "rank_OLD", "rank_NEW"]].to_string(index=False))


if __name__ == "__main__":
    main()
