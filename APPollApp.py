#!/usr/bin/env python
# coding: utf-8

# In[2]:


import streamlit as st
import pandas as pd
import lightgbm as lgb
st.set_page_config(page_title="CFB Poll Predictor", page_icon="🏈", layout="wide")


# In[18]:


DATA_PATH = "data/CFBPollDataFinal.csv"
MODEL_PATH = "model/CFBLightGBM.txt"

DROP_COLS = ["total_wins_before", "total_losses_before", "team", "rank", "poll_point_diff"]
EXCLUDE_FROM_FEATURES = ["next_week_points", "year", "week"] + DROP_COLS
CATS = ["conference", "opponent_conference", "result", "bias", "opponent"]

# --- Loaders ---
@st.cache_data
def load_data():
    return pd.read_csv(DATA_PATH)

@st.cache_resource
def load_model():
    return lgb.Booster(model_file=MODEL_PATH)


# In[19]:


df = load_data()
model = load_model()

FEATURE_COLS = [col for col in df.columns if col not in EXCLUDE_FROM_FEATURES]


# In[20]:


# --- Page config ---
st.set_page_config(page_title="CFB Poll Predictor", page_icon="🏈", layout="wide")
st.title("🏈 Sports Analytics Club at FSU AP Poll Predictor")

# --- Selectors ---
col1, col2 = st.columns(2)
with col1:
    year = st.selectbox("Year", sorted(df["year"].unique(), reverse=True))
with col2:
    available_weeks = sorted(
        w for w in df[df["year"] == year]["week"].unique()
        if 3 <= w <= 14
    )
    week = st.selectbox("Week", available_weeks, index=len(available_weeks) - 1)

# --- Filter to selected week ---
current = df[(df["year"] == year) & (df["week"] == week)].copy()

if current.empty:
    st.warning("No data found for that year/week.")
    st.stop()

# --- Cast categoricals ---
for col in CATS:
    current[col] = current[col].astype("category")

# --- Predict ---
current["predicted_points"] = model.predict(current[FEATURE_COLS])

# Predicted rank (1 = highest predicted points)
current["model_rank"] = (
    current["predicted_points"]
    .rank(method="min", ascending=False)
    .astype(int)
)

# Is next week's poll out yet? (NA = this is the live, not-yet-released week)
poll_out = current["next_week_points"].notna().any()

# Actual next week rank
current["actual_rank"] = (
    current["next_week_points"]
    .rank(method="min", ascending=False)
)

# Only ranked teams get numeric rank
current["actual_rank"] = current["actual_rank"].where(
    current["next_week_points"] > 0
)

current["actual_rank"] = current["actual_rank"].fillna("Unranked" if poll_out else "TBD")

# --- Build display table ---
display = current[[
    "team",
    "rank",
    "model_rank",
    "actual_rank",
    "poll_points",
    "result",
    "pts_scored",
    "pts_allowed",
    "opponent",
    "predicted_points",
    "next_week_points"
]].copy()

display = display.sort_values("predicted_points", ascending=False).reset_index(drop=True)
display.index += 1  # rank starts at 1

display.columns = [
    "Team",
    "Prev Rank",
    "Model Rank",
    "Actual Rank",
    "Prev Poll Pts",
    "Result",
    "Pts Scored",
    "Pts Allowed",
    "Opponent",
    "Predicted Pts",
    "Actual Poll Pts"
]

display = display.head(25)

display["Actual Rank"] = pd.to_numeric(display["Actual Rank"], errors="coerce")
display["Actual Rank"] = display["Actual Rank"].astype("Int64")

display["Pts Scored"] = pd.to_numeric(display["Pts Scored"], errors="coerce").astype("Int64")
display["Pts Allowed"] = pd.to_numeric(display["Pts Allowed"], errors="coerce").astype("Int64")
display["Prev Rank"] = pd.to_numeric(display["Prev Rank"], errors="coerce").astype("Int64")
display["Prev Rank"] = display["Prev Rank"].astype(str).replace("<NA>", "NR")
# --- Metrics ---
st.subheader(f"Week {week}, {year}")

if poll_out:
    m1, m2 = st.columns(2)
    m1.metric("Correctly Ranked Teams", int((current["model_rank"] == current["actual_rank"]).sum()))
    # Average miss for the teams actually in next week's Top 25 (teams ranked
    # 26+ with a handful of votes are left out; their order is mostly noise)
    actual_num = pd.to_numeric(current["actual_rank"], errors="coerce")
    top25 = actual_num <= 25
    rank_diff = (current["model_rank"][top25] - actual_num[top25]).abs().mean()

    m2.metric("Avg Rank Difference (Top 25)", f"{rank_diff:.1f}")
else:
    st.info(f"The Week {week + 1} AP poll isn't out yet. This is the model's prediction for it.")

st.divider()

# --- Color code Result ---
def style_result(val):
    if val == "W":
        return "color: green; font-weight: bold"
    elif val == "L":
        return "color: red; font-weight: bold"
    return ""

st.dataframe(
    display.style.map(style_result, subset=["Result"]),
    use_container_width=True
)

# --- Week 1 note ---
if week == 1:
    st.info("No previous week poll data available for week 1.")

current_display = current.copy()
current_display["actual_rank"] = current_display["actual_rank"].astype(str)
# --- Raw data expander ---
with st.expander("Show raw data for this week"):
    st.dataframe(current, use_container_width=True)

