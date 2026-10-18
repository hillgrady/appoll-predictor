import html
import os

import altair as alt
import lightgbm as lgb
import pandas as pd
import streamlit as st

from team_logos import get_logo_url, BLANK_LOGO

st.set_page_config(page_title="CFB Poll Predictor", page_icon="🏈", layout="wide")

DATA_PATH = "data/CFBPollDataFinal.csv"
MODEL_PATH = "model/CFBLightGBM.txt"

DROP_COLS = ["total_wins_before", "total_losses_before", "team", "rank", "poll_point_diff"]
EXCLUDE_FROM_FEATURES = ["next_week_points", "year", "week"] + DROP_COLS
CATS = ["conference", "opponent_conference", "result", "bias", "opponent"]


def file_version(path):
    stat = os.stat(path)
    return (stat.st_mtime, stat.st_size)


@st.cache_data
def load_data(version):
    return pd.read_csv(DATA_PATH)


@st.cache_resource
def load_model(version):
    return lgb.Booster(model_file=MODEL_PATH)


df = load_data(file_version(DATA_PATH))
model = load_model(file_version(MODEL_PATH))

FEATURE_COLS = [col for col in df.columns if col not in EXCLUDE_FROM_FEATURES]

GARNET = "#782F40"
GARNET_DARK = "#4f1f2c"
GOLD = "#CEB888"
PAGE_BG = "#0d0d0d"
SURFACE = "#1a1a19"
BORDER = "rgba(255,255,255,0.10)"
TEXT_PRIMARY = "#f0efec"
TEXT_SECONDARY = "#c3c2b7"
TEXT_MUTED = "#898781"
GOOD = "#0ca30c"
CRITICAL = "#d03b3b"

CUSTOM_CSS = f"""
<style>
html, body, .stApp {{
    background-color: {PAGE_BG} !important;
    color: {TEXT_PRIMARY};
    font-family: system-ui, -apple-system, "Segoe UI", sans-serif;
}}
section[data-testid="stSidebar"] {{
    background-color: {SURFACE} !important;
    border-right: 1px solid {BORDER};
}}
section[data-testid="stSidebar"] * {{ color: {TEXT_PRIMARY}; }}
.block-container {{ padding-top: 2rem; max-width: 1100px; }}

.app-header {{
    background: linear-gradient(135deg, {GARNET} 0%, {GARNET_DARK} 100%);
    padding: 1.5rem 2rem;
    border-radius: 16px;
    margin-bottom: 1.25rem;
    border: 1px solid {BORDER};
}}
.app-header h1 {{ margin: 0; font-size: 1.8rem; color: #ffffff; }}
.app-header .subtitle {{
    color: {GOLD}; font-weight: 600; margin-top: 0.3rem; font-size: 0.95rem;
    letter-spacing: 0.02em;
}}

.stat-row {{ display: flex; gap: 1rem; margin-bottom: 1.25rem; flex-wrap: wrap; }}
.stat-tile {{
    background: {SURFACE}; border: 1px solid {BORDER}; border-radius: 12px;
    padding: 1rem 1.25rem; flex: 1; min-width: 220px;
}}
.stat-tile .label {{ color: {TEXT_SECONDARY}; font-size: 0.8rem; margin-bottom: 0.35rem; }}
.stat-tile .value {{ color: {TEXT_PRIMARY}; font-size: 1.9rem; font-weight: 600; }}

.info-banner {{
    background: {SURFACE}; border: 1px solid {GOLD}; border-left: 4px solid {GOLD};
    border-radius: 10px; padding: 0.85rem 1.1rem; margin-bottom: 1.25rem;
    color: {TEXT_SECONDARY}; font-size: 0.92rem;
}}

.team-card {{
    display: flex; align-items: center; gap: 1rem;
    background: {SURFACE}; border: 1px solid {BORDER}; border-radius: 14px;
    padding: 0.85rem 1.1rem; margin-bottom: 0.6rem;
}}
.rank-badge {{
    flex: 0 0 42px; height: 42px; border-radius: 50%; background: {GARNET};
    color: #ffffff; display: flex; align-items: center; justify-content: center;
    font-weight: 700; font-size: 1.05rem;
}}
.team-logo {{ width: 40px; height: 40px; object-fit: contain; flex: 0 0 40px; }}
.team-main {{ flex: 1 1 auto; min-width: 180px; }}
.team-name {{ font-weight: 700; font-size: 1.05rem; color: {TEXT_PRIMARY}; }}
.team-sub {{
    font-size: 0.8rem; color: {TEXT_SECONDARY}; display: flex;
    align-items: center; gap: 0.4rem; margin-top: 0.15rem;
}}
.opp-logo {{ width: 16px; height: 16px; object-fit: contain; vertical-align: middle; }}
.result-chip {{ font-weight: 700; }}
.result-chip.good {{ color: {GOOD}; }}
.result-chip.critical {{ color: {CRITICAL}; }}

.badge-row {{ display: flex; gap: 0.5rem; flex-wrap: wrap; flex: 0 0 auto; }}
.badge {{
    font-size: 0.72rem; padding: 0.22rem 0.6rem; border-radius: 999px;
    background: rgba(255,255,255,0.06); color: {TEXT_SECONDARY};
    border: 1px solid {BORDER}; white-space: nowrap;
}}
.badge.good {{ color: {GOOD}; border-color: {GOOD}; }}
.badge.accent {{ color: {GOLD}; border-color: {GOLD}; }}

.move {{
    font-size: 0.72rem; font-weight: 700; padding: 0.22rem 0.55rem; border-radius: 999px;
    border: 1px solid {BORDER}; white-space: nowrap; color: {TEXT_SECONDARY};
}}
.move.up {{ color: {GOOD}; border-color: {GOOD}; }}
.move.down {{ color: {CRITICAL}; border-color: {CRITICAL}; }}
.move.new {{ color: {GOLD}; border-color: {GOLD}; }}

.meter-wrap {{ flex: 0 0 150px; text-align: right; }}
.meter-value {{ font-weight: 600; color: {TEXT_PRIMARY}; font-size: 0.95rem; }}
.meter-track {{
    height: 6px; border-radius: 3px; background: rgba(120,47,64,0.30);
    margin-top: 0.35rem; overflow: hidden;
}}
.meter-fill {{ height: 100%; background: linear-gradient(90deg, {GARNET}, {GOLD}); }}

@media (max-width: 640px) {{
    .block-container {{ padding-top: 1rem; padding-left: 0.75rem; padding-right: 0.75rem; }}

    .app-header {{ padding: 1rem 1.25rem; border-radius: 12px; }}
    .app-header h1 {{ font-size: 1.25rem; line-height: 1.3; }}
    .app-header .subtitle {{ font-size: 0.78rem; }}

    .stat-row {{ gap: 0.6rem; }}
    .stat-tile {{ min-width: 0; flex: 1 1 calc(50% - 0.3rem); padding: 0.75rem 0.9rem; }}
    .stat-tile .label {{ font-size: 0.72rem; }}
    .stat-tile .value {{ font-size: 1.4rem; }}

    .team-card {{
        flex-wrap: wrap;
        gap: 0.6rem;
        padding: 0.7rem 0.85rem;
    }}
    .rank-badge {{ flex: 0 0 32px; height: 32px; font-size: 0.85rem; }}
    .team-logo {{ width: 30px; height: 30px; flex: 0 0 30px; }}
    .team-main {{ flex: 1 1 calc(100% - 78px); min-width: 0; }}
    .team-name {{ font-size: 0.95rem; }}
    .team-sub {{ font-size: 0.74rem; flex-wrap: wrap; }}

    .badge-row {{ flex: 1 1 100%; order: 3; margin-top: 0.1rem; }}
    .badge {{ font-size: 0.68rem; padding: 0.18rem 0.5rem; }}

    .meter-wrap {{
        flex: 1 1 100%; order: 4; text-align: left; margin-top: 0.1rem;
    }}
    .meter-track {{ max-width: 220px; }}
}}
</style>
"""

st.markdown(CUSTOM_CSS, unsafe_allow_html=True)


def stat_tile(label: str, value: str) -> str:
    return f"""<div class="stat-tile">
        <div class="label">{html.escape(label)}</div>
        <div class="value">{html.escape(value)}</div>
    </div>"""


def rank_badge(label) -> str:
    return f'<span class="badge">{html.escape(str(label))}</span>'


def movement_chip(prev_rank, model_rank) -> str:
    """Arrow showing how far the model moves a team from last week's poll."""
    if prev_rank == "NR":
        return '<span class="move new" title="Not ranked last week">NEW</span>'
    change = int(prev_rank) - int(model_rank)
    if change > 0:
        return f'<span class="move up" title="Model moves them up {change}">▲{change}</span>'
    if change < 0:
        return f'<span class="move down" title="Model moves them down {-change}">▼{-change}</span>'
    return '<span class="move" title="Same as last week">–</span>'


def team_card(row, max_predicted: float) -> str:
    team = str(row["Team"])
    logo = get_logo_url(team) or BLANK_LOGO
    opponent = str(row["Opponent"]) if pd.notna(row["Opponent"]) else ""
    opp_logo = get_logo_url(opponent) if opponent else None

    result = row["Result"]
    if result == "W":
        result_html = '<span class="result-chip good">▲ W</span>'
    elif result == "L":
        result_html = '<span class="result-chip critical">▼ L</span>'
    else:
        result_html = f'<span class="result-chip">{html.escape(str(result))}</span>'

    score = ""
    if pd.notna(row["Pts Scored"]) and pd.notna(row["Pts Allowed"]):
        score = f"{int(row['Pts Scored'])}&ndash;{int(row['Pts Allowed'])}"

    opp_html = ""
    if opponent:
        opp_logo_html = f'<img class="opp-logo" src="{opp_logo}" />' if opp_logo else ""
        opp_html = f"{opp_logo_html}vs {html.escape(opponent)} {score}"

    actual_rank = row["Actual Rank"]
    model_rank = row["Model Rank"]
    correct = str(actual_rank) == str(model_rank)
    actual_badge_class = "badge good" if correct else "badge"
    actual_label = f"✓ Actual #{actual_rank}" if correct and str(actual_rank).isdigit() else f"Actual: {actual_rank}"

    predicted = row["Predicted Pts"]
    pct = max(0.0, min(100.0, (predicted / max_predicted) * 100)) if max_predicted else 0

    prev_rank = row["Prev Rank"]
    prev_label = "Not ranked" if prev_rank == "NR" else f"Prev #{prev_rank}"

    return f"""<div class="team-card">
        <div class="rank-badge">{row.name}</div>
        <img class="team-logo" src="{logo}" />
        <div class="team-main">
            <div class="team-name">{html.escape(team)}</div>
            <div class="team-sub">{result_html}&nbsp;&middot;&nbsp;{opp_html}</div>
        </div>
        <div class="badge-row">
            {movement_chip(prev_rank, model_rank)}
            {rank_badge(prev_label)}
            {rank_badge(f"Model #{model_rank}")}
            <span class="{actual_badge_class}">{html.escape(actual_label)}</span>
        </div>
        <div class="meter-wrap">
            <div class="meter-value">{predicted:,.1f} pts</div>
            <div class="meter-track"><div class="meter-fill" style="width:{pct:.0f}%;"></div></div>
        </div>
    </div>"""


st.markdown(
    """<div class="app-header">
        <h1>🏈 Sports Analytics Club at FSU</h1>
        <div class="subtitle">AP Poll Predictor</div>
    </div>""",
    unsafe_allow_html=True,
)

with st.sidebar:
    st.markdown("### Settings")
    year = st.selectbox("Year", sorted(df["year"].unique(), reverse=True))
    available_weeks = sorted(
        w for w in df[df["year"] == year]["week"].unique()
        if 3 <= w <= 14
    )
    week = st.selectbox("Week", available_weeks, index=len(available_weeks) - 1)
    search = st.text_input("Filter by team", placeholder="e.g. Florida State")

current = df[(df["year"] == year) & (df["week"] == week)].copy()

if current.empty:
    st.warning("No data found for that year/week.")
    st.stop()

for col in CATS:
    current[col] = current[col].astype("category")

current["predicted_points"] = model.predict(current[FEATURE_COLS])

current["model_rank"] = (
    current["predicted_points"]
    .rank(method="min", ascending=False)
    .astype(int)
)

poll_out = current["next_week_points"].notna().any()

current["actual_rank"] = (
    current["next_week_points"]
    .rank(method="min", ascending=False)
)

current["actual_rank"] = current["actual_rank"].where(
    current["next_week_points"] > 0
)

current["actual_rank"] = current["actual_rank"].fillna("Unranked" if poll_out else "TBD")

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
display.index += 1

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

display["Prev Rank"] = pd.to_numeric(display["Prev Rank"], errors="coerce")
display["Prev Rank"] = display["Prev Rank"].apply(lambda x: "NR" if pd.isna(x) else str(int(x)))

display["Actual Rank"] = display["Actual Rank"].astype(object)
mask_blank = display["Actual Rank"].isna()
display.loc[mask_blank, "Actual Rank"] = "Unranked" if poll_out else "TBD"

if search:
    display_filtered = display[display["Team"].str.contains(search, case=False, na=False)]
else:
    display_filtered = display

tab_week, tab_season = st.tabs(["This week", "Season accuracy"])

with tab_week:
    st.markdown(f"#### Week {week}, {year}")

    if poll_out:
        correctly_ranked = int((current["model_rank"] == current["actual_rank"]).sum())
        actual_num = pd.to_numeric(current["actual_rank"], errors="coerce")
        top25 = actual_num <= 25
        rank_diff = (current["model_rank"][top25] - actual_num[top25]).abs().mean()

        st.markdown(
            f"""<div class="stat-row">
                {stat_tile("Correctly ranked teams", str(correctly_ranked))}
                {stat_tile("Avg rank difference (top 25)", f"{rank_diff:.1f}")}
            </div>""",
            unsafe_allow_html=True,
        )
    else:
        st.markdown(
            f"""<div class="info-banner">
                The Week {week + 1} AP poll isn't out yet. This is the model's prediction for it.
            </div>""",
            unsafe_allow_html=True,
        )

    if week == 1:
        st.markdown(
            '<div class="info-banner">No previous week poll data available for week 1.</div>',
            unsafe_allow_html=True,
        )

    max_predicted = display_filtered["Predicted Pts"].max() if not display_filtered.empty else 0

    if display_filtered.empty:
        st.info("No teams match that filter.")
    else:
        cards_html = "".join(team_card(row, max_predicted) for _, row in display_filtered.iterrows())
        st.markdown(cards_html, unsafe_allow_html=True)

    with st.expander("Show raw data for this week"):
        current_display = current.copy()
        current_display["actual_rank"] = current_display["actual_rank"].astype(str)
        st.dataframe(current_display, use_container_width=True)


@st.cache_data
def season_accuracy(_model, data_version, year):
    """Week-by-week top-25 accuracy for the model vs a 'last week's poll' baseline."""
    rows = []
    season = df[(df["year"] == year) & df["week"].between(3, 14)]
    for wk, g in season.groupby("week"):
        if g["next_week_points"].isna().all():
            continue
        g = g.copy()
        for col in CATS:
            g[col] = g[col].astype("category")
        pred = pd.Series(_model.predict(g[FEATURE_COLS]), index=g.index)
        model_rank = pred.rank(method="min", ascending=False)
        base_rank = g["poll_points"].rank(method="min", ascending=False)
        actual = g["next_week_points"]
        actual_rank = actual.rank(method="min", ascending=False).where(actual > 0)
        top = actual_rank <= 25
        rows.append({
            "Week": int(wk),
            "Model": (model_rank[top] - actual_rank[top]).abs().mean(),
            "Last week's poll": (base_rank[top] - actual_rank[top]).abs().mean(),
            "Model exact": int((model_rank[top] == actual_rank[top]).sum()),
            "Baseline exact": int((base_rank[top] == actual_rank[top]).sum()),
        })
    return pd.DataFrame(rows)


with tab_season:
    acc = season_accuracy(model, file_version(DATA_PATH), year)
    st.markdown(f"#### {year} season accuracy")
    if acc.empty:
        st.info("No weeks with a released poll yet this season.")
    else:
        m, b = acc["Model"].mean(), acc["Last week's poll"].mean()
        better = int((acc["Model"] < acc["Last week's poll"]).sum())
        st.markdown(
            f"""<div class="stat-row">
                {stat_tile("Model avg rank error", f"{m:.2f}")}
                {stat_tile("Last week's poll avg error", f"{b:.2f}")}
                {stat_tile("Weeks model beat baseline", f"{better} / {len(acc)}")}
            </div>""",
            unsafe_allow_html=True,
        )
        st.caption(
            "Average rank error for teams in the actual top 25 (lower is better). "
            "The baseline just assumes next week's poll matches this week's."
        )
        long = acc.melt("Week", ["Model", "Last week's poll"], var_name="Series", value_name="Avg rank error")
        chart = (
            alt.Chart(long)
            .mark_line(point=alt.OverlayMarkDef(size=60), strokeWidth=2.5)
            .encode(
                x=alt.X("Week:O", title="Week", axis=alt.Axis(labelAngle=0)),
                y=alt.Y("Avg rank error:Q", title="Avg rank error (top 25)", scale=alt.Scale(zero=True)),
                color=alt.Color(
                    "Series:N",
                    scale=alt.Scale(domain=["Model", "Last week's poll"], range=[GOLD, TEXT_MUTED]),
                    legend=alt.Legend(orient="top", title=None),
                ),
                strokeDash=alt.StrokeDash(
                    "Series:N",
                    scale=alt.Scale(domain=["Model", "Last week's poll"], range=[[1, 0], [5, 4]]),
                    legend=None,
                ),
                tooltip=["Week", "Series", alt.Tooltip("Avg rank error:Q", format=".2f")],
            )
            .properties(height=320)
            .configure(background="transparent")
            .configure_axis(labelColor=TEXT_SECONDARY, titleColor=TEXT_SECONDARY,
                            gridColor="rgba(255,255,255,0.08)", domainColor=BORDER)
            .configure_legend(labelColor=TEXT_PRIMARY)
            .configure_view(strokeWidth=0)
        )
        st.altair_chart(chart, use_container_width=True)

        table = acc.rename(columns={
            "Model": "Model avg error", "Last week's poll": "Baseline avg error",
            "Model exact": "Model exact ranks", "Baseline exact": "Baseline exact ranks",
        })
        st.dataframe(
            table.style.format({"Model avg error": "{:.2f}", "Baseline avg error": "{:.2f}"}),
            use_container_width=True, hide_index=True,
        )
