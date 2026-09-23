# =============================================================================
# 02_build_features.R  —  AP Poll Predictor, step 2 of 3
#
# Turns the cached polls + games from 01 into CFBPollDataFinal.csv: one row per
# team per week, with the same 24 columns (same names, same order, same
# conventions) that APPollLightGBM2.ipynb and APPollApp.py already expect.
#
# Replaces: APPollCleaning2.R, APPollWithRecords3.R, APPollAddingStreak.R,
#           APPollFillingNAs.R (plus the abandoned drafts of each)
#
# Column conventions kept from the original data:
#   * Poll "week N" = poll released before week-N games. next_week_points is
#     the week N+1 poll, i.e. the poll after this week's game (the target).
#   * Bye weeks: opponent / opponent_conference / result = "BYE",
#     game stats and opponent record NA.
#   * poll_points, next_week_points, opponent_points: 0 when unranked.
#   * rank: rank by points among every team receiving votes (so it can be > 25).
#   * poll_point_diff: only when the team got votes this week AND next week.
#   * opponent_wins/losses: opponent's record entering the game.
#   * total_*_before / total_*_after: team record before / after this week.
#   * streak: + for wins, - for losses, after this week; carries through byes;
#     resets each season.
#
# Fixes compared with the old scripts:
#   * one row per team-week (the old joins produced hundreds of duplicates a
#     year; two games in one week now count in the record, and the later game
#     is the one shown on the row)
#   * only FBS teams plus anyone receiving votes (the old grid padded every FCS
#     opponent out to a full season, which doubled the team count from 2022)
#   * name fixes applied in one place (NAME_MAP)
#   * no dependence on objects left in the R session or missing CSVs
# =============================================================================

library(tidyverse)

if (!file.exists("APPollApp.py"))
  stop("Set the working directory to the project root (the APPollModel folder):\n",
       "  Session > Set Working Directory, or setwd(\"~/Desktop/Sports Data/APPollModel\")")

DATA_DIR <- file.path("pipeline", "cache")
# Written to the cache first, NOT over the live file. 03_validate.R checks it
# and copies it to data/CFBPollDataFinal.csv only if every check passes.
OUT_CSV  <- file.path(DATA_DIR, "CFBPollDataFinal.csv")

KEEP_ONLY_FBS <- TRUE   # FALSE = old behaviour (keep every team in FBS games)

# cfbfastR name -> collegepolltracker name. 03_validate.R lists any other poll
# team that failed to match, so add to this as needed.
NAME_MAP <- c(
  "Miami"          = "Miami (FL)",
  "San José State" = "San Jose State",
  "App State"      = "Appalachian State"
)

# Conferences whose teams count as FBS (as cfbfastR names them)
FBS_CONFERENCES <- c("ACC", "Big Ten", "Big 12", "SEC", "Pac-12",
                     "American Athletic", "Mountain West", "Sun Belt",
                     "Conference USA", "Mid-American", "FBS Independents")

# "Historical perception" bias agreed in the R&D meeting (from APPollCleaning2.R)
bias_for <- function(team) {
  case_when(
    team %in% c("Alabama", "Ohio State") ~ 5L,
    team %in% c("Georgia", "Michigan", "Texas", "LSU", "Notre Dame", "Clemson") ~ 4L,
    team %in% c("Texas A&M", "Miami (FL)") ~ 3L,
    team %in% c("USC", "Tennessee", "Florida") ~ 2L,
    team %in% c("Auburn", "Florida State", "Nebraska") ~ 1L,
    TRUE ~ 0L
  )
}

# Running win/loss streak over a season's results ("W"/"L"/NA). NA carries.
calc_streak <- function(result) {
  out <- numeric(length(result))
  cur <- 0
  for (i in seq_along(result)) {
    r <- result[i]
    if (!is.na(r) && r == "W") cur <- if (cur > 0) cur + 1 else 1
    if (!is.na(r) && r == "L") cur <- if (cur < 0) cur - 1 else -1
    out[i] <- cur
  }
  out
}

polls <- readRDS(file.path(DATA_DIR, "polls.rds"))
status_path <- file.path(DATA_DIR, "season_status.rds")
season_status <- if (file.exists(status_path)) readRDS(status_path) else
  tibble(season = numeric(), last_complete_week = numeric())
games <- readRDS(file.path(DATA_DIR, "games.rds"))

# =============================================================================
# 1. One row per team per game
# =============================================================================
# Order games within a week (e.g. week 0 + week 1 both labelled week 1).
ord_col <- intersect(c("start_date", "game_id", "id"), names(games))[1]
games$game_order <- if (is.na(ord_col)) 0 else rank(games[[ord_col]], ties.method = "first")

team_games <- bind_rows(
  games %>% transmute(
    year = season, week, game_order,
    team = home_team, opponent = away_team,
    conference = home_conference, opponent_conference = away_conference,
    pts_scored = home_points, pts_allowed = away_points
  ),
  games %>% transmute(
    year = season, week, game_order,
    team = away_team, opponent = home_team,
    conference = away_conference, opponent_conference = home_conference,
    pts_scored = away_points, pts_allowed = home_points
  )
) %>%
  filter(!is.na(team), !is.na(opponent)) %>%
  mutate(
    team     = recode(team, !!!NAME_MAP),
    opponent = recode(opponent, !!!NAME_MAP),
    pt_diff  = pts_scored - pts_allowed,
    result   = case_when(pt_diff > 0 ~ "W", pt_diff < 0 ~ "L", TRUE ~ NA_character_),
    win      = as.integer(result %in% "W"),
    loss     = as.integer(result %in% "L")
  ) %>%
  arrange(year, team, week, game_order) %>%
  group_by(year, team) %>%
  mutate(
    wins_after   = cumsum(win),
    losses_after = cumsum(loss),
    streak_after = calc_streak(result)
  ) %>%
  ungroup()

# =============================================================================
# 2. Collapse to one row per team per week (keeps the later game if two)
# =============================================================================
team_weeks <- team_games %>%
  group_by(year, team, week) %>%
  summarise(
    wins_before   = first(wins_after - win),
    losses_before = first(losses_after - loss),
    across(c(opponent, conference, opponent_conference, pts_scored, pts_allowed,
             pt_diff, result, wins_after, losses_after, streak_after), last),
    .groups = "drop"
  )

# =============================================================================
# 3. Full week grid so bye weeks get a row, with records carried through
# =============================================================================
week_range <- team_weeks %>%
  group_by(year) %>%
  summarise(min_week = min(week), max_week = max(week), .groups = "drop")

weekly <- team_weeks %>%
  distinct(year, team) %>%
  left_join(week_range, by = "year") %>%
  mutate(week = map2(min_week, max_week, seq)) %>%
  select(year, team, week) %>%
  unnest(week) %>%
  left_join(team_weeks, by = c("year", "team", "week")) %>%
  arrange(year, team, week) %>%
  group_by(year, team) %>%
  fill(wins_after, losses_after, streak_after, .direction = "down") %>%
  fill(conference, .direction = "downup") %>%
  mutate(
    total_wins_after    = replace_na(wins_after, 0),
    total_losses_after  = replace_na(losses_after, 0),
    streak              = replace_na(streak_after, 0),
    bye                 = is.na(opponent),
    total_wins_before   = if_else(bye, total_wins_after, wins_before),
    total_losses_before = if_else(bye, total_losses_after, losses_before)
  ) %>%
  ungroup()

# Opponent's record entering the game (looked up before any FBS filtering, so
# FCS opponents still get their record)
opp_lookup <- weekly %>%
  select(year, week, opponent = team,
         opponent_wins = total_wins_before, opponent_losses = total_losses_before)

weekly <- weekly %>% left_join(opp_lookup, by = c("year", "week", "opponent"),
                              na_matches = "never")   # byes must not match

# =============================================================================
# 3b. Line up poll weeks with game weeks, season by season
# =============================================================================
# Our convention: poll week N = the poll going INTO week-N games. The site
# follows that for 2014-2025, but labels 2026 polls one week later (its
# "Week 3" poll came out after week 3's games). Each poll lists team records,
# so compare them with our records before vs after that week's games and shift
# the season's poll weeks if "after" fits better.
rec_check <- polls %>%
  mutate(rec = str_extract(record, "\\d+-\\d+")) %>%
  filter(!is.na(rec)) %>%
  separate(rec, c("pw", "pl"), sep = "-", convert = TRUE) %>%
  select(team, year, week, pw, pl) %>%
  inner_join(weekly %>% select(team, year, week,
                               bw = total_wins_before, bl = total_losses_before,
                               aw = total_wins_after,  al = total_losses_after),
             by = c("team", "year", "week"))
poll_offsets <- rec_check %>%
  group_by(year) %>%
  summarise(match_before = mean(pw == bw & pl == bl, na.rm = TRUE),
            match_after  = mean(pw == aw & pl == al, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(offset = if_else(match_after > match_before, 1, 0))
for (i in which(poll_offsets$offset == 1)) {
  message(poll_offsets$year[i], ": poll site numbers weeks one later than usual ",
          sprintf("(records match after-game %.0f%% vs before-game %.0f%%)",
                  100 * poll_offsets$match_after[i], 100 * poll_offsets$match_before[i]),
          " -> shifting that season's polls by +1 week")
}
polls <- polls %>%
  left_join(poll_offsets %>% select(year, offset), by = "year") %>%
  mutate(week = week + replace_na(offset, 0)) %>%
  select(-offset)
saveRDS(polls, file.path(DATA_DIR, "polls_aligned.rds"))   # used by 03

# =============================================================================
# 4. Polls: rank, next-week points, opponent points
# =============================================================================
polls <- polls %>%
  filter(!is.na(points), !is.na(team)) %>%
  distinct(year, week, team, .keep_all = TRUE) %>%
  group_by(year, week) %>%
  mutate(rank = rank(-points, ties.method = "min")) %>%
  ungroup()

poll_now  <- polls %>% select(team, year, week, poll_points = points, rank)
# (year, week) polls that exist. For finished seasons every week counts as
# "out" (week 16 was never scraped, and its 0s match the original data).
poll_weeks_out <- c(paste(polls$year, polls$week),
                    as.vector(outer(setdiff(unique(polls$year), season_status$season),
                                    1:20, paste)))
poll_next <- polls %>% transmute(team, year, week = week - 1, next_week_points = points)
poll_opp  <- polls %>% select(opponent = team, year, week, opponent_points = points)

poll_teams <- polls %>% distinct(year, team)

if (KEEP_ONLY_FBS) {
  fbs_teams <- weekly %>%
    filter(conference %in% FBS_CONFERENCES) %>%
    distinct(year, team)
  keep <- bind_rows(fbs_teams, poll_teams) %>% distinct()
  weekly <- weekly %>% semi_join(keep, by = c("year", "team"))
}

# =============================================================================
# 5. Assemble the final table
# =============================================================================
final_data <- weekly %>%
  full_join(poll_now, by = c("team", "year", "week")) %>%
  left_join(poll_next, by = c("team", "year", "week")) %>%
  left_join(poll_opp,  by = c("opponent", "year", "week")) %>%
  mutate(
    poll_point_diff     = next_week_points - poll_points,   # NA unless in both polls
    poll_points         = replace_na(poll_points, 0),
    # 0 = next poll is out and the team got no votes. NA = next poll not
    # released yet (the newest week of the current season): that row is what
    # the app predicts, and the notebook skips it when training.
    next_poll_out       = paste(year, week + 1) %in% poll_weeks_out,
    next_week_points    = if_else(next_poll_out, replace_na(next_week_points, 0),
                                  NA_real_),
    opponent_points     = replace_na(opponent_points, 0),
    conference          = replace_na(conference, "FCS"),
    opponent_conference = case_when(
      is.na(opponent)            ~ "BYE",
      is.na(opponent_conference) ~ "FCS",
      TRUE                       ~ opponent_conference
    ),
    result   = if_else(is.na(opponent), "BYE", result),
    opponent = replace_na(opponent, "BYE"),
    bias       = bias_for(team),
    undefeated = as.integer(total_losses_after == 0)
  ) %>%
  select(team, year, week, conference, poll_points, rank, result,
         pts_scored, pts_allowed, pt_diff, opponent, opponent_conference,
         opponent_wins, opponent_losses, opponent_points, next_week_points,
         poll_point_diff, bias, undefeated,
         total_wins_before, total_losses_before, total_wins_after,
         total_losses_after, streak) %>%
  arrange(year, week, desc(poll_points), team)

# Current season: keep only weeks whose games are all finished
for (i in seq_len(nrow(season_status))) {
  yr <- season_status$season[i]; wk <- season_status$last_complete_week[i]
  final_data <- final_data %>% filter(!(year == yr & week > wk))
  message(yr, ": rows through week ", wk,
          if (any(final_data$year == yr & final_data$week == wk & is.na(final_data$next_week_points)))
            paste0(" (week ", wk + 1, " poll not out yet -> week ", wk, " is the live prediction)")
          else "")
}

write.csv(final_data, OUT_CSV, row.names = FALSE)
message("Wrote ", OUT_CSV, ": ", nrow(final_data), " rows. Now run 03_validate.R.")
