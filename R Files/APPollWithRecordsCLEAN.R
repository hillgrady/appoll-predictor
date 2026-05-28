library(cfbfastR)
library(tidyverse)

poll_data <- read.csv("CFBPollDataFeb16.csv") %>%
  mutate(team = str_trim(team))

poll_name_map <- c("Miami" = "Miami (FL)")

years <- 2014:2024

games <- purrr::map_dfr(years, function(y) {
  cfbd_game_info(year = y, division = "fbs")
})


home <- games %>%
  transmute(
    year = season,
    week,
    team = home_team,
    opponent = away_team,
    conference = home_conference,
    win = as.integer(home_points > away_points),
    loss = as.integer(home_points < away_points),
    pts_scored = home_points,
    pts_allowed = away_points,
    pt_diff = home_points - away_points
  )

away <- games %>%
  transmute(
    year = season,
    week,
    team = away_team,
    opponent = home_team,
    conference = away_conference,
    win = as.integer(away_points > home_points),
    loss = as.integer(away_points < home_points),
    pts_scored = away_points,
    pts_allowed = home_points,
    pt_diff = away_points - home_points
  )

team_games <- bind_rows(home, away) %>%
  mutate(
    team = recode(team, !!!poll_name_map),
    opponent = recode(opponent, !!!poll_name_map)
  ) %>%
  filter(year != 2020)

team_records <- team_games %>%
  arrange(year, team, week) %>%
  group_by(year, team) %>%
  mutate(
    total_wins = lag(cumsum(win), default = 0),
    total_losses = lag(cumsum(loss), default = 0),
    record = paste0(total_wins, "-", total_losses)
  ) %>%
  ungroup()

View(team_records)

max_weeks <- team_records %>%
  group_by(year) %>%
  summarise(max_week = max(week), .groups = "drop")

weekly_records_full <- team_records %>%
  left_join(max_weeks, by = "year") %>%
  group_by(year, team) %>%
  complete(week = 1:first(max_week)) %>%
  arrange(year, team, week) %>%
  fill(total_wins, total_losses, record, conference, .direction = "down") %>%
  mutate(
    total_wins = replace_na(total_wins, 0),
    total_losses = replace_na(total_losses, 0),
    record = paste0(total_wins, "-", total_losses)
  ) %>%
  ungroup() %>%
  select(year, week, team, conference, opponent, pts_scored, pts_allowed, pt_diff,
         total_wins, total_losses, record)
View(weekly_records_full)
View(team_games)
weekly_records_full <- weekly_records_full %>%
  left_join(
    team_games %>%
      select(year, week, team, opponent, pts_scored, pts_allowed, ),
    by = c("year", "week", "team")
  )

opponent_lookup <- weekly_records_full %>%
  select(year, week, team,
         opponent_wins = total_wins,
         opponent_losses = total_losses,
         opponent_record = record,
         opponent_conference = conference)

weekly_records_full <- weekly_records_full %>%
  left_join(
    opponent_lookup,
    by = c("year", "week", "opponent" = "team")
  )

final_data <- poll_data %>%
  full_join(
    weekly_records_full,
    by = c("team", "year", "week")
  ) %>%
  arrange(year, week, Rank) %>%
  select(-X)

final_data <- final_data %>%
  select(-opponent.y) %>%
  rename(opponent = opponent.x)
View(final_data2)
View(poll_data)
View(weekly_records_full)

colnames(final_data)
final_data <- final_data %>%
  select(team, year, week, conference, points, Rank, total_wins, total_losses, result, pts_scored, pts_allowed, pt_diff, opponent, opponent_conference, opponent_wins, opponent_losses, opponent_points, next_week_points, poll_point_diff, bias) %>%
  rename(
    poll_points = points,
    rank = Rank,
    pts_scored = team_points,
    pts_allowed = opp_points,
    opponent_poll_points = opponent_points,
    next_week_poll_points = next_week_points,
    poll_point_change = poll_point_diff
  )

final_data <- final_data %>%
  mutate(
    undefeated = as.integer(total_losses == 0 & result != "L" | is.na(result))
  )

View(final_data)
write.csv(weekly_records_full, "CFBTeamResults.csv")
write.csv(final_data, "CFBPollDataFeb23.csv")
View(final_data)
View(weekly_records_full)
View(team_records)
