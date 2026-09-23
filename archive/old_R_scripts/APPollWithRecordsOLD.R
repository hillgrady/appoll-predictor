library(cfbfastR)
library(tidyverse)

years <- 2014:2024

games <- purrr::map_dfr(years, function(y) {
  cfbd_game_info(year = y, division = "fbs")
})
poll_data <- read.csv("CFBPollDataFeb16.csv")
team_games <- games %>%
  filter(!is.na(home_points), !is.na(away_points)) %>%
  select(season, week, home_team, away_team, home_points, away_points) %>%
  pivot_longer(
    cols = c(home_team, away_team),
    names_to = "location",
    values_to = "team"
  ) %>%
  mutate(
    points_for = ifelse(location == "home_team", home_points, away_points),
    points_against = ifelse(location == "home_team", away_points, home_points),
    win = as.integer(points_for > points_against),
    loss = as.integer(points_for < points_against)
  ) %>%
  select(season, week, team, win, loss)

View(weekly_records)

weekly_records <- team_games %>%
  arrange(season, team, week) %>%
  group_by(season, team) %>%
  mutate(
    total_wins = cumsum(win),
    total_losses = cumsum(loss),
    record = paste0(total_wins, "-", total_losses)
  ) %>%
  ungroup()




weekly_records <- weekly_records %>%
  rename(year = season) %>%
  filter(year != 2020)

max_poll_weeks <- poll_data %>%
  group_by(year) %>%
  summarise(max_week = max(week), .groups = "drop")

weekly_records_full <- weekly_records %>%
  left_join(max_poll_weeks, by = "year") %>%
  group_by(year, team) %>%
  complete(week = 1[first(max_week)]) %>%
  arrange(year, team, week) %>%
  fill(total_wins, total_losses, .direction = "down") %>%
  mutate(
    total_wins = replace_na(total_wins, 0),
    total_losses = replace_na(total_losses, 0),
    record = paste0(total_wins, "-", total_losses)
  ) %>%
  ungroup() %>%
  select(-max_week)


weekly_records_full <- weekly_records_full %>%
  mutate(team = recode(team,
                       "Miami" = "Miami (FL)"))

final_data <- poll_data %>%
  left_join(
    weekly_records_full,
    by = c("team", "year", "week")
  )


View(final_data)

mistakes <- final_data %>%
  filter(is.na(record))
View(mistakes)
