library(cfbfastR)
library(tidyverse)

poll_data <- read.csv("CFBPollDataFeb16.csv")


poll_name_map <- c(
  "Miami" = "Miami (FL)"
)

poll_data <- poll_data %>%
  mutate(team = str_trim(team))


years <- 2014:2024
games <- purrr::map_dfr(years, function(y) {
  cfbd_game_info(year = y, division = "fbs")
})


home <- games %>%
  filter(!is.na(home_points), !is.na(away_points)) %>%
  transmute(
    year = season,
    week,
    team = home_team,
    opponent = away_team,
    conference = home_conference,
    opponent_conference = away_conference,
    win = as.integer(home_points > away_points),
    loss = as.integer(home_points < away_points)
  )

away <- games %>%
  filter(!is.na(home_points), !is.na(away_points)) %>%
  transmute(
    year = season,
    week,
    team = away_team,
    opponent = home_team,
    conference = away_conference,
    opponent_conference = home_conference,
    win = as.integer(away_points > home_points),
    loss = as.integer(away_points < home_points)
  )

team_games <- bind_rows(home, away)

conference_lookup <- team_games %>%
  select(year, team, conference) %>%
  distinct()


weekly_records <- team_games %>%
  arrange(year, team, week) %>%
  group_by(year, team) %>%
  mutate(
    total_wins = cumsum(win),
    total_losses = cumsum(loss),
    record = paste0(total_wins, "-", total_losses)
  ) %>%
  ungroup() %>%
  filter(year != 2020) %>%
  mutate(team = recode(team, !!!poll_name_map))


max_weeks <- weekly_records %>%
  group_by(year) %>%
  summarise(max_week = max(week), .groups = "drop")

weekly_records_full <- weekly_records %>%
  left_join(max_weeks, by = "year") %>%
  group_by(year, team) %>%
  complete(week = 1:first(max_week)) %>%
  arrange(year, team, week) %>%
  fill(total_wins, total_losses, .direction = "down") %>%
  mutate(
    total_wins = replace_na(total_wins, 0),
    total_losses = replace_na(total_losses, 0),
    record = paste0(total_wins, "-", total_losses),
    ) %>%
    ungroup() %>%
    select(-c(max_week, conference))

weekly_records_full <- weekly_records_full %>%
  left_join(conference_lookup, by = c("year", "team"))

opponent_lookup <- weekly_records_full %>%
  arrange(year, team, week) %>%
  group_by(year, team) %>%
  mutate(
    opp_wins_entering = lag(total_wins, default = 0),
    opp_losses_entering = lag(total_losses, default = 0)
  ) %>%
  ungroup() %>%
  select(year, week, team,
         opp_wins_entering,
         opp_losses_entering,
         conference)

team_games <- team_games %>%
  left_join(
    opponent_lookup,
    by = c("year", "week", "opponent" = "team")
  )


team_records <- team_games %>%
  arrange(year, team, week) %>%
  group_by(year, team) %>%
  mutate(
    total_wins = lag(cumsum(win), default = 0),
    total_losses = lag(cumsum(loss), default = 0),
    record = paste0(total_wins, "-", total_losses)
  ) %>%
  ungroup() %>%
  select(year, week, team,
         total_wins, total_losses, record)

team_games <- team_games %>%
  left_join(
    team_records,
    by = c("year",
           "week",
           "opponent" = "team")
  ) %>%
  rename(
    opponent_wins = total_wins,
    opponent_losses = total_losses,
    opponent_record = record
  )
View(team_games)
View(weekly_records_full)
View(poll_data)
write.csv(weekly_records_full, "CFBTeamResults.csv")

final_data <- poll_data %>%
  left_join(
    weekly_records_full,
    by = c("team", "year", "week")
  ) %>%
  arrange(year, week, Rank) %>%
  select(-X)

View(final_data)
View(weekly_records_full)
