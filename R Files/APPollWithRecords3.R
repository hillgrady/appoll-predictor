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
    opponent_conference = away_conference,
    pts_scored = home_points,
    pts_allowed = away_points,
    pt_diff = home_points - away_points,
    win = as.integer(home_points > away_points),
    loss = as.integer(home_points < away_points)
  )

away <- games %>%
  transmute(
    year = season,
    week,
    team = away_team,
    opponent = home_team,
    conference = away_conference,
    opponent_conference = home_conference,
    pts_scored = away_points,
    pts_allowed = home_points,
    pt_diff = away_points - home_points,
    win = as.integer(away_points > home_points),
    loss = as.integer(away_points < home_points)
  )

team_games <- bind_rows(home, away) %>%
  mutate(
    team = recode(team, !!!poll_name_map),
    opponent = recode(opponent, !!!poll_name_map)
  ) %>%
  filter(year != 2020)

team_games <- team_games %>%
  arrange(year, team, week) %>%
  group_by(year, team) %>%
  mutate(
    total_wins = lag(cumsum(win), default = 0),
    total_losses = lag(cumsum(loss), default = 0),
    record = paste0(total_wins, "-", total_losses),
    result = case_when(
      win == 1 ~ "W",
      loss == 1 ~ "L",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

max_weeks <- team_games %>%
  group_by(year) %>%
  summarise(max_week = max(week), .groups = "drop")

weekly_full <- team_games %>%
  left_join(max_weeks, by = "year") %>%
  group_by(year, team) %>%
  complete(week = 1:first(max_week)) %>%
  arrange(year, team, week) %>%
  fill(
    total_wins, total_losses, record, conference,
    .direction = "down"
  ) %>%
  mutate(
    total_wins = replace_na(total_wins, 0),
    total_losses = replace_na(total_losses, 0),
    record = paste0(total_wins, "-", total_losses)
  ) %>%
  ungroup()


weekly_full <- weekly_full %>%
  left_join(
    team_games %>%
      select(
        year, week, team,
        opponent,
        opponent_conference,
        pts_scored,
        pts_allowed,
        pt_diff,
        result
      ),
    by = c("year", "week", "team")
  )

opponent_lookup <- weekly_full %>%
  select(
    year, week, team,
    opponent_wins = total_wins,
    opponent_losses = total_losses,
    opponent_record = record
  )

View(weekly_full)
colnames(weekly_full)
weekly_full <- weekly_full %>%
  left_join(
    opponent_lookup,
    by = c("year", "week", "opponent.x" = "team")
  )


final_data <- poll_data %>%
  full_join(
    weekly_full,
    by = c("team", "year", "week")
  ) %>%
  arrange(year, week, Rank)

final_data <- final_data %>%
  rename(
    poll_points = points,
    rank = Rank
  ) %>%
  mutate(
    undefeated = as.integer(total_losses == 0 & (is.na(result.x) | result.x != "L"))
  )

View(final_data)

colnames(final_data)

final_data <- final_data %>%
  select(team, year, week, conference, poll_points, rank, result.x, pts_scored.x, pts_allowed.x, pt_diff.x, opponent.x, opponent_conference.x, opponent_wins, opponent_losses, opponent_points, next_week_points, poll_point_diff, bias, undefeated, total_wins, total_losses)

final_data$poll_points[is.na(final_data$poll_points)] <- 0
final_data$next_week_points[is.na(final_data$next_week_points)] <- 0
final_data$opponent_points[is.na(final_data$opponent_points)] <- 0
final_data <- final_data %>%
  rename(
    pts_scored = pts_scored.x,
    pts_allowed = pts_allowed.x,
    pt_diff = pt_diff.x,
    opponent = opponent.x,
    opponent_conference = opponent_conference.x,
  )
final_data <- final_data %>%
  rename(result = result.x)
write.csv(weekly_full, "CFBTeamResults.csv", row.names = FALSE)
write.csv(final_data, "CFBPollDataFinal.csv", row.names = FALSE)

final_data <- read.csv("CFBPollDataFinal.csv")
final_data <- final_data %>%
  mutate(
    bias = case_when(
      team %in% c("Alabama", "Ohio State") ~ 5,
      team %in% c("Georgia", "Michigan", "Texas", "LSU", "Notre Dame", "Clemson") ~ 4,
      team %in% c("Texas A&M", "Miami (FL)") ~ 3,
      team %in% c("USC", "Tennessee", "Florida") ~ 2,
      team %in% c("Auburn", "Florida State", "Nebraska") ~ 1,
      TRUE ~ 0,
    )
  )
