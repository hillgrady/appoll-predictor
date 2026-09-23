library(tidyverse)

poll_data <- read_csv("APPollData.csv")
colnames(poll_data)
View(poll_data)

poll_data <- poll_data %>%
  select(-...1)

poll_data$opponent_points <- ifelse(
  is.na(poll_data$opponent_points),
  0,
  poll_data$opponent_points
)

poll_data <- poll_data %>%
  filter(year != 2020) %>%
  group_by(year, week) %>%
  mutate(
    Rank = rank(-points, ties.method = "min")
  ) %>%
  ungroup() %>%
  group_by(team) %>%
  arrange(team, year, week) %>%
  group_by(team, year) %>%
  mutate(
    next_ranked_week = lead(week),
    next_week_rank = if_else(
      next_ranked_week == week + 1,
      lead(Rank),
      NA_integer_
    )
  )%>%
  ungroup() %>%
  mutate(
    game_result = case_when
    (
      result == 'W' ~ 1,
      result == 'L' ~ 0
    ),
    pt_diff = team_points - opp_points,
    bias=case_when(
      team %in% c("Alabama", "Ohio State") ~ 4,
      team %in% c("Georgia", "Michigan", "Texas", "LSU", "Notre Dame", "Clemson") ~ 3,
      team %in% c("Texas A&M", "Miami (FL)") ~ 2.5,
      team %in% c("USC", "Tennessee", "Florida") ~ 2,
      team %in% c("Auburn", "Florida State", "Nebraska") ~ 1,
      TRUE ~ 0
    )
    
  ) %>%
  arrange(year, week, Rank)

View(poll_data)
model <- lm(next_week_rank ~ Rank + pt_diff*opponent_points + bias, data=poll_data)
summary(model)
plot(model)

poll_data$pred_next_week_rank <- predict(model, newdata = poll_data)
poll_data_summary <- poll_data %>%
  select(team, year, week, next_week_rank, pred_next_week_rank, digits=0) %>%
  filter(!is.na(next_week_rank), !is.na(pred_next_week_rank))

View(poll_data_summary)

preds
