library(dplyr)

poll_data <- read_csv("APPollData.csv")
View(poll_data)

poll_data <- poll_data %>%
  select(-...1)

poll_data$opponent_points <- ifelse(
  is.na(poll_data$opponent_points),
  0,
  poll_data$opponent_points
)

poll_data <- poll_data %>%
  filter(year != 2020) %>% # leave out at first
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
    pt_diff = team_points - opp_points
  ) %>%
  arrange(year, week, Rank)


linear_model <- lm(next_week_rank ~ Rank + pt_diff + opponent_points, data=poll_data)
plot(linear_model)
summary(linear_model)

poll_data <- poll_data %>%
  mutate(
    bias=case_when(
      team %in% c("Alabama", "Ohio State") ~ 4,
      team %in% c("Georgia", "Michigan", "Texas", "LSU", "Notre Dame", "Clemson") ~ 3,
      team %in% c("Texas A&M", "Miami (FL)") ~ 2.5,
      team %in% c("USC", "Tennessee", "Florida") ~ 2,
      team %in% c("Auburn", "Florida State", "Nebraska") ~ 1,
      TRUE ~ 0
    )
  )

linear_model_w_bias <- lm(next_week_rank ~ Rank + pt_diff + opponent_points + bias, data=poll_data)
summary(linear_model_w_bias)


poll_data$pred_next_week_rank <- predict(linear_model_w_bias, newdata = poll_data)
poll_data_summary <- poll_data %>%
  select(team, year, week, next_week_rank, pred_next_week_rank, digits=0) %>%
  filter(!is.na(next_week_rank), !is.na(pred_next_week_rank))

View(poll_data_summary)

