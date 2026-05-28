library(dplyr)
library(utils)
poll_data <- read.csv("APPollData.csv")

View(poll_data)

poll_data <- poll_data %>%
  select(-X)

poll_data$opponent_points <- ifelse(
  is.na(poll_data$opponent_points),
  0,
  poll_data$opponent_points
)


poll_data <- poll_data %>%
  filter(year != 2020) %>%
  group_by(year, week) %>%
  mutate(
    Rank = rank(-points, ties.method= "min")
  ) %>%
  ungroup() %>%
  group_by(team) %>%
  arrange(team, year, week) %>%
  mutate(
    next_ranked_week = lead(week),
    next_week_rank = ifelse(
      next_ranked_week == week + 1,
      lead(Rank),
      NA_integer_
    )
  ) %>%
  ungroup() %>%
  mutate(
    pt_diff = team_points - opp_points
  ) %>%
  arrange(year, week, Rank)


poll_data <- poll_data %>%
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


write.csv(poll_data, "CFBPollDataFeb16.csv")
linear_model <- lm(next_week_rank ~ points + pt_diff + opponent_points + bias, data=poll_data)
summary(linear_model)
plot(linear_model)


poll_data$pred_next_week_rank <- predict(linear_model, newdata = poll_data)
poll_data_summary <- poll_data %>%
  select(team, year, week, Rank, next_week_rank, pred_next_week_rank)
View(poll_data_summary)
