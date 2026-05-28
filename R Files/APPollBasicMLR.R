library(dplyr)

poll_data <- final_data

poll_data <- poll_data %>%
  filter(team != "Miami (FL)") %>%
  filter(week != 15)
colnames(poll_data)
View(poll_data)
predictors <- poll_data %>%
  select(-team, -year, -opponent.x, -rank, -poll_point_diff, -total_wins_before, -total_losses_before) %>%
  mutate(
    conference = factor(conference),
    opponent_conference.x = factor(opponent_conference.x),
    result.x = factor(result.x, levels = c("W", "L"))
  )


colnames(predictors)

model <- lm(next_week_points ~ ., data=predictors)

summary(model)

plot(model)

View(predictors)
plot(
  poll_data$pred_points2,
  poll_data$next_week_poll_points,
  xlab = "Predicted Points",
  ylab = "Actual Points",
  main = "Predicted vs Actual Next Week Poll Points",
  pch = 16,
  col = rgb(0, 0, 1, 0.5)
)

# add y = x reference line
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)
plot(model)
View(poll_data)

poll_data$pred_points2 <- coalesce(poll_data$predicted_points, poll_data$poll_points) 
poll_data$pred_points2[poll_data$pred_points2 < 0] <- 0
poll_data$next_week_poll_points[is.na(poll_data$next_week_poll_points)] <- 0
poll_data$pred_error <- poll_data$pred_points2 - poll_data$next_week_poll_points

sum_actual <- sum(poll_data$next_week_poll_points)
sum_pred <- sum(poll_data$pred_points2)
sum_actual
sum_pred
poll_data$abs_error <- abs(poll_data$pred_error)

farthest_off <- poll_data[order(-poll_data$abs_error), ]

head(farthest_off, 10)
View(farthest_off)

avg_points_by_rank <- poll_data %>%
  group_by(rank) %>%
  summarise(
    avg_points = mean(next_week_poll_points, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(rank)

View(avg_points_by_rank)
colnames(predictors)

poll_data$err_sq <- (poll_data$pred_error)^2
rmse <- sqrt(mean(poll_data$err_sq))
rmse

test <- poll_data %>%
  filter(year==2014 & week == 7)
View(test)
