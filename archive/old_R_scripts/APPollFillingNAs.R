library(dplyr)


final_data <- read.csv("CFBPollDataFinal.csv")


View(final_data)

nas <- final_data %>%
  filter(is.na(opponent_conference))

View(nas)
final_data$conference <- ifelse(final_data$team == "San Jose State", "Mountain West", final_data$conference)
final_data$conference <- ifelse(is.na(final_data$conference), "FCS", final_data$conference)

final_data$opponent_conference[
  final_data$opponent_conference == "BYE" &
    final_data$opponent != "BYE"
] <- "FCS"
