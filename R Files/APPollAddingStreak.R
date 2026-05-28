library(dplyr)
team_records <- read.csv("CFBTeamResultsPRE.csv")

View(team_records)

team_records <- team_records %>%
  mutate(
    undefeated = ifelse(total_losses == 0, 1, 0)
  )


calc_streak <- function(wins, losses) {
  # Initialize variables
  current_streak <- 0
  streaks <- numeric(length(wins))
  
  for (i in seq_along(wins)) {
    w <- wins[i]
    l <- losses[i]
    
    # Check for Bye Week: 
    # If both win and loss are NA, OR both are 0 (no result)
    # The streak carries over unchanged.
    if ((is.na(w) && is.na(l)) || (w == 0 && l == 0)) {
      streaks[i] <- current_streak
    } 
    # Check for Win
    else if (!is.na(w) && w == 1) {
      if (current_streak > 0) {
        current_streak <- current_streak + 1
      } else {
        current_streak <- 1
      }
      streaks[i] <- current_streak
    } 
    # Check for Loss
    else if (!is.na(l) && l == 1) {
      if (current_streak < 0) {
        current_streak <- current_streak - 1
      } else {
        current_streak <- -1
      }
      streaks[i] <- current_streak
    } 
    # Fallback (should be covered by Bye Week logic, but just in case)
    else {
      streaks[i] <- current_streak
    }
  }
  return(streaks)
}
# 3. Apply the logic to the dataframe
team_records <- team_records %>%
  # Ensure the games are in the correct chronological order
  arrange(team, year, week) %>%
  # Group by BOTH Team and Year to reset streak every season
  group_by(team, year) %>%
  # Create the new column using the custom function
  # passing in the 'win' and 'loss' columns
  mutate(streak = calc_streak(win, loss)) %>%
  ungroup()




View(team_records)
poll_data <- read.csv("CFBPollDataFeb23.csv")
View(poll_data)

team_records_tmp <- team_records %>%
  select(team, year, week, streak)
final_data <- final_data %>%
  left_join(
    team_records_tmp,
    by = c("team", "year", "week")
  )
View(final_data)
colnames(final_data)
final_data <- final_data %>%
  arrange(team, year, week) %>%
  group_by(team, year) %>%
  mutate(
    total_wins_before  = total_wins,
    total_losses_before = total_losses,
    total_wins_after   = ifelse(
      is.na(result.x), total_wins_before, total_wins_before + ifelse(result.x == "W", 1, 0)
    ),
    total_losses_after = ifelse(
      is.na(result.x), total_losses_before, total_losses_before + ifelse(result.x == "L", 1, 0)
    )
  ) %>%
  select(-total_wins, -total_losses) %>%
  ungroup() %>%
  arrange(year, week, -poll_points)

View(final_data)

final_data <- final_data %>%
  select(-X) %>%
  arrange(year, week, -poll_points)

write.csv(final_data, "CFBPollDataMar2.csv")
opponent_lookup <- team_records %>%
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

team_records <- team_records %>%
  left_join(
    opponent_lookup,
    by = c("year", "week", "opponent" = "team")
  )
team_records <- team_records %>%
  rename(opponent_conference = conference.y)

colnames(team_records)
write.csv(final_data, "CFBPollDataFeb23.csv")


test2 <- final_data2 %>%
  filter(team == "Florida State")
View(test2)


final_data2 <- unique(final_data)
View(final_data2)
