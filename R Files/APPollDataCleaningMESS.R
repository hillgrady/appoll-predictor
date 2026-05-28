library(dplyr)

poll_data <- read.csv("CFBPollDataFeb23.csv")
weekly_records_full <- read.csv("CFBTeamResults.csv")
head(weekly_records_full)
head(team_records)
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

poll_data2 <- team_records %>%
  arrange(team, year, week) %>%
  group_by(team, year) %>%
  mutate(
    streak = {
      streak_vec <- numeric(n())
      current <- 0
      for (i in seq_along(win)) {
        if (is.na(win[i]) & is.na(loss[i])) {
          streak_vec[i] <- current  # bye week
        } else if (win[i] == 1) {
          current <- if (current > 0) current + 1 else 1
          streak_vec[i] <- current
        } else if (loss[i] == 1) {
          current <- if (current < 0) current - 1 else -1
          streak_vec[i] <- current
        } else { 
          streak_vec[i] <- current  # win=0 & loss=0
        }
      }
      streak_vec
    },
    total_wins_after = ifelse(win==1, total_wins+1, total_wins)
  ) %>%
  ungroup() %>%
  select(team, year, week, streak) %>%
  right_join(poll_data, by = c("team", "year", "week"))


View(poll_data2)
View(team_records)
