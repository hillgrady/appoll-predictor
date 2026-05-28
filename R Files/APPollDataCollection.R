library(cfbfastR)
library(dplyr)
library(purrr)
head(polls)

# Load each team's schedule
scheds<- load_cfb_schedules()
View(scheds)
next_week <- final_polls %>%
  mutate(next_week = week + 1)

games <- map_df(2014:2024, function(y){
  cfbd_game_info(year=y)
})


games_team_level <- games %>%
  mutate(
    opponent = away_team,
    team = home_team,
    team_points = home_points,
    opp_points = away_points,
    result = case_when(
      home_points > away_points ~ 1,
      home_points < away_points ~ 0,
    ),
    pt_diff = team_points - opp_points
  ) %>%
  
  select(season, week, team, opponent, team_points, opp_points, result, pt_diff) %>%
  
  bind_rows(
    games %>% 
      mutate(
        opponent = home_team,
        team = away_team,
        team_points = away_points,
        opp_points = home_points,
        result = case_when(
          away_points > home_points ~ 1,
          away_points < home_points ~ 0,
        ),
        pt_diff = home_points-away_points
      ) %>%
      select(season, week, team, opponent, team_points, opp_points, result, pt_diff)
  )


# Source - https://stackoverflow.com/a/70804686
# Posted by stefan
# Retrieved 2026-03-05, License - CC BY-SA 4.0
# I'm pretty sure this is useless
df <- data.frame(
  P1 = c(0L, 2L),
  P2 = c("3 Coins", "4"),
  P3 = c("2", "-2 Coins"),
  P4 = c(1L, 4L)
)

games_team_level[] <- lapply(games_team_level, function(x) {x[grepl("coin", tolower(x), fixed = TRUE)] <- 0; x})

df
#>   P1 P2 P3 P4
#> 1  0  0  2  1
#> 2  2  4  0  4


polls2 <- next_week %>%
  left_join(
    games_team_level,
    by = c("team" = "team",
           "year" = "season",
           "week" = "week")
  )


View(polls2)

polls3 <- polls2 %>%
  left_join(
    final_polls %>%
      select(team, year, week, points) %>%
      rename(next_week_points = points,
             next_week = week),
    by = c("team", "year", "next_week")
  ) %>%
  mutate(
    poll_point_diff = next_week_points - points
  )

polls3 <- polls3 %>%
  left_join(
    final_polls %>%
      select(team, year, week, points) %>%
      rename(
        opponent = team,
        opponent_points = points
      ),
    by = c("opponent", "year", "week")
  )

View(games_team_level)

View(polls3)

model <- lm(next_week_points ~ points + pt_diff*opponent_points, data=polls3)
summary(model)

no_byes <- polls3 %>%
  filter(!is.na(opponent))

#write.csv(polls3, "APPollData.csv")
