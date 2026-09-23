library(rvest)
library(dplyr)
library(tidyverse)
library(glue)
library(gt)
library(gtExtras)
library(magick)
library(here)
library(purrr)
get_ap_voters <- function(year, week) {
  
  html <- read_html(glue('https://collegepolltracker.com/football/grid/{year}/week-{week}')) %>% 
    html_nodes('.gridRow')
  
  voter_data <- html %>%
    map_df(~{
      voter_name <- .x %>%
        html_node(".gridPollster a") %>%
        html_text(trim = TRUE)
      
      teams <- .x %>%
        html_nodes(".gridTeam img") %>% 
        html_attr("title")
      
      tibble(voter = voter_name, teams = list(teams))
    }) 
  
  voter_data_wide <- voter_data %>%
    unnest(teams) %>%
    mutate(rank = row_number(), .by = voter) %>%
    pivot_wider(names_from = rank, values_from = teams, names_prefix = "x")
  
  return(voter_data_wide)
  
}

get_ap_poll <- function(year, week) {
  
  html <- read_html(glue('https://collegepolltracker.com/football/{year}/week-{week}'))
  
  teams_data <- html %>%
    html_nodes(".teamBar") %>%
    map_df(~{
      team <- .x %>% html_node(".teamName a") %>% html_text()
      rank <- .x %>% html_node(".teamRank") %>% html_text() %>% as.numeric()
      record <- .x %>% html_node(".teamRecord") %>% html_text()
      previous <- .x %>% html_node(xpath = ".//span[contains(@class, 'teamDataLabel') and contains(text(), 'Previous:')]/following-sibling::text()[1]") %>% html_text() %>% 
        str_squish() %>% as.numeric()
      high <- .x %>% html_node(xpath = ".//span[contains(@class, 'teamDataLabel') and contains(text(), 'High:')]/following-sibling::span[@class='rName']") %>% html_text()
      low <- .x %>% html_node(xpath = ".//span[contains(@class, 'teamDataLabel') and contains(text(), 'Low:')]/following-sibling::span[@class='rName']") %>% html_text()
      next_game <- .x %>% html_node(xpath = ".//span[contains(@class, 'teamDataLabel') and contains(text(), 'Next:')]/following-sibling::text()[1]") %>% html_text()
      points <- .x %>% html_node(".teamPoints b") %>% html_text() %>% as.numeric()
      
      tibble(team, rank, record, previous, high, low, next_game, points)
    }) %>% 
    # sometimes the ranking is NA so lets rank based on order (correct)
    mutate(rank = row_number())
  
  return(teams_data)
  
}
test <- get_ap_poll(2015,7)
#View(test)
years  <- 2014:2024
weeks  <- 3:15  # adjust if needed
polls <- c()

polls <- map_df(years, function(y) {
  map_df(weeks, function(w) {
    ap_poll <- get_ap_poll(y, w) %>% 
      select(team, rank) %>% 
      filter(rank <= 25) %>%
      mutate(voter = "AP POLL") %>% 
      pivot_wider(names_from = rank, values_from = team, names_prefix = "x")
    
    voters <- get_ap_voters(y, w)
    
    bind_rows(ap_poll, voters) %>%
      mutate(year = y, week = w)
  })
})
polls <- polls %>%
  select(voter, year, week, everything())


#View(polls)


years  <- 2014:2024
weeks  <- 3:15  # adjust if needed
final_polls <- c()

final_polls <- map_df(years, function(y) {
  map_df(weeks, function(w) {
    ap_poll <- get_ap_poll(y, w)
  ap_poll %>%
    select(team, points) %>%
    mutate(
      year=y, week=w
  )
  })
})

View(final_polls)
