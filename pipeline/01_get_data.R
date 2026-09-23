# =============================================================================
# 01_get_data.R  —  AP Poll Predictor, step 1 of 3
#
# Pulls the raw inputs, cached one file per season in data/:
#   polls_<year>.rds - every team receiving AP votes (collegepolltracker.com)
#   pbp_<year>.rds   - game results built from cfbfastR play-by-play
#   espn_<year>.rds  - ESPN scoreboard (fills games with no pbp + tells us
#                      which weeks of the current season are finished)
# Finished seasons download once. CURRENT_SEASON is re-downloaded on every run,
# so the weekly update is just: run 01, 02, 03 again.
#
# Replaces: APPollScraping.R, APPollDataCollection.R (and the cfbd pulls that
#           were copy-pasted into every APPollWithRecords*.R draft)
#
# Run from the project root (the folder with APPollApp.py). No API key needed:
# play-by-play files + ESPN scoreboard, both through cfbfastR.
# =============================================================================

library(tidyverse)

if (!file.exists("APPollApp.py"))
  stop("Set the working directory to the project root (the APPollModel folder):\n",
       "  Session > Set Working Directory, or setwd(\"~/Desktop/Sports Data/APPollModel\")")
library(rvest)
library(glue)
library(cfbfastR)

# ---- Config -----------------------------------------------------------------
LAST_SEASON    <- 2026                          # bump to 2027 next year
YEARS          <- setdiff(2014:LAST_SEASON, 2020) # 2020 (COVID) left out, as before
CURRENT_SEASON <- LAST_SEASON                     # always re-pulled; others cached
POLL_WEEKS     <- 3:15                            # collegepolltracker "week-N" pages
DATA_DIR       <- file.path("pipeline", "cache")
REFRESH        <- FALSE                           # TRUE = re-pull every season

dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

# Per-season cache. Seasons from the earlier all-years cache files are split
# out of those automatically, so nothing already downloaded is re-downloaded.
cached_by_season <- function(prefix, legacy_file, fetch) {
  legacy <- NULL
  legacy_path <- file.path(DATA_DIR, legacy_file)
  map_dfr(YEARS, function(y) {
    path <- file.path(DATA_DIR, sprintf("%s_%d.rds", prefix, y))
    if (!REFRESH && y != CURRENT_SEASON) {
      if (file.exists(path)) return(readRDS(path))
      if (is.null(legacy) && file.exists(legacy_path)) legacy <<- readRDS(legacy_path)
      yr_col <- intersect(c("season", "year"), names(legacy))[1]
      if (!is.null(legacy) && !is.na(yr_col) && any(legacy[[yr_col]] == y)) {
        x <- legacy[legacy[[yr_col]] == y, ]
        saveRDS(x, path)
        return(x)
      }
    }
    x <- fetch(y)
    saveRDS(x, path)
    x
  })
}

# =============================================================================
# 1. AP poll scrape
# =============================================================================
# Poll "week N" on collegepolltracker is the poll released BEFORE week-N games,
# so it lines up with that week's game in step 2 (the 03 script checks this
# using the record the site shows next to each team).
get_ap_poll <- function(year, week) {
  html <- read_html(glue("https://collegepolltracker.com/football/{year}/week-{week}"))
  html %>%
    html_nodes(".teamBar") %>%
    map_df(~ tibble(
      team   = .x %>% html_node(".teamName a") %>% html_text() %>% str_trim(),
      record = .x %>% html_node(".teamRecord") %>% html_text() %>% str_trim(),
      points = .x %>% html_node(".teamPoints b") %>% html_text() %>% as.numeric()
    )) %>%
    mutate(year = year, week = week)
}

polls_for_season <- function(year) {
  # Current season: also scrape the early and late pages. The site's week
  # numbering can shift by a season (02 detects and corrects that), so the
  # poll going into week 3 may sit on the "week-2" page.
  weeks <- if (year == CURRENT_SEASON) 1:16 else POLL_WEEKS
  out <- map_dfr(weeks, function(week) {
    message("Scraping poll ", year, " week ", week)
    Sys.sleep(1)  # be polite to the site
    tryCatch(get_ap_poll(year, week), error = function(e) tibble())
  })
  if (nrow(out) == 0) return(out)
  # A week that hasn't happened yet can come back as a copy of the latest
  # poll. Real polls never repeat exactly, so drop any exact repeat.
  sig <- out %>% arrange(week, team) %>% group_by(week) %>%
    summarise(sig = paste(team, points, collapse = "|"), .groups = "drop")
  keep_weeks <- sig$week[!duplicated(sig$sig)]
  out %>% filter(week %in% keep_weeks)
}
raw_polls <- cached_by_season("polls", "raw_polls.rds", polls_for_season)

# =============================================================================
# 2. Game results from cfbfastR — NO API KEY ANYWHERE
# =============================================================================
# a) load_cfb_pbp(): play-by-play files from the sportsdataverse GitHub. Main
#    source (has conferences). Each season is boiled down to one row per game.
# b) espn_cfb_schedule(): ESPN scoreboard. Fills in any game that has no
#    play-by-play (a few dozen a year in 2014-2019, plus FCS-vs-FCS games for
#    teams like North Dakota State). Matched on game_id, which ESPN and
#    CollegeFootballData share, and team names come from the same team ids.
FILL_FROM_ESPN <- TRUE
ESPN_WEEKS     <- 1:16
ESPN_GROUPS    <- c("FBS", "FCS")

# Everything downstream (02) uses these column names:
REQUIRED_GAME_COLS <- c("season", "week", "home_team", "away_team",
                        "home_points", "away_points",
                        "home_conference", "away_conference")

safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
# ids as plain text (avoids 4.01e+08-style scientific notation)
id_chr <- function(x) if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE) else as.character(x)
col_or_na <- function(df, nm) if (nm %in% names(df)) as.character(df[[nm]]) else rep(NA_character_, nrow(df))

# ---- a) play-by-play -> one row per game ------------------------------------
games_from_pbp <- function(year) {
  message("Downloading play-by-play ", year)
  pbp <- tryCatch(load_cfb_pbp(year), error = function(e) NULL)
  if (is.null(pbp) || nrow(pbp) == 0) {
    message("  no play-by-play for ", year, " yet; ESPN will cover it")
    return(tibble())
  }
  for (nm in c("home_team_id", "away_team_id")) if (!nm %in% names(pbp)) pbp[[nm]] <- NA
  out <- pbp %>%
    mutate(
      # running score from the home/away point of view
      home_run = pmax(if_else(pos_team == home, pos_team_score, def_pos_team_score),
                      if_else(offense_play == home, offense_score, defense_score),
                      na.rm = TRUE),
      away_run = pmax(if_else(pos_team == away, pos_team_score, def_pos_team_score),
                      if_else(offense_play == away, offense_score, defense_score),
                      na.rm = TRUE),
      # score at the end of each drive: catches the final score incl. overtime
      home_drive_end = if_else(as.logical(drive_is_home_offense), drive_end_offense_score,
                               drive_end_defense_score),
      away_drive_end = if_else(as.logical(drive_is_home_offense), drive_end_defense_score,
                               drive_end_offense_score)
    ) %>%
    group_by(game_id) %>%
    summarise(
      season          = first(year),
      week            = first(week),
      season_type     = first(season_type),
      start_date      = first(start_date),
      home_team       = first(home_team),
      away_team       = first(away_team),
      home_team_id    = first(home_team_id),
      away_team_id    = first(away_team_id),
      home_conference = first(home_team_conference),
      away_conference = first(away_team_conference),
      score_team      = first(team),
      team_score      = first(team_score),
      opp_score       = first(opponent_score),
      home_max        = safe_max(c(home_run, home_drive_end)),
      away_max        = safe_max(c(away_run, away_drive_end)),
      .groups = "drop"
    ) %>%
    mutate(
      home_from_team = case_when(score_team == home_team ~ team_score,
                                 score_team == away_team ~ opp_score),
      away_from_team = case_when(score_team == home_team ~ opp_score,
                                 score_team == away_team ~ team_score),
      home_points = pmax(home_from_team, home_max, na.rm = TRUE),
      away_points = pmax(away_from_team, away_max, na.rm = TRUE)
    ) %>%
    select(game_id, season, week, season_type, start_date,
           home_team, away_team, home_team_id, away_team_id,
           home_conference, away_conference, home_points, away_points) %>%
    mutate(across(c(game_id, home_team_id, away_team_id), id_chr),
           across(c(season_type, start_date, home_team, away_team,
                    home_conference, away_conference), as.character),
           across(c(season, week, home_points, away_points), as.numeric),
           source = "pbp")
  rm(pbp); invisible(gc())
  out
}

raw_games <- cached_by_season("pbp", "raw_games_v3.rds", games_from_pbp)

missing <- setdiff(REQUIRED_GAME_COLS, names(raw_games))
if (length(missing) > 0) {
  message("Columns returned:\n  ", paste(names(raw_games), collapse = ", "))
  stop("Columns not found: ", paste(missing, collapse = ", "),
       "\nSend this output to Claude to update the mapping.")
}

# ---- b) ESPN scoreboard for games with no play-by-play ----------------------
espn_week <- function(year, week, group) {
  s <- tryCatch(espn_cfb_schedule(year = year, week = week, groups = group),
                error = function(e) NULL)
  if (is.null(s) || nrow(s) == 0) return(tibble())
  tibble(
    game_id       = id_chr(s$game_id),
    group         = group,
    season        = year,
    week          = week,
    type          = col_or_na(s, "type"),
    status        = col_or_na(s, "status_name"),
    start_date    = col_or_na(s, "game_date_time"),
    home_team_id  = id_chr(col_or_na(s, "home_team_id")),
    away_team_id  = id_chr(col_or_na(s, "away_team_id")),
    home_location = col_or_na(s, "home_team_location"),
    away_location = col_or_na(s, "away_team_location"),
    home_points   = suppressWarnings(as.numeric(col_or_na(s, "home_score"))),
    away_points   = suppressWarnings(as.numeric(col_or_na(s, "away_score")))
  )
}

if (FILL_FROM_ESPN) {
  espn_for_season <- function(year) {
    grid <- expand_grid(week = ESPN_WEEKS, group = ESPN_GROUPS)
    pmap_dfr(grid, function(week, group) {
      message("ESPN scoreboard ", year, " week ", week, " (", group, ")")
      Sys.sleep(0.3)
      espn_week(year, week, group)
    })
  }
  raw_espn <- cached_by_season("espn", "raw_espn_games.rds", espn_for_season)
  if (nrow(raw_espn) == 0) stop("ESPN returned no games. Send this output to Claude.")

  # Team id -> the play-by-play (CollegeFootballData) team name, so names match
  id_names <- bind_rows(
    raw_games %>% select(id = home_team_id, name = home_team),
    raw_games %>% select(id = away_team_id, name = away_team)
  ) %>% filter(!is.na(id)) %>% distinct(id, .keep_all = TRUE)

  # Each team's conference that season, from play-by-play
  conf_lookup <- bind_rows(
    raw_games %>% select(season, team = home_team, conference = home_conference),
    raw_games %>% select(season, team = away_team, conference = away_conference)
  ) %>% filter(!is.na(conference)) %>% distinct(season, team, .keep_all = TRUE)

  espn_fill <- raw_espn %>%
    distinct(game_id, .keep_all = TRUE) %>%               # FBS-vs-FCS shows up twice
    filter(!game_id %in% raw_games$game_id,               # only games pbp lacks
           is.na(type) | grepl("reg", type, ignore.case = TRUE),
           is.na(status) | grepl("FINAL", status),
           !is.na(home_points), !is.na(away_points)) %>%
    left_join(id_names %>% rename(home_team = name), by = c("home_team_id" = "id")) %>%
    left_join(id_names %>% rename(away_team = name), by = c("away_team_id" = "id")) %>%
    mutate(home_team = coalesce(home_team, home_location),
           away_team = coalesce(away_team, away_location)) %>%
    left_join(conf_lookup %>% rename(home_team = team, home_conference = conference),
              by = c("season", "home_team")) %>%
    left_join(conf_lookup %>% rename(away_team = team, away_conference = conference),
              by = c("season", "away_team")) %>%
    transmute(game_id, season, week, season_type = "regular", start_date,
              home_team, away_team, home_team_id, away_team_id,
              home_conference, away_conference, home_points, away_points,
              source = "espn") %>%
    filter(!is.na(home_team), !is.na(away_team))          # need both team names

  # Official final scores: where ESPN has a game that play-by-play also has,
  # use ESPN's score (play-by-play can miss overtime or a last-second score).
  # Matched by team id so neutral-site home/away flips are handled.
  espn_scores <- raw_espn %>%
    filter(is.na(status) | grepl("FINAL", status),
           !is.na(home_points), !is.na(away_points)) %>%
    distinct(game_id, .keep_all = TRUE) %>%
    select(game_id, e_home_id = home_team_id, e_home_pts = home_points,
           e_away_pts = away_points)
  raw_games <- raw_games %>%
    left_join(espn_scores, by = "game_id") %>%
    mutate(
      same  = !is.na(e_home_id) & e_home_id == home_team_id,
      flip  = !is.na(e_home_id) & e_home_id == away_team_id,
      home_points = case_when(same ~ e_home_pts, flip ~ e_away_pts, TRUE ~ home_points),
      away_points = case_when(same ~ e_away_pts, flip ~ e_home_pts, TRUE ~ away_points)
    ) %>%
    select(-e_home_id, -e_home_pts, -e_away_pts, -same, -flip)

  message("ESPN filled ", nrow(espn_fill), " games missing from play-by-play:")
  print(espn_fill %>% count(season, name = "games_filled"))
  raw_games <- bind_rows(raw_games, espn_fill)
}

# ---- Keep every regular-season game, all divisions ---------------------------
# 02 decides which teams get rows (FBS teams + anyone receiving AP votes).
games <- raw_games %>%
  filter(is.na(season_type) | season_type == "regular") %>%
  select(all_of(REQUIRED_GAME_COLS), start_date, game_id, source) %>%
  filter(!is.na(home_points), !is.na(away_points))   # unplayed / cancelled

saveRDS(games, file.path(DATA_DIR, "games.rds"))   # cleaned, read by 02
saveRDS(raw_polls, file.path(DATA_DIR, "polls.rds"))

# ---- Current season: which weeks are finished? -------------------------------
# A week counts as finished once every FBS game in it is final (or canceled /
# postponed). 02 only keeps current-season weeks up to the last finished one,
# so a half-played week never shows up with most teams looking like a bye.
FINISHED <- "FINAL|CANCEL|POSTPON|FORFEIT"
cur <- raw_espn %>%
  filter(season == CURRENT_SEASON, !is.na(status), group == "FBS") %>%
  group_by(week) %>%
  summarise(done = all(grepl(FINISHED, status)), .groups = "drop") %>%
  arrange(week)
last_done <- if (nrow(cur) == 0) 0 else {
  first_open <- which(!cur$done)[1]
  if (is.na(first_open)) max(cur$week) else if (first_open == 1) 0 else cur$week[first_open - 1]
}
saveRDS(tibble(season = CURRENT_SEASON, last_complete_week = last_done),
        file.path(DATA_DIR, "season_status.rds"))

latest_poll <- raw_polls$week[raw_polls$year == CURRENT_SEASON]
message(CURRENT_SEASON, ": games complete through week ", last_done,
        "; latest AP poll on the site = its \"Week ",
        if (length(latest_poll)) max(latest_poll) else "none", "\" (02 lines up the numbering)")

message("Done: ", nrow(raw_polls), " poll rows, ", nrow(games), " regular-season games.")
print(games %>% count(season, source) %>% pivot_wider(names_from = source, values_from = n))
