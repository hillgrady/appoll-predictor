# =============================================================================
# 03_validate.R  —  AP Poll Predictor, step 3 of 3
#
# Checks the CSV from 02 before it goes anywhere near the model:
#   1. same 24 columns, in the same order, the notebook/app expect
#   2. every feature in feature_cols.json exists
#   3. one row per team-year-week
#   4. poll teams that didn't match a cfbfastR team (-> add to NAME_MAP in 02)
#   5. poll/game week alignment, using the record shown on collegepolltracker
#   6. conference names for poll teams (catches 3.0 renaming conferences)
#   7. side-by-side with the current live CFBPollDataFinal.csv
#
# If every check passes, the new CSV is copied to data/CFBPollDataFinal.csv
# (what the app reads). If anything fails, data/ is left untouched.
# =============================================================================

library(tidyverse)

if (!file.exists("APPollApp.py"))
  stop("Set the working directory to the project root (the APPollModel folder):\n",
       "  Session > Set Working Directory, or setwd(\"~/Desktop/Sports Data/APPollModel\")")

DATA_DIR <- file.path("pipeline", "cache")
new  <- read.csv(file.path(DATA_DIR, "CFBPollDataFinal.csv"), stringsAsFactors = FALSE)
live_path <- file.path("data", "CFBPollDataFinal.csv")

problems <- character()
flag <- function(msg) { problems <<- c(problems, msg); message("  FAIL: ", msg) }
ok   <- function(msg) message("  ok:   ", msg)

# ---- 1. Column contract -----------------------------------------------------
message("\n[1] Columns")
EXPECTED <- c("team", "year", "week", "conference", "poll_points", "rank", "result",
              "pts_scored", "pts_allowed", "pt_diff", "opponent", "opponent_conference",
              "opponent_wins", "opponent_losses", "opponent_points", "next_week_points",
              "poll_point_diff", "bias", "undefeated", "total_wins_before",
              "total_losses_before", "total_wins_after", "total_losses_after", "streak")
if (identical(names(new), EXPECTED)) ok("24 columns, same order") else
  flag(paste("column mismatch. extra:", paste(setdiff(names(new), EXPECTED), collapse = ","),
             "| missing:", paste(setdiff(EXPECTED, names(new)), collapse = ",")))

# ---- 2. feature_cols.json ---------------------------------------------------
message("\n[2] feature_cols.json")
feat_path <- file.path("model", "feature_cols.json")
if (file.exists(feat_path) && requireNamespace("jsonlite", quietly = TRUE)) {
  feats <- jsonlite::fromJSON(feat_path)
  miss <- setdiff(feats, names(new))
  if (length(miss) == 0) ok(paste(length(feats), "model features present")) else
    flag(paste("features missing from CSV:", paste(miss, collapse = ", ")))
} else message("  (skipped: feature_cols.json or jsonlite not found)")

# ---- 3. Duplicates ----------------------------------------------------------
message("\n[3] Duplicates")
dups <- new %>% count(team, year, week) %>% filter(n > 1)
if (nrow(dups) == 0) ok("one row per team-year-week") else {
  flag(paste(nrow(dups), "duplicate team-year-weeks"))
  print(head(dups, 10))
}

# ---- 4. Poll teams with no game data (name mismatches) ----------------------
message("\n[4] Poll teams not matched to cfbfastR")
unmatched <- new %>%
  filter(poll_points > 0, is.na(total_wins_after)) %>%
  distinct(team, year) %>%
  group_by(team) %>% summarise(years = paste(year, collapse = ","), .groups = "drop")
if (nrow(unmatched) == 0) ok("every poll team matched") else {
  flag(paste(nrow(unmatched), "poll team name(s) unmatched: add to NAME_MAP in 02"))
  print(unmatched, n = Inf)
}

# ---- 5. Week alignment ------------------------------------------------------
# The poll site shows each team's record at poll time. If poll week N really is
# the poll before week-N games, that record should equal total_*_before.
message("\n[5] Poll/game week alignment")
polls <- readRDS(file.path(DATA_DIR, "polls_aligned.rds"))   # after 02's week fix
rec <- polls %>%
  mutate(rec = str_extract(record, "\\d+-\\d+")) %>%
  filter(!is.na(rec)) %>%
  separate(rec, c("pw", "pl"), sep = "-", convert = TRUE) %>%
  distinct(team, year, week, .keep_all = TRUE) %>%
  inner_join(new, by = c("team", "year", "week"))
if (nrow(rec) > 0) {
  before <- mean(rec$pw == rec$total_wins_before & rec$pl == rec$total_losses_before, na.rm = TRUE)
  after  <- mean(rec$pw == rec$total_wins_after  & rec$pl == rec$total_losses_after,  na.rm = TRUE)
  message(sprintf("  poll record == record BEFORE this week's game: %.1f%%", 100 * before))
  message(sprintf("  poll record == record AFTER  this week's game: %.1f%%", 100 * after))
  if (before < 0.9) flag("poll records don't line up with 'before' records; check week alignment")
  else ok("poll week N = poll before week-N games")
} else message("  (skipped: no records scraped)")

# ---- 6. Conference names ----------------------------------------------------
message("\n[6] Conferences of poll teams (should look like the old names)")
print(as_tibble(new) %>% filter(poll_points > 0) %>% count(conference, sort = TRUE), n = Inf)

# ---- 7. NA summary + comparison with the live file --------------------------
message("\n[7] NAs per column")
print(colSums(is.na(new))[colSums(is.na(new)) > 0])

if (file.exists(live_path)) {
  message("\n[7] New vs live CFBPollDataFinal.csv")
  old <- read.csv(live_path, stringsAsFactors = FALSE)
  by_year <- full_join(
    old %>% group_by(year) %>% summarise(old_rows = n(), old_teams = n_distinct(team)),
    new %>% group_by(year) %>% summarise(new_rows = n(), new_teams = n_distinct(team)),
    by = "year")
  print(as_tibble(by_year), n = Inf)

  # Poll-side values should agree closely for ranked team-weeks
  cmp <- inner_join(
    old %>% filter(poll_points > 0) %>% distinct(team, year, week, .keep_all = TRUE),
    new %>% filter(poll_points > 0),
    by = c("team", "year", "week"), suffix = c("_old", "_new"))
  for (col in c("poll_points", "next_week_points", "result", "total_wins_after", "streak")) {
    a <- cmp[[paste0(col, "_old")]]; b <- cmp[[paste0(col, "_new")]]
    message(sprintf("  %-18s agree on %.1f%% of %d ranked team-weeks",
                    col, 100 * mean(a == b, na.rm = TRUE), nrow(cmp)))
  }
}

if (length(problems) == 0) {
  file.copy(file.path(DATA_DIR, "CFBPollDataFinal.csv"), live_path, overwrite = TRUE)
  message("\nALL CHECKS PASSED -> copied to ", live_path,
          "\nNext: git commit -am \"Week update\" && git push mine main")
} else {
  message("\n", length(problems), " problem(s), data/ NOT updated:\n - ",
          paste(problems, collapse = "\n - "))
}
