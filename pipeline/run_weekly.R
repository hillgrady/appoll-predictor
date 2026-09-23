# =============================================================================
# run_weekly.R  —  the whole weekly update in one command
#
#   setwd("~/Desktop/Sports Data/APPollModel")
#   source("pipeline/run_weekly.R")
#
# 01 downloads the current season (finished seasons are cached), 02 builds the
# features, 03 checks everything and, if it all passes, updates
# data/CFBPollDataFinal.csv. Then commit and push from Terminal.
# =============================================================================
source("pipeline/01_get_data.R")
source("pipeline/02_build_features.R")
source("pipeline/03_validate.R")
