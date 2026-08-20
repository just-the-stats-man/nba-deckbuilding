# ============================================================
# 01_setup_project.R
# Load libraries, config, folders, and helper functions.
# Source this at the top of every phase script.
# ============================================================

required_packages <- c(
  "hoopR", "tidyverse", "janitor", "glue", "fs",
  "arrow", "here", "lubridate", "naniar"
)

missing_packages <- required_packages[!purrr::map_lgl(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Install missing packages first: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(hoopR)
  library(tidyverse)
  library(janitor)
  library(glue)
  library(fs)
  library(arrow)
  library(here)
  library(lubridate)
  library(naniar)
})

source("00_config.R")
source("R/helpers.R")

ensure_dirs(PROJECT_DIRS)

season <- PROJECT_SEASON
team_abbr <- PROJECT_TEAM_ABBR
max_games <- PROJECT_MAX_GAMES
sample_games <- PROJECT_SAMPLE_GAMES
min_minutes <- PROJECT_MIN_MINUTES

message("Project loaded")
message("- Season: ", season)
message("- Team: ", team_abbr)
message("- Max games: ", ifelse(is.null(max_games), "full available schedule", max_games))
message("- Debug sample games: ", sample_games)
message("- Repo root: ", here::here())
