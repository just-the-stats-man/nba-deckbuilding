# ============================================================
# CourtContext Project Loader
# ============================================================

# ------------------------------------------------------------
# 1. Libraries
# ------------------------------------------------------------

library(hoopR)
library(tidyverse)
library(janitor)
library(glue)
library(fs)
library(arrow)
library(here)
library(lubridate)
library(naniar)

dirs <- c(
  "data/raw",
  "data/processed",
  "data/cache",
  "data/raw/games",
  "data/raw/pbp",
  "data/raw/rotations",
  "R",
  "notebooks",
  "outputs/plots",
  "outputs/tables",
  "docs"
)

walk(dirs, dir_create)

# ------------------------------------------------------------
# 2. Global settings
# ------------------------------------------------------------

season <- "2025-26"
team_abbr <- "LAL"

# ------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------

z_score <- function(x) {
  as.numeric(scale(x))
}

validate_columns <- function(df, cols) {
  
  missing_cols <- setdiff(cols, colnames(df))
  
  if (length(missing_cols) == 0) {
    message("✅ All required columns exist.")
    return(invisible(TRUE))
  }
  
  message("❌ Missing columns detected:")
  
  existing <- colnames(df)
  
  for (col in missing_cols) {
    
    message("\n- ", col)
    
    distances <- adist(col, existing)
    close_matches <- existing[order(distances)][1:5]
    
    message("Closest matches:")
    print(close_matches)
  }
  
  stop("Fix column names before proceeding.", call. = FALSE)
}

# ------------------------------------------------------------
# 4. Load saved datasets
# ------------------------------------------------------------

player_master <- read_parquet(
  glue("data/processed/player_master_{season}.parquet")
)

mvp_beta <- read_parquet(
  glue("data/processed/mvp_beta_{season}.parquet")
)

games <- read_parquet(
  glue("data/raw/games/{team_abbr}_games_{season}.parquet")
)

pbp_sample <- read_parquet(
  glue("data/raw/pbp/{team_abbr}_pbp_sample_{season}.parquet")
)

rotation_sample <- read_parquet(
  glue("data/raw/rotations/{team_abbr}_rotation_sample_{season}.parquet")
)

# ------------------------------------------------------------
# 5. Rebuild lightweight derived objects
# ------------------------------------------------------------

rotation_clean <- rotation_sample %>%
  mutate(
    in_time = as.numeric(in_time_real),
    out_time = as.numeric(out_time_real)
  ) %>%
  select(game_id, team_id, person_id, in_time, out_time)

# ------------------------------------------------------------
# 6. Sanity checks
# ------------------------------------------------------------

message("Loaded objects:")

message("- player_master: ", nrow(player_master), " rows")
message("- mvp_beta: ", nrow(mvp_beta), " rows")
message("- pbp_sample: ", nrow(pbp_sample), " rows")
message("- rotation_sample: ", nrow(rotation_sample), " rows")