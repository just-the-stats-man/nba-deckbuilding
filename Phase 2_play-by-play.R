# ------------------------------------------------------------
# Phase 2: Pull games + play-by-play + rotations
# ------------------------------------------------------------

library(hoopR)
library(tidyverse)
library(janitor)
library(glue)
library(arrow)
library(fs)

season <- "2025-26"
team_abbr <- "LAL"

dir_create("data/raw/games")
dir_create("data/raw/pbp")
dir_create("data/raw/rotations")

# 1. Get team schedule / games
games <- nba_leaguegamefinder(
  season_nullable = season,
  team_abbreviation_nullable = team_abbr
) %>%
  extract_hoopr_table() %>%
  clean_names()

glimpse(games)

# Save games
write_parquet(
  games,
  glue("data/raw/games/{team_abbr}_games_{season}.parquet")
)

sample_games <- games %>%
  distinct(game_id) %>%
  slice_head(n = 3) %>%
  pull(game_id)

sample_games



pbp_sample <- map_dfr(sample_games, pull_pbp_safe)

glimpse(pbp_sample)

write_parquet(
  pbp_sample,
  glue("data/raw/pbp/{team_abbr}_pbp_sample_{season}.parquet")
)

pull_rotation_safe <- function(game_id) {
  message("Pulling rotations for game: ", game_id)
  
  nba_gamerotation(game_id = game_id) %>%
    extract_hoopr_table() %>%
    clean_names() %>%
    mutate(game_id = game_id)
}

rotation_sample <- map_dfr(sample_games, pull_rotation_safe)

glimpse(rotation_sample)

write_parquet(
  rotation_sample,
  glue("data/raw/rotations/{team_abbr}_rotation_sample_{season}.parquet")
)

n_distinct(pbp_sample$game_id)
n_distinct(rotation_sample$game_id)

pbp_sample %>% count(game_id)
rotation_sample %>% count(game_id)