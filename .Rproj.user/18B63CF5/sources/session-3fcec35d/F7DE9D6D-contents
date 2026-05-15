# ============================================================
# CourtContext
# NBA Plus-Minus + Matchup Modeling Project
#
# Project goal:
# Build a basketball analytics pipeline that separates:
#   1. Team Impact Plus-Minus
#   2. Individual / matchup-level value
#
# Current status:
#   Phase 1 complete: player-season data pulled and cleaned
#   Phase 2 complete: sample games, play-by-play, rotations pulled
#   Phase 3 complete: rotation data converted into 10-player stints
# ============================================================


# ============================================================
# 0. Package setup
# ============================================================

# install.packages(c(
#   "hoopR", "tidyverse", "janitor", "glue", "fs",
#   "arrow", "here", "lubridate", "naniar"
# ))

library(hoopR)
library(tidyverse)
library(janitor)
library(glue)
library(fs)
library(arrow)
library(here)
library(lubridate)
library(naniar)


# ============================================================
# 1. Folder structure
# ============================================================
# Keep raw data, processed data, scripts, notebooks, and outputs separate.
# This prevents the project from turning into one giant cursed R script.
# ============================================================

# ============================================================
# Phase 1: Pull player-season data
# ============================================================
# Purpose:
# Create a player-season master table.
#
# This is NOT the final plus-minus data.
# This is for player summaries, simple MVP experiments,
# visual prototypes, and sanity checks.
# ============================================================

pull_player_stats <- function(season = "2025-26") {
  
  base <- nba_leaguedashplayerstats(
    season = season,
    per_mode_detailed = "Per100Possessions",
    measure_type_detailed_defense = "Base"
  ) %>%
    extract_hoopr_table() %>%
    clean_names()
  
  advanced <- nba_leaguedashplayerstats(
    season = season,
    per_mode_detailed = "Per100Possessions",
    measure_type_detailed_defense = "Advanced"
  ) %>%
    extract_hoopr_table() %>%
    clean_names()
  
  scoring <- nba_leaguedashplayerstats(
    season = season,
    per_mode_detailed = "Per100Possessions",
    measure_type_detailed_defense = "Scoring"
  ) %>%
    extract_hoopr_table() %>%
    clean_names()
  
  misc <- nba_leaguedashplayerstats(
    season = season,
    per_mode_detailed = "Per100Possessions",
    measure_type_detailed_defense = "Misc"
  ) %>%
    extract_hoopr_table() %>%
    clean_names()
  
  usage <- nba_leaguedashplayerstats(
    season = season,
    per_mode_detailed = "Per100Possessions",
    measure_type_detailed_defense = "Usage"
  ) %>%
    extract_hoopr_table() %>%
    clean_names()
  
  list(
    base = base,
    advanced = advanced,
    scoring = scoring,
    misc = misc,
    usage = usage
  )
}

player_stats_raw <- pull_player_stats(season)


# ============================================================
# Phase 1A: Save raw player tables
# ============================================================
# We save raw outputs before heavy transformation.
# This protects us if the API changes or if later cleaning code breaks.
# ============================================================

walk2(
  player_stats_raw,
  names(player_stats_raw),
  ~ save_parquet(.x, glue("data/raw/player_{.y}_{season}.parquet"))
)


# ============================================================
# Phase 1B: Build player master table
# ============================================================
# Join Base, Advanced, Scoring, Misc, and Usage tables.
#
# Note:
# The NBA API/hoopR output may return numeric-looking columns as character.
# We convert numeric columns after joining.
# ============================================================

player_master <- player_stats_raw$base %>%
  left_join(
    player_stats_raw$advanced,
    by = c("player_id", "player_name", "team_id", "team_abbreviation"),
    suffix = c("", "_advanced")
  ) %>%
  left_join(
    player_stats_raw$scoring,
    by = c("player_id", "player_name", "team_id", "team_abbreviation"),
    suffix = c("", "_scoring")
  ) %>%
  left_join(
    player_stats_raw$misc,
    by = c("player_id", "player_name", "team_id", "team_abbreviation"),
    suffix = c("", "_misc")
  ) %>%
  left_join(
    player_stats_raw$usage,
    by = c("player_id", "player_name", "team_id", "team_abbreviation"),
    suffix = c("", "_usage")
  ) %>%
  mutate(season = season) %>%
  convert_numeric_cols()


# ============================================================
# Phase 1C: Add derived player metrics
# ============================================================
# TS% was not present in the player_master table, so we compute it.
# Formula:
#   TS% = PTS / [2 * (FGA + 0.44 * FTA)]
# ============================================================

player_master <- player_master %>%
  mutate(
    ts_pct = ifelse(
      (fga + 0.44 * fta) > 0,
      pts / (2 * (fga + 0.44 * fta)),
      NA_real_
    )
  )


# ============================================================
# Phase 1D: Player table quality checks
# ============================================================

player_master %>%
  summarise(
    players = n_distinct(player_id),
    rows = n(),
    missing_player_names = sum(is.na(player_name))
  )

player_master %>%
  arrange(desc(min)) %>%
  select(player_name, team_abbreviation, gp, min, pts, ast, reb) %>%
  head(60)

save_parquet(
  player_master,
  glue("data/processed/player_master_{season}.parquet")
)


# ============================================================
# Phase 1E: Placeholder MVP-style score
# ============================================================
# This is NOT the final model.
#
# Purpose:
#   - Confirm the player data behaves sensibly
#   - Test column validation
#   - Create a first visualizable ranking
#
# Important:
#   We removed usg_pct because it was not present.
# ============================================================

required_cols <- c(
  "pts",
  "ast",
  "reb",
  "tov",
  "ts_pct",
  "min"
)

validate_columns(player_master, required_cols)

mvp_beta <- player_master %>%
  filter(min >= min_minutes) %>%
  mutate(
    pts_z = z_score(pts),
    ast_z = z_score(ast),
    reb_z = z_score(reb),
    ts_pct_z = z_score(ts_pct),
    tov_rate = tov / min,
    tov_rate_z = z_score(tov_rate),
    
    simple_mvp_score =
      0.30 * pts_z +
      0.25 * ast_z +
      0.15 * reb_z +
      0.25 * ts_pct_z -
      0.05 * tov_rate_z
  ) %>%
  arrange(desc(simple_mvp_score))

mvp_beta %>%
  select(
    player_name,
    team_abbreviation,
    min,
    pts,
    ast,
    reb,
    ts_pct,
    tov_rate,
    simple_mvp_score
  ) %>%
  head(25)

save_parquet(
  mvp_beta,
  glue("data/processed/mvp_beta_{season}.parquet")
)


# ============================================================
# Phase 2: Pull games, play-by-play, and rotations
# ============================================================
# Purpose:
# Move from player-season summaries to game-level data.
#
# For plus-minus, we need:
#   - games
#   - play-by-play
#   - rotation intervals
#
# We intentionally sample a few LAL games first.
# Do not pull the whole league until the pipeline is validated.
# ============================================================

games <- nba_leaguegamefinder(
  season_nullable = season,
  team_abbreviation_nullable = team_abbr
) %>%
  extract_hoopr_table() %>%
  clean_names()

glimpse(games)

save_parquet(
  games,
  glue("data/raw/games/{team_abbr}_games_{season}.parquet")
)


# ============================================================
# Phase 2A: Select sample games
# ============================================================
# Important bug we found:
# nba_leaguegamefinder returned rows for both teams in some games.
# So we must filter to team_abbreviation == team_abbr before sampling.
# ============================================================

team_games <- games %>%
  filter(team_abbreviation == team_abbr) %>%
  distinct(game_id, team_id, team_abbreviation, matchup)

sample_games <- team_games %>%
  slice_head(n = 3) %>%
  pull(game_id)

sample_games


# ============================================================
# Phase 2B: Pull play-by-play
# ============================================================

pull_pbp_safe <- function(game_id) {
  message("Pulling PBP for game: ", game_id)
  
  nba_playbyplayv3(game_id = game_id) %>%
    extract_hoopr_table() %>%
    clean_names() %>%
    mutate(game_id = game_id)
}

pbp_sample <- map_dfr(sample_games, pull_pbp_safe)

glimpse(pbp_sample)

save_parquet(
  pbp_sample,
  glue("data/raw/pbp/{team_abbr}_pbp_sample_{season}.parquet")
)


# ============================================================
# Phase 2C: Pull rotations
# ============================================================
# Important bug we found:
# nba_gamerotation() may return multiple data frames internally.
# extract_hoopr_table() only grabbed the first one.
#
# Fix:
# Use extract_all_hoopr_tables() so both teams' rotations are retained.
# ============================================================

pull_rotation_safe <- function(game_id) {
  message("Pulling rotations for game: ", game_id)
  
  nba_gamerotation(game_id = game_id) %>%
    extract_all_hoopr_tables() %>%
    clean_names() %>%
    mutate(game_id = game_id)
}

rotation_sample <- map_dfr(sample_games, pull_rotation_safe)

glimpse(rotation_sample)

save_parquet(
  rotation_sample,
  glue("data/raw/rotations/{team_abbr}_rotation_sample_{season}.parquet")
)


# ============================================================
# Phase 2D: Confirm PBP and rotations were pulled
# ============================================================

n_distinct(pbp_sample$game_id)
n_distinct(rotation_sample$game_id)

pbp_sample %>% count(game_id)
rotation_sample %>% count(game_id)

rotation_sample %>%
  distinct(game_id, team_id) %>%
  arrange(game_id, team_id)


# ============================================================
# Phase 3: Build stint table from rotation data
# ============================================================
# Purpose:
# Create one row per stint.
#
# A stint is a time interval where the 10 players on court do not change.
#
# Output target:
#   stints with exactly 10 players in players_on_court.
# ============================================================


# ============================================================
# Phase 3A: Clean rotation intervals
# ============================================================
# Note:
# In rotation data, player ID is person_id, not player_id.
# ============================================================

rotation_clean <- rotation_sample %>%
  mutate(
    in_time = as.numeric(in_time_real),
    out_time = as.numeric(out_time_real)
  ) %>%
  select(game_id, team_id, person_id, in_time, out_time)


# ============================================================
# Phase 3B: Create stint boundaries
# ============================================================
# Every substitution in-time or out-time is a potential stint boundary.
# ============================================================

stint_boundaries <- rotation_clean %>%
  select(game_id, in_time, out_time) %>%
  pivot_longer(cols = c(in_time, out_time)) %>%
  rename(time = value) %>%
  distinct(game_id, time) %>%
  arrange(game_id, time)


# ============================================================
# Phase 3C: Build start/end intervals
# ============================================================

stints <- stint_boundaries %>%
  group_by(game_id) %>%
  arrange(time) %>%
  mutate(
    stint_id = row_number(),
    start_time = time,
    end_time = lead(time)
  ) %>%
  filter(!is.na(end_time)) %>%
  ungroup()


# ============================================================
# Phase 3D: Identify players on court for each stint
# ============================================================
# Logic:
# A player is on court for a stint if:
#   in_time <= stint start
#   out_time >= stint end
# ============================================================

get_players_on_court <- function(game_id_i, start_time_i, end_time_i, rotation_data) {
  rotation_data %>%
    filter(
      game_id == game_id_i,
      in_time <= start_time_i,
      out_time >= end_time_i
    ) %>%
    pull(person_id)
}

stints <- stints %>%
  mutate(
    players_on_court = pmap(
      list(game_id, start_time, end_time),
      ~ get_players_on_court(..1, ..2, ..3, rotation_clean)
    )
  )


# ============================================================
# Phase 3E: Stint validation
# ============================================================
# Success condition:
# Every stint should have 10 players.
#
# Current result achieved:
#   88 stints
#   10 players per stint
# ============================================================

stints %>%
  mutate(n_players = map_int(players_on_court, length)) %>%
  count(n_players)


# ============================================================
# End of current pipeline
# ============================================================
# Next phase:
# Phase 4 = attach score change to each stint.
#
# Once each stint has:
#   - game_id
#   - start_time
#   - end_time
#   - 10 players on court
#   - point differential during stint
#
# We can build the first crude Team Impact Plus-Minus model.
# ============================================================

# ============================================================
# Phase 4: Attach score change to each stint
# ============================================================
# Goal:
# For each stint, identify:
#   - score at stint start
#   - score at stint end
#   - point differential change during stint
#
# This gives us the first crude plus-minus outcome.
# ============================================================



source("./R/00_load_project.R")
# ============================================================
# Phase 4A: Helper - parse NBA clock
# ============================================================

parse_nba_clock_seconds <- function(clock) {
  
  clock_chr <- as.character(clock)
  
  case_when(
    str_detect(clock_chr, "^PT") ~ {
      mins <- str_match(clock_chr, "PT(\\d+)M")[, 2] %>% as.numeric()
      secs <- str_match(clock_chr, "M([0-9.]+)S")[, 2] %>% as.numeric()
      replace_na(mins, 0) * 60 + replace_na(secs, 0)
    },
    
    str_detect(clock_chr, ":") ~ {
      mins <- str_split_fixed(clock_chr, ":", 2)[, 1] %>% as.numeric()
      secs <- str_split_fixed(clock_chr, ":", 2)[, 2] %>% as.numeric()
      mins * 60 + secs
    },
    
    TRUE ~ NA_real_
  )
}


# ============================================================
# Phase 4B: Create event time from play-by-play
# ============================================================
# Rotation data uses game time in tenths of seconds.
# So we convert PBP clock into the same scale.
#
# Regulation quarters = 12 minutes
# Overtime periods = 5 minutes
# ============================================================

pbp_score_events <- pbp_sample %>%
  mutate(
    clock_seconds_remaining = parse_nba_clock_seconds(clock),
    
    period_length_seconds = ifelse(period <= 4, 12 * 60, 5 * 60),
    
    period_start_seconds = case_when(
      period <= 4 ~ (period - 1) * 12 * 60,
      period > 4  ~ 4 * 12 * 60 + (period - 5) * 5 * 60
    ),
    
    elapsed_seconds =
      period_start_seconds +
      (period_length_seconds - clock_seconds_remaining),
    
    event_time = elapsed_seconds * 10,
    
    score_home = as.numeric(score_home),
    score_away = as.numeric(score_away)
  ) %>%
  filter(!is.na(score_home), !is.na(score_away)) %>%
  select(
    game_id,
    action_number,
    period,
    clock,
    event_time,
    score_home,
    score_away
  ) %>%
  arrange(game_id, event_time, action_number)


# ============================================================
# Phase 4C: Build score timeline
# ============================================================
# Add 0-0 at time 0 for every game.
# If multiple scoring events happen at the same time, keep the latest one.
# ============================================================

score_timeline <- pbp_score_events %>%
  group_by(game_id, event_time) %>%
  arrange(action_number, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(game_id, event_time, score_home, score_away)

score_baseline <- pbp_sample %>%
  distinct(game_id) %>%
  mutate(
    event_time = 0,
    score_home = 0,
    score_away = 0
  )

score_timeline <- bind_rows(score_baseline, score_timeline) %>%
  arrange(game_id, event_time)


# ============================================================
# Phase 4D: Helper - look up latest score at a given time
# ============================================================

get_score_at_time <- function(game_id_i, time_i, score_data) {
  
  score_row <- score_data %>%
    filter(
      game_id == game_id_i,
      event_time <= time_i
    ) %>%
    arrange(desc(event_time)) %>%
    slice_head(n = 1)
  
  if (nrow(score_row) == 0) {
    return(tibble(
      score_time = NA_real_,
      score_home = NA_real_,
      score_away = NA_real_
    ))
  }
  
  tibble(
    score_time = score_row$event_time,
    score_home = score_row$score_home,
    score_away = score_row$score_away
  )
}


# ============================================================
# Phase 4E: Attach start and end scores to stints
# ============================================================

start_scores <- pmap_dfr(
  list(stints$game_id, stints$start_time),
  ~ get_score_at_time(..1, ..2, score_timeline)
) %>%
  rename(
    start_score_time = score_time,
    start_score_home = score_home,
    start_score_away = score_away
  )

end_scores <- pmap_dfr(
  list(stints$game_id, stints$end_time),
  ~ get_score_at_time(..1, ..2, score_timeline)
) %>%
  rename(
    end_score_time = score_time,
    end_score_home = score_home,
    end_score_away = score_away
  )

stints_scored <- bind_cols(
  stints,
  start_scores,
  end_scores
)


# ============================================================
# Phase 4F: Add target team perspective
# ============================================================
# For LAL:
#   home margin if LAL is home
#   away margin if LAL is away
# ============================================================

team_game_context <- team_games %>%
  mutate(
    target_team_is_home = str_detect(matchup, "vs\\.")
  ) %>%
  select(
    game_id,
    target_team_id = team_id,
    target_team_abbr = team_abbreviation,
    matchup,
    target_team_is_home
  )

stints_scored <- stints_scored %>%
  left_join(team_game_context, by = "game_id") %>%
  mutate(
    start_home_margin = start_score_home - start_score_away,
    end_home_margin   = end_score_home - end_score_away,
    home_margin_change = end_home_margin - start_home_margin,
    
    start_target_margin = ifelse(
      target_team_is_home,
      start_score_home - start_score_away,
      start_score_away - start_score_home
    ),
    
    end_target_margin = ifelse(
      target_team_is_home,
      end_score_home - end_score_away,
      end_score_away - end_score_home
    ),
    
    target_margin_change = end_target_margin - start_target_margin,
    
    target_points_for = ifelse(
      target_team_is_home,
      end_score_home - start_score_home,
      end_score_away - start_score_away
    ),
    
    target_points_against = ifelse(
      target_team_is_home,
      end_score_away - start_score_away,
      end_score_home - start_score_home
    )
  )


# ============================================================
# Phase 4G: Validate scored stints
# ============================================================

stints_scored %>%
  summarise(
    stints = n(),
    missing_start_scores = sum(is.na(start_score_home) | is.na(start_score_away)),
    missing_end_scores = sum(is.na(end_score_home) | is.na(end_score_away)),
    total_target_margin_change = sum(target_margin_change, na.rm = TRUE)
  )

stints_scored %>%
  select(
    game_id,
    stint_id,
    start_time,
    end_time,
    start_score_home,
    start_score_away,
    end_score_home,
    end_score_away,
    target_points_for,
    target_points_against,
    target_margin_change
  ) %>%
  head(20)


# ============================================================
# Phase 4H: Save scored stints
# ============================================================

save_parquet(
  stints_scored,
  glue("data/processed/{team_abbr}_stints_scored_{season}.parquet")
)