# ============================================================
# 00_config.R
# Project-wide settings. Edit this file first.
# ============================================================

PROJECT_SEASON <- "2025-26"
PROJECT_TEAM_ABBR <- "LAL"
PROJECT_MAX_GAMES <- NULL
PROJECT_SAMPLE_GAMES <- FALSE
PROJECT_MIN_MINUTES <- 500

# Keep all paths relative to the repo root.
PROJECT_DIRS <- c(
  "R",
  "data/raw/player",
  "data/raw/games",
  "data/raw/pbp",
  "data/raw/rotations",
  "data/processed/player",
  "data/processed/stints",
  "outputs/plots",
  "outputs/attacks",
  "outputs/tables",
  "docs"
)
