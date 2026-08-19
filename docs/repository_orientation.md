# Repository Orientation Summary

This summary was created during the first RWE reboot pass before modifying project files.

## Existing Legacy Work

- The repository contains an R-first NBA pipeline built around hoopR, player statistics, team games, play-by-play, rotations, stints, lineup plus-minus, player-card style summaries, attack/creation/defense components, and physical profile outputs.
- Root-level scripts `00_config.R` through `28_build_body_adjusted_card_signals.R` define the legacy workflow. `99_run_pipeline.R` currently sources phases through attack identity work.
- `01_setup_project.R` loads packages including `hoopR`, `tidyverse`, `janitor`, `glue`, `fs`, `arrow`, `here`, `lubridate`, and `naniar`, then sources `00_config.R` and `R/helpers.R`.
- `R/helpers.R` contains reusable helpers for extracting hoopR tables, validating columns, reading/writing parquet files, directory creation, and safe pull wrappers for play-by-play and rotations.
- Existing data include raw player, game, PBP, rotation, tracking, physical, and processed/intermediate parquet files. These should be treated as legacy artifacts and not deleted during the reboot.
- `README_REPLACEMENT.md` and `docs/CODEX_NEXT_PROMPT.txt` document an earlier cleanup direction for the CourtContext/deck-building pipeline.
- The RStudio project file `nba-deckbuilding.Rproj` is configured with UTF-8 encoding, two-space indentation, and standard RStudio project settings.

## Reboot Interpretation

The repository should now be organized around RWE and biostatistics methodology. Legacy basketball-specific engineering can be mined later, but the first methodological foundation should start with player-season, player-game, and game/team data rather than possession or stint engineering.

## Preservation Notes

No legacy scripts, data folders, outputs, R project files, or CourtContext/deck-building artifacts were removed in this first pass. New RWE material was added alongside the existing work.
