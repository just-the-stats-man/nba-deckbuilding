# NBA Deckbuilding / CourtContext R Pipeline

This repo is organized so scripts can be run without relying on objects hidden in the R Global Environment.

## Run order

Run the full sample pipeline:

```r
source("99_run_pipeline.R")
```

Or run phase-by-phase:

```r
source("02_player_season.R")
source("03_pull_games_pbp_rotations.R")
source("04_build_stints.R")
source("05_score_stints.R")
```

## File purpose

- `00_config.R`: project settings such as season, team, sample game count, and folders.
- `01_setup_project.R`: packages, folders, helpers, and shared objects.
- `R/helpers.R`: reusable functions only.
- `02_player_season.R`: pulls player stats and builds player master + beta MVP score.
- `03_pull_games_pbp_rotations.R`: pulls team games, sample PBP, and rotations.
- `04_build_stints.R`: converts rotations into stint rows with players on court.
- `05_score_stints.R`: attaches score changes to stints as a first conservative plus-minus dataset.
- `99_run_pipeline.R`: one-button runner.

## Important limitation

`05_score_stints.R` is intentionally conservative. If the PBP time/score columns returned by hoopR differ, the script stops and tells you which mapping Codex needs to fix. Do not fake lineup plus-minus until the time and score columns are verified.

## Suggested Codex instruction

Ask Codex:

```text
Review this reorganized R pipeline. Do not rewrite the project. First, run static checks for missing functions, missing sourced files, undefined objects, and inconsistent paths. Then make the smallest fixes needed so source("99_run_pipeline.R") can run locally.
```
