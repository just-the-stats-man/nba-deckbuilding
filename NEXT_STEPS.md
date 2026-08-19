# Next Steps

The next implementation pass should build the reproducible player-season data foundation. Do not begin substantive analyses until these steps are complete.

## Immediate Tasks

1. Choose and document public data sources for player-season records and established metrics such as BPM, OBPM, DBPM, VORP, Win Shares, WS/48, and PER.
2. Create a metric provenance register in `docs/data_dictionary.md` or a dedicated `docs/metric_provenance.md`.
3. Define the player-season grain, especially how to handle split-team seasons and `TOT` rows.
4. Create R data acquisition functions under `R/data/` that write raw source extracts to `data/raw/` without analysis logic.
5. Create R curation functions that transform raw extracts into a documented `data/derived/player_season/` dataset.
6. Add validation checks for required fields, duplicate player-season keys, season formats, missingness, and impossible values.
7. Add a small Quarto or R script in `analyses/00_data_foundation/` that reports data provenance and validation results only.
8. Initialize `renv` after the package set stabilizes enough to justify locking versions.
9. Add focused `testthat` tests for reusable data-processing functions once they exist.

## Guardrails

- Keep acquisition code separate from analysis reports.
- Do not scrape or redistribute data in violation of source terms.
- Do not use future information to define baseline covariates.
- Do not move into player-game, stint, tracking, injury, or transaction modules until a planned case study requires them.
