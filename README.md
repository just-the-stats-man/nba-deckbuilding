# NBA RWE Methods Portfolio

This repository is being rebooted as a Real-World Evidence (RWE) and biostatistics methods portfolio using NBA observational data.

The goal is methodological demonstration, not inventing a proprietary basketball ranking. NBA data provide an accessible sandbox for questions that resemble applied RWE work: cohort construction, temporal alignment, confounding, missingness, selection bias, repeated measures, quasi-experimental designs, time-to-event endpoints, transportability, prediction, and evidence synthesis.

Established public basketball measures such as BPM, OBPM, DBPM, VORP, Win Shares, WS/48, PER, and conventional game or team outcomes will be treated as measurements, outcomes, covariates, or sensitivity endpoints where appropriate. They are not being replaced by a new all-in-one metric.

## Current Status

This is the first reboot pass. The repository now has a methodological roadmap, analysis template, data dictionary skeleton, project principles, Quarto site skeleton, and future analysis directories. Substantive NBA data acquisition and statistical analyses have not started in this pass.

## Repository Structure

- `CODEX_PROMPT_RWE_REBOOT.md`: authoritative reboot specification.
- `RWE_METHODS_ROADMAP.md`: controlling roadmap for methods and candidate NBA case studies.
- `NEXT_STEPS.md`: immediate implementation plan for the reproducible player-season data foundation.
- `_quarto.yml` and `index.qmd`: Quarto site skeleton for future rendered case studies.
- `docs/`: templates, principles, data dictionary, and repository orientation notes.
- `R/data/`: future reusable data acquisition and curation functions.
- `R/methods/`: future reusable statistical method helpers.
- `R/diagnostics/`: future balance, model, and assumption diagnostics.
- `R/utils/`: future shared utility functions for the RWE reboot.
- `analyses/`: planned Quarto/R case study folders organized by methods domain.
- `data/raw/`, `data/interim/`, `data/derived/`: planned data layers; large generated data are ignored by default.

## Reproducibility Direction

Analyses should be reproducible, modular, and explicit about provenance. Data acquisition should remain separate from analysis code. Future work should use `here` for paths, Quarto for reports, and `renv` once package versions need to be locked. `targets` should be considered only when the workflow becomes complex enough to justify it.

Each substantive case study should state its research question, target population, eligibility criteria, exposure or comparison, time zero, follow-up, outcome, estimand, assumptions, diagnostics, uncertainty, sensitivity analyses, limitations, and interpretation. Descriptive or associational analyses must not be presented as causal.

## Legacy History

The repository began as CourtContext / NBA deck-building work with R scripts for hoopR data pulls, play-by-play, rotations, stints, lineup plus-minus, player cards, attack/creation/defense components, and physical profiles. That work is preserved as legacy material and may be reused selectively later, but it is no longer the organizing objective of the project.

## Roadmap

Start with [RWE_METHODS_ROADMAP.md](RWE_METHODS_ROADMAP.md), then use [docs/estimand_template.md](docs/estimand_template.md) for every substantive case study.

When code is available, each analysis folder should include a Quarto report and any analysis-specific scripts needed to reproduce the results from documented data layers.
