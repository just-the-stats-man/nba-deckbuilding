# Project Principles

## Purpose

This project uses NBA observational data to demonstrate RWE and biostatistics methods. The portfolio should teach both what the analysis says about basketball and what the analysis teaches about study design, assumptions, diagnostics, and uncertainty.

## Principles

1. Preserve legacy CourtContext/deck-building work. Do not delete old scripts, outputs, or data without a separate review.
2. Do not create a proprietary all-in-one basketball metric.
3. Treat established basketball metrics as measurements with provenance, assumptions, and limitations.
4. Separate data acquisition from analysis.
5. Start with player-season, player-game, and game/team data unless a specific method requires finer granularity.
6. Define the target population, time zero, follow-up, outcome, and estimand before modeling.
7. Keep descriptive, associational, predictive, and causal claims clearly separated.
8. Do not claim causal effects without explicit identification assumptions and diagnostics.
9. Audit temporal alignment to avoid leakage, immortal-time bias, and post-treatment adjustment.
10. Assess missingness, selection, measurement error, overlap, model fit, and uncertainty.
11. Prefer reproducible R workflows, Quarto reports, and documented data provenance.
12. Avoid committing secrets, API keys, or large regenerated datasets.

## Communication Standard

Write for technically literate basketball fans and statistically trained readers at the same time. Reports should be precise enough for method review and clear enough that the basketball analogue remains understandable.
