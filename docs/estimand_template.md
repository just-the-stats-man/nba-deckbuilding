# Estimand and Analysis Template

Use this template for every substantive case study. Each report should state whether it is descriptive, associational, predictive, or causal. Do not imply causal effects unless the design, estimand, and assumptions support a causal interpretation.

## Analysis Type

- Descriptive: Summarizes observed patterns. No causal estimand.
- Associational: Estimates relationships conditional on observed variables. No causal estimand unless explicitly justified.
- Predictive: Optimizes prediction of future or held-out outcomes. Prediction performance is not causal evidence.
- Causal: Estimates a contrast between exposure or treatment strategies under stated identification assumptions.

## Template

1. Research question
2. Target population
3. Eligibility criteria
4. Exposure/treatment strategy
5. Comparator
6. Time zero / index date
7. Follow-up
8. Outcome
9. Estimand
10. Baseline covariates/confounders
11. Causal diagram/DAG considerations where appropriate
12. Identification assumptions
13. Statistical method
14. Diagnostics
15. Primary estimate with uncertainty interval
16. Sensitivity analyses
17. Limitations
18. Interpretation
19. Reproducibility/data provenance

## Descriptive or Associational Analyses

For non-causal analyses, replace causal language with descriptive targets:

- Target quantity: observed mean, rate, distribution, trajectory, association, prediction error, or model parameter.
- Timing: define the observation window and make clear whether covariates are baseline, concurrent, or post-outcome.
- Interpretation: describe what was observed in the available data and avoid counterfactual statements.
- Diagnostics: include missingness, influential observations, model fit, functional form, clustering, and sensitivity to inclusion criteria.

## Causal Analyses

For causal analyses, include a protocol-style specification:

- Treatment strategies must be well-defined.
- Eligibility must be assessed before time zero.
- Baseline confounders must be measured before exposure.
- Follow-up must begin at a defensible index time.
- Outcomes must occur after time zero.
- Positivity and overlap must be assessed before interpreting estimates.
- Sensitivity analyses should address unmeasured confounding, missingness, exposure definitions, outcome definitions, and temporal alignment.

Any causal conclusion should name the target population, treatment contrast, estimand, assumptions, uncertainty interval, and main limitations.
