# Codex Prompt — Reboot NBA Project as an RWE Methods Portfolio

You are working inside the existing GitHub repository `just-the-stats-man/nba-deckbuilding`.

## Mission

Reboot this project from an attempt to invent a new basketball player/lineup metric into a rigorous, reproducible **Real-World Evidence (RWE) methods portfolio using NBA observational data**.

The central idea is:

> Use observational basketball data as a sandbox to demonstrate, compare, explain, and validate the major study designs and statistical methodologies used in real-world evidence research.

This is primarily a **biostatistics / RWE methods project**, not a sports-ranking project. Do **not** invent a proprietary all-in-one basketball metric. Prefer established, publicly documented basketball outcomes and measures such as BPM, OBPM, DBPM, VORP, Win Shares, WS/48, PER, plus conventional game/team outcomes where appropriate.

The long-term goal is for the repository to function like an applied RWE textbook/portfolio: each method has a basketball case study, explicit estimand, assumptions, diagnostics, R implementation, results, sensitivity analyses, and interpretation.

## Critical constraints

1. **Preserve all existing work.** Do not delete or overwrite the existing R scripts, data folders, R project file, or previous CourtContext/deck-building work. Treat them as legacy material that may be reused later.
2. Do not build a new basketball metric.
3. Do not start with possession-level or stint-level engineering unless it is actually required for a specific methodology. Start with player-season, player-game, and game/team data.
4. Use **R** as the primary analysis language.
5. Use **Quarto** for reports/site-ready methodological case studies.
6. Prioritize reproducibility, clean data provenance, modular functions, and transparent assumptions.
7. Do not claim causal effects unless the design and assumptions justify them.
8. Keep data acquisition separate from analysis code.
9. Do not commit large raw datasets or secrets/API keys. Add appropriate `.gitignore` rules if necessary.
10. Before modifying existing files, inspect them and preserve anything useful.

## First task: orient to the repository

Inspect the current repository structure and summarize what already exists. In particular, review the existing setup scripts, play-by-play/stint work, `R/` directory, `data/` directory, and R project configuration.

Then create a clean new structure for the RWE reboot without destroying the legacy structure. A reasonable target is:

```text
nba-deckbuilding/
├── README.md
├── CODEX_PROMPT_RWE_REBOOT.md
├── RWE_METHODS_ROADMAP.md
├── _quarto.yml
├── index.qmd
├── R/
│   ├── data/
│   ├── methods/
│   ├── diagnostics/
│   └── utils/
├── analyses/
│   ├── 00_data_foundation/
│   ├── 01_descriptive_longitudinal/
│   ├── 02_confounding_causal/
│   ├── 03_longitudinal_causal/
│   ├── 04_quasi_experimental/
│   ├── 05_survival/
│   ├── 06_missing_bias/
│   ├── 07_heterogeneity_transportability/
│   └── 08_bayesian_prediction_synthesis/
├── data/
│   ├── raw/
│   ├── interim/
│   └── derived/
└── docs/
    ├── data_dictionary.md
    ├── estimand_template.md
    └── project_principles.md
```

Adapt this structure if the repository already has conventions that should be preserved.

## Build the master RWE methodology roadmap

Create `RWE_METHODS_ROADMAP.md`. This is the controlling document for the project.

For every method, include at minimum:

- Method/domain
- What problem it solves
- Typical RWE use
- Basketball analogue / candidate case study
- Required data level
- Key estimand, where applicable
- Major assumptions
- Important diagnostics
- Candidate R packages
- Status (`Not started`, `Planned`, `In progress`, `Complete`)

Organize the roadmap into the following domains. Be comprehensive rather than minimalist, but distinguish core methods from advanced/optional extensions.

### 1. Study design and estimands

Include:
- descriptive epidemiology / descriptive cohort studies
- cross-sectional studies
- cohort studies
- case-control and nested case-control designs
- new-user / active-comparator design concepts
- target trial emulation
- eligibility criteria
- treatment strategies
- time zero alignment
- immortal-time bias
- prevalent-user bias
- estimands: ATE, ATT, ATC, overlap-population effects, risk difference, risk ratio, odds ratio, rate ratio, mean difference, survival estimands

### 2. Conventional adjustment and causal inference

Include:
- covariate adjustment / outcome regression
- propensity scores
- PS matching
- PS stratification/subclassification
- IPTW
- stabilized weights
- overlap weighting
- entropy balancing if appropriate
- standardized mortality/morbidity weighting concepts if useful
- g-computation / parametric g-formula
- augmented inverse probability weighting
- doubly robust estimation
- TMLE
- positivity / overlap assessment
- covariate-balance diagnostics

### 3. Longitudinal causal inference

Include:
- time-varying exposures
- time-varying confounding
- marginal structural models
- inverse probability of treatment weights over time
- inverse probability of censoring weights
- longitudinal g-formula
- structural nested models / g-estimation as an advanced topic
- clone-censor-weight approaches where relevant

### 4. Quasi-experimental and natural-experiment methods

Include:
- difference-in-differences
- event-study formulations
- parallel-trends diagnostics
- interrupted time series
- segmented regression
- synthetic control
- instrumental variables
- two-stage least squares
- regression discontinuity
- regression kink design as optional
- negative-control or falsification-style designs where relevant

### 5. Repeated-measures and longitudinal outcome modeling

Include:
- repeated-measures regression
- generalized estimating equations
- linear mixed-effects models
- generalized linear mixed models
- random intercepts/slopes
- splines and nonlinear trajectories
- within-person versus between-person effects
- fixed-effects/panel models where useful

### 6. Time-to-event methods

Include:
- Kaplan-Meier
- log-rank comparisons
- Cox proportional hazards models
- proportional-hazards diagnostics
- flexible parametric survival models
- competing risks
- cause-specific hazards
- Fine-Gray models
- recurrent-event models
- Andersen-Gill / Prentice-Williams-Peterson concepts
- multi-state models
- time-varying covariates
- landmarking where appropriate

### 7. Missing data, measurement error, bias, and sensitivity analysis

Include:
- missingness mechanisms (MCAR/MAR/MNAR)
- complete-case analysis
- multiple imputation
- inverse probability approaches to missingness/censoring
- delta-adjustment / tipping-point concepts
- measurement error / misclassification
- probabilistic bias analysis
- quantitative bias analysis
- unmeasured-confounding sensitivity analysis
- E-values
- negative controls
- falsification endpoints/exposures
- robustness/sensitivity analyses across exposure and outcome definitions

### 8. Heterogeneity, generalizability, and transportability

Include:
- subgroup analysis
- statistical interaction / effect modification
- heterogeneous treatment effects
- standardization
- inverse odds of sampling weights / transport weighting concepts
- generalizability versus transportability
- temporal transport across NBA eras
- team/context transportability

### 9. Bayesian, prediction/ML, and evidence synthesis

Include:
- Bayesian regression
- hierarchical Bayesian models
- partial pooling
- posterior predictive checks
- Bayesian causal inference concepts
- penalized regression (ridge/LASSO/elastic net)
- tree-based ML / boosting where useful
- prediction versus causal inference distinction
- model discrimination
- calibration
- internal validation
- bootstrap/cross-validation
- temporal validation
- external validation
- meta-analysis
- fixed-effect and random-effects meta-analysis
- heterogeneity measures
- meta-regression
- hierarchical synthesis across seasons/teams

Also include any major RWE methodology that is conspicuously missing from the above list. Mark highly specialized topics as advanced rather than pretending every method must be used.

## Standard analysis template

Create `docs/estimand_template.md` defining the structure that every substantive case study should follow:

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

The template should explicitly separate **descriptive/associational analyses** from **causal analyses**.

## Data foundation plan

Create a data architecture document and a data dictionary skeleton. The initial target data layers should be:

### Player-season
Candidate fields:
- player ID
- player name
- season
- team
- age
- games
- minutes
- conventional box-score statistics
- advanced public statistics
- BPM
- OBPM
- DBPM
- VORP
- Win Shares
- WS/48
- PER

### Player-game
Candidate fields:
- player ID
- game ID
- date
- season
- team
- opponent
- home/away
- starter status
- minutes
- box-score statistics
- plus/minus where available
- team result
- days rest
- relevant pre-game covariates that can be defined without leakage

### Game/team
Candidate fields:
- game ID
- date
- season
- team/opponent
- home/away
- score
- possessions or pace where available
- offensive/defensive rating where appropriate
- rest/travel proxies where defensible

Do not yet require injury, transaction, lineup, tracking, or possession-level data. Document these as optional future modules that should be added only when a study requires them.

## Established basketball metrics

Treat existing basketball metrics as measurements/outcomes, not as something this project must replace.

For each metric considered, document:
- source
- public availability
- whether the formula/methodology is public
- reproducibility from public data
- interpretation
- known limitations
- whether it is appropriate as an outcome, covariate, descriptive measure, or sensitivity outcome

Prioritize publicly documented/reproducible measures. Do not scrape or redistribute data in violation of source terms. Clearly document provenance and access methods.

## Initial case-study sequence

Do not try to implement the whole roadmap immediately. Create placeholders/plans for the following progression.

### Case Study 1 — Descriptive longitudinal modeling

Question:
> How does established NBA player performance vary with age across a player's career?

Primary purpose: demonstrate cohort construction, descriptive statistics, nonlinear modeling, and repeated measures—not causal inference.

Suggested progression:
- descriptive cohort table
- visualization of age versus BPM
- naive linear regression
- nonlinear age effect using restricted cubic splines or another defensible smoother
- repeated-measures model using GEE and/or mixed effects
- diagnostics
- sensitivity outcome using another established metric such as WS/48 or VORP where appropriate

### Case Study 2 — Confounding and causal adjustment

Candidate question:
> Among comparable NBA players, what is the effect of changing teams between seasons on subsequent-season performance?

Do not assume this question is automatically identifiable. First define a target-trial-style protocol and discuss selection, confounding, positivity, and time-zero alignment.

If defensible, compare:
- unadjusted estimate
- conventional multivariable adjustment
- propensity score matching
- IPTW
- overlap weighting
- g-computation
- doubly robust estimation

The educational objective is to compare assumptions, diagnostics, target populations/estimands, and estimates—not to crown one estimator as universally best.

### Later case studies

Create candidate questions for:
- marginal structural models / time-varying confounding
- difference-in-differences
- interrupted time series
- IV or regression discontinuity if a credible natural experiment exists
- survival / competing risks / recurrent events
- missing-data and quantitative bias analysis
- HTE
- transportability across eras or teams
- Bayesian hierarchical modeling
- predictive modeling and temporal validation
- meta-analysis/evidence synthesis across seasons or contexts

Do not fabricate a causal instrument or discontinuity merely to demonstrate a method. If no credible basketball example exists, document that limitation and use a simulation or clearly labeled methodological demonstration instead.

## README

Create or update the repository `README.md` so a visitor immediately understands the pivot.

It should explain:
- this is an RWE/biostatistics methods portfolio using NBA observational data
- the goal is methodological demonstration, not inventing a player ranking
- established basketball metrics are used where useful
- analyses are reproducible and assumption-driven
- current project status
- repository structure
- roadmap link
- how to reproduce analyses when code becomes available

Preserve mention of the legacy CourtContext/deck-building work in a short history/legacy section rather than pretending it never existed.

## Reproducibility tooling

Set up a sensible R reproducibility foundation. Prefer lightweight, conventional tools. Consider:
- `renv` for package versions
- `targets` only if/when the workflow becomes complex enough to justify it
- `here` for file paths
- Quarto for reports/site
- `testthat` for reusable data-processing functions where valuable

Do not add complexity merely because a package exists.

## Git hygiene

Review `.gitignore`. Ensure typical RStudio user-state files and large/raw generated datasets are ignored. Existing accidentally tracked files should not be deleted without explaining the change first.

Make changes in small, interpretable commits where possible.

## Deliverables for this first reboot pass

Complete only the project-foundation work, not the substantive analyses. The desired deliverables are:

1. Repository orientation summary
2. `README.md`
3. `RWE_METHODS_ROADMAP.md`
4. `docs/estimand_template.md`
5. `docs/data_dictionary.md`
6. `docs/project_principles.md`
7. Quarto skeleton (`_quarto.yml`, `index.qmd`) if it fits cleanly
8. sensible directory structure for future analyses
9. `.gitignore` cleanup where appropriate
10. a concise `NEXT_STEPS.md` specifying exactly what should be implemented next to build the reproducible player-season dataset

Do **not** perform major data scraping or run the first analysis during this pass unless needed only to validate repository setup.

## Quality bar

Approach this like a senior biostatistics/RWE portfolio rather than a tutorial. Be explicit about estimands, assumptions, temporal alignment, data leakage, confounding, selection bias, missingness, uncertainty, and causal versus associational interpretation.

At the same time, keep the writing understandable to technically literate basketball fans. Every analysis should eventually answer both:

1. **What does this tell us about basketball?**
2. **What does this teach us about RWE methodology?**

Before making changes, inspect the repository. After making changes, summarize exactly what you created or modified, what legacy work was preserved, and the next recommended action.