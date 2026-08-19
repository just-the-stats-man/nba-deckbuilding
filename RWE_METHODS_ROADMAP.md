# RWE Methods Roadmap

This document controls the NBA RWE methods portfolio. Each method should eventually have a case study or a documented reason why a real NBA example is not credible. Established basketball metrics are used as observed measurements or outcomes, not as replacements to be reinvented.

Status values: `Not started`, `Planned`, `In progress`, `Complete`.

## 1. Study Design and Estimands

| Method/domain | Problem it solves | Typical RWE use | Basketball analogue / candidate case study | Required data level | Key estimand | Major assumptions | Important diagnostics | Candidate R packages | Status |
|---|---|---|---|---|---|---|---|---|---|
| Descriptive cohort study | Defines who is observed and summarizes patterns | Disease burden, treatment utilization, outcomes over time | Career aging curves for BPM or WS/48 | Player-season | Mean trajectory, prevalence, rates | Complete enough capture, consistent definitions | Missingness by season/age, cohort flow | tidyverse, gtsummary, ggplot2 | Planned |
| Cross-sectional study | Describes associations at one time point | Risk factor and outcome association | Association between role, age, minutes, and BPM in one season | Player-season | Mean difference, odds ratio | No causal interpretation without design | Collinearity, influential observations | broom, sandwich | Not started |
| Cohort study | Follows eligible units from time zero | Comparative effectiveness, safety | Players followed from age 22 or first qualifying season | Player-season | Risk difference, mean difference, rate ratio | Exchangeability if causal, consistent follow-up | Attrition, censoring, baseline balance | survival, geepack, lme4 | Planned |
| Case-control / nested case-control | Efficiently studies rare outcomes | Rare adverse events | Cases: major performance decline; controls sampled from risk sets | Player-season | Odds ratio | Controls represent source population | Case definition sensitivity, matching quality | Epi, survival | Not started |
| New-user / active-comparator concepts | Reduces prevalent-user and comparator bias | Medication initiator designs | First season after team change vs comparable non-changing players | Player-season | ATT or ATE | Clear treatment initiation and comparable comparator | Baseline windows, prior exposure history | MatchIt, WeightIt | Planned |
| Target trial emulation | Aligns causal question to trial protocol | Emulating pragmatic trials | Team-change effect on next-season performance | Player-season | ATE, ATT, risk/mean difference | Eligibility, time zero, strategies, follow-up correctly aligned | Protocol table, immortal-time checks | TrialEmulation, survival | Planned |
| Eligibility criteria | Defines analyzable target population | Study inclusion/exclusion | Minimum minutes/games before index season | Player-season | Depends on study | Criteria do not condition on post-index information | Flow diagram, excluded-player summaries | dplyr, gtsummary | Planned |
| Treatment strategies | Defines interventions/exposures | Treatment definitions | Changed team vs stayed with team between seasons | Player-season | Strategy-specific contrast | Strategies are well-defined and observable | Exposure definition sensitivity | tidyverse | Planned |
| Time zero alignment | Prevents biased follow-up | Index date definition | Start of next season after team-change classification | Player-season/player-game | Depends on study | No outcome time included before index | Timeline audit | lubridate, slider | Planned |
| Immortal-time bias | Avoids guaranteed survival/follow-up time | Pharmacoepidemiology exposure timing | Classifying a player as traded based on future season events | Player-season/player-game | Bias diagnostic | Exposure defined before follow-up | Person-time plots | survival | Planned |
| Prevalent-user bias | Avoids mixing continuing and initiating exposures | New-user designs | Long-tenured changed-role players vs first role change | Player-season | Bias diagnostic | Prior exposure measurable | Prior-history summaries | dplyr | Not started |
| Estimands | Clarifies target quantity | Treatment policy, hypothetical, attributable effects | ATE of team change among eligible players; ATT among changers | Any | ATE, ATT, ATC, overlap effect, RD, RR, OR, rate ratio, mean difference, survival estimands | Estimand matches design and data | Estimand table | marginaleffects, emmeans | Planned |

## 2. Conventional Adjustment and Causal Inference

| Method/domain | Problem it solves | Typical RWE use | Basketball analogue / candidate case study | Required data level | Key estimand | Major assumptions | Important diagnostics | Candidate R packages | Status |
|---|---|---|---|---|---|---|---|---|---|
| Outcome regression | Adjusts observed covariates in outcome model | Confounding control | Team-change association adjusted for prior BPM, age, minutes | Player-season | Conditional or standardized mean difference | Correct model specification, exchangeability | Residuals, functional form, leverage | stats, rms, marginaleffects | Planned |
| Propensity scores | Models treatment assignment | Confounding adjustment | Probability of team change given prior season covariates | Player-season | ATE/ATT support tool | No unmeasured confounding, positivity | PS overlap, balance | MatchIt, WeightIt, cobalt | Planned |
| PS matching | Creates comparable groups | Active-comparator studies | Match changers to stayers | Player-season | Often ATT | Common support, matched exchangeability | Love plots, standardized mean differences | MatchIt, cobalt | Planned |
| PS stratification | Compares within PS strata | Confounding control | Quintiles of team-change propensity | Player-season | ATE approximation | Within-stratum exchangeability | Balance by stratum | cobalt, survey | Not started |
| IPTW and stabilized weights | Creates pseudo-population | Marginal causal effects | Weighted comparison of changers vs stayers | Player-season | ATE | Positivity, correct PS model | Weight distribution, ESS, balance | WeightIt, survey, cobalt | Planned |
| Overlap weighting | Targets equipoise population | Poor-overlap settings | Players plausibly able to change or stay | Player-season | Overlap-population effect | Overlap population is meaningful | Extreme weights, overlap plots | WeightIt, cobalt | Planned |
| Entropy balancing | Directly balances covariates | Sensitivity to PS modeling | Balance changers/stayers on prior performance | Player-season | ATT/ATE depending setup | Measured confounding sufficient | Balance, weight concentration | WeightIt, ebalance | Not started |
| Standardized weighting concepts | Standardizes rates or risks | Indirect/direct standardization | Standardize performance changes to a reference age/minutes mix | Player-season | Standardized mean/rate | Correct standard population | Standardized vs crude contrast | survey, epitools | Not started |
| G-computation | Standardizes model-based potential outcomes | Parametric causal contrasts | Predict next BPM under change vs no change for all eligible players | Player-season | ATE/ATT | Correct outcome model, exchangeability | Model diagnostics, bootstrap | gfoRmula, marginaleffects | Planned |
| AIPW | Combines outcome and treatment models | Doubly robust estimation | Team-change effect with nuisance models | Player-season | ATE/ATT | One nuisance model correct, positivity | Balance, nuisance fit, influence | AIPW, DoubleML | Not started |
| TMLE | Targeted doubly robust estimation | Semi-parametric causal inference | Educational comparison to IPTW/g-computation | Player-season | ATE | Exchangeability, positivity, valid learners | Clever covariate, influence diagnostics | tmle, tlverse | Not started |
| Positivity and overlap | Detects unsupported causal comparisons | Feasibility assessment | Sparse comparability for stars or low-minute players | Player-season | Design diagnostic | Covariate support exists | PS histograms, min/max by group | cobalt, ggplot2 | Planned |
| Covariate balance diagnostics | Checks adjustment quality | PS and weighting workflows | Balance prior age, role, minutes, BPM | Player-season | Diagnostic | Measured covariates adequate | SMD, variance ratios, Love plots | cobalt, tableone | Planned |

## 3. Longitudinal Causal Inference

| Method/domain | Problem it solves | Typical RWE use | Basketball analogue / candidate case study | Required data level | Key estimand | Major assumptions | Important diagnostics | Candidate R packages | Status |
|---|---|---|---|---|---|---|---|---|---|
| Time-varying exposures | Exposure changes over follow-up | Dynamic treatment regimes | Role or minutes category across seasons | Player-season/player-game | Regime contrast | Exposure history correctly observed | Exposure histories and transitions | data.table, dplyr | Not started |
| Time-varying confounding | Confounders affected by prior exposure | Longitudinal treatment effects | Prior minutes affect later role and performance | Player-season | Regime-specific mean | Sequential exchangeability | Covariate history checks | WeightIt, ipw | Not started |
| Marginal structural models | Handles time-varying confounding | HIV, treatment adherence | Sustained high-usage role vs lower usage over seasons | Player-season | Marginal regime effect | Sequential exchangeability, positivity | Time-specific balance, weights | ipw, survey | Not started |
| IPTW over time | Weights treatment histories | Longitudinal causal studies | Seasonal role trajectory weighting | Player-season | Regime effect | Correct treatment models | Cumulative weights, ESS | ipw, WeightIt | Not started |
| IPCW | Handles informative censoring | Loss to follow-up | Attrition from NBA roster or minutes threshold | Player-season | Censoring-adjusted estimand | Correct censoring model | Censoring weights, reasons | ipw, survival | Not started |
| Longitudinal g-formula | Simulates potential histories | Dynamic interventions | Hypothetical minutes cap or role strategy | Player-season/player-game | Regime-specific outcome | Correct longitudinal models | Simulation checks | gfoRmula | Not started |
| Structural nested models / g-estimation | Advanced causal modeling | Treatment timing effects | Advanced demonstration if credible exposure exists | Player-season | Blip function | Strong identification assumptions | Sensitivity to model | custom, gfoRmula references | Not started |
| Clone-censor-weight | Compares dynamic strategies | Target trial emulation | Sustained starter vs bench role strategies | Player-game/player-season | Strategy contrast | Correct artificial censoring weights | Protocol adherence, censoring weights | survival, ipw | Not started |

## 4. Quasi-Experimental and Natural-Experiment Methods

| Method/domain | Problem it solves | Typical RWE use | Basketball analogue / candidate case study | Required data level | Key estimand | Major assumptions | Important diagnostics | Candidate R packages | Status |
|---|---|---|---|---|---|---|---|---|---|
| Difference-in-differences | Estimates effects using pre/post and controls | Policy evaluations | Rule change or team strategic shift with comparison teams | Game/team or player-season | ATT for treated units | Parallel trends, no anticipation | Pre-trend plots, placebo tests | fixest, did, bacondecomp | Not started |
| Event study | Dynamic DiD effects | Policy timing studies | Performance before/after trade deadline role shock | Player-game/team-game | Time-relative effects | Parallel pre-trends | Leads/lags plot | fixest, did | Not started |
| Interrupted time series | Evaluates level/slope changes | Single-system policy changes | League rule change effect on pace or shot profile | Game/team by date | Level/slope change | No concurrent shocks or modeled confounding | Autocorrelation, seasonality | nlme, fable, forecast | Not started |
| Segmented regression | Models pre/post trend shifts | ITS implementation | Pace before/after rule emphasis | Game/team | Level/slope differences | Correct time trend | Residual autocorrelation | segmented, nlme | Not started |
| Synthetic control | Builds weighted comparison unit | Comparative policy analysis | Team adoption of a distinctive strategy | Team-season/game | Treated-unit effect over time | Donor pool can reproduce pre-period | Pre-period fit, placebo units | Synth, tidysynth | Not started |
| Instrumental variables | Addresses unmeasured confounding | Preference or distance instruments | Only if credible NBA instrument exists; otherwise simulation | Player/team | LATE | Relevance, exclusion, monotonicity | First stage, falsification | AER, ivreg | Not started |
| Two-stage least squares | Implements linear IV | IV estimation | Same as above | Player/team | LATE | Linear IV assumptions | Weak instrument diagnostics | ivreg, fixest | Not started |
| Regression discontinuity | Local causal effects at threshold | Threshold policies | Draft lottery/order or award cutoff only if credible | Player/team | Local ATE | Continuity at cutoff, no manipulation | Density and covariate continuity | rdrobust, rddensity | Not started |
| Regression kink design | Optional threshold-slope design | Policy schedule kinks | Advanced only if a real kink exists | Team/player | Local slope effect | Smooth potential outcomes | Kink visualization | rdd | Not started |
| Negative-control/falsification designs | Probes residual bias | Bias detection | Outcome impossible before exposure; unrelated endpoint | Any | Diagnostic | Negative control valid | Null checks | sensemakr, custom | Not started |

## 5. Repeated-Measures and Longitudinal Outcome Modeling

| Method/domain | Problem it solves | Typical RWE use | Basketball analogue / candidate case study | Required data level | Key estimand | Major assumptions | Important diagnostics | Candidate R packages | Status |
|---|---|---|---|---|---|---|---|---|---|
| Repeated-measures regression | Accounts for correlated observations | Longitudinal outcomes | Age-performance curves across careers | Player-season | Mean trajectory | Correlation handled appropriately | Residual and correlation checks | geepack, lme4 | Planned |
| GEE | Population-average longitudinal effects | Cohort repeated outcomes | Mean BPM by age with player clustering | Player-season | Marginal mean difference | Working correlation adequate for SE | Robust SE, leverage | geepack | Planned |
| Linear mixed models | Subject-specific trajectories | Growth curves | Random player intercepts/slopes for aging | Player-season | Conditional age trajectory | Random-effects assumptions | BLUPs, residuals | lme4, nlme | Planned |
| GLMMs | Non-Gaussian repeated outcomes | Binary/count outcomes | Starter status or award selection over age | Player-season | Conditional odds/rate | Link and random effects valid | Overdispersion, convergence | glmmTMB, lme4 | Not started |
| Random intercepts/slopes | Separates baseline and change | Individual heterogeneity | Player-specific peak and decline | Player-season | Variance components | Adequate repeated observations | RE distribution | lme4, broom.mixed | Planned |
| Splines/nonlinear trajectories | Models nonlinear aging | Flexible dose-response | Restricted cubic spline for age vs BPM | Player-season | Smooth mean trajectory | Sufficient data support | Knot sensitivity | splines, rms, mgcv | Planned |
| Within- vs between-person effects | Avoids ecological mixing | Longitudinal decomposition | Aging within a player vs differences between players | Player-season | Within-player association | Correct decomposition | Centering checks | panelr, fixest | Not started |
| Fixed-effects/panel models | Controls time-invariant confounding | Panel studies | Within-player changes after role/team changes | Player-season | Within-player effect | No time-varying unmeasured confounding | Hausman-style comparison, FE leverage | fixest, plm | Not started |

## 6. Time-to-Event Methods

| Method/domain | Problem it solves | Typical RWE use | Basketball analogue / candidate case study | Required data level | Key estimand | Major assumptions | Important diagnostics | Candidate R packages | Status |
|---|---|---|---|---|---|---|---|---|---|
| Kaplan-Meier | Describes event-free survival | Time to discontinuation/event | Time from debut to last qualifying NBA season | Player-season | Survival probability | Independent censoring for interpretation | Censoring summaries | survival, survminer | Not started |
| Log-rank comparisons | Compares survival curves | Group comparisons | Career longevity by position/era | Player-season | Curve comparison | Proportional alternatives most powerful | KM curves, risk tables | survival | Not started |
| Cox models | Adjusted hazard ratios | Time-to-event regression | Hazard of exiting NBA qualifying minutes | Player-season | Hazard ratio | Proportional hazards, exchangeability if causal | Schoenfeld residuals | survival, broom | Not started |
| Flexible parametric survival | Smooth survival/hazards | Alternative survival modeling | Career duration with flexible baseline hazard | Player-season | Survival contrasts | Model form adequate | Calibration, residuals | flexsurv, rstpm2 | Not started |
| Competing risks | Multiple event types | Cause-specific outcomes | Retirement, injury absence, non-NBA transition if observable | Player-season | Cause-specific or subdistribution effects | Event classification valid | Cumulative incidence | cmprsk, tidycmprsk | Not started |
| Cause-specific hazards | Models event-specific hazards | Etiology-focused competing risks | Exit for performance vs other reasons if source supports | Player-season | Cause-specific HR | Independent censoring by cause | Cause-specific curves | survival | Not started |
| Fine-Gray models | Predicts cumulative incidence | Prognosis with competing risks | Probability of career exit type | Player-season | Subdistribution HR | Competing event handling appropriate | CIF calibration | cmprsk | Not started |
| Recurrent-event models | Repeated event occurrence | Hospitalizations, relapses | Recurrent missed games if injury data later added | Player-game | Event rate ratio | Event process observed | Mean cumulative function | survival, reda | Not started |
| Andersen-Gill / PWP | Recurrent-event extensions | Ordered/recurrent events | Multiple absences or team changes | Player-game/player-season | Intensity/rate contrasts | Model-specific risk sets valid | Gap-time checks | survival | Not started |
| Multi-state models | Transitions across states | Disease progression | Bench/starter/out-of-league role states | Player-season | Transition probabilities | Markov/semi-Markov assumptions | Transition matrix, state occupancy | mstate, msm | Not started |
| Time-varying covariates | Updates covariates over follow-up | Survival with changing status | Current age/minutes/role predicting exit | Player-season | Time-updated HR | Covariate timing valid | Start-stop audits | survival | Not started |
| Landmarking | Dynamic prediction | Prognosis from fixed time | Predict next 3 seasons among players active at age 25 | Player-season | Conditional risk | Landmark eligibility well-defined | Calibration at landmarks | survival, riskRegression | Not started |

## 7. Missing Data, Measurement Error, Bias, and Sensitivity Analysis

| Method/domain | Problem it solves | Typical RWE use | Basketball analogue / candidate case study | Required data level | Key estimand | Major assumptions | Important diagnostics | Candidate R packages | Status |
|---|---|---|---|---|---|---|---|---|---|
| Missingness mechanisms | Classifies missing data process | Planning analyses | Missing advanced metrics in older seasons | Player-season | Diagnostic | MCAR/MAR/MNAR framing plausible | Missingness maps by season/source | naniar, visdat | Planned |
| Complete-case analysis | Simple baseline approach | Sensitivity or primary when justified | Age-performance among seasons with BPM available | Player-season | Complete-case estimate | Complete cases representative conditional on covariates | Complete vs incomplete comparison | tidyverse | Planned |
| Multiple imputation | Handles MAR missingness | Missing covariates/outcomes | Impute missing covariates, not fabricate unavailable outcomes casually | Player-season | Analysis estimand under MAR | MAR, imputation model congenial | Trace plots, density checks | mice, mitools | Not started |
| IP missingness/censoring | Weights observed records | Missing outcomes/loss to follow-up | Weight seasons with observed advanced metrics | Player-season | Missingness-adjusted mean/effect | Correct missingness model | Weight distribution | ipw, survey | Not started |
| Delta/tipping-point analysis | Explores MNAR sensitivity | Robustness to departures from MAR | How strong missing older-season bias must be to change conclusion | Player-season | Sensitivity range | Specified MNAR shifts meaningful | Tipping-point plots | mice, custom | Not started |
| Measurement error/misclassification | Accounts for imperfect exposure/outcome | Claims code validity | Minutes, position, role, or team-change coding errors | Player-season/game | Bias-adjusted estimate | Error model plausible | Validation subsample if available | simex, custom | Not started |
| Probabilistic bias analysis | Quantifies bias uncertainty | Epidemiologic sensitivity | Bias parameters for role misclassification | Any | Bias-adjusted interval | Bias distributions defensible | Simulation distribution | episensr, custom | Not started |
| Quantitative bias analysis | Structured bias correction | Unmeasured confounding, selection | Bias needed to explain team-change estimate | Any | Bias-adjusted estimate | Bias parameters plausible | Tornado/tipping plots | episensr | Not started |
| Unmeasured-confounding sensitivity | Assesses hidden bias | Observational causal claims | Omitted motivation, injury status, coaching context | Player-season | Robustness value | Sensitivity model relevant | Sensitivity curves | sensemakr, tipr | Planned |
| E-values | Measures minimum confounding strength | Effect robustness | For ratio estimates when appropriate | Any | E-value | Ratio-scale effect suitable | E-value interpretation | EValue | Not started |
| Negative controls | Detects residual bias | Bias diagnostics | Future exposure predicting past outcome | Player-season | Diagnostic null | Control not causally affected | Null estimate | custom | Planned |
| Robustness across definitions | Tests design dependence | Sensitivity analyses | Team change definitions, minutes thresholds, BPM vs WS/48 | Player-season | Range of estimates | Definitions pre-specified | Specification curve | specr, multiverse | Planned |

## 8. Heterogeneity, Generalizability, and Transportability

| Method/domain | Problem it solves | Typical RWE use | Basketball analogue / candidate case study | Required data level | Key estimand | Major assumptions | Important diagnostics | Candidate R packages | Status |
|---|---|---|---|---|---|---|---|---|---|
| Subgroup analysis | Describes effects by strata | Effect variation | Aging curves by position or role | Player-season | Stratum-specific effect | Adequate sample, pre-specified groups | Forest plots, interaction tests | broom, gtsummary | Planned |
| Statistical interaction | Tests effect modification | HTE | Team-change effect modified by age/minutes | Player-season | Interaction contrast | Correct scale and model | Marginal effects by subgroup | marginaleffects, emmeans | Not started |
| Heterogeneous treatment effects | Estimates individual/group variation | Personalized effects | Which player profiles benefit after team changes | Player-season | CATE | Causal assumptions plus learner validity | Calibration for HTE, overlap | grf, causalTree | Not started |
| Standardization | Applies estimates to target population | Generalizability | Age-standardized performance summaries across eras | Player-season | Standardized mean/effect | Target distribution measured | Source vs target covariates | survey, stdReg | Planned |
| Transport weighting | Transports effects across populations | Trial-to-real-world transport | Transport modern-era estimates to earlier eras or teams | Player-season/team | Transported ATE | Conditional exchangeability in target | Sampling weight diagnostics | WeightIt, survey | Not started |
| Generalizability vs transportability | Clarifies target use | External validity | League-wide vs team-specific inference | Any | Target-population contrast | Target population specified | Population comparison table | tableone, cobalt | Planned |
| Temporal transport across eras | Tests era dependence | Historical external validity | Aging/performance models across rule eras | Player-season | Era-specific or transported estimate | Measurement harmonization | Era covariate overlap | fixest, WeightIt | Not started |
| Team/context transportability | Tests context dependence | Site heterogeneity | Whether team-change patterns transport across franchises | Player-season/team | Site-specific and transported effects | Site covariates adequate | Team-level heterogeneity | metafor, lme4 | Not started |

## 9. Bayesian, Prediction/ML, and Evidence Synthesis

| Method/domain | Problem it solves | Typical RWE use | Basketball analogue / candidate case study | Required data level | Key estimand | Major assumptions | Important diagnostics | Candidate R packages | Status |
|---|---|---|---|---|---|---|---|---|---|
| Bayesian regression | Incorporates prior uncertainty | Small samples, uncertainty propagation | Aging curve with weakly informative priors | Player-season | Posterior mean contrasts | Priors and likelihood reasonable | R-hat, ESS, posterior predictive checks | brms, rstanarm | Not started |
| Hierarchical Bayesian models | Partial pooling | Multi-site or subgroup estimates | Player/team/season random effects | Player-season/team | Pooled subgroup estimates | Exchangeability across groups | Shrinkage, PPCs | brms, cmdstanr | Not started |
| Partial pooling | Stabilizes sparse estimates | Site/subgroup heterogeneity | Position-specific aging curves | Player-season | Group-level estimates | Groups share information plausibly | Pooling sensitivity | brms, lme4 | Not started |
| Posterior predictive checks | Evaluates Bayesian fit | Model criticism | Simulated BPM distributions vs observed | Any | Diagnostic | Model can generate observed features | PPC plots | bayesplot, brms | Not started |
| Bayesian causal concepts | Combines causal estimands and Bayesian models | Probabilistic causal estimates | Bayesian g-computation demonstration | Player-season | Posterior causal contrast | Causal identification plus model assumptions | Prior sensitivity | brms, rstanarm | Not started |
| Penalized regression | Handles many predictors | Prediction and confounder modeling | Predict next-season BPM | Player-season | Prediction, not causal by default | Training/validation split valid | Coefficient paths, calibration | glmnet, tidymodels | Not started |
| Tree-based ML/boosting | Flexible prediction | Risk prediction, nuisance models | Predict performance decline | Player-season | Prediction or nuisance function | Validation representative | Variable importance, calibration | ranger, xgboost, tidymodels | Not started |
| Prediction vs causal distinction | Prevents overclaiming | RWE communication | Compare predictive accuracy with causal interpretability | Any | Not an estimand | Objective clearly stated | Calibration/discrimination vs bias diagnostics | tidymodels | Planned |
| Discrimination | Measures ranking ability | Risk models | Identify players at risk of decline | Player-season | AUC/C-index | Outcome labels valid | ROC, PR, C-index | yardstick, pROC | Not started |
| Calibration | Measures probability accuracy | Prediction validation | Predicted vs observed decline risk | Player-season | Calibration slope/intercept | Validation sample adequate | Calibration plots | yardstick, rms | Not started |
| Internal validation | Estimates optimism | Model development | Bootstrap/cross-validation within seasons | Player-season | Optimism-corrected performance | Resampling respects clustering/time | Resampling diagnostics | rsample, boot | Not started |
| Temporal validation | Tests future performance | Real-world deployment | Train on prior seasons, test on later season | Player-season | Future predictive performance | Future data comparable | Temporal drift | tidymodels | Planned |
| External validation | Tests other population | Transportability of prediction | Validate model in different era/team subset | Player-season/team | External performance | Target data comparable | Calibration/discrimination by target | yardstick | Not started |
| Meta-analysis | Synthesizes estimates | Evidence synthesis | Combine team- or era-specific effects | Summary estimates | Pooled estimate | Estimates comparable | Forest/funnel plots | metafor, meta | Not started |
| Fixed/random-effects meta-analysis | Handles heterogeneity | Multi-study synthesis | Season-specific aging slope synthesis | Summary estimates | Pooled effect | Fixed or random effects appropriate | I2, tau2 | metafor | Not started |
| Meta-regression | Explains heterogeneity | Moderator analysis | Era rules or pace as moderators | Summary estimates | Moderator contrast | Ecological limitations | Influence diagnostics | metafor | Not started |
| Hierarchical synthesis | Pools across seasons/teams | Multi-context evidence | Team-level effects with partial pooling | Summary or individual data | Hierarchical pooled effect | Exchangeable contexts | Posterior heterogeneity | brms, metafor | Not started |

## Additional RWE Topics to Track

| Method/domain | Problem it solves | Typical RWE use | Basketball analogue / candidate case study | Required data level | Key estimand | Major assumptions | Important diagnostics | Candidate R packages | Status |
|---|---|---|---|---|---|---|---|---|---|
| DAG-based design | Makes causal assumptions explicit | Confounder selection | Team-change DAG with age, prior performance, role, injury proxies | Any | Identification guide | DAG encodes plausible relations | Adjustment set review | dagitty, ggdag | Planned |
| Selection diagrams | External validity assumptions | Transportability | Era/team transport assumptions | Any | Transport estimand guide | Selection nodes meaningful | Source-target covariate comparison | dagitty | Not started |
| Clustered and robust uncertainty | Corrects standard errors | Multi-level observational data | Players nested in teams/seasons | Any | Valid uncertainty interval | Clustering specified correctly | SE sensitivity | sandwich, clubSandwich | Planned |
| Multiplicity and selective reporting | Controls interpretation risk | Many endpoints/subgroups | Multiple metrics and subgroup analyses | Any | Family of estimates | Pre-specification | Specification count, adjusted intervals | multcomp, p.adjust | Planned |

## Initial Case-Study Sequence

1. Descriptive longitudinal modeling: How does established NBA player performance vary with age across a player's career? This is descriptive and repeated-measures focused, not causal.
2. Confounding and causal adjustment: Among comparable NBA players, what is the effect of changing teams between seasons on subsequent-season performance? Begin with target-trial-style design and feasibility diagnostics before estimating effects.
3. Later modules: longitudinal causal inference, quasi-experiments, survival, missing data and quantitative bias analysis, HTE, transportability, Bayesian hierarchical modeling, prediction with temporal validation, and evidence synthesis.
