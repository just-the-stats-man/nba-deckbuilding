# Data Dictionary Skeleton

This document defines planned data layers for the RWE reboot. It is a skeleton, not a claim that all fields are currently available.

## Data Layers

| Layer | Grain | Purpose | Planned location | Status |
|---|---|---|---|---|
| Player-season | One player-season, optionally player-team-season when split-team seasons matter | Foundation for descriptive cohorts, aging curves, team-change designs, longitudinal models, survival cohorts | `data/derived/player_season/` | Planned |
| Player-game | One player-game | Time-varying covariates, short-window outcomes, rest and schedule features | `data/derived/player_game/` | Planned |
| Game/team | One team-game | Team outcomes, pace/context, quasi-experimental and schedule analyses | `data/derived/game_team/` | Planned |
| Optional injury module | Player absence or injury event | Survival/recurrent-event and missingness examples only when a defensible source exists | `data/derived/injury/` | Future optional |
| Optional transaction module | Player movement event | Team-change exposure definitions and time zero validation | `data/derived/transactions/` | Future optional |
| Optional lineup/tracking/possession module | Stint, lineup, tracking, or possession | Only for studies that require this granularity | `data/derived/contextual/` | Future optional |

## Player-Season Fields

| Field | Description | Type | Source/provenance | Notes |
|---|---|---|---|---|
| `player_id` | Stable player identifier | character | TBD | Prefer source-native ID plus documented crosswalk if needed |
| `player_name` | Player display name | character | TBD | Retain source spelling |
| `season` | NBA season label | character | TBD | Use consistent `YYYY-YY` convention |
| `team` | Team abbreviation or `TOT` handling rule | character | TBD | Define split-team logic before analysis |
| `age` | Player age for season | numeric | TBD | Source definition must be documented |
| `games` | Games played | integer | TBD | Conventional box-score field |
| `minutes` | Minutes played | numeric | TBD | Needed for eligibility and weighting |
| `box_score_stats` | Conventional counting/rate statistics | mixed | TBD | Expand into named fields during implementation |
| `advanced_public_stats` | Public advanced measures | mixed | TBD | Expand into named fields |
| `bpm` | Box Plus/Minus | numeric | TBD | Outcome/covariate candidate |
| `obpm` | Offensive BPM | numeric | TBD | Outcome/covariate candidate |
| `dbpm` | Defensive BPM | numeric | TBD | Outcome/covariate candidate |
| `vorp` | Value Over Replacement Player | numeric | TBD | Sensitivity outcome |
| `win_shares` | Win Shares | numeric | TBD | Sensitivity outcome |
| `ws_48` | Win Shares per 48 minutes | numeric | TBD | Sensitivity outcome |
| `per` | Player Efficiency Rating | numeric | TBD | Descriptive/sensitivity measure |

## Player-Game Fields

| Field | Description | Type | Source/provenance | Notes |
|---|---|---|---|---|
| `player_id` | Stable player identifier | character | TBD | Must link to player-season |
| `game_id` | Stable game identifier | character | TBD | Source-native ID |
| `date` | Game date | date | TBD | Needed for time ordering |
| `season` | NBA season label | character | TBD | Consistent convention |
| `team` | Player team | character | TBD | Handle trades explicitly |
| `opponent` | Opponent team | character | TBD | |
| `home_away` | Home/away indicator | character/logical | TBD | Define coding |
| `starter_status` | Whether player started | logical | TBD | Baseline or game-level covariate |
| `minutes` | Minutes played | numeric | TBD | Outcome/covariate depending question |
| `box_score_stats` | Player box-score statistics | mixed | TBD | Expand during implementation |
| `plus_minus` | Game plus/minus where available | numeric | TBD | Contextual measure, not causal alone |
| `team_result` | Win/loss or margin | mixed | TBD | Outcome/covariate depending question |
| `days_rest` | Days since prior game | numeric | Derived | Must avoid future leakage |
| `pregame_covariates` | Features available before game | mixed | Derived/TBD | Explicitly separate from post-game fields |

## Game/Team Fields

| Field | Description | Type | Source/provenance | Notes |
|---|---|---|---|---|
| `game_id` | Stable game identifier | character | TBD | |
| `date` | Game date | date | TBD | |
| `season` | NBA season label | character | TBD | |
| `team` | Team abbreviation | character | TBD | |
| `opponent` | Opponent abbreviation | character | TBD | |
| `home_away` | Home/away indicator | character/logical | TBD | |
| `score` | Team score | integer | TBD | |
| `possessions` | Possessions where available/defensible | numeric | TBD | Document formula/source |
| `pace` | Pace where available | numeric | TBD | Source or derived definition |
| `offensive_rating` | Points per 100 possessions | numeric | TBD | Source or derived definition |
| `defensive_rating` | Points allowed per 100 possessions | numeric | TBD | Source or derived definition |
| `rest_travel_proxy` | Rest/travel proxy | mixed | Derived/TBD | Must be defensible and pre-specified |

## Established Basketball Metrics Register

For each metric used, document:

- Source
- Public availability
- Whether the formula/methodology is public
- Reproducibility from public data
- Interpretation
- Known limitations
- Appropriate use: outcome, covariate, descriptive measure, sensitivity outcome

Initial metrics to register: BPM, OBPM, DBPM, VORP, Win Shares, WS/48, PER, plus conventional box-score and game/team outcomes.
