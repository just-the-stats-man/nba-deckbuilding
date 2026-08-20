# ============================================================
# 99_run_pipeline.R
# One-button local pipeline runner.
# Run this after replacing the old messy scripts.
# ============================================================

source("02_player_season.R")
source("03_pull_games_pbp_rotations.R")
source("04_build_stints.R")
source("05_score_stints.R")
source("06_build_lineup_plus_minus.R")
source("07_build_true_lineups.R")
source("08_build_player_stint_contributions.R")
source("09_build_player_card_stats.R")
source("10_build_player_context_metrics.R")
source("11_build_player_attacks.R")
source("12_build_player_movesets.R")
source("13_build_attack_identity.R")

message("Pipeline finished.")
