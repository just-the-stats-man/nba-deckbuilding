# ============================================================
# 09_build_player_card_stats.R
# Phase 7: Prototype player card stats.
#
# These are naive, transparent prototype scores for early card design.
# They are NOT adjusted player ratings and should not be interpreted as RAPM,
# APM, Bayesian impact, or opponent-adjusted value.
#
# TODO: Replace these with model-backed archetype/card ratings after the
# possession, lineup, and opponent-adjustment pipeline is mature.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

phase6_path <- "outputs/Phase6_player_stint_contributions.parquet"
phase7_path <- "outputs/Phase7_player_card_stats.parquet"

if (!file.exists(phase6_path)) {
  stop("Missing Phase 6 player-stint bridge: ", phase6_path, ". Run 08_build_player_stint_contributions.R first.", call. = FALSE)
}

sum_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  sum(x, na.rm = TRUE)
}

scale_0_100 <- function(x, higher_is_better = TRUE, neutral = 50) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  valid <- !is.na(x)

  if (sum(valid) == 0) {
    return(out)
  }

  x_valid <- x[valid]
  spread <- max(x_valid) - min(x_valid)

  if (spread == 0) {
    out[valid] <- neutral
    return(out)
  }

  scaled <- (x_valid - min(x_valid)) / spread * 100

  if (!higher_is_better) {
    scaled <- 100 - scaled
  }

  out[valid] <- scaled
  out
}

phase6 <- read_project_parquet(phase6_path)

validate_columns(
  phase6,
  c(
    "player_id",
    "player_name",
    "team_abbreviation",
    "stint_minutes",
    "points_for",
    "points_against",
    "raw_point_differential",
    "target_lineup_key",
    "matchup_lineup_key"
  )
)

player_card_base <- phase6 %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    player_name = dplyr::first(stats::na.omit(.data$player_name), default = NA_character_),
    team_abbreviation = dplyr::first(stats::na.omit(.data$team_abbreviation), default = NA_character_),
    total_minutes = sum(.data$stint_minutes, na.rm = TRUE),
    total_stints = dplyr::n_distinct(paste(.data$game_id, .data$stint_id, sep = "::")),
    total_points_for = sum_or_na(.data$points_for),
    total_points_against = sum_or_na(.data$points_against),
    raw_point_differential = sum_or_na(.data$raw_point_differential),
    lineup_count = dplyr::n_distinct(.data$target_lineup_key, na.rm = TRUE),
    matchup_count = dplyr::n_distinct(.data$matchup_lineup_key, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    point_diff_per_minute = dplyr::if_else(
      .data$total_minutes > 0 & !is.na(.data$raw_point_differential),
      .data$raw_point_differential / .data$total_minutes,
      NA_real_
    ),
    offense_per_minute = dplyr::if_else(
      .data$total_minutes > 0 & !is.na(.data$total_points_for),
      .data$total_points_for / .data$total_minutes,
      NA_real_
    ),
    defense_per_minute = dplyr::if_else(
      .data$total_minutes > 0 & !is.na(.data$total_points_against),
      .data$total_points_against / .data$total_minutes,
      NA_real_
    )
  )

# Naive prototype formulas:
# ATK_score scales observed points-for per minute.
# DEF_score scales inverse points-against per minute, so fewer points allowed is better.
# IMPACT_score blends scaled raw point differential rate with lineup variety.
# STABILITY_score blends minutes, stint count, and lineup count.
# HP_score is a minutes-based placeholder for load/availability.
player_card_stats <- player_card_base %>%
  dplyr::mutate(
    ATK_score = scale_0_100(.data$offense_per_minute),
    DEF_score = scale_0_100(.data$defense_per_minute, higher_is_better = FALSE),
    IMPACT_score = 0.85 * scale_0_100(.data$point_diff_per_minute) +
      0.15 * scale_0_100(.data$lineup_count),
    STABILITY_score = (
      scale_0_100(.data$total_minutes) +
        scale_0_100(.data$total_stints) +
        scale_0_100(.data$lineup_count)
    ) / 3,
    HP_score = scale_0_100(.data$total_minutes)
  ) %>%
  dplyr::arrange(dplyr::desc(.data$total_minutes), dplyr::desc(.data$IMPACT_score))

write_project_parquet(player_card_stats, phase7_path)

message("Phase 7 player card stats summary:")
print(
  player_card_stats %>%
    dplyr::summarise(
      players = dplyr::n(),
      total_minutes = sum(.data$total_minutes, na.rm = TRUE),
      median_ATK_score = stats::median(.data$ATK_score, na.rm = TRUE),
      median_DEF_score = stats::median(.data$DEF_score, na.rm = TRUE),
      median_IMPACT_score = stats::median(.data$IMPACT_score, na.rm = TRUE),
      median_STABILITY_score = stats::median(.data$STABILITY_score, na.rm = TRUE),
      median_HP_score = stats::median(.data$HP_score, na.rm = TRUE)
    )
)

message("Phase 7 note: card scores are naive prototypes only; no adjusted ratings or Bayesian models were built.")
