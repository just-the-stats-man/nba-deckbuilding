# ============================================================
# 10_build_player_context_metrics.R
# Phase 8: Player lineup environment/context metrics.
#
# These metrics describe the contexts in which players appeared to succeed:
# teammates, opponents, lineup variety, and dependence on specific lineups.
# They are NOT pure player skill metrics.
#
# TODO: Future versions should include opponent adjustment, possession quality,
# lineup priors, and Bayesian shrinkage before using these as ratings.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

phase5_path <- "outputs/Phase5_true_lineups.parquet"
phase6_path <- "outputs/Phase6_player_stint_contributions.parquet"
phase8_path <- "outputs/Phase8_player_context_metrics.parquet"

if (!file.exists(phase5_path)) {
  stop("Missing Phase 5 true lineup output: ", phase5_path, ". Run 07_build_true_lineups.R first.", call. = FALSE)
}

if (!file.exists(phase6_path)) {
  stop("Missing Phase 6 player-stint bridge: ", phase6_path, ". Run 08_build_player_stint_contributions.R first.", call. = FALSE)
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

sum_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  sum(x, na.rm = TRUE)
}

mean_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

lineup_players <- function(lineup_key) {
  ids <- stringr::str_extract_all(as.character(lineup_key), "\\d+")[[1]]
  sort(unique(stats::na.omit(ids)))
}

weighted_mean_or_na <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0

  if (sum(valid) == 0) {
    return(NA_real_)
  }

  stats::weighted.mean(x[valid], w[valid])
}

phase5 <- read_project_parquet(phase5_path)
phase6 <- read_project_parquet(phase6_path)

validate_columns(
  phase5,
  c("target_lineup_key", "minutes", "raw_point_differential", "per_minute_differential")
)

validate_columns(
  phase6,
  c(
    "player_id",
    "player_name",
    "team_abbreviation",
    "target_lineup_key",
    "opponent_lineup_key",
    "matchup_lineup_key",
    "stint_minutes",
    "points_against",
    "raw_point_differential"
  )
)

player_stints <- phase6 %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    target_lineup_key = as.character(.data$target_lineup_key),
    opponent_lineup_key = as.character(.data$opponent_lineup_key),
    matchup_lineup_key = as.character(.data$matchup_lineup_key),
    stint_minutes = as.numeric(.data$stint_minutes),
    raw_point_differential = as.numeric(.data$raw_point_differential),
    points_against_per_minute = dplyr::if_else(
      .data$stint_minutes > 0 & !is.na(.data$points_against),
      as.numeric(.data$points_against) / .data$stint_minutes,
      NA_real_
    ),
    target_lineup_players = purrr::map(.data$target_lineup_key, lineup_players),
    opponent_lineup_players = purrr::map(.data$opponent_lineup_key, lineup_players)
  )

lineup_reference <- phase5 %>%
  dplyr::transmute(
    target_lineup_key = as.character(.data$target_lineup_key),
    phase5_lineup_minutes = as.numeric(.data$minutes),
    phase5_lineup_raw_point_differential = as.numeric(.data$raw_point_differential),
    phase5_lineup_per_minute_differential = as.numeric(.data$per_minute_differential)
  ) %>%
  dplyr::distinct(.data$target_lineup_key, .keep_all = TRUE)

player_lineup_context <- player_stints %>%
  dplyr::group_by(.data$player_id, .data$target_lineup_key) %>%
  dplyr::summarise(
    player_lineup_minutes = sum(.data$stint_minutes, na.rm = TRUE),
    player_lineup_raw_point_differential = sum_or_na(.data$raw_point_differential),
    .groups = "drop"
  ) %>%
  dplyr::left_join(lineup_reference, by = "target_lineup_key")

player_teammates <- player_stints %>%
  dplyr::mutate(
    teammate_ids = purrr::map2(
      .data$target_lineup_players,
      .data$player_id,
      ~ setdiff(.x, .y)
    )
  ) %>%
  dplyr::select("player_id", "stint_minutes", "teammate_ids") %>%
  tidyr::unnest_longer("teammate_ids", values_to = "teammate_id") %>%
  dplyr::filter(!is.na(.data$teammate_id))

teammate_summary <- player_teammates %>%
  dplyr::group_by(.data$player_id, .data$teammate_id) %>%
  dplyr::summarise(teammate_minutes = sum(.data$stint_minutes, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    unique_teammates_count = dplyr::n_distinct(.data$teammate_id),
    max_teammate_minutes = max(.data$teammate_minutes, na.rm = TRUE),
    .groups = "drop"
  )

opponent_summary <- player_stints %>%
  dplyr::select("player_id", "opponent_lineup_players") %>%
  tidyr::unnest_longer("opponent_lineup_players", values_to = "opponent_id") %>%
  dplyr::filter(!is.na(.data$opponent_id)) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(unique_opponents_count = dplyr::n_distinct(.data$opponent_id), .groups = "drop")

lineup_result_rates <- player_lineup_context %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    lineup_count = dplyr::n_distinct(.data$target_lineup_key, na.rm = TRUE),
    positive_lineup_rate = mean_or_na(.data$player_lineup_raw_point_differential > 0),
    negative_lineup_rate = mean_or_na(.data$player_lineup_raw_point_differential < 0),
    average_phase5_lineup_minutes = mean_or_na(.data$phase5_lineup_minutes),
    max_lineup_minutes_share = dplyr::if_else(
      sum(.data$player_lineup_minutes, na.rm = TRUE) > 0,
      max(.data$player_lineup_minutes, na.rm = TRUE) / sum(.data$player_lineup_minutes, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

context_base <- player_stints %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    player_name = dplyr::first(stats::na.omit(.data$player_name), default = NA_character_),
    team_abbreviation = dplyr::first(stats::na.omit(.data$team_abbreviation), default = NA_character_),
    total_minutes = sum(.data$stint_minutes, na.rm = TRUE),
    stints = dplyr::n_distinct(paste(.data$game_id, .data$stint_id, sep = "::")),
    matchup_count = dplyr::n_distinct(.data$matchup_lineup_key, na.rm = TRUE),
    average_matchup_difficulty_proxy = weighted_mean_or_na(
      .data$points_against_per_minute,
      .data$stint_minutes
    ),
    .groups = "drop"
  )

player_context_metrics <- context_base %>%
  dplyr::left_join(teammate_summary, by = "player_id") %>%
  dplyr::left_join(opponent_summary, by = "player_id") %>%
  dplyr::left_join(lineup_result_rates, by = "player_id") %>%
  dplyr::mutate(
    unique_teammates_count = dplyr::coalesce(.data$unique_teammates_count, 0L),
    unique_opponents_count = dplyr::coalesce(.data$unique_opponents_count, 0L),
    average_lineup_minutes = dplyr::if_else(
      .data$lineup_count > 0,
      .data$total_minutes / .data$lineup_count,
      NA_real_
    ),
    teammate_stability_score = dplyr::if_else(
      .data$total_minutes > 0,
      .data$max_teammate_minutes / .data$total_minutes * 100,
      NA_real_
    ),
    lineup_dependency_score = .data$max_lineup_minutes_share * 100,
    # Diversity is a scaled count of unique lineups. It measures variety of
    # environments, not whether those environments were easy or difficult.
    lineup_diversity_score = scale_0_100(.data$lineup_count)
  ) %>%
  dplyr::select(
    tidyselect::all_of(c(
      "player_id",
      "player_name",
      "team_abbreviation",
      "unique_teammates_count",
      "unique_opponents_count",
      "lineup_diversity_score",
      "average_lineup_minutes",
      "average_matchup_difficulty_proxy",
      "teammate_stability_score",
      "lineup_dependency_score",
      "positive_lineup_rate",
      "negative_lineup_rate"
    )),
    tidyselect::any_of(c(
      "total_minutes",
      "stints",
      "lineup_count",
      "matchup_count"
    ))
  ) %>%
  dplyr::arrange(dplyr::desc(.data$total_minutes), dplyr::desc(.data$lineup_diversity_score))

write_project_parquet(player_context_metrics, phase8_path)

message("Phase 8 player context metrics summary:")
print(
  player_context_metrics %>%
    dplyr::summarise(
      players = dplyr::n(),
      median_unique_teammates = stats::median(.data$unique_teammates_count, na.rm = TRUE),
      median_unique_opponents = stats::median(.data$unique_opponents_count, na.rm = TRUE),
      median_lineup_diversity_score = stats::median(.data$lineup_diversity_score, na.rm = TRUE),
      median_lineup_dependency_score = stats::median(.data$lineup_dependency_score, na.rm = TRUE),
      missing_matchup_difficulty_proxy = sum(is.na(.data$average_matchup_difficulty_proxy))
    )
)

message("Phase 8 note: context metrics describe environment, not pure player skill. Future versions should add opponent adjustment and Bayesian shrinkage.")
