# ============================================================
# 20_build_creation_rating.R
# Phase 20: Build Creation Rating (CR).
#
# CR = offensive creation burden and self-generated offense.
# CR is separate from ATK. ATK describes scoring threat; CR describes how much
# of the offense a player appears to create or carry.
#
# This version uses measured tracking creation metrics from Phase 19b plus the
# player attack library for observable creation weapon mix and finisher
# dependency. observed_CR_score reflects measured creation burden plus attack
# tendencies. CR_score remains NA until broader tracking coverage is available.
#
# This phase does not modify previous phases or outputs.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_attack_library_path <- "outputs/attacks/player_attack_library.parquet"
player_movesets_path <- "outputs/attacks/player_movesets.parquet"
player_tracking_creation_metrics_path <- "outputs/attacks/player_tracking_creation_metrics.parquet"
cr_output_path <- "outputs/player_card_CR.parquet"

required_inputs <- c(
  player_attack_library_path,
  player_movesets_path,
  player_tracking_creation_metrics_path
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "Missing Phase 20 input(s): ",
    paste(missing_inputs, collapse = ", "),
    ". Run Phases 11, 12, and 19b first.",
    call. = FALSE
  )
}

safe_divide <- function(num, den) {
  dplyr::if_else(!is.na(den) & den > 0, num / den, NA_real_)
}

weighted_mean_or_na <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0

  if (sum(valid) == 0) {
    return(NA_real_)
  }

  stats::weighted.mean(x[valid], w[valid])
}

scale_0_100 <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  valid <- !is.na(x)

  if (sum(valid) == 0) {
    return(out)
  }

  x_valid <- x[valid]
  spread <- max(x_valid) - min(x_valid)

  if (spread == 0) {
    return(out)
  }

  out[valid] <- (x_valid - min(x_valid)) / spread * 100
  out
}

clamp_0_100 <- function(x) {
  pmax(0, pmin(100, as.numeric(x)))
}

coalesce_numeric <- function(x, fallback = 0) {
  dplyr::coalesce(as.numeric(x), fallback)
}

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

attack_library <- read_project_parquet(player_attack_library_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c(
    "player_id",
    "attack_variant",
    "attempts",
    "expected_damage_per_attempt",
    "points_per_attempt"
  ), NA_real_) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    attack_variant = stringr::str_to_lower(as.character(.data$attack_variant)),
    attempts = suppressWarnings(as.numeric(.data$attempts)),
    attack_expected_damage = dplyr::coalesce(
      suppressWarnings(as.numeric(.data$expected_damage_per_attempt)),
      suppressWarnings(as.numeric(.data$points_per_attempt))
    )
  )

movesets <- read_project_parquet(player_movesets_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  dplyr::mutate(player_id = as.character(.data$player_id))

tracking_creation_metrics <- read_project_parquet(player_tracking_creation_metrics_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c(
    "touches",
    "usage_percentage",
    "assists",
    "secondary_assists",
    "possessions"
  ), NA_real_) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    touches = suppressWarnings(as.numeric(.data$touches)),
    usage_percentage = suppressWarnings(as.numeric(.data$usage_percentage)),
    assists = suppressWarnings(as.numeric(.data$assists)),
    secondary_assists = suppressWarnings(as.numeric(.data$secondary_assists)),
    possessions = suppressWarnings(as.numeric(.data$possessions))
  )

validate_columns(attack_library, c("player_id", "attack_variant", "attempts"))
validate_columns(movesets, c("player_id", "player_name", "total_attack_attempts"))
validate_columns(tracking_creation_metrics, c("player_id", "touches", "usage_percentage", "assists", "secondary_assists", "possessions"))

creation_weapon_variants <- c(
  "stepback jumper",
  "pullup jumper",
  "driving layup",
  "floater",
  "fadeaway jumper"
)

finisher_dependency_variants <- c(
  "alley oop",
  "putback",
  "tip"
)

attack_creation_components <- attack_library %>%
  dplyr::mutate(
    is_creation_weapon = .data$attack_variant %in% creation_weapon_variants,
    is_finisher_dependency = .data$attack_variant %in% finisher_dependency_variants,
    is_dunk = .data$attack_variant == "dunk"
  ) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    attack_library_attempts = sum(.data$attempts, na.rm = TRUE),
    creation_weapon_attempts = sum(dplyr::if_else(.data$is_creation_weapon, .data$attempts, 0), na.rm = TRUE),
    creation_weapon_expected_damage = weighted_mean_or_na(
      dplyr::if_else(.data$is_creation_weapon, .data$attack_expected_damage, NA_real_),
      dplyr::if_else(.data$is_creation_weapon, .data$attempts, 0)
    ),
    finisher_dependency_attempts = sum(dplyr::if_else(.data$is_finisher_dependency, .data$attempts, 0), na.rm = TRUE),
    dunk_attempts = sum(dplyr::if_else(.data$is_dunk, .data$attempts, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    creation_weapon_attempt_share = safe_divide(.data$creation_weapon_attempts, .data$attack_library_attempts),
    finisher_dependency_attempt_share = safe_divide(.data$finisher_dependency_attempts, .data$attack_library_attempts),
    dunk_attempt_share = safe_divide(.data$dunk_attempts, .data$attack_library_attempts),
    creation_weapon_volume_score = 100 * sqrt(coalesce_numeric(.data$creation_weapon_attempt_share)),
    creation_weapon_efficiency_score = scale_0_100(.data$creation_weapon_expected_damage),
    creation_weapon_component = clamp_0_100(
      0.70 * .data$creation_weapon_volume_score +
        0.30 * dplyr::coalesce(.data$creation_weapon_efficiency_score, 0)
    ),
    finisher_dependency_penalty = clamp_0_100(
      0.65 * (100 * sqrt(coalesce_numeric(.data$finisher_dependency_attempt_share))) +
        0.35 * (((coalesce_numeric(.data$dunk_attempt_share) - 0.20) / 0.80) * 100)
    )
  )

# Phase 19b writes player-level tracking creation metrics. These are still
# limited-coverage descriptive inputs, but they are real measured values rather
# than endpoint-availability placeholders.
tracking_creation_components <- tracking_creation_metrics %>%
  dplyr::select(
    "player_id",
    "touches",
    "usage_percentage",
    "assists",
    "secondary_assists",
    "possessions",
    tidyselect::any_of(c("source_endpoints", "source_tables"))
  ) %>%
  add_missing_cols(c("source_endpoints", "source_tables"), NA_character_) %>%
  dplyr::mutate(
    touch_burden_component = scale_0_100(.data$touches),
    usage_burden_component = scale_0_100(.data$usage_percentage),
    playmaking_burden_component = scale_0_100(.data$assists),
    tracking_creation_metric_available = !is.na(.data$touches) |
      !is.na(.data$usage_percentage) |
      !is.na(.data$assists) |
      !is.na(.data$secondary_assists) |
      !is.na(.data$possessions)
  )

tracking_metric_available_player_count <- tracking_creation_components %>%
  dplyr::filter(.data$tracking_creation_metric_available) %>%
  dplyr::distinct(.data$player_id) %>%
  nrow()

tracking_metric_coverage_sufficient <- tracking_metric_available_player_count >= 30

player_cr <- movesets %>%
  dplyr::select(
    "player_id",
    "player_name",
    tidyselect::any_of(c("player_nickname", "team_abbreviation", "total_attack_attempts", "sample_size_tier"))
  ) %>%
  add_missing_cols(c("player_nickname", "team_abbreviation", "total_attack_attempts", "sample_size_tier"), NA_character_) %>%
  dplyr::left_join(attack_creation_components, by = "player_id") %>%
  dplyr::left_join(tracking_creation_components, by = "player_id") %>%
  dplyr::mutate(
    touch_burden_available = !is.na(.data$touch_burden_component),
    usage_burden_available = !is.na(.data$usage_burden_component),
    playmaking_burden_available = !is.na(.data$playmaking_burden_component),
    tracking_creation_metric_available = dplyr::coalesce(.data$tracking_creation_metric_available, FALSE),
    raw_CR_before_penalty =
      0.20 * dplyr::coalesce(.data$touch_burden_component, 0) +
      0.25 * dplyr::coalesce(.data$usage_burden_component, 0) +
      0.20 * dplyr::coalesce(.data$playmaking_burden_component, 0) +
      0.25 * dplyr::coalesce(.data$creation_weapon_component, 0),
    raw_CR = clamp_0_100(.data$raw_CR_before_penalty - 0.10 * dplyr::coalesce(.data$finisher_dependency_penalty, 0)),
    observed_CR_score = .data$raw_CR,
    CR_available = tracking_metric_coverage_sufficient & .data$tracking_creation_metric_available,
    CR_score = dplyr::if_else(.data$CR_available, .data$observed_CR_score, NA_real_),
    CR_note = dplyr::case_when(
      .data$tracking_creation_metric_available & !tracking_metric_coverage_sufficient ~
        "Tracking metrics available for this player, but broader coverage is insufficient for final CR.",
      !.data$tracking_creation_metric_available ~
        "Tracking creation metrics unavailable. Current CR reflects attack-style tendencies only.",
      TRUE ~ "Tracking metrics available for this player and coverage is sufficient for final CR."
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "total_attack_attempts",
    "sample_size_tier",
    "touches",
    "usage_percentage",
    "assists",
    "secondary_assists",
    "possessions",
    "touch_burden_component",
    "usage_burden_component",
    "playmaking_burden_component",
    "creation_weapon_component",
    "finisher_dependency_penalty",
    "observed_CR_score",
    "CR_available",
    "CR_score",
    "CR_note",
    "raw_CR_before_penalty",
    "raw_CR",
    "creation_weapon_attempts",
    "creation_weapon_attempt_share",
    "creation_weapon_expected_damage",
    "finisher_dependency_attempts",
    "finisher_dependency_attempt_share",
    "dunk_attempt_share",
    "tracking_creation_metric_available",
    "source_endpoints",
    "source_tables",
    "touch_burden_available",
    "usage_burden_available",
    "playmaking_burden_available"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$observed_CR_score), .data$player_name)

write_project_parquet(player_cr, cr_output_path)

message("Phase 20 Creation Rating diagnostics:")

message("Tracking metrics available player count:")
print(tracking_metric_available_player_count)

message("Tracking metric coverage sufficient for final CR score:")
print(tracking_metric_coverage_sufficient)

message("Top CR players:")
print(
  player_cr %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "observed_CR_score",
      "CR_available",
      "CR_score",
      "CR_note",
      "creation_weapon_component",
      "touch_burden_component",
      "usage_burden_component",
      "playmaking_burden_component",
      "finisher_dependency_penalty",
      "touches",
      "usage_percentage",
      "assists",
      "secondary_assists",
      "possessions",
      "total_attack_attempts"
    ) %>%
    utils::head(25)
)

message("Requested player examples:")
print(
  player_cr %>%
    dplyr::filter(stringr::str_detect(.data$player_name, "Luka|Doncic|Dončić|LeBron|Austin Reaves|Ayton|Jaxson Hayes|Luke Kennard")) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "observed_CR_score",
      "CR_available",
      "CR_score",
      "CR_note",
      "creation_weapon_component",
      "touch_burden_component",
      "usage_burden_component",
      "playmaking_burden_component",
      "finisher_dependency_penalty",
      "touches",
      "usage_percentage",
      "assists",
      "secondary_assists",
      "possessions",
      "creation_weapon_attempt_share",
      "finisher_dependency_attempt_share",
      "dunk_attempt_share",
      "tracking_creation_metric_available"
    )
)

message("CR component availability:")
print(
  player_cr %>%
    dplyr::summarise(
      players = dplyr::n(),
      touch_burden_available_players = sum(.data$touch_burden_available, na.rm = TRUE),
      usage_burden_available_players = sum(.data$usage_burden_available, na.rm = TRUE),
      playmaking_burden_available_players = sum(.data$playmaking_burden_available, na.rm = TRUE),
      tracking_creation_metric_available_players = sum(.data$tracking_creation_metric_available, na.rm = TRUE),
      tracking_metrics_available_player_count = tracking_metric_available_player_count,
      tracking_metric_coverage_sufficient = tracking_metric_coverage_sufficient,
      CR_available_players = sum(.data$CR_available, na.rm = TRUE),
      creation_weapon_available_players = sum(!is.na(.data$creation_weapon_component)),
      median_observed_CR_score = stats::median(.data$observed_CR_score, na.rm = TRUE),
      median_CR_score = stats::median(.data$CR_score, na.rm = TRUE),
      median_creation_weapon_component = stats::median(.data$creation_weapon_component, na.rm = TRUE),
      median_finisher_dependency_penalty = stats::median(.data$finisher_dependency_penalty, na.rm = TRUE)
    )
)

message("Phase 20 note: CR = offensive creation burden and self-generated offense. CR is separate from ATK. No previous phases were modified.")
