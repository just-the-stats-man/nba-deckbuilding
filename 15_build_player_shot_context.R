# ============================================================
# 15_build_player_shot_context.R
# Phase 15: Player shot-context metrics.
#
# These are descriptive shot-context metrics only. They describe observed shot
# difficulty and creation burden from NBA Player Tracking Shots data pulled in
# Phase 14. They are not ATK scores and are not fused into attack identity,
# movesets, lineup impact, or defense.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

tracking_raw_dir <- "data/raw/tracking_shots"
player_master_path <- "data/processed/player/player_master_2025-26.parquet"
player_shot_context_path <- "outputs/attacks/player_shot_context.parquet"

if (!dir.exists(tracking_raw_dir)) {
  stop("Missing tracking shot raw directory: ", tracking_raw_dir, ". Run 14_audit_shot_context_data.R first.", call. = FALSE)
}

if (!file.exists(player_master_path)) {
  stop("Missing player master input: ", player_master_path, ". Run 02_player_season.R first.", call. = FALSE)
}

tracking_files <- fs::dir_ls(tracking_raw_dir, regexp = "\\.parquet$")

if (length(tracking_files) == 0) {
  stop("No tracking shot parquet files found in ", tracking_raw_dir, ". Run 14_audit_shot_context_data.R first.", call. = FALSE)
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

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

clamp_0_100 <- function(x) {
  pmax(0, pmin(100, as.numeric(x)))
}

read_tracking_file <- function(path) {
  read_project_parquet(path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols() %>%
    add_missing_cols(c(
      "player_id",
      "requested_player_id",
      "requested_player_name_resolved",
      "requested_team_abbreviation",
      "table_name",
      "fga"
    ), NA_character_) %>%
    dplyr::mutate(
      source_file = basename(path),
      player_id = dplyr::coalesce(as.character(.data$player_id), as.character(.data$requested_player_id)),
      table_name = as.character(.data$table_name),
      fga = suppressWarnings(as.numeric(.data$fga))
    )
}

tracking_raw <- purrr::map_dfr(tracking_files, read_tracking_file) %>%
  dplyr::filter(!is.na(.data$player_id))

if (nrow(tracking_raw) == 0) {
  stop("Tracking shot files were found, but no player_id/requested_player_id values were readable.", call. = FALSE)
}

player_master <- read_project_parquet(player_master_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  dplyr::mutate(player_id = as.character(.data$player_id))

validate_columns(player_master, c("player_id", "player_name", "team_abbreviation"))

player_metadata <- player_master %>%
  dplyr::select(tidyselect::any_of(c("player_id", "player_name", "nickname", "team_abbreviation"))) %>%
  dplyr::rename_with(
    ~ "player_nickname",
    tidyselect::any_of("nickname")
  ) %>%
  add_missing_cols(c("player_nickname"), NA_character_) %>%
  dplyr::distinct(.data$player_id, .keep_all = TRUE)

table_name_lower <- stringr::str_to_lower(tracking_raw$table_name)

# Prefer the standard ClosestDefenderShooting table when present. The 10ft+
# table is useful availability evidence, but using both can double-count the
# same conceptual openness split.
closest_standard_exists <- any(
  stringr::str_detect(table_name_lower, "closestdefendershooting") &
    !stringr::str_detect(table_name_lower, "10ft|10_ft|10plus|10_plus"),
  na.rm = TRUE
)

closest_defender_rows <- tracking_raw %>%
  dplyr::filter(stringr::str_detect(stringr::str_to_lower(.data$table_name), "closestdefender")) %>%
  {
    if (closest_standard_exists) {
      dplyr::filter(
        .,
        stringr::str_detect(stringr::str_to_lower(.data$table_name), "closestdefendershooting"),
        !stringr::str_detect(stringr::str_to_lower(.data$table_name), "10ft|10_ft|10plus|10_plus")
      )
    } else {
      .
    }
  } %>%
  add_missing_cols(c("close_def_dist_range"), NA_character_) %>%
  dplyr::mutate(
    close_def_dist_range = stringr::str_to_lower(as.character(.data$close_def_dist_range)),
    defender_bucket_score = dplyr::case_when(
      stringr::str_detect(.data$close_def_dist_range, "0.*2|0-2") ~ 0,
      stringr::str_detect(.data$close_def_dist_range, "2.*4|2-4") ~ 25,
      stringr::str_detect(.data$close_def_dist_range, "4.*6|4-6") ~ 70,
      stringr::str_detect(.data$close_def_dist_range, "6\\+|6\\s*\\+|wide open") ~ 100,
      TRUE ~ NA_real_
    ),
    is_tight_defense = stringr::str_detect(.data$close_def_dist_range, "0.*2|0-2|2.*4|2-4"),
    is_wide_open = stringr::str_detect(.data$close_def_dist_range, "6\\+|6\\s*\\+|wide open")
  )

openness_metrics <- closest_defender_rows %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    openness_score = weighted_mean_or_na(.data$defender_bucket_score, .data$fga),
    tight_defense_frequency = 100 * safe_divide(sum(.data$fga[.data$is_tight_defense], na.rm = TRUE), sum(.data$fga, na.rm = TRUE)),
    wide_open_frequency = 100 * safe_divide(sum(.data$fga[.data$is_wide_open], na.rm = TRUE), sum(.data$fga, na.rm = TRUE)),
    .groups = "drop"
  )

dribble_rows <- tracking_raw %>%
  dplyr::filter(stringr::str_detect(stringr::str_to_lower(.data$table_name), "dribble")) %>%
  add_missing_cols(c("dribble_range"), NA_character_) %>%
  dplyr::mutate(
    dribble_range = stringr::str_to_lower(as.character(.data$dribble_range)),
    dribble_bucket_score = dplyr::case_when(
      stringr::str_detect(.data$dribble_range, "^0|0 dribble") ~ 0,
      stringr::str_detect(.data$dribble_range, "^1|1 dribble") ~ 20,
      stringr::str_detect(.data$dribble_range, "^2|2 dribble") ~ 40,
      stringr::str_detect(.data$dribble_range, "3.*6|3-6") ~ 75,
      stringr::str_detect(.data$dribble_range, "7\\+|7\\s*\\+") ~ 100,
      TRUE ~ NA_real_
    ),
    is_high_dribble = stringr::str_detect(.data$dribble_range, "3.*6|3-6|7\\+|7\\s*\\+")
  )

dribble_metrics <- dribble_rows %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    dribble_creation_score = weighted_mean_or_na(.data$dribble_bucket_score, .data$fga),
    high_dribble_frequency = 100 * safe_divide(sum(.data$fga[.data$is_high_dribble], na.rm = TRUE), sum(.data$fga, na.rm = TRUE)),
    .groups = "drop"
  )

touch_rows <- tracking_raw %>%
  dplyr::filter(stringr::str_detect(stringr::str_to_lower(.data$table_name), "touch")) %>%
  add_missing_cols(c("touch_time_range"), NA_character_) %>%
  dplyr::mutate(
    touch_time_range = stringr::str_to_lower(as.character(.data$touch_time_range)),
    quick_trigger_bucket_score = dplyr::case_when(
      stringr::str_detect(.data$touch_time_range, "<\\s*2|0-2|touch < 2") ~ 100,
      stringr::str_detect(.data$touch_time_range, "2.*6|2-6") ~ 45,
      stringr::str_detect(.data$touch_time_range, "6\\+|6\\s*\\+") ~ 0,
      TRUE ~ NA_real_
    ),
    is_long_touch = stringr::str_detect(.data$touch_time_range, "6\\+|6\\s*\\+")
  )

touch_metrics <- touch_rows %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    quick_trigger_score = weighted_mean_or_na(.data$quick_trigger_bucket_score, .data$fga),
    long_touch_frequency = 100 * safe_divide(sum(.data$fga[.data$is_long_touch], na.rm = TRUE), sum(.data$fga, na.rm = TRUE)),
    .groups = "drop"
  )

shot_clock_rows <- tracking_raw %>%
  dplyr::filter(stringr::str_detect(stringr::str_to_lower(.data$table_name), "shotclock")) %>%
  add_missing_cols(c("shot_clock_range"), NA_character_) %>%
  dplyr::mutate(
    shot_clock_range = stringr::str_to_lower(as.character(.data$shot_clock_range)),
    shot_clock_burden_score = dplyr::case_when(
      stringr::str_detect(.data$shot_clock_range, "24.*22|22.*18|18.*15|very early|early") ~ 0,
      stringr::str_detect(.data$shot_clock_range, "15.*7|average") ~ 35,
      stringr::str_detect(.data$shot_clock_range, "7.*4|late") ~ 75,
      stringr::str_detect(.data$shot_clock_range, "4.*0|very late") ~ 100,
      TRUE ~ NA_real_
    ),
    is_late_clock = stringr::str_detect(.data$shot_clock_range, "7.*4|4.*0|late|very late"),
    is_early_clock = stringr::str_detect(.data$shot_clock_range, "24.*22|22.*18|18.*15|very early|early")
  )

shot_clock_metrics <- shot_clock_rows %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    late_clock_burden = weighted_mean_or_na(.data$shot_clock_burden_score, .data$fga),
    early_clock_frequency = 100 * safe_divide(sum(.data$fga[.data$is_early_clock], na.rm = TRUE), sum(.data$fga, na.rm = TRUE)),
    .groups = "drop"
  )

player_shot_context <- player_metadata %>%
  dplyr::inner_join(
    tracking_raw %>% dplyr::distinct(.data$player_id),
    by = "player_id"
  ) %>%
  dplyr::left_join(openness_metrics, by = "player_id") %>%
  dplyr::left_join(dribble_metrics, by = "player_id") %>%
  dplyr::left_join(touch_metrics, by = "player_id") %>%
  dplyr::left_join(shot_clock_metrics, by = "player_id") %>%
  dplyr::mutate(
    openness_score = clamp_0_100(.data$openness_score),
    tight_defense_frequency = clamp_0_100(.data$tight_defense_frequency),
    wide_open_frequency = clamp_0_100(.data$wide_open_frequency),
    dribble_creation_score = clamp_0_100(.data$dribble_creation_score),
    high_dribble_frequency = clamp_0_100(.data$high_dribble_frequency),
    quick_trigger_score = clamp_0_100(.data$quick_trigger_score),
    long_touch_frequency = clamp_0_100(.data$long_touch_frequency),
    late_clock_burden = clamp_0_100(.data$late_clock_burden),
    early_clock_frequency = clamp_0_100(.data$early_clock_frequency),
    context_difficulty_score = clamp_0_100(
      0.35 * dplyr::coalesce(.data$tight_defense_frequency, 0) +
        0.25 * dplyr::coalesce(.data$high_dribble_frequency, 0) +
        0.20 * dplyr::coalesce(.data$long_touch_frequency, 0) +
        0.20 * dplyr::coalesce(.data$late_clock_burden, 0)
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "openness_score",
    "tight_defense_frequency",
    "wide_open_frequency",
    "dribble_creation_score",
    "high_dribble_frequency",
    "quick_trigger_score",
    "long_touch_frequency",
    "late_clock_burden",
    "early_clock_frequency",
    "context_difficulty_score"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$context_difficulty_score), .data$player_name)

write_project_parquet(player_shot_context, player_shot_context_path)

message("Phase 15 shot-context diagnostics:")

message("Top players by context_difficulty_score:")
print(
  player_shot_context %>%
    dplyr::arrange(dplyr::desc(.data$context_difficulty_score)) %>%
    utils::head(20)
)

message("Luka / LeBron / Austin examples, if available:")
print(
  player_shot_context %>%
    dplyr::filter(stringr::str_detect(.data$player_name, "Luka|Doncic|Dončić|LeBron|Austin Reaves")) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "openness_score",
      "tight_defense_frequency",
      "wide_open_frequency",
      "dribble_creation_score",
      "high_dribble_frequency",
      "quick_trigger_score",
      "long_touch_frequency",
      "late_clock_burden",
      "early_clock_frequency",
      "context_difficulty_score"
    )
)

message("Score distributions:")
print(
  player_shot_context %>%
    dplyr::summarise(
      players = dplyr::n(),
      median_openness_score = stats::median(.data$openness_score, na.rm = TRUE),
      median_dribble_creation_score = stats::median(.data$dribble_creation_score, na.rm = TRUE),
      median_quick_trigger_score = stats::median(.data$quick_trigger_score, na.rm = TRUE),
      median_late_clock_burden = stats::median(.data$late_clock_burden, na.rm = TRUE),
      median_context_difficulty_score = stats::median(.data$context_difficulty_score, na.rm = TRUE),
      missing_openness = sum(is.na(.data$openness_score)),
      missing_dribble = sum(is.na(.data$dribble_creation_score)),
      missing_touch = sum(is.na(.data$quick_trigger_score)),
      missing_shot_clock = sum(is.na(.data$late_clock_burden))
    )
)

message("Phase 15 note: descriptive shot-context metrics only. No ATK score was built and no attack library, moveset, or attack identity outputs were modified.")
