# ============================================================
# 07_build_true_lineups.R
# Phase 5: Split 10-player matchup stints into true team lineups.
#
# This script decomposes each scored stint into:
# - target-team 5-player lineup
# - opponent-team 5-player lineup
#
# It keeps the original 10-player matchup identifier for future matchup
# modeling and intentionally does not estimate APM/RAPM/Bayesian models.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

phase3_path <- glue("data/processed/stints/{team_abbr}_stints_scored_{season}.parquet")
rotation_clean_path <- glue("data/processed/stints/{team_abbr}_rotation_clean_{season}.parquet")
phase5_path <- "outputs/Phase5_true_lineups.parquet"

if (!file.exists(phase3_path)) {
  stop("Missing Phase 3 scored stints: ", phase3_path, ". Run 05_score_stints.R first.", call. = FALSE)
}

if (!file.exists(rotation_clean_path)) {
  stop("Missing clean rotation file: ", rotation_clean_path, ". Run 04_build_stints.R first.", call. = FALSE)
}

sum_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  sum(x, na.rm = TRUE)
}

collapse_unique <- function(x) {
  x <- purrr::map_chr(x, normalize_lineup_key)
  x <- sort(unique(stats::na.omit(x)))

  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(x, collapse = " | ")
}

normalize_player_ids <- function(player_ids) {
  player_ids <- unlist(player_ids, recursive = TRUE, use.names = FALSE)
  player_ids <- stringr::str_extract_all(paste(player_ids, collapse = "-"), "\\d+")[[1]]
  sort(unique(stats::na.omit(as.character(player_ids))))
}

normalize_lineup_key <- function(player_ids) {
  player_ids <- normalize_player_ids(player_ids)

  if (length(player_ids) == 0) {
    return(NA_character_)
  }

  paste(player_ids, collapse = "-")
}

make_lineup_key <- normalize_lineup_key

lineup_slot <- function(lineup_key, slot) {
  players <- stringr::str_split(normalize_lineup_key(lineup_key), "-", simplify = FALSE)[[1]]
  dplyr::nth(players, slot, default = NA_character_)
}

infer_stint_time_scale_factor <- function(stints_scored) {
  if ("stint_time_scale_factor" %in% names(stints_scored)) {
    scale_factor <- suppressWarnings(stats::median(stints_scored$stint_time_scale_factor, na.rm = TRUE))

    if (!is.na(scale_factor) && scale_factor > 0) {
      return(scale_factor)
    }
  }

  max_time <- suppressWarnings(max(c(stints_scored$start_time, stints_scored$end_time), na.rm = TRUE))

  dplyr::case_when(
    is.na(max_time) ~ 1,
    max_time > 3600 ~ 10,
    TRUE ~ 1
  )
}

get_stint_lineup_split <- function(game_id_i, start_time_i, end_time_i, target_team_id_i, rotation_data) {
  on_court <- rotation_data %>%
    dplyr::filter(
      .data$game_id == game_id_i,
      .data$in_time <= start_time_i,
      .data$out_time >= end_time_i
    ) %>%
    dplyr::mutate(
      team_id_chr = as.character(.data$team_id),
      person_id_chr = as.character(.data$person_id),
      is_target_team = .data$team_id_chr == as.character(target_team_id_i)
    )

  target_players <- on_court %>%
    dplyr::filter(.data$is_target_team) %>%
    dplyr::pull(.data$person_id_chr)

  opponent_players <- on_court %>%
    dplyr::filter(!.data$is_target_team) %>%
    dplyr::pull(.data$person_id_chr)

  target_count <- length(unique(target_players))
  opponent_count <- length(unique(opponent_players))
  total_count <- length(unique(on_court$person_id_chr))
  malformed_reasons <- c()

  if (target_count != 5) {
    malformed_reasons <- c(malformed_reasons, paste0("target_players=", target_count))
  }

  if (opponent_count != 5) {
    malformed_reasons <- c(malformed_reasons, paste0("opponent_players=", opponent_count))
  }

  if (total_count != 10) {
    malformed_reasons <- c(malformed_reasons, paste0("total_players=", total_count))
  }

  tibble::tibble(
    target_players = list(sort(unique(target_players))),
    opponent_players = list(sort(unique(opponent_players))),
    target_player_count = target_count,
    opponent_player_count = opponent_count,
    total_player_count = total_count,
    target_lineup_key = make_lineup_key(target_players),
    opponent_lineup_key = make_lineup_key(opponent_players),
    malformed_stint = length(malformed_reasons) > 0,
    malformed_reason = if (length(malformed_reasons) == 0) NA_character_ else paste(malformed_reasons, collapse = "; ")
  )
}

stints_scored <- read_project_parquet(phase3_path)
rotation_clean <- read_project_parquet(rotation_clean_path)

validate_columns(
  stints_scored,
  c(
    "game_id",
    "stint_id",
    "lineup_key",
    "start_time",
    "end_time",
    "stint_seconds_proxy",
    "target_team_id",
    "target_team_abbr",
    "target_margin_change"
  )
)

validate_columns(rotation_clean, c("game_id", "team_id", "person_id", "in_time", "out_time"))

if (!"target_points_for" %in% names(stints_scored)) {
  stints_scored <- stints_scored %>%
    dplyr::mutate(target_points_for = NA_real_)
}

if (!"target_points_against" %in% names(stints_scored)) {
  stints_scored <- stints_scored %>%
    dplyr::mutate(target_points_against = NA_real_)
}

rotation_clean <- rotation_clean %>%
  dplyr::mutate(
    game_id = as.character(.data$game_id),
    in_time = as.numeric(.data$in_time),
    out_time = as.numeric(.data$out_time)
  )

stint_time_scale_factor <- infer_stint_time_scale_factor(stints_scored)

lineup_splits <- purrr::pmap_dfr(
  list(
    as.character(stints_scored$game_id),
    as.numeric(stints_scored$start_time),
    as.numeric(stints_scored$end_time),
    stints_scored$target_team_id
  ),
  ~ get_stint_lineup_split(..1, ..2, ..3, ..4, rotation_clean)
)

stints_decomposed <- dplyr::bind_cols(stints_scored, lineup_splits) %>%
  dplyr::rename(matchup_lineup_key = "lineup_key") %>%
  dplyr::mutate(
    target_lineup_key = purrr::map_chr(.data$target_lineup_key, normalize_lineup_key),
    opponent_lineup_key = purrr::map_chr(.data$opponent_lineup_key, normalize_lineup_key),
    matchup_lineup_key = purrr::map_chr(.data$matchup_lineup_key, normalize_lineup_key),
    stint_minutes = as.numeric(.data$stint_seconds_proxy) / stint_time_scale_factor / 60,
    points_available = !is.na(.data$target_points_for) & !is.na(.data$target_points_against),
    margin_available = !is.na(.data$target_margin_change),
    margin_minutes = dplyr::if_else(.data$margin_available, .data$stint_minutes, 0)
  )

message("Phase 5 lineup decomposition diagnostics:")
print(
  stints_decomposed %>%
    dplyr::summarise(
      stints = dplyr::n(),
      stints_with_5_target_players = sum(.data$target_player_count == 5, na.rm = TRUE),
      stints_with_5_opponent_players = sum(.data$opponent_player_count == 5, na.rm = TRUE),
      malformed_stints = sum(.data$malformed_stint, na.rm = TRUE),
      distinct_target_lineups = dplyr::n_distinct(.data$target_lineup_key, na.rm = TRUE),
      distinct_opponent_lineups = dplyr::n_distinct(.data$opponent_lineup_key, na.rm = TRUE),
      distinct_10_player_matchups = dplyr::n_distinct(.data$matchup_lineup_key, na.rm = TRUE)
    )
)

message("Target player count distribution:")
print(
  stints_decomposed %>%
    dplyr::count(.data$target_player_count, name = "stints") %>%
    dplyr::arrange(.data$target_player_count)
)

message("Opponent player count distribution:")
print(
  stints_decomposed %>%
    dplyr::count(.data$opponent_player_count, name = "stints") %>%
    dplyr::arrange(.data$opponent_player_count)
)

message("Malformed stint examples:")
print(
  stints_decomposed %>%
    dplyr::filter(.data$malformed_stint) %>%
    dplyr::select(
      tidyselect::any_of(c(
        "game_id",
        "stint_id",
        "start_time",
        "end_time",
        "matchup_lineup_key",
        "target_team_id",
        "target_team_abbr",
        "target_player_count",
        "opponent_player_count",
        "total_player_count",
        "malformed_reason"
      ))
    ) %>%
    dplyr::slice_head(n = 10)
)

valid_stints <- stints_decomposed %>%
  dplyr::filter(!.data$malformed_stint)

target_lineups <- valid_stints %>%
  dplyr::group_by(.data$target_team_id, .data$target_team_abbr, .data$target_lineup_key) %>%
  dplyr::summarise(
    season = .env$season,
    team_abbr = .env$team_abbr,
    games = dplyr::n_distinct(.data$game_id),
    stints = dplyr::n(),
    minutes = sum(.data$stint_minutes, na.rm = TRUE),
    points_available_stints = sum(.data$points_available, na.rm = TRUE),
    margin_available_stints = sum(.data$margin_available, na.rm = TRUE),
    points_for = sum_or_na(.data$target_points_for),
    points_against = sum_or_na(.data$target_points_against),
    raw_point_differential = sum_or_na(.data$target_margin_change),
    per_minute_differential = dplyr::if_else(
      sum(.data$margin_minutes, na.rm = TRUE) > 0,
      sum_or_na(.data$target_margin_change) / sum(.data$margin_minutes, na.rm = TRUE),
      NA_real_
    ),
    target_player_ids = dplyr::first(.data$target_lineup_key),
    opponent_lineup_keys = collapse_unique(.data$opponent_lineup_key),
    matchup_lineup_keys = collapse_unique(.data$matchup_lineup_key),
    original_10_player_matchup_count = dplyr::n_distinct(.data$matchup_lineup_key, na.rm = TRUE),
    malformed_stints_excluded = 0L,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    target_player_1_id = purrr::map_chr(.data$target_lineup_key, lineup_slot, slot = 1),
    target_player_2_id = purrr::map_chr(.data$target_lineup_key, lineup_slot, slot = 2),
    target_player_3_id = purrr::map_chr(.data$target_lineup_key, lineup_slot, slot = 3),
    target_player_4_id = purrr::map_chr(.data$target_lineup_key, lineup_slot, slot = 4),
    target_player_5_id = purrr::map_chr(.data$target_lineup_key, lineup_slot, slot = 5)
  ) %>%
  dplyr::relocate(
    tidyselect::all_of(c(
      "season",
      "team_abbr",
      "target_team_id",
      "target_team_abbr",
      "target_lineup_key",
      "target_player_ids",
      "target_player_1_id",
      "target_player_2_id",
      "target_player_3_id",
      "target_player_4_id",
      "target_player_5_id",
      "games",
      "stints",
      "minutes",
      "points_for",
      "points_against",
      "raw_point_differential",
      "per_minute_differential"
    ))
  ) %>%
  dplyr::arrange(dplyr::desc(.data$minutes), dplyr::desc(.data$raw_point_differential))

write_project_parquet(target_lineups, phase5_path)

message("Phase 5 target lineup summary:")
print(
  target_lineups %>%
    dplyr::summarise(
      target_lineups = dplyr::n(),
      total_minutes = sum(.data$minutes, na.rm = TRUE),
      total_points_for = sum_or_na(.data$points_for),
      total_points_against = sum_or_na(.data$points_against),
      total_raw_point_differential = sum_or_na(.data$raw_point_differential)
    )
)
