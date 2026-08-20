# ============================================================
# 08_build_player_stint_contributions.R
# Phase 6: Player-level stint contribution bridge table.
#
# One row per target-team player per valid stint.
# This is a clean bridge table only; it does not estimate player ratings.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

phase3_path <- glue("data/processed/stints/{team_abbr}_stints_scored_{season}.parquet")
rotation_clean_path <- glue("data/processed/stints/{team_abbr}_rotation_clean_{season}.parquet")
phase5_path <- "outputs/Phase5_true_lineups.parquet"
player_master_path <- glue("data/processed/player/player_master_{season}.parquet")
legacy_player_master_path <- glue("data/processed/player_master_{season}.parquet")
phase6_path <- "outputs/Phase6_player_stint_contributions.parquet"

if (!file.exists(phase3_path)) {
  stop("Missing Phase 3 scored stints: ", phase3_path, ". Run 05_score_stints.R first.", call. = FALSE)
}

if (!file.exists(rotation_clean_path)) {
  stop("Missing clean rotation file: ", rotation_clean_path, ". Run 04_build_stints.R first.", call. = FALSE)
}

if (!file.exists(phase5_path)) {
  message("Phase 5 output not found at ", phase5_path, "; rebuilding player-stint bridge directly from Phase 3 and rotations.")
}

if (!file.exists(player_master_path) && file.exists(legacy_player_master_path)) {
  player_master_path <- legacy_player_master_path
}

if (!file.exists(player_master_path)) {
  stop(
    "Missing player master file. Expected one of: ",
    glue("data/processed/player/player_master_{season}.parquet"),
    " or ",
    glue("data/processed/player_master_{season}.parquet"),
    call. = FALSE
  )
}

sum_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  sum(x, na.rm = TRUE)
}

scalar_count <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(0L)
  }

  as.integer(x)
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
    dplyr::pull(.data$person_id_chr) %>%
    normalize_player_ids()

  opponent_players <- on_court %>%
    dplyr::filter(!.data$is_target_team) %>%
    dplyr::pull(.data$person_id_chr) %>%
    normalize_player_ids()

  target_count <- length(target_players)
  opponent_count <- length(opponent_players)
  total_count <- length(normalize_player_ids(on_court$person_id_chr))
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
    target_players = list(target_players),
    opponent_players = list(opponent_players),
    target_player_count = target_count,
    opponent_player_count = opponent_count,
    total_player_count = total_count,
    target_lineup_key = normalize_lineup_key(target_players),
    opponent_lineup_key = normalize_lineup_key(opponent_players),
    malformed_stint = length(malformed_reasons) > 0,
    malformed_reason = if (length(malformed_reasons) == 0) NA_character_ else paste(malformed_reasons, collapse = "; ")
  )
}

build_player_metadata <- function(player_master) {
  player_id_col <- safe_first_existing_col(player_master, c("player_id", "person_id", "nba_player_id", "id"))
  player_name_col <- safe_first_existing_col(
    player_master,
    c("player_name", "player", "full_name", "display_first_last", "display_name", "name"),
    required = FALSE
  )
  team_abbreviation_col <- safe_first_existing_col(
    player_master,
    c("team_abbreviation", "team_abbr", "team_tricode", "team"),
    required = FALSE
  )
  position_col <- safe_first_existing_col(
    player_master,
    c("position", "pos", "player_position"),
    required = FALSE
  )

  if (is.na(position_col)) {
    message("Player metadata diagnostic: position is unavailable from current player_master source data; position will be NA_character_.")
  }

  out <- player_master %>%
    dplyr::transmute(
      player_id = as.character(.data[[player_id_col]]),
      player_name = if (!is.na(player_name_col)) as.character(.data[[player_name_col]]) else NA_character_,
      team_abbreviation = if (!is.na(team_abbreviation_col)) as.character(.data[[team_abbreviation_col]]) else NA_character_,
      position = if (!is.na(position_col)) as.character(.data[[position_col]]) else NA_character_
    ) %>%
    dplyr::filter(!is.na(.data$player_id), .data$player_id != "") %>%
    dplyr::distinct(.data$player_id, .keep_all = TRUE)

  if (!is.na(position_col) && all(is.na(out$position))) {
    message("Player metadata diagnostic: position column '", position_col, "' exists but has no usable values; position will remain NA_character_.")
  }

  out
}

stints_scored <- read_project_parquet(phase3_path)
rotation_clean <- read_project_parquet(rotation_clean_path)
player_master <- read_project_parquet(player_master_path)
player_metadata <- build_player_metadata(player_master)

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
    raw_point_differential = as.numeric(.data$target_margin_change),
    point_diff_per_minute = dplyr::if_else(
      .data$stint_minutes > 0 & !is.na(.data$raw_point_differential),
      .data$raw_point_differential / .data$stint_minutes,
      NA_real_
    )
  )

valid_stints <- stints_decomposed %>%
  dplyr::filter(!.data$malformed_stint)

player_stint_contributions <- valid_stints %>%
  dplyr::select(
    tidyselect::any_of(c(
      "game_id",
      "stint_id",
      "target_players",
      "target_lineup_key",
      "opponent_lineup_key",
      "matchup_lineup_key",
      "stint_minutes",
      "target_points_for",
      "target_points_against",
      "raw_point_differential",
      "point_diff_per_minute"
    ))
  ) %>%
  tidyr::unnest_longer("target_players", values_to = "player_id") %>%
  dplyr::mutate(
    season = .env$season,
    team_abbr = .env$team_abbr,
    player_id = as.character(.data$player_id),
    points_for = as.numeric(.data$target_points_for),
    points_against = as.numeric(.data$target_points_against),
    player_on_target_team = TRUE
  ) %>%
  dplyr::left_join(player_metadata, by = "player_id") %>%
  dplyr::mutate(
    team_abbreviation = dplyr::coalesce(.data$team_abbreviation, .env$team_abbr)
  ) %>%
  dplyr::select(
    tidyselect::all_of(c(
      "season",
      "team_abbr",
      "game_id",
      "stint_id",
      "player_id",
      "player_name",
      "team_abbreviation",
      "position",
      "target_lineup_key",
      "opponent_lineup_key",
      "matchup_lineup_key",
      "stint_minutes",
      "points_for",
      "points_against",
      "raw_point_differential",
      "point_diff_per_minute",
      "player_on_target_team"
    ))
  ) %>%
  dplyr::arrange(.data$game_id, .data$stint_id, .data$player_id)

message("Phase 6 player-stint contribution diagnostics:")
phase6_row_diagnostics <- tibble::tibble(
  valid_stints = scalar_count(nrow(valid_stints)),
  expected_player_stint_rows = scalar_count(nrow(valid_stints) * 5),
  actual_player_stint_rows = scalar_count(nrow(player_stint_contributions)),
  malformed_stints_excluded = scalar_count(sum(stints_decomposed$malformed_stint, na.rm = TRUE))
)

print(
  phase6_row_diagnostics
)

message("Target player rows per valid stint:")
print(
  player_stint_contributions %>%
    dplyr::count(.data$game_id, .data$stint_id, name = "target_player_rows") %>%
    dplyr::count(.data$target_player_rows, name = "stints") %>%
    dplyr::arrange(.data$target_player_rows)
)

message("Player metadata join diagnostics:")
print(
  player_stint_contributions %>%
    dplyr::summarise(
      player_stint_rows = dplyr::n(),
      missing_player_name_rows = sum(is.na(.data$player_name)),
      missing_team_abbreviation_rows = sum(is.na(.data$team_abbreviation)),
      missing_position_rows = sum(is.na(.data$position)),
      distinct_players_missing_name = dplyr::n_distinct(.data$player_id[is.na(.data$player_name)])
    )
)

stint_diff_total <- sum_or_na(valid_stints$raw_point_differential)
player_diff_total <- sum_or_na(player_stint_contributions$raw_point_differential)

message("Player-stint differential bridge check:")
print(
  tibble::tibble(
    stint_total_raw_point_differential = stint_diff_total,
    expected_player_stint_raw_point_differential = if (is.na(stint_diff_total)) NA_real_ else 5 * stint_diff_total,
    actual_player_stint_raw_point_differential = player_diff_total,
    matches_expected = dplyr::if_else(
      is.na(stint_diff_total) & is.na(player_diff_total),
      TRUE,
      isTRUE(all.equal(player_diff_total, 5 * stint_diff_total, tolerance = 1e-8))
    )
  )
)

write_project_parquet(player_stint_contributions, phase6_path)

message("Phase 6 player-stint output summary:")
print(
  player_stint_contributions %>%
    dplyr::summarise(
      player_stint_rows = dplyr::n(),
      players = dplyr::n_distinct(.data$player_id),
      stints = dplyr::n_distinct(paste(.data$game_id, .data$stint_id, sep = "::")),
      total_points_for = sum_or_na(.data$points_for),
      total_points_against = sum_or_na(.data$points_against),
      total_raw_point_differential = sum_or_na(.data$raw_point_differential)
    )
)
