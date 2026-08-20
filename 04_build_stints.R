# ============================================================
# 04_build_stints.R
# Convert rotation intervals into stint rows.
# ============================================================

source("01_setup_project.R")

rotation_sample <- read_project_parquet(
  glue("data/raw/rotations/{team_abbr}_rotation_sample_{season}.parquet")
)

validate_columns(rotation_sample, c("game_id", "team_id", "person_id"))

in_col <- safe_first_existing_col(rotation_sample, c("in_time_real", "in_time", "in_time_actual"))
out_col <- safe_first_existing_col(rotation_sample, c("out_time_real", "out_time", "out_time_actual"))

rotation_clean <- rotation_sample %>%
  dplyr::mutate(
    game_id = as.character(.data$game_id),
    in_time = as.numeric(.data[[in_col]]),
    out_time = as.numeric(.data[[out_col]])
  ) %>%
  dplyr::filter(!is.na(.data$in_time), !is.na(.data$out_time)) %>%
  dplyr::select(.data$game_id, .data$team_id, .data$person_id, .data$in_time, .data$out_time)

stint_boundaries <- rotation_clean %>%
  dplyr::select(.data$game_id, .data$in_time, .data$out_time) %>%
  tidyr::pivot_longer(cols = c(.data$in_time, .data$out_time), values_to = "time") %>%
  dplyr::distinct(.data$game_id, .data$time) %>%
  dplyr::arrange(.data$game_id, .data$time)

stints <- stint_boundaries %>%
  dplyr::group_by(.data$game_id) %>%
  dplyr::arrange(.data$time, .by_group = TRUE) %>%
  dplyr::mutate(
    stint_id = dplyr::row_number(),
    start_time = .data$time,
    end_time = dplyr::lead(.data$time),
    stint_seconds_proxy = .data$end_time - .data$start_time
  ) %>%
  dplyr::filter(!is.na(.data$end_time), .data$end_time > .data$start_time) %>%
  dplyr::ungroup()

players_on_court <- purrr::pmap(
  list(stints$game_id, stints$start_time, stints$end_time),
  ~ get_players_on_court(..1, ..2, ..3, rotation_clean)
)

stints <- stints %>%
  dplyr::mutate(
    players_on_court = players_on_court,
    n_players = purrr::map_int(.data$players_on_court, length),
    lineup_key = purrr::map_chr(.data$players_on_court, ~ paste(sort(.x), collapse = "-"))
  )

stints_10 <- stints %>%
  dplyr::filter(.data$n_players == 10)

write_project_parquet(rotation_clean, glue("data/processed/stints/{team_abbr}_rotation_clean_{season}.parquet"))
write_project_parquet(stints, glue("data/processed/stints/{team_abbr}_stints_all_{season}.parquet"))
write_project_parquet(stints_10, glue("data/processed/stints/{team_abbr}_stints_10_players_{season}.parquet"))

message("All stints: ", nrow(stints))
message("10-player stints: ", nrow(stints_10))
print(stints %>% dplyr::count(.data$n_players))
