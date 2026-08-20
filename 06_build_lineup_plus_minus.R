# ============================================================
# 06_build_lineup_plus_minus.R
# Phase 4: Conservative lineup-level plus-minus dataframe.
#
# This script aggregates Phase 3 scored stint-event proxies into a
# lineup-level table that is safe for downstream simulation/archetype work.
# It is intentionally descriptive only; no adjusted plus-minus, RAPM, or
# Bayesian shrinkage models are estimated here.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

phase3_path <- glue("data/processed/stints/{team_abbr}_stints_scored_{season}.parquet")
phase4_path <- "outputs/Phase4_lineup_plus_minus.parquet"

if (!file.exists(phase3_path)) {
  stop(
    "Missing Phase 3 output: ",
    phase3_path,
    ". Run 05_score_stints.R before Phase 4.",
    call. = FALSE
  )
}

stints_scored <- read_project_parquet(phase3_path)

validate_columns(
  stints_scored,
  c(
    "game_id",
    "stint_id",
    "lineup_key",
    "players_on_court",
    "n_players",
    "stint_seconds_proxy",
    "target_team_id",
    "target_team_abbr",
    "target_margin_change"
  )
)

sum_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  sum(x, na.rm = TRUE)
}

if (!"opponent_margin_change" %in% names(stints_scored)) {
  stints_scored <- stints_scored %>%
    dplyr::mutate(opponent_margin_change = -.data$target_margin_change)
}

if (!"target_points_for" %in% names(stints_scored)) {
  stints_scored <- stints_scored %>%
    dplyr::mutate(target_points_for = NA_real_)
}

if (!"target_points_against" %in% names(stints_scored)) {
  stints_scored <- stints_scored %>%
    dplyr::mutate(target_points_against = NA_real_)
}

build_lineup_player_slots <- function(players_on_court) {
  sorted_players <- sort(unique(as.character(players_on_court)))

  tibble::tibble(
    lineup_player_ids = paste(sorted_players, collapse = "-"),
    player_1_id = dplyr::nth(sorted_players, 1, default = NA_character_),
    player_2_id = dplyr::nth(sorted_players, 2, default = NA_character_),
    player_3_id = dplyr::nth(sorted_players, 3, default = NA_character_),
    player_4_id = dplyr::nth(sorted_players, 4, default = NA_character_),
    player_5_id = dplyr::nth(sorted_players, 5, default = NA_character_),
    player_6_id = dplyr::nth(sorted_players, 6, default = NA_character_),
    player_7_id = dplyr::nth(sorted_players, 7, default = NA_character_),
    player_8_id = dplyr::nth(sorted_players, 8, default = NA_character_),
    player_9_id = dplyr::nth(sorted_players, 9, default = NA_character_),
    player_10_id = dplyr::nth(sorted_players, 10, default = NA_character_)
  )
}

# Phase 3 stores all 10 players on court together. That is useful for a
# conservative matchup lineup id, but it is not enough to split the table into
# target-team five-man units vs opponent five-man units without additional team
# membership at the stint level.
# TODO(Phase 3): Attach player-team side for each on-court player so Phase 4 can
# expose target_lineup_player_ids and opponent_lineup_player_ids separately.
lineup_player_slots <- stints_scored %>%
  dplyr::group_by(.data$lineup_key) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::select("lineup_key", "players_on_court") %>%
  dplyr::mutate(player_slots = purrr::map(.data$players_on_court, build_lineup_player_slots)) %>%
  tidyr::unnest(cols = "player_slots") %>%
  dplyr::select(-tidyselect::all_of("players_on_court"))

lineup_level_base <- stints_scored %>%
  dplyr::filter(.data$n_players == 10) %>%
  dplyr::mutate(
    stint_seconds_proxy = as.numeric(.data$stint_seconds_proxy),
    target_points_for = as.numeric(.data$target_points_for),
    target_points_against = as.numeric(.data$target_points_against),
    target_margin_change = as.numeric(.data$target_margin_change),
    opponent_margin_change = as.numeric(.data$opponent_margin_change),
    margin_available = !is.na(.data$target_margin_change),
    points_available = !is.na(.data$target_points_for) &
      !is.na(.data$target_points_against)
  ) %>%
  dplyr::group_by(.data$lineup_key) %>%
  dplyr::summarise(
    season = .env$season,
    team_abbr = .env$team_abbr,
    target_team_id = dplyr::first(.data$target_team_id),
    target_team_abbr = dplyr::first(.data$target_team_abbr),
    games = dplyr::n_distinct(.data$game_id),
    stints = dplyr::n(),
    margin_available_stints = sum(.data$margin_available, na.rm = TRUE),
    points_available_stints = sum(.data$points_available, na.rm = TRUE),
    minutes_proxy = sum(.data$stint_seconds_proxy, na.rm = TRUE) / 60,
    margin_minutes_proxy = sum(
      dplyr::if_else(.data$margin_available, .data$stint_seconds_proxy, 0),
      na.rm = TRUE
    ) / 60,
    points_minutes_proxy = sum(
      dplyr::if_else(.data$points_available, .data$stint_seconds_proxy, 0),
      na.rm = TRUE
    ) / 60,
    seconds_proxy = sum(.data$stint_seconds_proxy, na.rm = TRUE),
    margin_seconds_proxy = sum(
      dplyr::if_else(.data$margin_available, .data$stint_seconds_proxy, 0),
      na.rm = TRUE
    ),
    points_seconds_proxy = sum(
      dplyr::if_else(.data$points_available, .data$stint_seconds_proxy, 0),
      na.rm = TRUE
    ),
    points_for = sum_or_na(.data$target_points_for),
    points_against = sum_or_na(.data$target_points_against),
    target_margin_change = sum_or_na(.data$target_margin_change),
    opponent_margin_change = sum_or_na(.data$opponent_margin_change),
    raw_point_differential = sum_or_na(.data$target_margin_change),
    missing_margin_stints = sum(!.data$margin_available, na.rm = TRUE),
    missing_points_stints = sum(!.data$points_available, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    point_diff_per_minute = dplyr::if_else(
      .data$margin_minutes_proxy > 0,
      .data$target_margin_change / .data$margin_minutes_proxy,
      NA_real_
    ),
    point_diff_per_48 = .data$point_diff_per_minute * 48,
    points_for_per_minute = dplyr::if_else(
      .data$points_minutes_proxy > 0 & !is.na(.data$points_for),
      .data$points_for / .data$points_minutes_proxy,
      NA_real_
    ),
    points_against_per_minute = dplyr::if_else(
      .data$points_minutes_proxy > 0 & !is.na(.data$points_against),
      .data$points_against / .data$points_minutes_proxy,
      NA_real_
    ),
    possession_proxy = NA_real_,
    point_diff_per_possession = NA_real_,
    points_for_per_possession = NA_real_,
    points_against_per_possession = NA_real_,
    data_quality_note = dplyr::case_when(
      .data$missing_points_stints == .data$stints & .data$margin_available_stints > 0 ~ "Phase 3 provides margin change but not safely aligned true points for/against; point totals and point rates are NA.",
      .data$missing_margin_stints > 0 | .data$missing_points_stints > 0 ~ "Some Phase 3 stints lack complete score endpoints; rates use only available real score proxies.",
      TRUE ~ "Conservative Phase 3 score-delta aggregation; no possession model or opponent-adjustment applied."
    )
  )

# TODO(Phase 3): Add reliable possession boundaries or play-event possession ids.
# Until then, Phase 4 leaves possession_proxy and per-possession rates as NA
# rather than deriving misleading values from event order.

lineup_plus_minus <- lineup_level_base %>%
  dplyr::left_join(lineup_player_slots, by = "lineup_key") %>%
  dplyr::relocate(
    tidyselect::all_of(c(
      "season",
      "team_abbr",
      "target_team_id",
      "target_team_abbr",
      "lineup_key",
      "lineup_player_ids"
    )),
    tidyselect::starts_with("player_")
  ) %>%
  dplyr::arrange(dplyr::desc(.data$minutes_proxy), dplyr::desc(.data$raw_point_differential))

write_project_parquet(lineup_plus_minus, phase4_path)

message("Phase 4 lineup plus-minus summary:")
print(
  lineup_plus_minus %>%
    dplyr::summarise(
      lineups = dplyr::n(),
      total_minutes_proxy = sum(.data$minutes_proxy, na.rm = TRUE),
      total_points_for = sum_or_na(.data$points_for),
      total_points_against = sum_or_na(.data$points_against),
      total_target_margin_change = sum_or_na(.data$target_margin_change),
      total_opponent_margin_change = sum_or_na(.data$opponent_margin_change),
      total_raw_point_differential = sum_or_na(.data$raw_point_differential)
    )
)
