# ============================================================
# 20b_build_creation_opportunity_signals.R
# Phase 20b: Creation Opportunity Signals.
#
# Goal:
# Expand creation burden beyond raw assists using the real player-level tracking
# metrics extracted in Phase 19b.
#
# These are descriptive opportunity signals only. They are intended to help
# future CR versions understand on-ball burden, touch quality, and creation
# environment. This phase does not build a final CR score and does not modify
# previous phases.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_tracking_creation_metrics_path <- "outputs/attacks/player_tracking_creation_metrics.parquet"
player_creation_signals_path <- "outputs/attacks/player_creation_signals.parquet"

if (!file.exists(player_tracking_creation_metrics_path)) {
  stop(
    "Missing Phase 20b input: ",
    player_tracking_creation_metrics_path,
    ". Run 19b_extract_tracking_creation_metrics.R first.",
    call. = FALSE
  )
}

safe_divide <- function(numerator, denominator) {
  numerator <- as.numeric(numerator)
  denominator <- as.numeric(denominator)

  out <- numerator / denominator
  out[is.na(numerator) | is.na(denominator) | denominator == 0] <- NA_real_
  out
}

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

creation_metric_cols <- c(
  "touches",
  "front_court_touches",
  "points_per_touch",
  "avg_sec_per_touch",
  "avg_drib_per_touch",
  "secondary_assists",
  "possessions"
)

tracking_creation_metrics <- read_project_parquet(player_tracking_creation_metrics_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    creation_metric_cols,
    "source_endpoints",
    "source_tables"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name = as.character(.data$player_name),
    player_nickname = as.character(.data$player_nickname),
    team_abbreviation = as.character(.data$team_abbreviation),
    touches = suppressWarnings(as.numeric(.data$touches)),
    front_court_touches = suppressWarnings(as.numeric(.data$front_court_touches)),
    points_per_touch = suppressWarnings(as.numeric(.data$points_per_touch)),
    avg_sec_per_touch = suppressWarnings(as.numeric(.data$avg_sec_per_touch)),
    avg_drib_per_touch = suppressWarnings(as.numeric(.data$avg_drib_per_touch)),
    secondary_assists = suppressWarnings(as.numeric(.data$secondary_assists)),
    possessions = suppressWarnings(as.numeric(.data$possessions)),
    source_endpoints = as.character(.data$source_endpoints),
    source_tables = as.character(.data$source_tables)
  )

validate_columns(
  tracking_creation_metrics,
  c("player_id", "touches", "front_court_touches", "points_per_touch", "avg_sec_per_touch", "avg_drib_per_touch", "secondary_assists", "possessions")
)

total_observed_touches <- sum(tracking_creation_metrics$touches, na.rm = TRUE)

player_creation_signals <- tracking_creation_metrics %>%
  dplyr::mutate(
    # touch_share is within the currently pulled tracking sample, not league-wide.
    touch_share = safe_divide(.data$touches, total_observed_touches),
    # front_court_touch_share describes what share of this player's touches are
    # front-court touches, a better creation-opportunity proxy than raw count.
    front_court_touch_share = safe_divide(.data$front_court_touches, .data$touches),
    creation_signal_available = !is.na(.data$touches) |
      !is.na(.data$front_court_touches) |
      !is.na(.data$points_per_touch) |
      !is.na(.data$avg_sec_per_touch) |
      !is.na(.data$avg_drib_per_touch) |
      !is.na(.data$secondary_assists) |
      !is.na(.data$possessions)
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "touch_share",
    "front_court_touch_share",
    "points_per_touch",
    "avg_sec_per_touch",
    "avg_drib_per_touch",
    "secondary_assists",
    "possessions",
    "touches",
    "front_court_touches",
    "creation_signal_available",
    "source_endpoints",
    "source_tables"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$touch_share), .data$player_name)

write_project_parquet(player_creation_signals, player_creation_signals_path)

message("Phase 20b Creation Opportunity Signals diagnostics:")

message("Creation signal availability:")
print(
  player_creation_signals %>%
    dplyr::summarise(
      players = dplyr::n(),
      creation_signal_available_players = sum(.data$creation_signal_available, na.rm = TRUE),
      total_observed_touches = total_observed_touches,
      players_with_touches = sum(!is.na(.data$touches)),
      players_with_front_court_touches = sum(!is.na(.data$front_court_touches)),
      players_with_points_per_touch = sum(!is.na(.data$points_per_touch)),
      players_with_avg_sec_per_touch = sum(!is.na(.data$avg_sec_per_touch)),
      players_with_avg_drib_per_touch = sum(!is.na(.data$avg_drib_per_touch)),
      players_with_secondary_assists = sum(!is.na(.data$secondary_assists)),
      players_with_possessions = sum(!is.na(.data$possessions))
    )
)

message("Luka / LeBron / Austin / Ayton / Jaxson / Kennard examples:")
print(
  player_creation_signals %>%
    dplyr::filter(
      .data$player_id %in% c("1629029", "2544", "1630559", "1629028", "1629637", "1628379") |
        stringr::str_detect(.data$player_name, "Luka|Doncic|Dončić|LeBron|Austin Reaves|Ayton|Jaxson Hayes|Luke Kennard")
    ) %>%
    dplyr::select(
      "player_id",
      "player_name",
      "team_abbreviation",
      "touch_share",
      "front_court_touch_share",
      "points_per_touch",
      "avg_sec_per_touch",
      "avg_drib_per_touch",
      "secondary_assists",
      "possessions",
      "touches",
      "front_court_touches",
      "creation_signal_available"
    )
)

message("Top players by observed touch share:")
print(
  player_creation_signals %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "touch_share",
      "front_court_touch_share",
      "points_per_touch",
      "avg_sec_per_touch",
      "avg_drib_per_touch",
      "secondary_assists",
      "possessions"
    ) %>%
    utils::head(20)
)

message("Saved player creation opportunity signals to: ", player_creation_signals_path)
message("Phase 20b note: descriptive creation opportunity signals only. No previous phases were modified.")
