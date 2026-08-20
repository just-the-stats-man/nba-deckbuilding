# ============================================================
# 17_build_shot_making_surplus.R
# Phase 17: Shot-making surplus metrics.
#
# These are descriptive offensive skill metrics. They estimate whether a player
# outperforms the shot difficulty profile visible in NBA Player Tracking Shots
# splits, and they are intended to feed a later ATK version.
#
# This phase does not modify previous phases and does not build ATK directly.
# The expected hit-rate model is deliberately transparent: it estimates baseline
# hit rates for observable context buckets, then asks how each player's observed
# hit rate compares with the hit rate expected from that context mix.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_shot_context_path <- "outputs/attacks/player_shot_context.parquet"
tracking_raw_dir <- "data/raw/tracking_shots"
player_shot_making_path <- "outputs/attacks/player_shot_making.parquet"

if (!file.exists(player_shot_context_path)) {
  stop("Missing shot-context input: ", player_shot_context_path, ". Run 15_build_player_shot_context.R first.", call. = FALSE)
}

if (!dir.exists(tracking_raw_dir)) {
  stop("Missing tracking shot raw directory: ", tracking_raw_dir, ". Run 14_audit_shot_context_data.R first.", call. = FALSE)
}

tracking_files <- fs::dir_ls(tracking_raw_dir, regexp = "\\.parquet$")

if (length(tracking_files) == 0) {
  stop("No tracking shot parquet files found in ", tracking_raw_dir, ". Run 14_audit_shot_context_data.R first.", call. = FALSE)
}

safe_divide <- function(num, den) {
  dplyr::if_else(!is.na(den) & den > 0, num / den, NA_real_)
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

z_score <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  valid <- !is.na(x)

  if (sum(valid) == 0) {
    return(out)
  }

  spread <- stats::sd(x[valid])

  if (is.na(spread) || spread == 0) {
    out[valid] <- 0
    return(out)
  }

  out[valid] <- (x[valid] - mean(x[valid])) / spread
  out
}

z_to_card_score <- function(z) {
  clamp_0_100(50 + 15 * z)
}

normalize_if_enough_players <- function(x, player_count, min_players = 30) {
  if (player_count < min_players) {
    return(rep(NA_real_, length(x)))
  }

  z_score(x)
}

normalize_rate <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  dplyr::if_else(!is.na(x) & x > 1, x / 100, x)
}

weighted_mean_or_na <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0

  if (sum(valid) == 0) {
    return(NA_real_)
  }

  stats::weighted.mean(x[valid], w[valid])
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
      "fgm",
      "fga",
      "fg_pct"
    ), NA_character_) %>%
    dplyr::mutate(
      source_file = basename(path),
      player_id = dplyr::coalesce(as.character(.data$player_id), as.character(.data$requested_player_id)),
      table_name = as.character(.data$table_name),
      fgm = suppressWarnings(as.numeric(.data$fgm)),
      fga = suppressWarnings(as.numeric(.data$fga)),
      fg_pct = normalize_rate(.data$fg_pct),
      row_hit_rate = dplyr::coalesce(.data$fg_pct, safe_divide(.data$fgm, .data$fga))
    )
}

tracking_raw <- purrr::map_dfr(tracking_files, read_tracking_file) %>%
  dplyr::filter(!is.na(.data$player_id))

if (nrow(tracking_raw) == 0) {
  stop("Tracking shot files were found, but no player_id/requested_player_id values were readable.", call. = FALSE)
}

player_shot_context <- read_project_parquet(player_shot_context_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  dplyr::mutate(player_id = as.character(.data$player_id))

validate_columns(player_shot_context, c(
  "player_id",
  "player_name",
  "player_nickname",
  "high_dribble_frequency",
  "long_touch_frequency",
  "late_clock_burden"
))

table_name_lower <- stringr::str_to_lower(tracking_raw$table_name)

# Prefer the standard ClosestDefenderShooting table when present so that the
# 10ft+ table does not double-count a parallel openness split.
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
    dimension = "closest_defender_distance",
    context_bucket = stringr::str_to_lower(as.character(.data$close_def_dist_range)),
    is_tight_defense = stringr::str_detect(.data$context_bucket, "0.*2|0-2|2.*4|2-4"),
    is_wide_open = stringr::str_detect(.data$context_bucket, "6\\+|6\\s*\\+|wide open")
  )

dribble_rows <- tracking_raw %>%
  dplyr::filter(stringr::str_detect(stringr::str_to_lower(.data$table_name), "dribble")) %>%
  add_missing_cols(c("dribble_range"), NA_character_) %>%
  dplyr::mutate(
    dimension = "dribbles",
    context_bucket = stringr::str_to_lower(as.character(.data$dribble_range))
  )

touch_rows <- tracking_raw %>%
  dplyr::filter(stringr::str_detect(stringr::str_to_lower(.data$table_name), "touch")) %>%
  add_missing_cols(c("touch_time_range"), NA_character_) %>%
  dplyr::mutate(
    dimension = "touch_time",
    context_bucket = stringr::str_to_lower(as.character(.data$touch_time_range))
  )

shot_clock_rows <- tracking_raw %>%
  dplyr::filter(stringr::str_detect(stringr::str_to_lower(.data$table_name), "shotclock")) %>%
  add_missing_cols(c("shot_clock_range"), NA_character_) %>%
  dplyr::mutate(
    dimension = "shot_clock",
    context_bucket = stringr::str_to_lower(as.character(.data$shot_clock_range))
  )

context_rows <- dplyr::bind_rows(
  closest_defender_rows,
  dribble_rows,
  touch_rows,
  shot_clock_rows
) %>%
  dplyr::filter(!is.na(.data$context_bucket), !is.na(.data$fga), .data$fga > 0)

if (nrow(context_rows) == 0) {
  stop("No usable closest defender, dribble, touch time, or shot clock rows were found in tracking shot data.", call. = FALSE)
}

# Expected hit rate is estimated from bucket-level observed rates across the
# available tracking-shot pull. This is a prototype context expectation, not a
# league-wide model yet. Future versions should use full-league tracking data,
# add shot zone/type controls, and shrink noisy buckets.
bucket_expectations <- context_rows %>%
  dplyr::group_by(.data$dimension, .data$context_bucket) %>%
  dplyr::summarise(
    bucket_fgm = sum(.data$fgm, na.rm = TRUE),
    bucket_fga = sum(.data$fga, na.rm = TRUE),
    expected_bucket_hit_rate = safe_divide(.data$bucket_fgm, .data$bucket_fga),
    .groups = "drop"
  )

player_dimension_expectations <- context_rows %>%
  dplyr::left_join(bucket_expectations, by = c("dimension", "context_bucket")) %>%
  dplyr::group_by(.data$player_id, .data$dimension) %>%
  dplyr::summarise(
    dimension_fga = sum(.data$fga, na.rm = TRUE),
    expected_dimension_hit_rate = weighted_mean_or_na(.data$expected_bucket_hit_rate, .data$fga),
    observed_dimension_hit_rate = safe_divide(sum(.data$fgm, na.rm = TRUE), sum(.data$fga, na.rm = TRUE)),
    .groups = "drop"
  )

expected_hit_rates <- player_dimension_expectations %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    expected_hit_rate = weighted_mean_or_na(.data$expected_dimension_hit_rate, .data$dimension_fga),
    context_dimensions_available = sum(!is.na(.data$expected_dimension_hit_rate)),
    .groups = "drop"
  )

# Prefer Overall for observed hit rate when available because it is closest to a
# de-duplicated player-level tracking total. Fallbacks keep the phase usable if
# a future hoopR response omits the Overall table.
overall_hit_rates <- tracking_raw %>%
  dplyr::filter(stringr::str_detect(stringr::str_to_lower(.data$table_name), "^overall$|overall")) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    observed_hit_rate = safe_divide(sum(.data$fgm, na.rm = TRUE), sum(.data$fga, na.rm = TRUE)),
    observed_fga = sum(.data$fga, na.rm = TRUE),
    .groups = "drop"
  )

general_hit_rates <- tracking_raw %>%
  dplyr::filter(stringr::str_detect(stringr::str_to_lower(.data$table_name), "general")) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    general_observed_hit_rate = safe_divide(sum(.data$fgm, na.rm = TRUE), sum(.data$fga, na.rm = TRUE)),
    general_observed_fga = sum(.data$fga, na.rm = TRUE),
    .groups = "drop"
  )

context_hit_rates <- context_rows %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    context_observed_hit_rate = safe_divide(sum(.data$fgm, na.rm = TRUE), sum(.data$fga, na.rm = TRUE)),
    context_observed_fga = sum(.data$fga, na.rm = TRUE),
    .groups = "drop"
  )

tight_wide_rates <- closest_defender_rows %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    tight_defense_hit_rate = safe_divide(
      sum(.data$fgm[.data$is_tight_defense], na.rm = TRUE),
      sum(.data$fga[.data$is_tight_defense], na.rm = TRUE)
    ),
    wide_open_hit_rate = safe_divide(
      sum(.data$fgm[.data$is_wide_open], na.rm = TRUE),
      sum(.data$fga[.data$is_wide_open], na.rm = TRUE)
    ),
    tight_defense_fga = sum(.data$fga[.data$is_tight_defense], na.rm = TRUE),
    .groups = "drop"
  )

tight_bucket_expectation <- bucket_expectations %>%
  dplyr::filter(
    .data$dimension == "closest_defender_distance",
    stringr::str_detect(.data$context_bucket, "0.*2|0-2|2.*4|2-4")
  ) %>%
  dplyr::summarise(
    tight_expected_hit_rate = safe_divide(sum(.data$bucket_fgm, na.rm = TRUE), sum(.data$bucket_fga, na.rm = TRUE))
  ) %>%
  dplyr::pull(.data$tight_expected_hit_rate)

if (length(tight_bucket_expectation) == 0 || is.na(tight_bucket_expectation)) {
  tight_bucket_expectation <- NA_real_
}

player_shot_making_raw <- player_shot_context %>%
  dplyr::left_join(expected_hit_rates, by = "player_id") %>%
  dplyr::left_join(overall_hit_rates, by = "player_id") %>%
  dplyr::left_join(general_hit_rates, by = "player_id") %>%
  dplyr::left_join(context_hit_rates, by = "player_id") %>%
  dplyr::left_join(tight_wide_rates, by = "player_id") %>%
  dplyr::mutate(
    observed_hit_rate = dplyr::coalesce(
      .data$observed_hit_rate,
      .data$general_observed_hit_rate,
      .data$context_observed_hit_rate
    ),
    shot_making_surplus = .data$observed_hit_rate - .data$expected_hit_rate,
    contest_resistance_raw = .data$tight_defense_hit_rate - tight_bucket_expectation,
    creation_burden_raw = clamp_0_100(
      0.40 * dplyr::coalesce(.data$high_dribble_frequency, 0) +
        0.30 * dplyr::coalesce(.data$long_touch_frequency, 0) +
        0.30 * dplyr::coalesce(.data$late_clock_burden, 0)
    )
  )

shot_making_player_count <- player_shot_making_raw %>%
  dplyr::filter(!is.na(.data$shot_making_surplus)) %>%
  dplyr::summarise(players = dplyr::n_distinct(.data$player_id)) %>%
  dplyr::pull(.data$players)

if (length(shot_making_player_count) == 0 || is.na(shot_making_player_count)) {
  shot_making_player_count <- 0L
}

normalization_available <- shot_making_player_count >= 30

player_shot_making <- player_shot_making_raw %>%
  dplyr::mutate(
    # Keep raw descriptive fields for small pulls, but pause normalized card
    # components until the tracking-shot player pool is wide enough to support
    # stable z-score scaling.
    shot_making_surplus_z = normalize_if_enough_players(.data$shot_making_surplus, shot_making_player_count),
    contest_resistance_z = normalize_if_enough_players(.data$contest_resistance_raw, shot_making_player_count),
    creation_burden_z = normalize_if_enough_players(.data$creation_burden_raw, shot_making_player_count),
    shot_making_component = z_to_card_score(.data$shot_making_surplus_z),
    contest_resistance_score = z_to_card_score(.data$contest_resistance_z),
    creation_burden_score = z_to_card_score(.data$creation_burden_z)
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "expected_hit_rate",
    "observed_hit_rate",
    "shot_making_surplus",
    "tight_defense_hit_rate",
    "wide_open_hit_rate",
    "contest_resistance_raw",
    "creation_burden_raw",
    "shot_making_surplus_z",
    "contest_resistance_z",
    "creation_burden_z",
    "shot_making_component",
    "contest_resistance_score",
    "creation_burden_score"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$shot_making_surplus), .data$player_name)

write_project_parquet(player_shot_making, player_shot_making_path)

message("Phase 17 shot-making surplus diagnostics:")

message("Shot-making player count: ", shot_making_player_count)

if (!normalization_available) {
  warning("Small sample normalization warning", call. = FALSE)
  message("Normalized shot-making components are paused until at least 30 players have shot-making data.")
}

message("Top shot-making surplus players:")
print(
  player_shot_making %>%
    dplyr::arrange(dplyr::desc(.data$shot_making_surplus)) %>%
    utils::head(20)
)

message("Luka / LeBron / Austin examples, if available:")
print(
  player_shot_making %>%
    dplyr::filter(stringr::str_detect(.data$player_name, "Luka|Doncic|DonÄiÄ‡|LeBron|Austin Reaves")) %>%
    dplyr::select(
      "player_name",
      "expected_hit_rate",
      "observed_hit_rate",
      "shot_making_surplus",
      "tight_defense_hit_rate",
      "wide_open_hit_rate",
      "shot_making_component",
      "contest_resistance_score",
      "creation_burden_score"
    )
)

message("Raw values, z scores, and scaled values:")
print(
  player_shot_making %>%
    dplyr::select(
      "player_name",
      "shot_making_surplus",
      "shot_making_surplus_z",
      "shot_making_component",
      "contest_resistance_raw",
      "contest_resistance_z",
      "contest_resistance_score",
      "creation_burden_raw",
      "creation_burden_z",
      "creation_burden_score"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$shot_making_component))
)

message("Score distributions:")
print(
  player_shot_making %>%
    dplyr::summarise(
      players = dplyr::n(),
      median_expected_hit_rate = stats::median(.data$expected_hit_rate, na.rm = TRUE),
      median_observed_hit_rate = stats::median(.data$observed_hit_rate, na.rm = TRUE),
      median_shot_making_surplus = stats::median(.data$shot_making_surplus, na.rm = TRUE),
      median_shot_making_surplus_z = stats::median(.data$shot_making_surplus_z, na.rm = TRUE),
      median_shot_making_component = stats::median(.data$shot_making_component, na.rm = TRUE),
      median_tight_defense_hit_rate = stats::median(.data$tight_defense_hit_rate, na.rm = TRUE),
      median_wide_open_hit_rate = stats::median(.data$wide_open_hit_rate, na.rm = TRUE),
      median_contest_resistance_raw = stats::median(.data$contest_resistance_raw, na.rm = TRUE),
      median_contest_resistance_z = stats::median(.data$contest_resistance_z, na.rm = TRUE),
      median_contest_resistance_score = stats::median(.data$contest_resistance_score, na.rm = TRUE),
      median_creation_burden_raw = stats::median(.data$creation_burden_raw, na.rm = TRUE),
      median_creation_burden_z = stats::median(.data$creation_burden_z, na.rm = TRUE),
      median_creation_burden_score = stats::median(.data$creation_burden_score, na.rm = TRUE),
      missing_expected_hit_rate = sum(is.na(.data$expected_hit_rate)),
      missing_observed_hit_rate = sum(is.na(.data$observed_hit_rate)),
      missing_tight_defense_hit_rate = sum(is.na(.data$tight_defense_hit_rate))
    )
)

message("Phase 17 note: descriptive offensive shot-making metrics only. These will later feed ATK, but this phase does not modify previous phases or build final ATK.")
