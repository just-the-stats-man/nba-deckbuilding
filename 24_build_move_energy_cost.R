# ============================================================
# 24_build_move_energy_cost.R
# Phase 24: Prototype move energy cost.
#
# Goal:
# Build a first move-level energy cost table using currently measurable
# movement, creation, contact, and explosive burden signals.
#
# Energy is possession effort cost / move stamina cost. It is not calories
# burned and not athleticism.
#
# Prototype formula:
# Move Energy Cost =
#   movement burden
# + creation burden
# + contact burden
# + explosive burden
#
# This phase does not modify previous phases and does not create final card
# stamina rules. It only establishes a transparent prototype output.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

energy_output_dir <- "outputs/energy"
player_move_energy_path <- file.path(energy_output_dir, "player_move_energy.parquet")

player_attack_library_path <- "outputs/attacks/player_attack_library.parquet"
player_tracking_creation_metrics_path <- "outputs/attacks/player_tracking_creation_metrics.parquet"
player_creation_signals_path <- "outputs/attacks/player_creation_signals.parquet"
player_base_path <- glue("data/raw/player/player_base_{season}.parquet")
player_misc_path <- glue("data/raw/player/player_misc_{season}.parquet")
player_usage_path <- glue("data/raw/player/player_usage_{season}.parquet")

fs::dir_create(energy_output_dir)

if (!file.exists(player_attack_library_path)) {
  stop(
    "Missing Phase 24 input: ",
    player_attack_library_path,
    ". Run 11_build_player_attacks.R first.",
    call. = FALSE
  )
}

safe_read_parquet <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(player_id = character()))
  }

  read_project_parquet(path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols() %>%
    dplyr::mutate(player_id = as.character(.data$player_id))
}

add_missing_cols <- function(df, cols, value = NA_real_) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

coalesce_numeric_cols <- function(df, candidates) {
  matches <- candidates[candidates %in% names(df)]

  if (length(matches) == 0) {
    return(rep(NA_real_, nrow(df)))
  }

  values <- lapply(matches, function(col) suppressWarnings(as.numeric(df[[col]])))
  Reduce(dplyr::coalesce, values)
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

safe_divide <- function(numerator, denominator) {
  numerator <- as.numeric(numerator)
  denominator <- as.numeric(denominator)

  out <- numerator / denominator
  out[is.na(numerator) | is.na(denominator) | denominator == 0] <- NA_real_
  out
}

row_mean_available <- function(...) {
  pieces <- list(...)
  mat <- do.call(cbind, lapply(pieces, as.numeric))
  out <- rowMeans(mat, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

energy_tier_for_score <- function(score) {
  dplyr::case_when(
    is.na(score) ~ NA_character_,
    score < 20 ~ "Very Low",
    score < 40 ~ "Low",
    score < 60 ~ "Moderate",
    score < 80 ~ "High",
    TRUE ~ "Very High"
  )
}

move_explosive_weight <- function(attack_variant) {
  dplyr::case_when(
    attack_variant %in% c("dunk", "alley oop") ~ 1.00,
    attack_variant %in% c("putback", "tip") ~ 0.90,
    attack_variant %in% c("driving layup", "layup") ~ 0.65,
    TRUE ~ 0.15
  )
}

move_contact_weight <- function(attack_family, attack_variant) {
  dplyr::case_when(
    attack_variant %in% c("driving layup", "dunk", "alley oop", "putback", "tip") ~ 1.00,
    attack_variant %in% c("layup", "hook", "floater") ~ 0.70,
    attack_family == "Rim Pressure" ~ 0.85,
    TRUE ~ 0.15
  )
}

attack_library <- read_project_parquet(player_attack_library_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c("attempts", "possessions", "usage_volume"), NA_real_) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    attack_family = as.character(.data$attack_family),
    attack_variant = stringr::str_to_lower(as.character(.data$attack_variant)),
    move_name = .data$attack_variant,
    attempts = suppressWarnings(as.numeric(.data$attempts)),
    possessions = suppressWarnings(as.numeric(.data$possessions)),
    usage_volume = suppressWarnings(as.numeric(.data$usage_volume))
  )

validate_columns(
  attack_library,
  c("player_id", "player_name", "team_abbreviation", "attack_family", "attack_variant", "attempts")
)

tracking_creation <- safe_read_parquet(player_tracking_creation_metrics_path) %>%
  add_missing_cols(c(
    "touches",
    "front_court_touches",
    "time_of_possession",
    "avg_sec_per_touch",
    "avg_drib_per_touch",
    "usage_percentage",
    "possessions",
    "assists",
    "secondary_assists",
    "potential_assists"
  ), NA_real_)

creation_signals <- safe_read_parquet(player_creation_signals_path) %>%
  add_missing_cols(c(
    "touch_share",
    "front_court_touch_share",
    "points_per_touch",
    "avg_sec_per_touch",
    "avg_drib_per_touch",
    "secondary_assists",
    "possessions",
    "touches"
  ), NA_real_) %>%
  dplyr::select(
    "player_id",
    creation_signal_touches = "touches",
    creation_signal_possessions = "possessions",
    creation_signal_avg_sec_per_touch = "avg_sec_per_touch",
    creation_signal_avg_drib_per_touch = "avg_drib_per_touch",
    creation_signal_secondary_assists = "secondary_assists",
    tidyselect::any_of(c("touch_share", "front_court_touch_share", "points_per_touch"))
  )

player_base <- safe_read_parquet(player_base_path)
player_misc <- safe_read_parquet(player_misc_path)
player_usage <- safe_read_parquet(player_usage_path)

base_metrics <- player_base %>%
  dplyr::transmute(
    player_id = as.character(.data$player_id),
    minutes_played = coalesce_numeric_cols(., c("min", "minutes", "minutes_played")),
    rebounds = coalesce_numeric_cols(., c("reb", "rebounds", "total_rebounds")),
    blocks = coalesce_numeric_cols(., c("blk", "blocks")),
    fouls_committed = coalesce_numeric_cols(., c("pf", "personal_fouls"))
  )

misc_metrics <- player_misc %>%
  dplyr::transmute(
    player_id = as.character(.data$player_id),
    fouls_drawn = coalesce_numeric_cols(., c("pfd", "personal_fouls_drawn", "fouls_drawn")),
    drives = coalesce_numeric_cols(., c("drives", "drive")),
    paint_touches = coalesce_numeric_cols(., c("paint_touches", "touches_paint")),
    post_touches = coalesce_numeric_cols(., c("post_touches", "touches_post"))
  )

usage_metrics <- player_usage %>%
  dplyr::transmute(
    player_id = as.character(.data$player_id),
    usage_percentage_raw = coalesce_numeric_cols(., c("usage_percentage", "usage_pct", "usg_pct", "usage"))
  )

player_workload <- tracking_creation %>%
  dplyr::select(
    "player_id",
    "touches",
    "front_court_touches",
    "time_of_possession",
    "avg_sec_per_touch",
    "avg_drib_per_touch",
    "usage_percentage",
    "possessions",
    "assists",
    "secondary_assists",
    "potential_assists"
  ) %>%
  dplyr::full_join(creation_signals, by = "player_id") %>%
  dplyr::full_join(base_metrics, by = "player_id") %>%
  dplyr::full_join(misc_metrics, by = "player_id") %>%
  dplyr::full_join(usage_metrics, by = "player_id") %>%
  dplyr::mutate(
    touches = dplyr::coalesce(.data$touches, .data$creation_signal_touches),
    possessions = dplyr::coalesce(.data$possessions, .data$creation_signal_possessions),
    avg_sec_per_touch = dplyr::coalesce(.data$avg_sec_per_touch, .data$creation_signal_avg_sec_per_touch),
    avg_drib_per_touch = dplyr::coalesce(.data$avg_drib_per_touch, .data$creation_signal_avg_drib_per_touch),
    secondary_assists = dplyr::coalesce(.data$secondary_assists, .data$creation_signal_secondary_assists),
    usage_percentage = dplyr::coalesce(.data$usage_percentage, .data$usage_percentage_raw),
    # Current saved tracking tables do not consistently expose distance/speed.
    # These remain NA until a wider movement endpoint is extracted.
    distance = NA_real_,
    speed = NA_real_
  ) %>%
  dplyr::select(
    "player_id",
    "distance",
    "speed",
    "minutes_played",
    "touches",
    "usage_percentage",
    "assists",
    "secondary_assists",
    "potential_assists",
    "avg_sec_per_touch",
    "avg_drib_per_touch",
    "possessions",
    "drives",
    "fouls_drawn",
    "paint_touches",
    "post_touches",
    "blocks",
    "rebounds",
    "fouls_committed"
  )

player_workload_components <- player_workload %>%
  dplyr::mutate(
    distance_component = scale_0_100(.data$distance),
    speed_component = scale_0_100(.data$speed),
    minutes_component = scale_0_100(.data$minutes_played),
    touches_component = scale_0_100(.data$touches),
    usage_component = scale_0_100(.data$usage_percentage),
    assists_component = scale_0_100(.data$assists),
    secondary_assists_component = scale_0_100(.data$secondary_assists),
    drives_component = scale_0_100(.data$drives),
    fouls_drawn_component = scale_0_100(.data$fouls_drawn),
    blocks_component = scale_0_100(.data$blocks),
    rebounds_component = scale_0_100(.data$rebounds),
    movement_burden_player_component = row_mean_available(
      .data$distance_component,
      .data$speed_component,
      .data$minutes_component
    ),
    creation_burden_player_component = row_mean_available(
      .data$touches_component,
      .data$usage_component,
      .data$assists_component,
      .data$secondary_assists_component
    ),
    contact_burden_player_component = row_mean_available(
      .data$drives_component,
      .data$fouls_drawn_component
    ),
    explosive_burden_player_component = row_mean_available(
      .data$blocks_component,
      .data$rebounds_component
    )
  )

player_move_totals <- attack_library %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    total_move_attempts = sum(.data$attempts, na.rm = TRUE),
    total_rim_attempts = sum(
      dplyr::if_else(
        .data$attack_variant %in% c("layup", "driving layup", "dunk", "alley oop", "putback", "tip"),
        .data$attempts,
        0
      ),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

player_move_energy <- attack_library %>%
  dplyr::left_join(player_move_totals, by = "player_id") %>%
  dplyr::left_join(player_workload_components, by = "player_id") %>%
  dplyr::mutate(
    move_attempt_share = safe_divide(.data$attempts, .data$total_move_attempts),
    move_rim_attempt_share = dplyr::if_else(
      .data$attack_variant %in% c("layup", "driving layup", "dunk", "alley oop", "putback", "tip"),
      safe_divide(.data$attempts, .data$total_rim_attempts),
      0
    ),
    move_contact_component = clamp_0_100(100 * sqrt(safe_divide(.data$attempts, .data$total_move_attempts))) *
      move_contact_weight(.data$attack_family, .data$attack_variant),
    move_explosive_component = clamp_0_100(100 * sqrt(safe_divide(.data$attempts, .data$total_move_attempts))) *
      move_explosive_weight(.data$attack_variant),
    contact_burden_component = row_mean_available(
      .data$contact_burden_player_component,
      .data$fouls_drawn_component,
      .data$drives_component,
      .data$move_contact_component
    ),
    explosive_burden_component = row_mean_available(
      .data$explosive_burden_player_component,
      .data$blocks_component,
      .data$rebounds_component,
      .data$move_explosive_component
    ),
    movement_burden_component = .data$movement_burden_player_component,
    creation_burden_component = .data$creation_burden_player_component,
    measurable_component_count =
      as.integer(!is.na(.data$movement_burden_component)) +
      as.integer(!is.na(.data$creation_burden_component)) +
      as.integer(!is.na(.data$contact_burden_component)) +
      as.integer(!is.na(.data$explosive_burden_component)),
    base_energy_cost = row_mean_available(
      .data$movement_burden_component,
      .data$creation_burden_component,
      .data$contact_burden_component,
      .data$explosive_burden_component
    ),
    base_energy_cost = clamp_0_100(.data$base_energy_cost),
    energy_tier = energy_tier_for_score(.data$base_energy_cost),
    energy_cost_note = dplyr::case_when(
      .data$measurable_component_count == 4 ~ "Prototype move energy cost uses all four burden families.",
      .data$measurable_component_count > 0 ~ "Prototype move energy cost uses available burden families only; missing signals remain unavailable.",
      TRUE ~ "No measurable energy burden signals available for this player-move row."
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "move_name",
    attack_family = "attack_family",
    attack_variant = "attack_variant",
    "attempts",
    "move_attempt_share",
    "base_energy_cost",
    "energy_tier",
    "movement_burden_component",
    "creation_burden_component",
    "contact_burden_component",
    "explosive_burden_component",
    "measurable_component_count",
    "distance_component",
    "speed_component",
    "minutes_component",
    "touches_component",
    "usage_component",
    "assists_component",
    "secondary_assists_component",
    "drives_component",
    "fouls_drawn_component",
    "move_contact_component",
    "blocks_component",
    "rebounds_component",
    "move_explosive_component",
    "distance",
    "speed",
    "minutes_played",
    "touches",
    "usage_percentage",
    "assists",
    "secondary_assists",
    "drives",
    "fouls_drawn",
    "blocks",
    "rebounds",
    "energy_cost_note"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$base_energy_cost), .data$player_name, .data$move_name)

write_project_parquet(player_move_energy, player_move_energy_path)

message("Phase 24 prototype move energy diagnostics:")

message("Energy tier distribution:")
print(
  player_move_energy %>%
    dplyr::count(.data$energy_tier, sort = TRUE)
)

message("Component availability:")
print(
  player_move_energy %>%
    dplyr::summarise(
      player_moves = dplyr::n(),
      movement_available = sum(!is.na(.data$movement_burden_component)),
      creation_available = sum(!is.na(.data$creation_burden_component)),
      contact_available = sum(!is.na(.data$contact_burden_component)),
      explosive_available = sum(!is.na(.data$explosive_burden_component)),
      median_base_energy_cost = stats::median(.data$base_energy_cost, na.rm = TRUE)
    )
)

message("Top high-energy moves:")
print(
  player_move_energy %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "move_name",
      "attempts",
      "base_energy_cost",
      "energy_tier",
      "movement_burden_component",
      "creation_burden_component",
      "contact_burden_component",
      "explosive_burden_component",
      "measurable_component_count"
    ) %>%
    utils::head(25)
)

message("Requested player examples, if available:")
print(
  player_move_energy %>%
    dplyr::filter(stringr::str_detect(.data$player_name, "Luka|Doncic|Dončić|LeBron|Stephen Curry|Giannis|Antetokounmpo|Gobert|Wembanyama")) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "move_name",
      "attempts",
      "base_energy_cost",
      "energy_tier",
      "movement_burden_component",
      "creation_burden_component",
      "contact_burden_component",
      "explosive_burden_component",
      "energy_cost_note"
    )
)

message("Saved prototype move energy costs to: ", player_move_energy_path)
message("Phase 24 note: prototype only. No previous phases were modified and no final stamina/card rules were built.")
