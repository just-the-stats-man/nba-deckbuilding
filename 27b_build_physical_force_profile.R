# ============================================================
# 27b_build_physical_force_profile.R
# Phase 27b: Build physical force profile.
#
# Goal:
# Refine the descriptive Finesse vs Power physical modifier with body-only
# force proxies:
# - mass
# - length
# - speed / acceleration / agility
# - explosiveness
# - strength
#
# Physical Attribute describes body-based basketball force, not skill or role.
# Power is not just weight, and Finesse does not mean weak. These labels are
# descriptive physical styles, not quality rankings.
#
# This phase does NOT overwrite Phase 27 and does not use ATK, DEF, CR,
# shooting, passing, role, IQ, or card stats.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

physical_measurements_path <- "outputs/physical/player_physical_measurements.parquet"
physical_attributes_path <- "outputs/player_physical_attributes.parquet"
energy_signal_audit_path <- "outputs/energy/energy_signal_audit.parquet"
physical_output_dir <- "outputs/physical"
physical_force_profile_path <- file.path(physical_output_dir, "player_physical_force_profile.parquet")

fs::dir_create(physical_output_dir)

if (!file.exists(physical_measurements_path)) {
  stop("Missing physical measurement input: ", physical_measurements_path, ". Run Phase 27a first.", call. = FALSE)
}

if (!file.exists(physical_attributes_path)) {
  stop("Missing physical attribute input: ", physical_attributes_path, ". Run Phase 27 first.", call. = FALSE)
}

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

normalize_missing_strings <- function(df) {
  df %>%
    dplyr::mutate(dplyr::across(
      where(is.character),
      ~ {
        x <- trimws(.x)
        x[x %in% c("", "--", "-", "NA", "N/A", "NULL", "NaN", "na", "n/a", "null")] <- NA_character_
        x
      }
    ))
}

safe_read_parquet <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(player_id = character()))
  }

  tryCatch(
    read_project_parquet(path) %>%
      janitor::clean_names() %>%
      normalize_missing_strings() %>%
      convert_numeric_cols(),
    error = function(e) {
      message("Could not read optional parquet: ", path, " | ", conditionMessage(e))
      tibble::tibble(player_id = character())
    }
  )
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

as_logical_measurement_flag <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  x_chr <- stringr::str_to_lower(as.character(x))

  dplyr::case_when(
    x_chr %in% c("true", "t", "1", "yes", "y") ~ TRUE,
    x_chr %in% c("false", "f", "0", "no", "n") ~ FALSE,
    TRUE ~ NA
  )
}

coalesce_numeric_cols <- function(df, candidates) {
  matches <- candidates[candidates %in% names(df)]

  if (length(matches) == 0) {
    return(rep(NA_real_, nrow(df)))
  }

  values <- lapply(matches, function(col) suppressWarnings(as.numeric(df[[col]])))
  Reduce(dplyr::coalesce, values)
}

percentile_0_100 <- function(x, reverse = FALSE) {
  x <- safe_numeric(x)
  out <- rep(NA_real_, length(x))
  valid <- !is.na(x)

  if (sum(valid) < 2) {
    return(out)
  }

  ranks <- dplyr::percent_rank(x[valid]) * 100

  if (reverse) {
    ranks <- 100 - ranks
  }

  out[valid] <- ranks
  out
}

clip_0_100 <- function(x) {
  pmax(0, pmin(100, safe_numeric(x)))
}

row_mean_available <- function(...) {
  mat <- do.call(cbind, lapply(list(...), safe_numeric))
  out <- rowMeans(mat, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

row_weighted_mean_available <- function(component_df, weight_named_vector) {
  if (nrow(component_df) == 0 || ncol(component_df) == 0) {
    return(numeric())
  }

  component_names <- names(weight_named_vector)
  component_names <- component_names[component_names %in% names(component_df)]

  if (length(component_names) == 0) {
    return(rep(NA_real_, nrow(component_df)))
  }

  mat <- as.matrix(component_df[component_names])
  mat <- apply(mat, 2, safe_numeric)
  weights <- as.numeric(weight_named_vector[component_names])
  out <- rep(NA_real_, nrow(component_df))

  for (i in seq_len(nrow(component_df))) {
    valid <- !is.na(mat[i, ])

    if (any(valid)) {
      out[i] <- sum(mat[i, valid] * weights[valid]) / sum(weights[valid])
    }
  }

  out
}

force_tier_from_score <- function(score) {
  dplyr::case_when(
    is.na(score) ~ "Unavailable",
    score >= 80 ~ "Very High",
    score >= 65 ~ "High",
    score >= 45 ~ "Moderate",
    score >= 25 ~ "Low",
    TRUE ~ "Very Low"
  )
}

force_modifier_from_components <- function(physical_force_score, mass_component, strength_component, acceleration_speed_component, explosiveness_component) {
  dplyr::case_when(
    is.na(physical_force_score) ~ NA_character_,
    !is.na(mass_component) & mass_component >= 70 ~ "Power",
    !is.na(strength_component) & strength_component >= 70 ~ "Power",
    !is.na(physical_force_score) & physical_force_score >= 62 ~ "Power",
    !is.na(acceleration_speed_component) & !is.na(explosiveness_component) &
      acceleration_speed_component >= 75 & explosiveness_component >= 75 & physical_force_score >= 55 ~ "Power",
    TRUE ~ "Finesse"
  )
}

find_player_id_col <- function(df) {
  candidates <- c("player_id", "person_id", "nba_player_id", "athlete_id", "requested_player_id")
  hit <- candidates[candidates %in% names(df)]

  if (length(hit) == 0) {
    return(NA_character_)
  }

  hit[[1]]
}

extract_tracking_speed_proxy <- function(paths) {
  out <- list()

  for (path in paths[file.exists(paths)]) {
    df <- safe_read_parquet(path)

    if (nrow(df) == 0) {
      next
    }

    player_id_col <- find_player_id_col(df)

    if (is.na(player_id_col)) {
      next
    }

    speed_cols <- names(df)[stringr::str_detect(names(df), "avg_speed|average_speed|speed")]
    distance_cols <- names(df)[stringr::str_detect(names(df), "distance|dist_miles|miles")]

    if (length(speed_cols) == 0 && length(distance_cols) == 0) {
      next
    }

    candidate <- df %>%
      dplyr::mutate(player_id = as.character(.data[[player_id_col]])) %>%
      dplyr::transmute(
        player_id = .data$player_id,
        tracking_speed_proxy_raw = coalesce_numeric_cols(., speed_cols),
        tracking_distance_proxy_raw = coalesce_numeric_cols(., distance_cols),
        tracking_movement_source = path
      ) %>%
      dplyr::filter(
        !is.na(.data$player_id),
        !is.na(.data$tracking_speed_proxy_raw) | !is.na(.data$tracking_distance_proxy_raw)
      )

    if (nrow(candidate) > 0) {
      out[[length(out) + 1]] <- candidate
    }
  }

  if (length(out) == 0) {
    return(tibble::tibble(
      player_id = character(),
      tracking_speed_proxy_raw = numeric(),
      tracking_distance_proxy_raw = numeric(),
      tracking_movement_source = character()
    ))
  }

  dplyr::bind_rows(out) %>%
    dplyr::group_by(.data$player_id) %>%
    dplyr::summarise(
      tracking_speed_proxy_raw = mean(.data$tracking_speed_proxy_raw, na.rm = TRUE),
      tracking_distance_proxy_raw = mean(.data$tracking_distance_proxy_raw, na.rm = TRUE),
      tracking_movement_source = paste(sort(unique(stats::na.omit(.data$tracking_movement_source))), collapse = " | "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      tracking_speed_proxy_raw = dplyr::if_else(is.nan(.data$tracking_speed_proxy_raw), NA_real_, .data$tracking_speed_proxy_raw),
      tracking_distance_proxy_raw = dplyr::if_else(is.nan(.data$tracking_distance_proxy_raw), NA_real_, .data$tracking_distance_proxy_raw)
    )
}

physical_measurements <- safe_read_parquet(physical_measurements_path) %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "height",
    "weight",
    "databallr_height_wo_shoes",
    "wingspan",
    "height_wingspan_diff",
    "standing_reach",
    "body_fat",
    "max_vertical_leap",
    "standing_vertical_leap",
    "lane_agility_time",
    "modified_lane_agility_time",
    "shuttle_run",
    "three_quarter_sprint",
    "bench_press",
    "databallr_measurement_found",
    "draft_combine_measurement_found",
    "measurement_source"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name_measurement = as.character(.data$player_name),
    height = safe_numeric(.data$height),
    weight = safe_numeric(.data$weight),
    databallr_height_wo_shoes = safe_numeric(.data$databallr_height_wo_shoes),
    wingspan = safe_numeric(.data$wingspan),
    height_wingspan_diff = safe_numeric(.data$height_wingspan_diff),
    standing_reach = safe_numeric(.data$standing_reach),
    body_fat = safe_numeric(.data$body_fat),
    max_vertical_leap = safe_numeric(.data$max_vertical_leap),
    standing_vertical_leap = safe_numeric(.data$standing_vertical_leap),
    lane_agility_time = safe_numeric(.data$lane_agility_time),
    modified_lane_agility_time = safe_numeric(.data$modified_lane_agility_time),
    shuttle_run = safe_numeric(.data$shuttle_run),
    three_quarter_sprint = safe_numeric(.data$three_quarter_sprint),
    bench_press = safe_numeric(.data$bench_press),
    databallr_measurement_found = as_logical_measurement_flag(.data$databallr_measurement_found),
    draft_combine_measurement_found = as_logical_measurement_flag(.data$draft_combine_measurement_found),
    measurement_source = as.character(.data$measurement_source)
  )

physical_attributes <- safe_read_parquet(physical_attributes_path) %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "height",
    "weight",
    "wingspan",
    "standing_reach",
    "bmi_proxy",
    "body_class_weight_percentile",
    "body_class",
    "physical_modifier",
    "physical_attribute"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name_attribute = as.character(.data$player_name),
    phase27_height = safe_numeric(.data$height),
    phase27_weight = safe_numeric(.data$weight),
    phase27_wingspan = safe_numeric(.data$wingspan),
    phase27_standing_reach = safe_numeric(.data$standing_reach),
    bmi_proxy = safe_numeric(.data$bmi_proxy),
    body_class_weight_percentile = safe_numeric(.data$body_class_weight_percentile),
    body_class = as.character(.data$body_class),
    existing_physical_modifier = as.character(.data$physical_modifier),
    physical_attribute = as.character(.data$physical_attribute)
  ) %>%
  dplyr::select(
    "player_id",
    "player_name_attribute",
    "phase27_height",
    "phase27_weight",
    "phase27_wingspan",
    "phase27_standing_reach",
    "bmi_proxy",
    "body_class_weight_percentile",
    "body_class",
    "existing_physical_modifier",
    "physical_attribute"
  )

validate_columns(physical_measurements, c("player_id", "player_name_measurement"))
validate_columns(physical_attributes, c("player_id", "body_class", "existing_physical_modifier"))

tracking_candidate_paths <- c(
  "outputs/attacks/player_tracking_creation_metrics.parquet",
  "outputs/attacks/player_creation_signals.parquet",
  list.files("data/raw/tracking_creation", pattern = "\\.parquet$", full.names = TRUE),
  list.files("data/raw/tracking_shots", pattern = "\\.parquet$", full.names = TRUE)
)

tracking_speed_proxy <- extract_tracking_speed_proxy(tracking_candidate_paths)

energy_signal_audit <- safe_read_parquet(energy_signal_audit_path) %>%
  add_missing_cols(c("signal_name", "available", "column_name", "source_endpoint"), NA)

energy_movement_availability_note <- if (nrow(energy_signal_audit) > 0) {
  available_movement <- energy_signal_audit %>%
    dplyr::filter(
      stringr::str_detect(as.character(.data$signal_name), "speed|distance|miles"),
      dplyr::coalesce(as_logical_measurement_flag(.data$available), FALSE)
    )

  if (nrow(available_movement) > 0) {
    paste("Energy audit found movement/speed candidate columns:", paste(sort(unique(stats::na.omit(available_movement$column_name))), collapse = "; "))
  } else {
    "Energy audit did not find reliable player-level movement/speed columns."
  }
} else {
  "Energy audit unavailable."
}

force_base <- physical_measurements %>%
  dplyr::left_join(physical_attributes, by = "player_id") %>%
  dplyr::left_join(tracking_speed_proxy, by = "player_id") %>%
  dplyr::mutate(
    player_name = dplyr::coalesce(.data$player_name_attribute, .data$player_name_measurement),
    height = dplyr::coalesce(.data$phase27_height, .data$height, .data$databallr_height_wo_shoes),
    weight = dplyr::coalesce(.data$phase27_weight, .data$weight),
    wingspan = dplyr::coalesce(.data$phase27_wingspan, .data$wingspan),
    standing_reach = dplyr::coalesce(.data$phase27_standing_reach, .data$standing_reach),
    height_wingspan_diff = dplyr::coalesce(.data$height_wingspan_diff, .data$wingspan - .data$height),
    bmi_proxy = dplyr::coalesce(
      .data$bmi_proxy,
      dplyr::if_else(!is.na(.data$height) & .data$height > 0 & !is.na(.data$weight), 703 * .data$weight / (.data$height^2), NA_real_)
    )
  ) %>%
  dplyr::group_by(.data$body_class) %>%
  dplyr::mutate(
    body_class_weight_percentile = dplyr::coalesce(
      .data$body_class_weight_percentile,
      dplyr::if_else(!is.na(.data$weight) & sum(!is.na(.data$weight)) > 1, dplyr::percent_rank(.data$weight), NA_real_)
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    weight_percentile_score = .data$body_class_weight_percentile * 100,
    bmi_percentile_score = percentile_0_100(.data$bmi_proxy),
    height_score = percentile_0_100(.data$height),
    wingspan_score = percentile_0_100(.data$wingspan),
    standing_reach_score = percentile_0_100(.data$standing_reach),
    height_wingspan_diff_score = percentile_0_100(.data$height_wingspan_diff),
    three_quarter_sprint_score = percentile_0_100(.data$three_quarter_sprint, reverse = TRUE),
    lane_agility_score = percentile_0_100(.data$lane_agility_time, reverse = TRUE),
    modified_lane_agility_score = percentile_0_100(.data$modified_lane_agility_time, reverse = TRUE),
    shuttle_run_score = percentile_0_100(.data$shuttle_run, reverse = TRUE),
    tracking_speed_score = percentile_0_100(.data$tracking_speed_proxy_raw),
    tracking_distance_score = percentile_0_100(.data$tracking_distance_proxy_raw),
    max_vertical_score = percentile_0_100(.data$max_vertical_leap),
    standing_vertical_score = percentile_0_100(.data$standing_vertical_leap),
    bench_press_score = percentile_0_100(.data$bench_press),
    mass_component = row_mean_available(.data$weight_percentile_score, .data$bmi_percentile_score),
    length_component = row_mean_available(
      .data$height_score,
      .data$wingspan_score,
      .data$standing_reach_score,
      .data$height_wingspan_diff_score
    ),
    combine_speed_component = row_mean_available(
      .data$three_quarter_sprint_score,
      .data$lane_agility_score,
      .data$modified_lane_agility_score,
      .data$shuttle_run_score
    ),
    tracking_movement_component = row_mean_available(.data$tracking_speed_score, .data$tracking_distance_score),
    acceleration_speed_component = dplyr::case_when(
      !is.na(.data$combine_speed_component) & !is.na(.data$tracking_movement_component) ~
        0.80 * .data$combine_speed_component + 0.20 * .data$tracking_movement_component,
      !is.na(.data$combine_speed_component) ~ .data$combine_speed_component,
      !is.na(.data$tracking_movement_component) ~ .data$tracking_movement_component,
      TRUE ~ NA_real_
    ),
    explosiveness_component = row_mean_available(.data$max_vertical_score, .data$standing_vertical_score),
    strength_component = dplyr::case_when(
      !is.na(.data$bench_press_score) & !is.na(.data$mass_component) ~ 0.70 * .data$bench_press_score + 0.30 * .data$mass_component,
      !is.na(.data$bench_press_score) ~ .data$bench_press_score,
      !is.na(.data$mass_component) ~ 0.75 * .data$mass_component,
      TRUE ~ NA_real_
    )
  )

component_weights <- c(
  mass_component = 0.30,
  acceleration_speed_component = 0.20,
  explosiveness_component = 0.20,
  strength_component = 0.25,
  length_component = 0.05
)

player_physical_force_profile <- force_base %>%
  dplyr::mutate(
    physical_force_score = clip_0_100(row_weighted_mean_available(
      dplyr::select(
        .,
        "mass_component",
        "acceleration_speed_component",
        "explosiveness_component",
        "strength_component",
        "length_component"
      ),
      component_weights
    )),
    physical_force_tier = force_tier_from_score(.data$physical_force_score),
    speed_agility_available = !is.na(.data$acceleration_speed_component),
    explosiveness_available = !is.na(.data$explosiveness_component),
    strength_direct_available = !is.na(.data$bench_press),
    tracking_movement_available = !is.na(.data$tracking_movement_component),
    force_component_count = rowSums(cbind(
      !is.na(.data$mass_component),
      !is.na(.data$length_component),
      !is.na(.data$acceleration_speed_component),
      !is.na(.data$explosiveness_component),
      !is.na(.data$strength_component)
    )),
    force_profile_confidence = dplyr::case_when(
      is.na(.data$physical_force_score) ~ "unavailable",
      .data$force_component_count >= 5 ~ "high",
      .data$force_component_count >= 3 & (.data$speed_agility_available | .data$explosiveness_available) ~ "medium",
      .data$force_component_count >= 2 ~ "low",
      TRUE ~ "very_low"
    ),
    force_refined_physical_modifier = force_modifier_from_components(
      .data$physical_force_score,
      .data$mass_component,
      .data$strength_component,
      .data$acceleration_speed_component,
      .data$explosiveness_component
    ),
    force_modifier_changed_from_phase27 = !is.na(.data$existing_physical_modifier) &
      !is.na(.data$force_refined_physical_modifier) &
      .data$existing_physical_modifier != .data$force_refined_physical_modifier,
    force_profile_note = dplyr::case_when(
      .data$force_profile_confidence %in% c("high", "medium") ~ paste(
        "Physical force profile uses body-only mass, length, speed/agility, explosiveness, and strength proxies.",
        "Power and Finesse are descriptive physical styles, not quality rankings.",
        energy_movement_availability_note
      ),
      TRUE ~ paste(
        "Physical force profile is provisional because speed/agility, explosiveness, or direct strength data are limited.",
        "Fallback strength uses mass/BMI proxy when bench press is missing.",
        "No ATK, DEF, CR, shooting, passing, role, IQ, or card stats were used.",
        energy_movement_availability_note
      )
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "height",
    "weight",
    "wingspan",
    "standing_reach",
    "height_wingspan_diff",
    "bmi_proxy",
    "body_class_weight_percentile",
    "body_class",
    "physical_attribute",
    "existing_physical_modifier",
    "force_refined_physical_modifier",
    "force_modifier_changed_from_phase27",
    "mass_component",
    "length_component",
    "acceleration_speed_component",
    "explosiveness_component",
    "strength_component",
    "physical_force_score",
    "physical_force_tier",
    "force_profile_confidence",
    "force_component_count",
    "speed_agility_available",
    "explosiveness_available",
    "strength_direct_available",
    "tracking_movement_available",
    "three_quarter_sprint",
    "lane_agility_time",
    "modified_lane_agility_time",
    "shuttle_run",
    "tracking_speed_proxy_raw",
    "tracking_distance_proxy_raw",
    "max_vertical_leap",
    "standing_vertical_leap",
    "bench_press",
    "body_fat",
    "databallr_measurement_found",
    "draft_combine_measurement_found",
    "tracking_movement_source",
    "measurement_source",
    "force_profile_note"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$physical_force_score), .data$player_name)

write_project_parquet(player_physical_force_profile, physical_force_profile_path)

message("Phase 27b physical force profile diagnostics:")

requested_player_pattern <- "Ja Morant|Anthony Edwards|LeBron James|Giannis Antetokounmpo|Zion Williamson|Luka Don|Doncic|Dončić|Austin Reaves|Chet Holmgren|Victor Wembanyama|Rudy Gobert|Deandre Ayton|DeAndre Ayton|Jaxson Hayes|Trae Young|Jrue Holiday"

message("Requested player examples:")
print(
  player_physical_force_profile %>%
    dplyr::filter(stringr::str_detect(.data$player_name, requested_player_pattern)) %>%
    dplyr::select(
      "player_name",
      "body_class",
      "existing_physical_modifier",
      "force_refined_physical_modifier",
      "physical_force_score",
      "physical_force_tier",
      "mass_component",
      "acceleration_speed_component",
      "explosiveness_component",
      "strength_component",
      "length_component",
      "force_profile_confidence",
      "force_modifier_changed_from_phase27"
    ) %>%
    dplyr::arrange(.data$player_name)
)

message("Highest physical_force_score players:")
print(
  player_physical_force_profile %>%
    dplyr::filter(!is.na(.data$physical_force_score)) %>%
    dplyr::arrange(dplyr::desc(.data$physical_force_score)) %>%
    dplyr::select(
      "player_name",
      "body_class",
      "existing_physical_modifier",
      "force_refined_physical_modifier",
      "physical_force_score",
      "physical_force_tier",
      "mass_component",
      "strength_component",
      "acceleration_speed_component",
      "explosiveness_component"
    ) %>%
    dplyr::slice_head(n = 25)
)

message("Lowest physical_force_score players:")
print(
  player_physical_force_profile %>%
    dplyr::filter(!is.na(.data$physical_force_score)) %>%
    dplyr::arrange(.data$physical_force_score) %>%
    dplyr::select(
      "player_name",
      "body_class",
      "existing_physical_modifier",
      "force_refined_physical_modifier",
      "physical_force_score",
      "physical_force_tier",
      "mass_component",
      "strength_component",
      "acceleration_speed_component",
      "explosiveness_component"
    ) %>%
    dplyr::slice_head(n = 25)
)

message("Players whose force-refined modifier changes from Phase 27:")
print(
  player_physical_force_profile %>%
    dplyr::filter(.data$force_modifier_changed_from_phase27) %>%
    dplyr::arrange(dplyr::desc(.data$physical_force_score)) %>%
    dplyr::select(
      "player_name",
      "body_class",
      "existing_physical_modifier",
      "force_refined_physical_modifier",
      "physical_force_score",
      "mass_component",
      "strength_component",
      "acceleration_speed_component",
      "explosiveness_component",
      "force_profile_confidence"
    ) %>%
    dplyr::slice_head(n = 50)
)

message("Missingness for speed/agility/explosiveness fields:")
print(
  player_physical_force_profile %>%
    dplyr::summarise(
      players = dplyr::n(),
      three_quarter_sprint_missing_pct = 100 * mean(is.na(.data$three_quarter_sprint)),
      lane_agility_time_missing_pct = 100 * mean(is.na(.data$lane_agility_time)),
      modified_lane_agility_time_missing_pct = 100 * mean(is.na(.data$modified_lane_agility_time)),
      shuttle_run_missing_pct = 100 * mean(is.na(.data$shuttle_run)),
      acceleration_speed_component_missing_pct = 100 * mean(is.na(.data$acceleration_speed_component)),
      max_vertical_leap_missing_pct = 100 * mean(is.na(.data$max_vertical_leap)),
      standing_vertical_leap_missing_pct = 100 * mean(is.na(.data$standing_vertical_leap)),
      explosiveness_component_missing_pct = 100 * mean(is.na(.data$explosiveness_component)),
      bench_press_missing_pct = 100 * mean(is.na(.data$bench_press)),
      .groups = "drop"
    )
)

message("Saved physical force profile to: ", physical_force_profile_path)
message("Phase 27b note: physical force profile only. No skill, role, ATK, DEF, CR, or card-stat inputs were used.")
