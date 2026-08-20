# ============================================================
# 27_build_physical_attributes.R
# Phase 27: Physical Attribute Engine.
#
# Purpose:
# Create a standalone physical attribute classification system that describes
# what kind of physical basketball body a player is.
#
# Physical Attributes answer:
# "What kind of physical basketball body is this?"
#
# They do NOT answer:
# - how good the player is
# - whether the player can shoot, pass, create, defend, or read the floor
# - offensive role, defensive role, archetype, ATK, DEF, CR, or skill effects
#
# Power does NOT mean better.
# Finesse does NOT mean weaker.
# These are descriptive physical styles, not rankings.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_master_path <- glue("data/processed/player/player_master_{season}.parquet")
physical_measurements_path <- "outputs/physical/player_physical_measurements.parquet"
physical_attributes_path <- "outputs/player_physical_attributes.parquet"

if (!file.exists(physical_measurements_path)) {
  stop(
    "Missing physical measurement input: ",
    physical_measurements_path,
    ". Run 27a_audit_physical_measurement_sources.R first.",
    call. = FALSE
  )
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

coalesce_character_cols <- function(df, candidates) {
  matches <- candidates[candidates %in% names(df)]

  if (length(matches) == 0) {
    return(rep(NA_character_, nrow(df)))
  }

  values <- lapply(matches, function(col) as.character(df[[col]]))
  Reduce(dplyr::coalesce, values)
}

as_logical_measurement_flag <- function(x) {
  x_chr <- stringr::str_to_lower(as.character(x))

  dplyr::case_when(
    x_chr %in% c("true", "t", "1", "yes", "y") ~ TRUE,
    x_chr %in% c("false", "f", "0", "no", "n") ~ FALSE,
    TRUE ~ NA
  )
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

infer_body_class <- function(height, wingspan, standing_reach, fallback_position) {
  pos <- stringr::str_to_upper(as.character(fallback_position))
  measurement_count <- rowSums(cbind(!is.na(height), !is.na(wingspan), !is.na(standing_reach)))

  # Body class is measurement-first. Position is used only when no useful body
  # measurement exists, because Phase 27 describes body template, not role.
  dplyr::case_when(
    !is.na(standing_reach) & standing_reach >= 108 ~ "Big",
    !is.na(height) & height >= 82 ~ "Big",
    !is.na(height) & !is.na(wingspan) & height >= 80 & wingspan >= 86 ~ "Big",
    !is.na(height) & height <= 77.5 & (is.na(standing_reach) | standing_reach <= 101) ~ "Guard",
    !is.na(height) & height <= 76.5 ~ "Guard",
    measurement_count > 0 ~ "Wing",
    measurement_count == 0 & !is.na(pos) & stringr::str_detect(pos, "C") ~ "Big",
    measurement_count == 0 & !is.na(pos) & stringr::str_detect(pos, "G") & !stringr::str_detect(pos, "F|C") ~ "Guard",
    measurement_count == 0 & !is.na(pos) & stringr::str_detect(pos, "F") ~ "Wing",
    TRUE ~ NA_character_
  )
}

expected_weight_by_body <- function(body_class, height) {
  dplyr::case_when(
    body_class == "Guard" ~ 195 + dplyr::coalesce(height - 75, 0) * 4,
    body_class == "Wing" ~ 220 + dplyr::coalesce(height - 79, 0) * 5,
    body_class == "Big" ~ 245 + dplyr::coalesce(height - 83, 0) * 6,
    TRUE ~ NA_real_
  )
}

infer_physical_modifier <- function(
  body_class,
  power_score,
  finesse_score
) {
  dplyr::case_when(
    is.na(body_class) ~ NA_character_,
    !is.na(power_score) & !is.na(finesse_score) & power_score >= finesse_score + 5 ~ "Power",
    !is.na(power_score) & is.na(finesse_score) & power_score >= 35 ~ "Power",
    !is.na(finesse_score) & is.na(power_score) & finesse_score >= 25 ~ "Finesse",
    !is.na(power_score) & !is.na(finesse_score) ~ "Finesse",
    TRUE ~ "Finesse"
  )
}

attribute_confidence_from_inputs <- function(
  height,
  weight,
  wingspan,
  standing_reach,
  databallr_measurement_found,
  draft_combine_measurement_found,
  body_class,
  physical_modifier
) {
  measurement_count <- rowSums(cbind(!is.na(height), !is.na(weight), !is.na(wingspan), !is.na(standing_reach)))
  source_count <- rowSums(cbind(
    dplyr::coalesce(databallr_measurement_found, FALSE),
    dplyr::coalesce(draft_combine_measurement_found, FALSE)
  ))

  dplyr::case_when(
    is.na(body_class) | is.na(physical_modifier) ~ "unavailable",
    measurement_count >= 4 | (measurement_count >= 3 & source_count >= 1) ~ "high",
    measurement_count >= 2 ~ "medium",
    measurement_count == 1 ~ "low",
    TRUE ~ "unavailable"
  )
}

player_master <- if (file.exists(player_master_path)) {
  read_project_parquet(player_master_path) %>%
    janitor::clean_names() %>%
    normalize_missing_strings() %>%
    add_missing_cols(c("player_id", "player_name", "nickname", "player_nickname", "position"), NA) %>%
    dplyr::mutate(
      player_id = as.character(.data$player_id),
      player_name_master = as.character(.data$player_name),
      player_nickname = coalesce_character_cols(., c("player_nickname", "nickname")),
      master_position = as.character(.data$position)
    ) %>%
    dplyr::select("player_id", "player_name_master", "player_nickname", "master_position") %>%
    dplyr::filter(!is.na(.data$player_id)) %>%
    dplyr::distinct(.data$player_id, .keep_all = TRUE)
} else {
  tibble::tibble(
    player_id = character(),
    player_name_master = character(),
    player_nickname = character(),
    master_position = character()
  )
}

physical_measurements <- read_project_parquet(physical_measurements_path) %>%
  janitor::clean_names() %>%
  normalize_missing_strings() %>%
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
    "three_quarter_sprint",
    "bench_press",
    "databallr_pos2",
    "databallr_primary_pos",
    "databallr_measurement_found",
    "draft_combine_measurement_found"
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
    three_quarter_sprint = safe_numeric(.data$three_quarter_sprint),
    bench_press = safe_numeric(.data$bench_press),
    databallr_pos2 = as.character(.data$databallr_pos2),
    databallr_primary_pos = as.character(.data$databallr_primary_pos),
    databallr_measurement_found = as_logical_measurement_flag(.data$databallr_measurement_found),
    draft_combine_measurement_found = as_logical_measurement_flag(.data$draft_combine_measurement_found)
  )

validate_columns(physical_measurements, c("player_id", "player_name_measurement"))

player_physical_attributes <- physical_measurements %>%
  dplyr::left_join(player_master, by = "player_id") %>%
  dplyr::mutate(
    player_name = dplyr::coalesce(.data$player_name_master, .data$player_name_measurement),
    height = dplyr::coalesce(.data$height, .data$databallr_height_wo_shoes),
    fallback_position = dplyr::coalesce(.data$databallr_primary_pos, .data$databallr_pos2, .data$master_position),
    body_class = infer_body_class(
      .data$height,
      .data$wingspan,
      .data$standing_reach,
      .data$fallback_position
    )
  ) %>%
  dplyr::group_by(.data$body_class) %>%
  dplyr::mutate(
    body_class_weight_percentile = dplyr::if_else(
      !is.na(.data$weight) & sum(!is.na(.data$weight)) > 1,
      dplyr::percent_rank(.data$weight),
      NA_real_
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    bmi_proxy = dplyr::if_else(
      !is.na(.data$height) & .data$height > 0 & !is.na(.data$weight),
      703 * .data$weight / (.data$height^2),
      NA_real_
    ),
    expected_body_weight = expected_weight_by_body(.data$body_class, .data$height),
    body_class_weight_delta = .data$weight - .data$expected_body_weight,
    length_delta = dplyr::coalesce(.data$height_wingspan_diff, .data$wingspan - .data$height),
    reach_bonus = dplyr::case_when(
      .data$body_class == "Guard" ~ .data$standing_reach - 96,
      .data$body_class == "Wing" ~ .data$standing_reach - 102,
      .data$body_class == "Big" ~ .data$standing_reach - 108,
      TRUE ~ NA_real_
    ),
    vertical_signal = dplyr::coalesce(.data$max_vertical_leap, .data$standing_vertical_leap),
    high_mass_threshold_hit = dplyr::case_when(
      .data$body_class == "Big" & !is.na(.data$weight) & .data$weight >= 245 ~ TRUE,
      .data$body_class == "Wing" & !is.na(.data$weight) & .data$weight >= 220 ~ TRUE,
      .data$body_class == "Guard" & !is.na(.data$weight) & .data$weight >= 205 ~ TRUE,
      TRUE ~ FALSE
    ),
    low_mass_threshold_hit = dplyr::case_when(
      .data$body_class == "Big" & !is.na(.data$weight) & .data$weight < 240 ~ TRUE,
      .data$body_class == "Wing" & !is.na(.data$weight) & .data$weight < 210 ~ TRUE,
      .data$body_class == "Guard" & !is.na(.data$weight) & .data$weight < 190 ~ TRUE,
      TRUE ~ FALSE
    ),
    high_bmi_threshold_hit = dplyr::case_when(
      .data$body_class == "Big" & !is.na(.data$bmi_proxy) & .data$bmi_proxy >= 25.0 ~ TRUE,
      .data$body_class == "Wing" & !is.na(.data$bmi_proxy) & .data$bmi_proxy >= 24.8 ~ TRUE,
      .data$body_class == "Guard" & !is.na(.data$bmi_proxy) & .data$bmi_proxy >= 25.0 ~ TRUE,
      TRUE ~ FALSE
    ),
    low_bmi_threshold_hit = dplyr::case_when(
      .data$body_class == "Big" & !is.na(.data$bmi_proxy) & .data$bmi_proxy < 24.0 ~ TRUE,
      .data$body_class == "Wing" & !is.na(.data$bmi_proxy) & .data$bmi_proxy < 23.5 ~ TRUE,
      .data$body_class == "Guard" & !is.na(.data$bmi_proxy) & .data$bmi_proxy < 23.8 ~ TRUE,
      TRUE ~ FALSE
    ),
    power_score =
      dplyr::if_else(.data$high_mass_threshold_hit, 40, 0) +
      dplyr::if_else(!is.na(.data$body_class_weight_percentile) & .data$body_class_weight_percentile >= 0.60, 25, 0) +
      dplyr::if_else(.data$high_bmi_threshold_hit, 20, 0) +
      dplyr::if_else(!is.na(.data$body_class_weight_delta) & .data$body_class_weight_delta >= 8, 15, 0) +
      dplyr::if_else(!is.na(.data$bench_press) & .data$bench_press >= 10, 8, 0) +
      dplyr::if_else(!is.na(.data$body_fat) & .data$body_fat >= 9 & !is.na(.data$body_class_weight_delta) & .data$body_class_weight_delta >= 4, 6, 0) +
      dplyr::if_else(!is.na(.data$vertical_signal) & .data$vertical_signal >= 38 & (.data$length_delta < 5 | is.na(.data$length_delta)), 5, 0),
    finesse_score =
      dplyr::if_else(.data$low_mass_threshold_hit, 35, 0) +
      dplyr::if_else(!is.na(.data$body_class_weight_percentile) & .data$body_class_weight_percentile <= 0.35, 20, 0) +
      dplyr::if_else(.data$low_bmi_threshold_hit, 15, 0) +
      dplyr::if_else(!is.na(.data$body_class_weight_delta) & .data$body_class_weight_delta <= -10, 15, 0) +
      dplyr::if_else(!is.na(.data$length_delta) & .data$length_delta >= 5 & !.data$high_mass_threshold_hit, 10, 0) +
      dplyr::if_else(!is.na(.data$reach_bonus) & .data$reach_bonus >= 3 & !.data$high_mass_threshold_hit, 8, 0),
    physical_modifier = infer_physical_modifier(
      .data$body_class,
      .data$power_score,
      .data$finesse_score
    ),
    physical_attribute = dplyr::if_else(
      !is.na(.data$physical_modifier) & !is.na(.data$body_class),
      paste(.data$physical_modifier, .data$body_class),
      NA_character_
    ),
    attribute_confidence = attribute_confidence_from_inputs(
      .data$height,
      .data$weight,
      .data$wingspan,
      .data$standing_reach,
      .data$databallr_measurement_found,
      .data$draft_combine_measurement_found,
      .data$body_class,
      .data$physical_modifier
    ),
    attribute_note = dplyr::case_when(
      .data$attribute_confidence == "unavailable" ~ paste(
        "Physical attribute unavailable because consolidated measurement data lacks enough body measurements.",
        "No skill, role, ATK, DEF, CR, shooting, passing, or IQ assumptions were used."
      ),
      TRUE ~ paste(
        "Heuristic body-only classification from consolidated height, weight, wingspan, standing reach, and optional combine athletic testing.",
        "Finesse versus Power is weighted primarily toward mass-for-size, BMI proxy, and body-class weight percentile; length is secondary context.",
        "DataBallR position labels are retained for diagnostics/fallback only and are not the main classifier when measurements are available.",
        "Power and Finesse are descriptive physical styles, not quality rankings.",
        "No shooting, passing, creation, defense, IQ, role, ATK, DEF, or CR inputs were used."
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
    "databallr_height_wo_shoes",
    "height_wingspan_diff",
    "bmi_proxy",
    "body_class_weight_percentile",
    "power_score",
    "finesse_score",
    "body_fat",
    "max_vertical_leap",
    "standing_vertical_leap",
    "lane_agility_time",
    "three_quarter_sprint",
    "bench_press",
    "databallr_measurement_found",
    "draft_combine_measurement_found",
    "body_class",
    "physical_modifier",
    "physical_attribute",
    "attribute_confidence",
    "attribute_note",
    tidyselect::any_of(c("player_nickname", "databallr_pos2", "databallr_primary_pos", "master_position"))
  ) %>%
  dplyr::arrange(.data$body_class, .data$physical_modifier, .data$player_name)

write_project_parquet(player_physical_attributes, physical_attributes_path)

message("Phase 27 physical attribute diagnostics:")

message("Requested player examples:")
print(
  player_physical_attributes %>%
    dplyr::filter(stringr::str_detect(
      .data$player_name,
      "LeBron James|Luka Don|Doncic|Dončić|Austin Reaves|Jaxson Hayes|Deandre Ayton|DeAndre Ayton|Victor Wembanyama|Rudy Gobert|Chet Holmgren|Jrue Holiday|Trae Young"
    )) %>%
    dplyr::select(
      "player_id",
      "player_name",
      "height",
      "weight",
      "databallr_height_wo_shoes",
      "wingspan",
      "height_wingspan_diff",
      "standing_reach",
      "bmi_proxy",
      "body_class_weight_percentile",
      "power_score",
      "finesse_score",
      "body_fat",
      "max_vertical_leap",
      "standing_vertical_leap",
      "lane_agility_time",
      "three_quarter_sprint",
      "bench_press",
      "databallr_measurement_found",
      "draft_combine_measurement_found",
      "body_class",
      "physical_modifier",
      "physical_attribute",
      "attribute_confidence",
      "attribute_note"
    ) %>%
    dplyr::arrange(.data$player_name)
)

message("Counts by body_class:")
print(
  player_physical_attributes %>%
    dplyr::count(.data$body_class, sort = TRUE)
)

message("Counts by physical_modifier:")
print(
  player_physical_attributes %>%
    dplyr::count(.data$physical_modifier, sort = TRUE)
)

message("Counts by physical_attribute:")
print(
  player_physical_attributes %>%
    dplyr::count(.data$physical_attribute, sort = TRUE)
)

message("Physical modifier score distribution:")
print(
  player_physical_attributes %>%
    dplyr::group_by(.data$body_class, .data$physical_modifier) %>%
    dplyr::summarise(
      players = dplyr::n(),
      median_bmi_proxy = stats::median(.data$bmi_proxy, na.rm = TRUE),
      median_weight_percentile = stats::median(.data$body_class_weight_percentile, na.rm = TRUE),
      median_power_score = stats::median(.data$power_score, na.rm = TRUE),
      median_finesse_score = stats::median(.data$finesse_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$body_class, .data$physical_modifier)
)

message("Players with highest reach:")
print(
  player_physical_attributes %>%
    dplyr::filter(!is.na(.data$standing_reach)) %>%
    dplyr::arrange(dplyr::desc(.data$standing_reach)) %>%
    dplyr::select("player_name", "standing_reach", "height", "weight", "physical_attribute", "power_score", "finesse_score", "attribute_confidence") %>%
    dplyr::slice_head(n = 20)
)

message("Players with highest weight:")
print(
  player_physical_attributes %>%
    dplyr::filter(!is.na(.data$weight)) %>%
    dplyr::arrange(dplyr::desc(.data$weight)) %>%
    dplyr::select("player_name", "weight", "height", "bmi_proxy", "body_class_weight_percentile", "physical_attribute", "attribute_confidence") %>%
    dplyr::slice_head(n = 20)
)

message("Players with highest wingspan:")
print(
  player_physical_attributes %>%
    dplyr::filter(!is.na(.data$wingspan)) %>%
    dplyr::arrange(dplyr::desc(.data$wingspan)) %>%
    dplyr::select("player_name", "wingspan", "height", "weight", "height_wingspan_diff", "physical_attribute", "power_score", "finesse_score", "attribute_confidence") %>%
    dplyr::slice_head(n = 20)
)

message("Low-confidence classifications:")
print(
  player_physical_attributes %>%
    dplyr::filter(.data$attribute_confidence %in% c("low", "unavailable")) %>%
    dplyr::select(
      "player_name",
      "height",
      "weight",
      "wingspan",
      "standing_reach",
      "bmi_proxy",
      "body_class_weight_percentile",
      "power_score",
      "finesse_score",
      "body_class",
      "physical_modifier",
      "physical_attribute",
      "attribute_confidence",
      "attribute_note"
    ) %>%
    dplyr::slice_head(n = 50)
)

message("Saved physical attributes to: ", physical_attributes_path)
message("Phase 27 note: physical body classification only. Finesse and Power are descriptive physical styles, not quality rankings.")
