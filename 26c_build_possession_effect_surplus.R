# ============================================================
# 26c_build_possession_effect_surplus.R
# Phase 26c: Normalize possession effects by physical attribute expectations.
#
# Goal:
# Prevent players from receiving skill credit for possession outcomes that are
# already expected from their body template.
#
# Skill Surplus = Observed Effect - Expected Effect
# Expected Effect = mean(effect | physical_attribute_group)
#
# Physical Attribute philosophy:
# - Physical Attribute describes body template only.
# - It does not describe playmaking, ball handling, floor general behavior,
#   offensive role, or defensive role.
# - Offense Type and Defense Type will own those role/skill dimensions later.
#
# Provisional structure:
# - Modifier: Finesse, Balanced, Power
# - Body Class: Guard, Wing, Big
#
# TODO:
# Future physical attributes may be learned from clustering height, weight,
# wingspan, standing reach, mobility, speed, explosiveness, and strength.
# Until then, these groups are heuristic-built and explicitly flagged.
#
# This phase does not overwrite Phase 26 or 26b and does not create ATK or DEF.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

effects_output_dir <- "outputs/effects"
refined_effects_path <- file.path(effects_output_dir, "player_possession_effects_refined.parquet")
effect_surplus_path <- file.path(effects_output_dir, "player_possession_effect_surplus.parquet")
player_master_path <- glue("data/processed/player/player_master_{season}.parquet")

fs::dir_create(effects_output_dir)

if (!file.exists(refined_effects_path)) {
  stop(
    "Missing refined possession effects input: ",
    refined_effects_path,
    ". Run 26b_refine_possession_effects.R first.",
    call. = FALSE
  )
}

if (!file.exists(player_master_path)) {
  stop("Missing player master input: ", player_master_path, ". Run 02_player_season.R first.", call. = FALSE)
}

add_missing_cols <- function(df, cols, value = NA) {
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

normalize_height_inches <- function(height_raw) {
  height_chr <- as.character(height_raw)
  height_num <- suppressWarnings(as.numeric(height_chr))

  feet_inches_match <- stringr::str_match(height_chr, "^(\\d+)\\s*[-']\\s*(\\d+)")
  parsed_feet_inches <- suppressWarnings(as.numeric(feet_inches_match[, 2]) * 12 + as.numeric(feet_inches_match[, 3]))

  dplyr::case_when(
    !is.na(height_num) & height_num > 90 ~ height_num / 2.54,
    !is.na(height_num) ~ height_num,
    !is.na(parsed_feet_inches) ~ parsed_feet_inches,
    TRUE ~ NA_real_
  )
}

infer_body_class <- function(height_inches, position) {
  position_chr <- stringr::str_to_upper(as.character(position))

  dplyr::case_when(
    !is.na(position_chr) & stringr::str_detect(position_chr, "C") ~ "Big",
    !is.na(position_chr) & stringr::str_detect(position_chr, "G") & !stringr::str_detect(position_chr, "F|C") ~ "Guard",
    !is.na(position_chr) & stringr::str_detect(position_chr, "F") ~ "Wing",
    !is.na(height_inches) & height_inches <= 77 ~ "Guard",
    !is.na(height_inches) & height_inches <= 81 ~ "Wing",
    !is.na(height_inches) ~ "Big",
    TRUE ~ NA_character_
  )
}

expected_weight_for_body_class <- function(body_class) {
  dplyr::case_when(
    body_class == "Guard" ~ 200,
    body_class == "Wing" ~ 225,
    body_class == "Big" ~ 250,
    TRUE ~ NA_real_
  )
}

infer_modifier <- function(weight, body_class) {
  expected_weight <- expected_weight_for_body_class(body_class)
  weight_delta <- as.numeric(weight) - expected_weight

  dplyr::case_when(
    is.na(weight_delta) ~ NA_character_,
    weight_delta <= -15 ~ "Finesse",
    weight_delta >= 15 ~ "Power",
    TRUE ~ "Balanced"
  )
}

target_effects <- c(
  "second-chance pressure",
  "possession termination",
  "turnover pressure",
  "offensive disruption"
)

player_master <- read_project_parquet(player_master_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(
    c(
      "player_id",
      "player_name",
      "nickname",
      "player_nickname",
      "height",
      "height_inches",
      "weight",
      "position"
    ),
    NA
  ) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_nickname = dplyr::coalesce(as.character(.data$player_nickname), as.character(.data$nickname)),
    height_value = dplyr::coalesce(
      coalesce_numeric_cols(., c("height_inches")),
      normalize_height_inches(.data$height)
    ),
    weight_value = coalesce_numeric_cols(., c("weight")),
    physical_attribute_body_class = infer_body_class(.data$height_value, .data$position),
    physical_attribute_modifier = infer_modifier(.data$weight_value, .data$physical_attribute_body_class),
    physical_attribute_group = dplyr::if_else(
      !is.na(.data$physical_attribute_modifier) & !is.na(.data$physical_attribute_body_class),
      paste(.data$physical_attribute_modifier, .data$physical_attribute_body_class),
      NA_character_
    ),
    physical_attribute_group_heuristic = !is.na(.data$physical_attribute_group),
    physical_attribute_note = dplyr::case_when(
      !is.na(.data$physical_attribute_group) ~ "Provisional physical group built from height, weight, and position heuristics.",
      TRUE ~ "Physical group unavailable because height, weight, or position inputs are missing."
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "height_value",
    "weight_value",
    tidyselect::any_of("position"),
    "physical_attribute_modifier",
    "physical_attribute_body_class",
    "physical_attribute_group",
    "physical_attribute_group_heuristic",
    "physical_attribute_note"
  )

validate_columns(player_master, c("player_id", "player_name"))

refined_effects <- read_project_parquet(refined_effects_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c("player_id", "player_name", "effect_name", "effect_strength", "proxy_built_effect"), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    effect_name = as.character(.data$effect_name),
    observed_effect = suppressWarnings(as.numeric(.data$effect_strength)),
    proxy_built_effect = dplyr::coalesce(as.logical(.data$proxy_built_effect), FALSE)
  ) %>%
  dplyr::filter(.data$effect_name %in% target_effects)

expected_effects_by_group <- refined_effects %>%
  dplyr::left_join(
    player_master %>% dplyr::select("player_id", "physical_attribute_group"),
    by = "player_id"
  ) %>%
  dplyr::filter(!is.na(.data$physical_attribute_group)) %>%
  dplyr::group_by(.data$physical_attribute_group, .data$effect_name) %>%
  dplyr::summarise(
    expected_effect = mean(.data$observed_effect, na.rm = TRUE),
    group_players_with_observed_effect = sum(!is.na(.data$observed_effect)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    expected_effect = dplyr::if_else(
      .data$group_players_with_observed_effect > 0,
      .data$expected_effect,
      NA_real_
    )
  )

player_possession_effect_surplus <- refined_effects %>%
  dplyr::left_join(player_master, by = c("player_id", "player_name")) %>%
  dplyr::left_join(
    expected_effects_by_group,
    by = c("physical_attribute_group", "effect_name")
  ) %>%
  dplyr::mutate(
    effect_surplus = dplyr::if_else(
      !is.na(.data$observed_effect) & !is.na(.data$expected_effect),
      .data$observed_effect - .data$expected_effect,
      NA_real_
    ),
    surplus_note = dplyr::case_when(
      is.na(.data$observed_effect) ~ "Observed effect unavailable in current pulled data.",
      is.na(.data$physical_attribute_group) ~ "Physical attribute group unavailable; expected effect and surplus not calculated.",
      is.na(.data$expected_effect) ~ "Expected effect unavailable for this physical attribute group.",
      .data$proxy_built_effect ~ paste("Surplus calculated against provisional physical group mean.", "Observed effect is proxy-built."),
      TRUE ~ "Surplus calculated as observed effect minus physical-group expected effect."
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "physical_attribute_modifier",
    "physical_attribute_body_class",
    "physical_attribute_group",
    "effect_name",
    observed_effect = "observed_effect",
    expected_effect = "expected_effect",
    effect_surplus = "effect_surplus",
    "surplus_note",
    "physical_attribute_group_heuristic",
    "proxy_built_effect",
    "group_players_with_observed_effect",
    tidyselect::any_of(c("player_nickname", "height_value", "weight_value", "position", "physical_attribute_note"))
  ) %>%
  dplyr::arrange(.data$player_name, .data$effect_name)

write_project_parquet(player_possession_effect_surplus, effect_surplus_path)

message("Phase 26c possession effect surplus diagnostics:")

message("Physical attribute group counts:")
print(
  player_master %>%
    dplyr::count(.data$physical_attribute_group, .data$physical_attribute_modifier, .data$physical_attribute_body_class, sort = TRUE)
)

message("Expected effects by physical attribute group:")
print(
  expected_effects_by_group %>%
    dplyr::arrange(.data$effect_name, .data$physical_attribute_group)
)

message("Top positive surplus players:")
print(
  player_possession_effect_surplus %>%
    dplyr::filter(!is.na(.data$effect_surplus)) %>%
    dplyr::group_by(.data$effect_name) %>%
    dplyr::arrange(dplyr::desc(.data$effect_surplus), .by_group = TRUE) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::ungroup() %>%
    dplyr::select("effect_name", "player_name", "physical_attribute_group", "observed_effect", "expected_effect", "effect_surplus", "proxy_built_effect")
)

message("Top negative surplus players:")
print(
  player_possession_effect_surplus %>%
    dplyr::filter(!is.na(.data$effect_surplus)) %>%
    dplyr::group_by(.data$effect_name) %>%
    dplyr::arrange(.data$effect_surplus, .by_group = TRUE) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::ungroup() %>%
    dplyr::select("effect_name", "player_name", "physical_attribute_group", "observed_effect", "expected_effect", "effect_surplus", "proxy_built_effect")
)

message("Proxy reliance:")
print(
  player_possession_effect_surplus %>%
    dplyr::count(.data$effect_name, .data$proxy_built_effect, sort = TRUE)
)

message("Requested player examples:")
print(
  player_possession_effect_surplus %>%
    dplyr::filter(stringr::str_detect(
      .data$player_name,
      "Josh Hart|Steven Adams|Alex Caruso|Gobert|Nikola Jokic|Jokić|LeBron James|Victor Wembanyama|Anthony Edwards|Chet Holmgren"
    )) %>%
    dplyr::select(
      "player_name",
      "physical_attribute_group",
      "effect_name",
      "observed_effect",
      "expected_effect",
      "effect_surplus",
      "proxy_built_effect",
      "surplus_note"
    )
)

message("Saved possession effect surplus to: ", effect_surplus_path)
message("Phase 26c note: surplus only. No ATK, DEF, or final card score was built.")
