# ============================================================
# 29_build_card_assembly_prototype.R
# Phase 29: Card assembly prototype.
#
# Goal:
# Create the first rough player-card backend output table.
#
# This is not final UI and does not create final DEF, AP, CR, or star logic.
# It assembles existing diagnostic/card-facing outputs into one transparent
# player-level table so future card rendering work has a single prototype input.
#
# Important:
# - Phase 27 physical_attribute remains the official card physical attribute.
# - Phase 27b force-profile fields are diagnostic-only.
# - Missing scores stay missing; this phase does not invent final scores.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

atk_v2_path <- "outputs/player_card_ATK_v2.parquet"
player_card_moves_path <- "outputs/player_card_moves.parquet"
body_adjusted_signals_path <- "outputs/cards/body_adjusted_card_signals.parquet"
physical_attributes_path <- "outputs/player_physical_attributes.parquet"
physical_force_profile_path <- "outputs/physical/player_physical_force_profile.parquet"
possession_effects_refined_path <- "outputs/effects/player_possession_effects_refined.parquet"
def_components_path <- "outputs/defense/player_DEF_components.parquet"
def_proxy_components_path <- "outputs/defense/player_DEF_proxy_components.parquet"

cards_output_dir <- "outputs/cards"
card_assembly_path <- file.path(cards_output_dir, "player_card_assembly_prototype.parquet")

fs::dir_create(cards_output_dir)

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

safe_read_card_parquet <- function(path) {
  if (!file.exists(path)) {
    message("Optional input not found: ", path)
    return(tibble::tibble(player_id = character()))
  }

  read_project_parquet(path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols()
}

to_logical_flag <- function(x) {
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

first_non_missing <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA)
  }

  x[[1]]
}

collapse_values <- function(x, max_items = 8) {
  x <- unique(stats::na.omit(as.character(x)))

  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(utils::head(x, max_items), collapse = " | ")
}

fmt_number <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))

  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

collapse_move_rows <- function(move_name, evidence_tier, move_contribution_total, body_adjusted_contribution_surplus = NULL, max_items = 6) {
  n <- length(move_name)

  if (n == 0) {
    return(NA_character_)
  }

  if (is.null(body_adjusted_contribution_surplus)) {
    labels <- paste0(
      move_name,
      " [evidence ",
      dplyr::coalesce(as.character(evidence_tier), "NA"),
      ", contrib ",
      fmt_number(move_contribution_total),
      "]"
    )
  } else {
    labels <- paste0(
      move_name,
      " [body+ ",
      fmt_number(body_adjusted_contribution_surplus),
      ", contrib ",
      fmt_number(move_contribution_total),
      "]"
    )
  }

  collapse_values(labels, max_items = max_items)
}

collapse_effect_rows <- function(effect_name, effect_type, effect_strength, body_adjusted_effect_surplus = NULL, max_items = 6) {
  n <- length(effect_name)

  if (n == 0) {
    return(NA_character_)
  }

  if (is.null(body_adjusted_effect_surplus)) {
    labels <- paste0(
      effect_name,
      " (",
      dplyr::coalesce(as.character(effect_type), "effect"),
      ") [strength ",
      fmt_number(effect_strength),
      "]"
    )
  } else {
    labels <- paste0(
      effect_name,
      " (",
      dplyr::coalesce(as.character(effect_type), "effect"),
      ") [body+ ",
      fmt_number(body_adjusted_effect_surplus),
      "]"
    )
  }

  collapse_values(labels, max_items = max_items)
}

atk_v2_raw <- safe_read_card_parquet(atk_v2_path) %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "team_abbreviation",
    "atk_v2_raw",
    "atk_v2_score",
    "atk_v2_note",
    "observed_data_scope_note"
  ), NA)

atk_v2_summary <- atk_v2_raw %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name = as.character(.data$player_name),
    team_abbreviation = as.character(.data$team_abbreviation),
    atk_v2_raw = suppressWarnings(as.numeric(.data$atk_v2_raw)),
    atk_v2_score = suppressWarnings(as.numeric(.data$atk_v2_score)),
    atk_v2_note = as.character(.data$atk_v2_note),
    observed_data_scope_note = as.character(.data$observed_data_scope_note)
  ) %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    player_name_atk = first_non_missing(.data$player_name),
    team_abbreviation = first_non_missing(.data$team_abbreviation),
    ATK_v2_raw = first_non_missing(.data$atk_v2_raw),
    ATK_v2_score = first_non_missing(.data$atk_v2_score),
    atk_v2_note = first_non_missing(.data$atk_v2_note),
    atk_observed_data_scope_note = first_non_missing(.data$observed_data_scope_note),
    .groups = "drop"
  )

player_card_moves <- safe_read_card_parquet(player_card_moves_path) %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "move_name",
    "move_classification",
    "is_card_eligible",
    "is_normal_card",
    "move_contribution_total",
    "evidence_tier",
    "move_card_note"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name = as.character(.data$player_name),
    move_name = as.character(.data$move_name),
    move_classification = as.character(.data$move_classification),
    is_card_eligible = to_logical_flag(.data$is_card_eligible),
    is_normal_card = to_logical_flag(.data$is_normal_card),
    move_contribution_total = suppressWarnings(as.numeric(.data$move_contribution_total)),
    evidence_tier = as.character(.data$evidence_tier),
    move_card_note = as.character(.data$move_card_note)
  )

eligible_moves <- player_card_moves %>%
  dplyr::filter(dplyr::coalesce(.data$is_card_eligible, FALSE), !is.na(.data$move_name)) %>%
  dplyr::arrange(.data$player_id, dplyr::desc(.data$move_contribution_total), .data$move_name)

moves_summary <- eligible_moves %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    player_name_moves = first_non_missing(.data$player_name),
    eligible_offensive_moves = collapse_move_rows(
      .data$move_name,
      .data$evidence_tier,
      .data$move_contribution_total,
      max_items = 8
    ),
    signature_moves = collapse_move_rows(
      .data$move_name[.data$move_classification == "PROVISIONAL_SIGNATURE"],
      .data$evidence_tier[.data$move_classification == "PROVISIONAL_SIGNATURE"],
      .data$move_contribution_total[.data$move_classification == "PROVISIONAL_SIGNATURE"],
      max_items = 3
    ),
    core_moves = collapse_move_rows(
      .data$move_name[.data$move_classification == "PROVISIONAL_CORE"],
      .data$evidence_tier[.data$move_classification == "PROVISIONAL_CORE"],
      .data$move_contribution_total[.data$move_classification == "PROVISIONAL_CORE"],
      max_items = 5
    ),
    utility_moves = collapse_move_rows(
      .data$move_name[.data$move_classification == "PROVISIONAL_UTILITY"],
      .data$evidence_tier[.data$move_classification == "PROVISIONAL_UTILITY"],
      .data$move_contribution_total[.data$move_classification == "PROVISIONAL_UTILITY"],
      max_items = 6
    ),
    eligible_offensive_move_count = dplyr::n(),
    move_notes = collapse_values(.data$move_card_note, max_items = 3),
    .groups = "drop"
  )

body_adjusted_signals <- safe_read_card_parquet(body_adjusted_signals_path) %>%
  add_missing_cols(c(
    "signal_domain",
    "player_id",
    "player_name",
    "physical_attribute",
    "body_class",
    "physical_modifier",
    "physical_attribute_source",
    "force_profile_available",
    "physical_force_score",
    "body_class_force_percentile",
    "physical_force_tier",
    "force_profile_confidence",
    "force_refined_physical_modifier",
    "force_refined_physical_attribute",
    "move_name",
    "move_contribution_total",
    "body_adjusted_contribution_surplus",
    "evidence_tier",
    "effect_name",
    "effect_type",
    "effect_strength",
    "body_adjusted_effect_surplus",
    "availability_flag",
    "body_adjusted_signal_note",
    "observed_data_scope_note"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name = as.character(.data$player_name),
    signal_domain = as.character(.data$signal_domain),
    physical_attribute = as.character(.data$physical_attribute),
    body_class = as.character(.data$body_class),
    physical_modifier = as.character(.data$physical_modifier),
    physical_attribute_source = as.character(.data$physical_attribute_source),
    force_profile_available = to_logical_flag(.data$force_profile_available),
    physical_force_score = suppressWarnings(as.numeric(.data$physical_force_score)),
    body_class_force_percentile = suppressWarnings(as.numeric(.data$body_class_force_percentile)),
    physical_force_tier = as.character(.data$physical_force_tier),
    force_profile_confidence = as.character(.data$force_profile_confidence),
    force_refined_physical_modifier = as.character(.data$force_refined_physical_modifier),
    force_refined_physical_attribute = as.character(.data$force_refined_physical_attribute),
    move_name = as.character(.data$move_name),
    move_contribution_total = suppressWarnings(as.numeric(.data$move_contribution_total)),
    body_adjusted_contribution_surplus = suppressWarnings(as.numeric(.data$body_adjusted_contribution_surplus)),
    evidence_tier = as.character(.data$evidence_tier),
    effect_name = as.character(.data$effect_name),
    effect_type = as.character(.data$effect_type),
    effect_strength = suppressWarnings(as.numeric(.data$effect_strength)),
    body_adjusted_effect_surplus = suppressWarnings(as.numeric(.data$body_adjusted_effect_surplus)),
    availability_flag = to_logical_flag(.data$availability_flag),
    body_adjusted_signal_note = as.character(.data$body_adjusted_signal_note),
    observed_data_scope_note = as.character(.data$observed_data_scope_note)
  )

body_context <- body_adjusted_signals %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    player_name_body = first_non_missing(.data$player_name),
    physical_attribute = first_non_missing(.data$physical_attribute),
    physical_attribute_source = first_non_missing(.data$physical_attribute_source),
    body_class = first_non_missing(.data$body_class),
    physical_modifier = first_non_missing(.data$physical_modifier),
    physical_force_score = first_non_missing(.data$physical_force_score),
    body_class_force_percentile = first_non_missing(.data$body_class_force_percentile),
    physical_force_tier = first_non_missing(.data$physical_force_tier),
    force_profile_confidence = first_non_missing(.data$force_profile_confidence),
    force_refined_physical_modifier = first_non_missing(.data$force_refined_physical_modifier),
    force_refined_physical_attribute = first_non_missing(.data$force_refined_physical_attribute),
    body_signal_scope_note = first_non_missing(.data$observed_data_scope_note),
    .groups = "drop"
  )

top_body_adjusted_moves <- body_adjusted_signals %>%
  dplyr::filter(
    .data$signal_domain == "offensive_move",
    !is.na(.data$move_name),
    !is.na(.data$body_adjusted_contribution_surplus)
  ) %>%
  dplyr::arrange(.data$player_id, dplyr::desc(.data$body_adjusted_contribution_surplus), .data$move_name) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::summarise(
    top_body_adjusted_moves = collapse_move_rows(
      .data$move_name,
      .data$evidence_tier,
      .data$move_contribution_total,
      .data$body_adjusted_contribution_surplus,
      max_items = 5
    ),
    .groups = "drop"
  )

effects_from_body_signals <- body_adjusted_signals %>%
  dplyr::filter(
    .data$signal_domain == "possession_effect",
    !is.na(.data$effect_name),
    dplyr::coalesce(.data$availability_flag, TRUE),
    !is.na(.data$body_adjusted_effect_surplus),
    .data$body_adjusted_effect_surplus > 0
  )

effects_summary <- effects_from_body_signals %>%
  dplyr::arrange(.data$player_id, dplyr::desc(.data$effect_strength), .data$effect_name) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    eligible_effects = collapse_effect_rows(
      .data$effect_name,
      .data$effect_type,
      .data$effect_strength,
      max_items = 6
    ),
    eligible_effect_count = dplyr::n(),
    .groups = "drop"
  )

top_body_adjusted_effects <- effects_from_body_signals %>%
  dplyr::filter(!is.na(.data$body_adjusted_effect_surplus)) %>%
  dplyr::arrange(.data$player_id, dplyr::desc(.data$body_adjusted_effect_surplus), .data$effect_name) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::summarise(
    top_body_adjusted_effects = collapse_effect_rows(
      .data$effect_name,
      .data$effect_type,
      .data$effect_strength,
      .data$body_adjusted_effect_surplus,
      max_items = 5
    ),
    .groups = "drop"
  )

physical_attributes <- safe_read_card_parquet(physical_attributes_path) %>%
  add_missing_cols(c("player_id", "player_name", "body_class", "physical_modifier", "physical_attribute"), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name_physical = as.character(.data$player_name),
    body_class = as.character(.data$body_class),
    physical_modifier = as.character(.data$physical_modifier),
    physical_attribute = as.character(.data$physical_attribute)
  ) %>%
  dplyr::select("player_id", "player_name_physical", "body_class", "physical_modifier", "physical_attribute") %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::distinct(.data$player_id, .keep_all = TRUE)

force_profile <- safe_read_card_parquet(physical_force_profile_path) %>%
  add_missing_cols(c(
    "player_id",
    "physical_force_score",
    "body_class_force_percentile",
    "physical_force_tier",
    "force_profile_confidence"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    physical_force_score = suppressWarnings(as.numeric(.data$physical_force_score)),
    body_class_force_percentile = suppressWarnings(as.numeric(.data$body_class_force_percentile)),
    physical_force_tier = as.character(.data$physical_force_tier),
    force_profile_confidence = as.character(.data$force_profile_confidence)
  ) %>%
  dplyr::select("player_id", "physical_force_score", "body_class_force_percentile", "physical_force_tier", "force_profile_confidence") %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::distinct(.data$player_id, .keep_all = TRUE)

def_components <- safe_read_card_parquet(def_components_path) %>%
  add_missing_cols(c("player_id", "team_abbreviation", "def_score", "def_placeholder_score"), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    team_abbreviation_def = as.character(.data$team_abbreviation),
    def_score = suppressWarnings(as.numeric(.data$def_score)),
    def_placeholder_score = suppressWarnings(as.numeric(.data$def_placeholder_score))
  ) %>%
  dplyr::transmute(
    player_id = .data$player_id,
    team_abbreviation_def = .data$team_abbreviation_def,
    DEF_placeholder_score = dplyr::coalesce(.data$def_score, .data$def_placeholder_score),
    def_source_note = dplyr::if_else(
      !is.na(.data$DEF_placeholder_score),
      "DEF placeholder score read from defensive input.",
      "DEF is not finalized; no DEF placeholder score is available."
    )
  ) %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::distinct(.data$player_id, .keep_all = TRUE)

def_proxy_components <- safe_read_card_parquet(def_proxy_components_path) %>%
  add_missing_cols(c("player_id", "on_ball_proxy_component", "versatility_proxy_component", "proxy_note"), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    on_ball_proxy_component = suppressWarnings(as.numeric(.data$on_ball_proxy_component)),
    versatility_proxy_component = suppressWarnings(as.numeric(.data$versatility_proxy_component)),
    proxy_note = as.character(.data$proxy_note)
  ) %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::distinct(.data$player_id, .keep_all = TRUE)

possession_effects_refined <- safe_read_card_parquet(possession_effects_refined_path) %>%
  add_missing_cols(c("player_id", "player_name"), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name_effect = as.character(.data$player_name)
  ) %>%
  dplyr::select("player_id", "player_name_effect") %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::distinct(.data$player_id, .keep_all = TRUE)

card_universe <- dplyr::bind_rows(
  atk_v2_summary %>% dplyr::transmute(player_id = .data$player_id),
  player_card_moves %>% dplyr::filter(!is.na(.data$player_id)) %>% dplyr::transmute(player_id = .data$player_id),
  body_adjusted_signals %>% dplyr::filter(!is.na(.data$player_id)) %>% dplyr::transmute(player_id = .data$player_id),
  possession_effects_refined %>% dplyr::transmute(player_id = .data$player_id),
  def_components %>% dplyr::transmute(player_id = .data$player_id),
  def_proxy_components %>% dplyr::transmute(player_id = .data$player_id)
) %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::distinct()

player_card_assembly_prototype <- card_universe %>%
  dplyr::left_join(atk_v2_summary, by = "player_id") %>%
  dplyr::left_join(moves_summary, by = "player_id") %>%
  dplyr::left_join(body_context, by = "player_id") %>%
  dplyr::left_join(top_body_adjusted_moves, by = "player_id") %>%
  dplyr::left_join(effects_summary, by = "player_id") %>%
  dplyr::left_join(top_body_adjusted_effects, by = "player_id") %>%
  dplyr::left_join(physical_attributes, by = "player_id", suffix = c("", "_phase27")) %>%
  dplyr::left_join(force_profile, by = "player_id", suffix = c("", "_phase27b")) %>%
  dplyr::left_join(def_components, by = "player_id") %>%
  dplyr::left_join(def_proxy_components, by = "player_id") %>%
  dplyr::left_join(possession_effects_refined, by = "player_id") %>%
  dplyr::mutate(
    player_name = dplyr::coalesce(
      .data$player_name_atk,
      .data$player_name_moves,
      .data$player_name_body,
      .data$player_name_physical,
      .data$player_name_effect
    ),
    team_abbreviation = dplyr::coalesce(.data$team_abbreviation, .data$team_abbreviation_def),
    physical_attribute = dplyr::coalesce(.data$physical_attribute, .data$physical_attribute_phase27),
    body_class = dplyr::coalesce(.data$body_class, .data$body_class_phase27),
    physical_modifier = dplyr::coalesce(.data$physical_modifier, .data$physical_modifier_phase27),
    physical_attribute_source = dplyr::coalesce(.data$physical_attribute_source, "phase27_body_heuristic"),
    physical_force_score = dplyr::coalesce(.data$physical_force_score, .data$physical_force_score_phase27b),
    body_class_force_percentile = dplyr::coalesce(.data$body_class_force_percentile, .data$body_class_force_percentile_phase27b),
    physical_force_tier = dplyr::coalesce(.data$physical_force_tier, .data$physical_force_tier_phase27b),
    force_profile_confidence = dplyr::coalesce(.data$force_profile_confidence, .data$force_profile_confidence_phase27b),
    eligible_offensive_move_count = dplyr::coalesce(.data$eligible_offensive_move_count, 0L),
    eligible_effect_count = dplyr::coalesce(.data$eligible_effect_count, 0L),
    is_normal_card = .data$eligible_offensive_move_count == 0 & .data$eligible_effect_count == 0,
    card_star_tier_placeholder = dplyr::case_when(
      is.na(.data$ATK_v2_score) ~ NA_character_,
      .data$ATK_v2_score >= 90 ~ "S_placeholder",
      .data$ATK_v2_score >= 80 ~ "A_placeholder",
      .data$ATK_v2_score >= 70 ~ "B_placeholder",
      .data$ATK_v2_score >= 60 ~ "C_placeholder",
      TRUE ~ "D_placeholder"
    ),
    card_build_note = paste(
      "Prototype backend card assembly only.",
      "Phase 27 physical_attribute is official for now; Phase 27b force fields are diagnostic-only.",
      "DEF is not finalized; DEF_placeholder_score remains NA unless a defensive input provides an explicit score.",
      "Move/effect text is provisional and observed-sample based."
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "team_abbreviation",
    "physical_attribute",
    "physical_attribute_source",
    "body_class",
    "physical_modifier",
    "physical_force_score",
    "body_class_force_percentile",
    "physical_force_tier",
    "force_profile_confidence",
    "ATK_v2_raw",
    "ATK_v2_score",
    "DEF_placeholder_score",
    "card_star_tier_placeholder",
    "is_normal_card",
    "eligible_offensive_moves",
    "signature_moves",
    "core_moves",
    "utility_moves",
    "top_body_adjusted_moves",
    "eligible_effects",
    "top_body_adjusted_effects",
    "card_build_note",
    tidyselect::any_of(c(
      "eligible_offensive_move_count",
      "eligible_effect_count",
      "atk_v2_note",
      "atk_observed_data_scope_note",
      "body_signal_scope_note",
      "move_notes",
      "def_source_note",
      "on_ball_proxy_component",
      "versatility_proxy_component",
      "proxy_note"
    ))
  ) %>%
  dplyr::arrange(dplyr::desc(.data$ATK_v2_score), .data$player_name)

write_project_parquet(player_card_assembly_prototype, card_assembly_path)

message("Phase 29 card assembly prototype diagnostics:")

message("Number of cards created: ", nrow(player_card_assembly_prototype))
message("Number of normal cards: ", sum(player_card_assembly_prototype$is_normal_card, na.rm = TRUE))
message("Number with eligible offensive moves: ", sum(player_card_assembly_prototype$eligible_offensive_move_count > 0, na.rm = TRUE))
message("Number with eligible effects: ", sum(player_card_assembly_prototype$eligible_effect_count > 0, na.rm = TRUE))

requested_player_pattern <- "Luka Don|Doncic|Dončić|LeBron James|Austin Reaves|Deandre Ayton|DeAndre Ayton|Jaxson Hayes|Shai Gilgeous-Alexander|Trae Young|Jrue Holiday"

message("Requested prototype card examples:")
print(
  player_card_assembly_prototype %>%
    dplyr::filter(stringr::str_detect(.data$player_name, requested_player_pattern)) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "physical_attribute",
      "physical_attribute_source",
      "physical_force_score",
      "ATK_v2_score",
      "DEF_placeholder_score",
      "card_star_tier_placeholder",
      "is_normal_card",
      "eligible_offensive_moves",
      "eligible_effects",
      "top_body_adjusted_moves",
      "top_body_adjusted_effects"
    ) %>%
    dplyr::arrange(.data$player_name)
)

message("Top 25 by ATK_v2_score:")
print(
  player_card_assembly_prototype %>%
    dplyr::filter(!is.na(.data$ATK_v2_score)) %>%
    dplyr::arrange(dplyr::desc(.data$ATK_v2_score)) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "physical_attribute",
      "ATK_v2_raw",
      "ATK_v2_score",
      "card_star_tier_placeholder",
      "eligible_offensive_move_count",
      "eligible_effect_count",
      "is_normal_card"
    ) %>%
    dplyr::slice_head(n = 25)
)

message("Saved card assembly prototype to: ", card_assembly_path)
message("Phase 29 note: prototype card assembly only. No final UI or final DEF score was created.")
