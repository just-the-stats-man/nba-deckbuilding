# ============================================================
# 28_build_body_adjusted_card_signals.R
# Phase 28: Build body-adjusted card signal baselines.
#
# Goal:
# Add provisional physical-attribute-adjusted baselines to card-facing signals.
#
# This phase does not modify:
# - ATK_v2
# - Phase 16c player_card_moves
# - Phase 26 / 26b possession effect outputs
#
# Important:
# These are observed-sample, provisional baselines. They should not be treated
# as final full-league NBA baselines until all teams / broader data are loaded.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

physical_attributes_path <- "outputs/player_physical_attributes.parquet"
player_card_moves_path <- "outputs/player_card_moves.parquet"
possession_effects_refined_path <- "outputs/effects/player_possession_effects_refined.parquet"
cards_output_dir <- "outputs/cards"
body_adjusted_card_signals_path <- file.path(cards_output_dir, "body_adjusted_card_signals.parquet")

fs::dir_create(cards_output_dir)

if (!file.exists(physical_attributes_path)) {
  stop(
    "Missing official physical attribute source: ",
    physical_attributes_path,
    ". Run 27_build_physical_attributes.R first.",
    call. = FALSE
  )
}

if (!file.exists(player_card_moves_path)) {
  stop(
    "Missing offensive card move input: ",
    player_card_moves_path,
    ". Run 16c_build_player_card_moves.R first.",
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

finite_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- mean(x, na.rm = TRUE)

  if (is.nan(out) || is.infinite(out)) {
    return(NA_real_)
  }

  out
}

physical_attributes <- read_project_parquet(physical_attributes_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c("player_id", "physical_attribute", "body_class", "physical_modifier"), NA_character_) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    physical_attribute = as.character(.data$physical_attribute),
    body_class = as.character(.data$body_class),
    physical_modifier = as.character(.data$physical_modifier)
  ) %>%
  dplyr::select("player_id", "physical_attribute", "body_class", "physical_modifier") %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::distinct(.data$player_id, .keep_all = TRUE)

validate_columns(physical_attributes, c("player_id", "physical_attribute", "body_class", "physical_modifier"))

player_card_moves <- read_project_parquet(player_card_moves_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "move_name",
    "move_classification",
    "is_card_eligible",
    "is_normal_card",
    "move_contribution_total",
    "move_attempts_per_game",
    "mean_adjusted_expected_damage",
    "damage_surplus",
    "frequency_surplus",
    "contribution_surplus",
    "evidence_score",
    "evidence_tier",
    "weapon_identity_score",
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
    move_attempts_per_game = suppressWarnings(as.numeric(.data$move_attempts_per_game)),
    mean_adjusted_expected_damage = suppressWarnings(as.numeric(.data$mean_adjusted_expected_damage)),
    damage_surplus = suppressWarnings(as.numeric(.data$damage_surplus)),
    frequency_surplus = suppressWarnings(as.numeric(.data$frequency_surplus)),
    contribution_surplus = suppressWarnings(as.numeric(.data$contribution_surplus)),
    evidence_score = suppressWarnings(as.numeric(.data$evidence_score)),
    evidence_tier = as.character(.data$evidence_tier),
    weapon_identity_score = suppressWarnings(as.numeric(.data$weapon_identity_score)),
    move_card_note = as.character(.data$move_card_note)
  )

validate_columns(player_card_moves, c("player_id", "player_name", "move_name"))

move_baselines_by_physical_attribute <- player_card_moves %>%
  dplyr::left_join(physical_attributes, by = "player_id") %>%
  dplyr::filter(!is.na(.data$physical_attribute), !is.na(.data$move_name)) %>%
  dplyr::group_by(.data$physical_attribute, .data$move_name) %>%
  dplyr::summarise(
    physical_attribute_move_damage_baseline = finite_mean(.data$mean_adjusted_expected_damage),
    physical_attribute_move_frequency_baseline = finite_mean(.data$move_attempts_per_game),
    physical_attribute_move_contribution_baseline = finite_mean(.data$move_contribution_total),
    physical_attribute_move_baseline_player_count = dplyr::n_distinct(.data$player_id),
    .groups = "drop"
  )

body_adjusted_moves <- player_card_moves %>%
  dplyr::left_join(physical_attributes, by = "player_id") %>%
  dplyr::left_join(move_baselines_by_physical_attribute, by = c("physical_attribute", "move_name")) %>%
  dplyr::mutate(
    signal_domain = "offensive_move",
    signal_name = .data$move_name,
    body_adjusted_damage_surplus = .data$mean_adjusted_expected_damage - .data$physical_attribute_move_damage_baseline,
    body_adjusted_frequency_surplus = .data$move_attempts_per_game - .data$physical_attribute_move_frequency_baseline,
    body_adjusted_contribution_surplus = .data$move_contribution_total - .data$physical_attribute_move_contribution_baseline,
    physical_attribute_effect_baseline = NA_real_,
    body_adjusted_effect_surplus = NA_real_,
    effect_name = NA_character_,
    effect_type = NA_character_,
    effect_strength = NA_real_,
    effect_note = NA_character_,
    availability_flag = NA,
    proxy_built_effect = NA,
    body_adjusted_signal_note = paste(
      "Offensive move surplus adjusted against players with the same physical_attribute.",
      "Baseline is provisional observed-sample only, not a final full-league NBA baseline."
    )
  )

body_adjusted_effects <- tibble::tibble()

if (file.exists(possession_effects_refined_path)) {
  possession_effects_refined <- read_project_parquet(possession_effects_refined_path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols() %>%
    add_missing_cols(c(
      "player_id",
      "player_name",
      "effect_name",
      "effect_type",
      "effect_strength",
      "effect_note",
      "availability_flag",
      "proxy_built_effect"
    ), NA) %>%
    dplyr::mutate(
      player_id = as.character(.data$player_id),
      player_name = as.character(.data$player_name),
      effect_name = as.character(.data$effect_name),
      effect_type = as.character(.data$effect_type),
      effect_strength = suppressWarnings(as.numeric(.data$effect_strength)),
      effect_note = as.character(.data$effect_note),
      availability_flag = to_logical_flag(.data$availability_flag),
      proxy_built_effect = to_logical_flag(.data$proxy_built_effect)
    )

  effect_baselines_by_physical_attribute <- possession_effects_refined %>%
    dplyr::left_join(physical_attributes, by = "player_id") %>%
    dplyr::filter(
      !is.na(.data$physical_attribute),
      !is.na(.data$effect_name),
      !is.na(.data$effect_strength),
      dplyr::coalesce(.data$availability_flag, TRUE)
    ) %>%
    dplyr::group_by(.data$physical_attribute, .data$effect_type, .data$effect_name) %>%
    dplyr::summarise(
      physical_attribute_effect_baseline = finite_mean(.data$effect_strength),
      physical_attribute_effect_baseline_player_count = dplyr::n_distinct(.data$player_id),
      .groups = "drop"
    )

  body_adjusted_effects <- possession_effects_refined %>%
    dplyr::left_join(physical_attributes, by = "player_id") %>%
    dplyr::left_join(
      effect_baselines_by_physical_attribute,
      by = c("physical_attribute", "effect_type", "effect_name")
    ) %>%
    dplyr::mutate(
      signal_domain = "possession_effect",
      signal_name = .data$effect_name,
      body_adjusted_effect_surplus = .data$effect_strength - .data$physical_attribute_effect_baseline,
      move_name = NA_character_,
      move_classification = NA_character_,
      is_card_eligible = NA,
      is_normal_card = NA,
      move_contribution_total = NA_real_,
      move_attempts_per_game = NA_real_,
      mean_adjusted_expected_damage = NA_real_,
      damage_surplus = NA_real_,
      frequency_surplus = NA_real_,
      contribution_surplus = NA_real_,
      physical_attribute_move_damage_baseline = NA_real_,
      physical_attribute_move_frequency_baseline = NA_real_,
      physical_attribute_move_contribution_baseline = NA_real_,
      physical_attribute_move_baseline_player_count = NA_integer_,
      body_adjusted_damage_surplus = NA_real_,
      body_adjusted_frequency_surplus = NA_real_,
      body_adjusted_contribution_surplus = NA_real_,
      evidence_score = NA_real_,
      evidence_tier = NA_character_,
      weapon_identity_score = NA_real_,
      move_card_note = NA_character_,
      body_adjusted_signal_note = paste(
        "Possession/effect surplus adjusted against players with the same physical_attribute.",
        "Baseline is provisional observed-sample only, not a final full-league NBA baseline."
      )
    )
} else {
  message(
    "Optional refined possession effect input not found: ",
    possession_effects_refined_path,
    ". Phase 28 will write offensive move body-adjusted signals only."
  )
}

body_adjusted_card_signals <- dplyr::bind_rows(
  body_adjusted_moves,
  body_adjusted_effects
) %>%
  add_missing_cols(c(
    "physical_attribute_effect_baseline_player_count",
    "physical_attribute_move_baseline_player_count"
  ), NA) %>%
  dplyr::mutate(
    provisional_baseline_scope = "Observed pulled sample only; not full-league.",
    body_adjusted_baseline_note = paste(
      "Physical-attribute baseline is provisional because full-league data is not loaded yet.",
      "Existing sample-wide surplus fields are preserved separately."
    )
  ) %>%
  dplyr::select(
    "signal_domain",
    "player_id",
    "player_name",
    "physical_attribute",
    "body_class",
    "physical_modifier",
    "signal_name",
    "move_name",
    "move_classification",
    "is_card_eligible",
    "is_normal_card",
    "move_contribution_total",
    "move_attempts_per_game",
    "mean_adjusted_expected_damage",
    "damage_surplus",
    "frequency_surplus",
    "contribution_surplus",
    "physical_attribute_move_damage_baseline",
    "physical_attribute_move_frequency_baseline",
    "physical_attribute_move_contribution_baseline",
    "body_adjusted_damage_surplus",
    "body_adjusted_frequency_surplus",
    "body_adjusted_contribution_surplus",
    "effect_name",
    "effect_type",
    "effect_strength",
    "physical_attribute_effect_baseline",
    "body_adjusted_effect_surplus",
    "availability_flag",
    "proxy_built_effect",
    "evidence_score",
    "evidence_tier",
    "weapon_identity_score",
    "move_card_note",
    "effect_note",
    "body_adjusted_signal_note",
    "provisional_baseline_scope",
    "body_adjusted_baseline_note",
    tidyselect::any_of(c(
      "physical_attribute_move_baseline_player_count",
      "physical_attribute_effect_baseline_player_count",
      "observed_move_attempts",
      "observed_games",
      "mean_activation_probability",
      "sample_move_damage",
      "sample_move_frequency",
      "sample_move_contribution",
      "baseline_player_count",
      "baseline_note",
      "evidence_methodology",
      "eligible_move_rank",
      "signals_used"
    ))
  ) %>%
  dplyr::arrange(.data$signal_domain, .data$player_name, .data$signal_name)

write_project_parquet(body_adjusted_card_signals, body_adjusted_card_signals_path)

message("Phase 28 body-adjusted card signal diagnostics:")

message("Rows by signal domain:")
print(
  body_adjusted_card_signals %>%
    dplyr::count(.data$signal_domain, sort = TRUE)
)

message("Move baseline coverage by physical_attribute:")
print(
  body_adjusted_card_signals %>%
    dplyr::filter(.data$signal_domain == "offensive_move") %>%
    dplyr::group_by(.data$physical_attribute) %>%
    dplyr::summarise(
      rows = dplyr::n(),
      players = dplyr::n_distinct(.data$player_id),
      moves = dplyr::n_distinct(.data$move_name),
      missing_body_adjusted_contribution = sum(is.na(.data$body_adjusted_contribution_surplus)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$rows))
)

if (nrow(body_adjusted_effects) > 0) {
  message("Effect baseline coverage by physical_attribute:")
  print(
    body_adjusted_card_signals %>%
      dplyr::filter(.data$signal_domain == "possession_effect") %>%
      dplyr::group_by(.data$physical_attribute) %>%
      dplyr::summarise(
        rows = dplyr::n(),
        players = dplyr::n_distinct(.data$player_id),
        effects = dplyr::n_distinct(.data$effect_name),
        missing_body_adjusted_effect = sum(is.na(.data$body_adjusted_effect_surplus)),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$rows))
  )
}

message("Top positive body-adjusted offensive move contribution surplus:")
print(
  body_adjusted_card_signals %>%
    dplyr::filter(.data$signal_domain == "offensive_move", !is.na(.data$body_adjusted_contribution_surplus)) %>%
    dplyr::arrange(dplyr::desc(.data$body_adjusted_contribution_surplus)) %>%
    dplyr::select(
      "player_name",
      "physical_attribute",
      "move_name",
      "move_contribution_total",
      "physical_attribute_move_contribution_baseline",
      "contribution_surplus",
      "body_adjusted_contribution_surplus"
    ) %>%
    dplyr::slice_head(n = 20)
)

if (nrow(body_adjusted_effects) > 0) {
  message("Top positive body-adjusted possession/effect surplus:")
  print(
    body_adjusted_card_signals %>%
      dplyr::filter(.data$signal_domain == "possession_effect", !is.na(.data$body_adjusted_effect_surplus)) %>%
      dplyr::arrange(dplyr::desc(.data$body_adjusted_effect_surplus)) %>%
      dplyr::select(
        "player_name",
        "physical_attribute",
        "effect_name",
        "effect_type",
        "effect_strength",
        "physical_attribute_effect_baseline",
        "body_adjusted_effect_surplus"
      ) %>%
      dplyr::slice_head(n = 20)
  )
}

message("Saved body-adjusted card signals to: ", body_adjusted_card_signals_path)
message("Phase 28 note: body-adjusted baselines are provisional observed-sample baselines only.")
