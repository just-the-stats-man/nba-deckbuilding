# ============================================================
# 30_audit_card_assembly.R
# Phase 30: Audit card assembly prototype.
#
# Goal:
# Audit outputs/cards/player_card_assembly_prototype.parquet and identify
# card-quality issues before building more features.
#
# This phase is audit-only:
# - It does not change Phase 29.
# - It does not change move eligibility.
# - It does not create final card logic.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

card_assembly_path <- "outputs/cards/player_card_assembly_prototype.parquet"
body_adjusted_signals_path <- "outputs/cards/body_adjusted_card_signals.parquet"
player_card_moves_path <- "outputs/player_card_moves.parquet"
atk_v2_path <- "outputs/player_card_ATK_v2.parquet"
possession_effects_refined_path <- "outputs/effects/player_possession_effects_refined.parquet"

cards_output_dir <- "outputs/cards"
card_assembly_audit_path <- file.path(cards_output_dir, "card_assembly_audit.parquet")

fs::dir_create(cards_output_dir)

if (!file.exists(card_assembly_path)) {
  stop(
    "Missing card assembly prototype input: ",
    card_assembly_path,
    ". Run 29_build_card_assembly_prototype.R first.",
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

safe_read_audit_parquet <- function(path) {
  if (!file.exists(path)) {
    message("Optional audit input not found: ", path)
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

finite_max <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- suppressWarnings(max(x, na.rm = TRUE))

  if (is.infinite(out) || is.nan(out)) {
    return(NA_real_)
  }

  out
}

finite_sum <- function(x) {
  x <- suppressWarnings(as.numeric(x))

  if (all(is.na(x))) {
    return(NA_real_)
  }

  sum(x, na.rm = TRUE)
}

cards <- safe_read_audit_parquet(card_assembly_path) %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "team_abbreviation",
    "physical_attribute",
    "ATK_v2_score",
    "is_normal_card",
    "eligible_offensive_moves",
    "top_body_adjusted_moves",
    "eligible_effects",
    "top_body_adjusted_effects",
    "eligible_offensive_move_count",
    "eligible_effect_count"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name = as.character(.data$player_name),
    team_abbreviation = as.character(.data$team_abbreviation),
    physical_attribute = as.character(.data$physical_attribute),
    ATK_v2_score = suppressWarnings(as.numeric(.data$atk_v2_score)),
    is_normal_card = to_logical_flag(.data$is_normal_card),
    eligible_offensive_moves = as.character(.data$eligible_offensive_moves),
    top_body_adjusted_moves = as.character(.data$top_body_adjusted_moves),
    eligible_effects = as.character(.data$eligible_effects),
    top_body_adjusted_effects = as.character(.data$top_body_adjusted_effects),
    eligible_offensive_move_count = suppressWarnings(as.integer(.data$eligible_offensive_move_count)),
    eligible_effect_count = suppressWarnings(as.integer(.data$eligible_effect_count))
  ) %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::distinct(.data$player_id, .keep_all = TRUE)

player_moves <- safe_read_audit_parquet(player_card_moves_path) %>%
  add_missing_cols(c(
    "player_id",
    "move_name",
    "is_card_eligible",
    "is_normal_card",
    "move_contribution_total",
    "observed_move_attempts",
    "observed_games",
    "evidence_score",
    "evidence_tier"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    move_name = as.character(.data$move_name),
    is_card_eligible = to_logical_flag(.data$is_card_eligible),
    phase16c_is_normal_card = to_logical_flag(.data$is_normal_card),
    move_contribution_total = suppressWarnings(as.numeric(.data$move_contribution_total)),
    observed_move_attempts = suppressWarnings(as.numeric(.data$observed_move_attempts)),
    observed_games = suppressWarnings(as.numeric(.data$observed_games)),
    evidence_score = suppressWarnings(as.numeric(.data$evidence_score)),
    evidence_tier = as.character(.data$evidence_tier)
  )

move_summary <- player_moves %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    move_rows = dplyr::n(),
    eligible_move_count_source = sum(dplyr::coalesce(.data$is_card_eligible, FALSE), na.rm = TRUE),
    available_move_count_source = sum(!is.na(.data$move_name) & !is.na(.data$move_contribution_total), na.rm = TRUE),
    max_move_contribution = finite_max(.data$move_contribution_total),
    total_observed_move_attempts = finite_sum(.data$observed_move_attempts),
    max_observed_games = finite_max(.data$observed_games),
    max_evidence_score = finite_max(.data$evidence_score),
    best_evidence_tier = first_non_missing(.data$evidence_tier[order(dplyr::desc(.data$evidence_score))]),
    phase16c_is_normal_card = any(dplyr::coalesce(.data$phase16c_is_normal_card, FALSE), na.rm = TRUE),
    top_available_moves_source = collapse_values(.data$move_name[order(-dplyr::coalesce(.data$move_contribution_total, -Inf))], max_items = 5),
    .groups = "drop"
  )

body_adjusted_signals <- safe_read_audit_parquet(body_adjusted_signals_path) %>%
  add_missing_cols(c(
    "player_id",
    "signal_domain",
    "move_name",
    "effect_name",
    "effect_type",
    "effect_strength",
    "body_adjusted_contribution_surplus",
    "body_adjusted_effect_surplus",
    "availability_flag"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    signal_domain = as.character(.data$signal_domain),
    move_name = as.character(.data$move_name),
    effect_name = as.character(.data$effect_name),
    effect_type = as.character(.data$effect_type),
    effect_strength = suppressWarnings(as.numeric(.data$effect_strength)),
    body_adjusted_contribution_surplus = suppressWarnings(as.numeric(.data$body_adjusted_contribution_surplus)),
    body_adjusted_effect_surplus = suppressWarnings(as.numeric(.data$body_adjusted_effect_surplus)),
    availability_flag = to_logical_flag(.data$availability_flag)
  )

diagnostic_move_summary <- body_adjusted_signals %>%
  dplyr::filter(.data$signal_domain == "offensive_move", !is.na(.data$player_id)) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    diagnostic_body_adjusted_move_count = sum(!is.na(.data$move_name) & !is.na(.data$body_adjusted_contribution_surplus), na.rm = TRUE),
    positive_body_adjusted_move_count = sum(.data$body_adjusted_contribution_surplus > 0, na.rm = TRUE),
    max_body_adjusted_move_surplus = finite_max(.data$body_adjusted_contribution_surplus),
    diagnostic_body_adjusted_moves = collapse_values(
      .data$move_name[order(-dplyr::coalesce(.data$body_adjusted_contribution_surplus, -Inf))],
      max_items = 5
    ),
    .groups = "drop"
  )

effect_summary <- body_adjusted_signals %>%
  dplyr::filter(.data$signal_domain == "possession_effect", !is.na(.data$player_id)) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    effect_rows = dplyr::n(),
    available_effect_rows = sum(dplyr::coalesce(.data$availability_flag, TRUE), na.rm = TRUE),
    max_effect_strength = finite_max(.data$effect_strength),
    max_body_adjusted_effect_surplus = finite_max(.data$body_adjusted_effect_surplus),
    suspicious_effect_names = collapse_values(
      paste0(
        .data$effect_name,
        " [strength ",
        round(.data$effect_strength, 1),
        ", body+ ",
        round(.data$body_adjusted_effect_surplus, 1),
        "]"
      )[
        (.data$effect_strength >= 95) |
          (.data$body_adjusted_effect_surplus >= 35)
      ],
      max_items = 5
    ),
    .groups = "drop"
  )

atk_v2_summary <- safe_read_audit_parquet(atk_v2_path) %>%
  add_missing_cols(c("player_id", "move_available", "observed_move_attempts", "observed_games"), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    move_available = to_logical_flag(.data$move_available),
    observed_move_attempts = suppressWarnings(as.numeric(.data$observed_move_attempts)),
    observed_games = suppressWarnings(as.numeric(.data$observed_games))
  ) %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    atk_v2_move_rows = dplyr::n(),
    atk_v2_available_move_contexts = sum(dplyr::coalesce(.data$move_available, FALSE), na.rm = TRUE),
    atk_v2_observed_attempts = finite_sum(.data$observed_move_attempts),
    atk_v2_observed_games = finite_max(.data$observed_games),
    .groups = "drop"
  )

possession_effects_summary <- safe_read_audit_parquet(possession_effects_refined_path) %>%
  add_missing_cols(c("player_id", "effect_strength", "availability_flag"), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    effect_strength = suppressWarnings(as.numeric(.data$effect_strength)),
    availability_flag = to_logical_flag(.data$availability_flag)
  ) %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    refined_effect_rows = dplyr::n(),
    refined_available_effect_rows = sum(dplyr::coalesce(.data$availability_flag, TRUE), na.rm = TRUE),
    refined_max_effect_strength = finite_max(.data$effect_strength),
    .groups = "drop"
  )

card_assembly_audit <- cards %>%
  dplyr::left_join(move_summary, by = "player_id") %>%
  dplyr::left_join(diagnostic_move_summary, by = "player_id") %>%
  dplyr::left_join(effect_summary, by = "player_id") %>%
  dplyr::left_join(atk_v2_summary, by = "player_id") %>%
  dplyr::left_join(possession_effects_summary, by = "player_id") %>%
  dplyr::mutate(
    eligible_offensive_move_count = dplyr::coalesce(.data$eligible_offensive_move_count, 0L),
    eligible_effect_count = dplyr::coalesce(.data$eligible_effect_count, 0L),
    eligible_move_count_source = dplyr::coalesce(.data$eligible_move_count_source, 0L),
    positive_body_adjusted_move_count = dplyr::coalesce(.data$positive_body_adjusted_move_count, 0L),
    available_effect_rows = dplyr::coalesce(.data$available_effect_rows, 0L),
    high_atk_no_moves_flag = !is.na(.data$ATK_v2_score) &
      .data$ATK_v2_score >= 70 &
      .data$eligible_offensive_move_count == 0,
    moves_low_atk_flag = !is.na(.data$ATK_v2_score) &
      .data$ATK_v2_score < 35 &
      .data$eligible_offensive_move_count > 0,
    diagnostic_moves_no_eligible_moves_flag = .data$positive_body_adjusted_move_count > 0 &
      .data$eligible_offensive_move_count == 0,
    effects_only_flag = .data$eligible_effect_count > 0 &
      .data$eligible_offensive_move_count == 0 &
      (
        is.na(.data$ATK_v2_score) |
          dplyr::coalesce(.data$atk_v2_available_move_contexts, 0) == 0 |
          dplyr::coalesce(.data$available_move_count_source, 0) == 0
      ),
    missing_atk_flag = is.na(.data$ATK_v2_score),
    missing_physical_attribute_flag = is.na(.data$physical_attribute),
    suspicious_effect_strength_flag = dplyr::coalesce(.data$max_effect_strength >= 95, FALSE) |
      dplyr::coalesce(.data$max_body_adjusted_effect_surplus >= 35, FALSE),
    probable_sample_coverage_issue_flag = (
      .data$is_normal_card %in% TRUE &
        (
          is.na(.data$ATK_v2_score) |
            dplyr::coalesce(.data$atk_v2_observed_attempts, 0) < 25 |
            dplyr::coalesce(.data$total_observed_move_attempts, 0) < 25 |
            dplyr::coalesce(.data$max_evidence_score, 0) < 35
        )
    ) |
      (
        .data$high_atk_no_moves_flag &
          dplyr::coalesce(.data$positive_body_adjusted_move_count, 0) > 0
      ),
    normal_card_audit_reason = dplyr::case_when(
      !(.data$is_normal_card %in% TRUE) ~ NA_character_,
      .data$probable_sample_coverage_issue_flag ~ "normal_card_probable_sample_coverage_issue",
      .data$positive_body_adjusted_move_count > 0 ~ "normal_card_has_diagnostic_body_adjusted_moves",
      .data$available_effect_rows > 0 ~ "normal_card_has_effect_rows_but_no_positive_eligible_effects",
      TRUE ~ "normal_card_no_current_eligible_signals"
    ),
    audit_issue_count = rowSums(cbind(
      .data$high_atk_no_moves_flag,
      .data$moves_low_atk_flag,
      .data$diagnostic_moves_no_eligible_moves_flag,
      .data$effects_only_flag,
      .data$missing_atk_flag,
      .data$missing_physical_attribute_flag,
      .data$suspicious_effect_strength_flag,
      .data$probable_sample_coverage_issue_flag
    ), na.rm = TRUE),
    audit_note = paste(
      "Audit-only Phase 30.",
      "Flags are conservative provisional checks for card assembly quality.",
      "No Phase 29, move eligibility, ATK, DEF, or effect logic was modified."
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "team_abbreviation",
    "physical_attribute",
    "ATK_v2_score",
    "is_normal_card",
    "eligible_offensive_move_count",
    "eligible_effect_count",
    "eligible_offensive_moves",
    "top_body_adjusted_moves",
    "eligible_effects",
    "top_body_adjusted_effects",
    "high_atk_no_moves_flag",
    "moves_low_atk_flag",
    "diagnostic_moves_no_eligible_moves_flag",
    "effects_only_flag",
    "missing_atk_flag",
    "missing_physical_attribute_flag",
    "suspicious_effect_strength_flag",
    "probable_sample_coverage_issue_flag",
    "normal_card_audit_reason",
    "audit_issue_count",
    "move_rows",
    "eligible_move_count_source",
    "available_move_count_source",
    "positive_body_adjusted_move_count",
    "diagnostic_body_adjusted_move_count",
    "max_body_adjusted_move_surplus",
    "diagnostic_body_adjusted_moves",
    "total_observed_move_attempts",
    "max_observed_games",
    "max_evidence_score",
    "best_evidence_tier",
    "atk_v2_available_move_contexts",
    "atk_v2_observed_attempts",
    "atk_v2_observed_games",
    "effect_rows",
    "available_effect_rows",
    "max_effect_strength",
    "max_body_adjusted_effect_surplus",
    "suspicious_effect_names",
    "refined_effect_rows",
    "refined_available_effect_rows",
    "refined_max_effect_strength",
    "audit_note"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$audit_issue_count), dplyr::desc(.data$ATK_v2_score), .data$player_name)

write_project_parquet(card_assembly_audit, card_assembly_audit_path)

message("Phase 30 card assembly audit diagnostics:")

message("Total audited cards: ", nrow(card_assembly_audit))

message("Number by audit flag:")
print(
  card_assembly_audit %>%
    dplyr::summarise(
      high_atk_no_moves = sum(.data$high_atk_no_moves_flag, na.rm = TRUE),
      moves_low_atk = sum(.data$moves_low_atk_flag, na.rm = TRUE),
      diagnostic_moves_no_eligible_moves = sum(.data$diagnostic_moves_no_eligible_moves_flag, na.rm = TRUE),
      effects_only = sum(.data$effects_only_flag, na.rm = TRUE),
      missing_atk = sum(.data$missing_atk_flag, na.rm = TRUE),
      missing_physical_attribute = sum(.data$missing_physical_attribute_flag, na.rm = TRUE),
      suspicious_effect_strength = sum(.data$suspicious_effect_strength_flag, na.rm = TRUE),
      probable_sample_coverage_issue = sum(.data$probable_sample_coverage_issue_flag, na.rm = TRUE)
    )
)

message("High ATK but no eligible offensive moves examples:")
print(
  card_assembly_audit %>%
    dplyr::filter(.data$high_atk_no_moves_flag) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "ATK_v2_score",
      "eligible_offensive_move_count",
      "top_body_adjusted_moves",
      "positive_body_adjusted_move_count",
      "max_evidence_score"
    ) %>%
    dplyr::slice_head(n = 20)
)

message("Diagnostic body-adjusted moves but no eligible offensive moves examples:")
print(
  card_assembly_audit %>%
    dplyr::filter(.data$diagnostic_moves_no_eligible_moves_flag) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "ATK_v2_score",
      "top_body_adjusted_moves",
      "positive_body_adjusted_move_count",
      "eligible_offensive_moves",
      "normal_card_audit_reason"
    ) %>%
    dplyr::slice_head(n = 20)
)

message("Effects-only examples:")
print(
  card_assembly_audit %>%
    dplyr::filter(.data$effects_only_flag) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "ATK_v2_score",
      "eligible_effects",
      "top_body_adjusted_effects",
      "eligible_effect_count"
    ) %>%
    dplyr::slice_head(n = 20)
)

message("Missing ATK examples:")
print(
  card_assembly_audit %>%
    dplyr::filter(.data$missing_atk_flag) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "physical_attribute",
      "is_normal_card",
      "eligible_effects",
      "normal_card_audit_reason"
    ) %>%
    dplyr::slice_head(n = 20)
)

message("Suspicious effects examples:")
print(
  card_assembly_audit %>%
    dplyr::filter(.data$suspicious_effect_strength_flag) %>%
    dplyr::arrange(dplyr::desc(.data$max_effect_strength), dplyr::desc(.data$max_body_adjusted_effect_surplus)) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "max_effect_strength",
      "max_body_adjusted_effect_surplus",
      "suspicious_effect_names",
      "eligible_effects"
    ) %>%
    dplyr::slice_head(n = 20)
)

message("Saved card assembly audit to: ", card_assembly_audit_path)
message("Phase 30 note: audit-only; no card assembly or eligibility logic was changed.")
