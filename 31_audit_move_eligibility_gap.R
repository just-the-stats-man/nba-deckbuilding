# ============================================================
# 31_audit_move_eligibility_gap.R
# Phase 31: Audit move eligibility gap.
#
# Goal:
# Explain why players with strong ATK/body-adjusted move signals do not receive
# eligible offensive moves.
#
# This phase compares:
# - Phase 16c provisional card move eligibility
# - Phase 28 body-adjusted move surplus diagnostics
# - Phase 30 card assembly audit context
# - Phase 16b ATK_v2 move availability/sample context
#
# Audit only. This phase does not change Phase 16c, 28, or 29.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_card_moves_path <- "outputs/player_card_moves.parquet"
body_adjusted_signals_path <- "outputs/cards/body_adjusted_card_signals.parquet"
card_assembly_audit_path <- "outputs/cards/card_assembly_audit.parquet"
atk_v2_path <- "outputs/player_card_ATK_v2.parquet"

cards_output_dir <- "outputs/cards"
move_eligibility_gap_audit_path <- file.path(cards_output_dir, "move_eligibility_gap_audit.parquet")

fs::dir_create(cards_output_dir)

if (!file.exists(player_card_moves_path)) {
  stop("Missing player card moves input: ", player_card_moves_path, ". Run Phase 16c first.", call. = FALSE)
}

if (!file.exists(body_adjusted_signals_path)) {
  stop("Missing body-adjusted signal input: ", body_adjusted_signals_path, ". Run Phase 28 first.", call. = FALSE)
}

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

safe_read_gap_parquet <- function(path) {
  if (!file.exists(path)) {
    message("Optional gap-audit input not found: ", path)
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

evidence_tier_rank <- function(evidence_tier) {
  dplyr::case_when(
    evidence_tier == "A" ~ 5L,
    evidence_tier == "B" ~ 4L,
    evidence_tier == "C" ~ 3L,
    evidence_tier == "D" ~ 2L,
    evidence_tier == "F" ~ 1L,
    TRUE ~ NA_integer_
  )
}

combine_gap_reasons <- function(
  rejected_for_low_evidence,
  rejected_for_nonpositive_sample_surplus,
  rejected_for_missing_card_eligibility,
  rejected_despite_positive_body_surplus,
  rejected_high_atk_body_signal
) {
  mapply(
    function(low_evidence, nonpositive_sample, missing_eligibility, positive_body, high_atk_body) {
      reasons <- character()

      if (isTRUE(high_atk_body)) {
        reasons <- c(reasons, "high_ATK_positive_body_surplus_not_card_eligible")
      }

      if (isTRUE(positive_body)) {
        reasons <- c(reasons, "positive_body_adjusted_surplus_not_card_eligible")
      }

      if (isTRUE(low_evidence)) {
        reasons <- c(reasons, "evidence_tier_below_C_or_missing")
      }

      if (isTRUE(nonpositive_sample)) {
        reasons <- c(reasons, "sample_wide_contribution_surplus_nonpositive_or_missing")
      }

      if (isTRUE(missing_eligibility)) {
        reasons <- c(reasons, "not_card_eligible_without_standard_rejection_trigger")
      }

      if (length(reasons) == 0) {
        return(NA_character_)
      }

      paste(unique(reasons), collapse = " | ")
    },
    rejected_for_low_evidence,
    rejected_for_nonpositive_sample_surplus,
    rejected_for_missing_card_eligibility,
    rejected_despite_positive_body_surplus,
    rejected_high_atk_body_signal,
    USE.NAMES = FALSE
  )
}

player_card_moves <- safe_read_gap_parquet(player_card_moves_path) %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "move_name",
    "move_classification",
    "is_card_eligible",
    "evidence_tier",
    "evidence_score",
    "move_contribution_total",
    "contribution_surplus",
    "observed_move_attempts",
    "observed_games",
    "move_attempts_per_game"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name_phase16c = as.character(.data$player_name),
    move_name = as.character(.data$move_name),
    move_classification = as.character(.data$move_classification),
    is_card_eligible = to_logical_flag(.data$is_card_eligible),
    evidence_tier = as.character(.data$evidence_tier),
    evidence_score = suppressWarnings(as.numeric(.data$evidence_score)),
    move_contribution_total = suppressWarnings(as.numeric(.data$move_contribution_total)),
    contribution_surplus = suppressWarnings(as.numeric(.data$contribution_surplus)),
    observed_move_attempts_phase16c = suppressWarnings(as.numeric(.data$observed_move_attempts)),
    observed_games_phase16c = suppressWarnings(as.numeric(.data$observed_games)),
    move_attempts_per_game_phase16c = suppressWarnings(as.numeric(.data$move_attempts_per_game))
  ) %>%
  dplyr::select(
    "player_id",
    "player_name_phase16c",
    "move_name",
    "move_classification",
    "is_card_eligible",
    "evidence_tier",
    "evidence_score",
    "move_contribution_total",
    "contribution_surplus",
    "observed_move_attempts_phase16c",
    "observed_games_phase16c",
    "move_attempts_per_game_phase16c"
  )

body_adjusted_moves <- safe_read_gap_parquet(body_adjusted_signals_path) %>%
  add_missing_cols(c(
    "signal_domain",
    "player_id",
    "player_name",
    "move_name",
    "physical_attribute",
    "body_adjusted_contribution_surplus",
    "body_adjusted_damage_surplus",
    "body_adjusted_frequency_surplus",
    "physical_attribute_move_contribution_baseline",
    "physical_attribute_move_damage_baseline",
    "physical_attribute_move_frequency_baseline",
    "observed_move_attempts",
    "observed_games",
    "move_attempts_per_game"
  ), NA) %>%
  dplyr::mutate(
    signal_domain = as.character(.data$signal_domain),
    player_id = as.character(.data$player_id),
    player_name_phase28 = as.character(.data$player_name),
    move_name = as.character(.data$move_name),
    physical_attribute = as.character(.data$physical_attribute),
    body_adjusted_contribution_surplus = suppressWarnings(as.numeric(.data$body_adjusted_contribution_surplus)),
    body_adjusted_damage_surplus = suppressWarnings(as.numeric(.data$body_adjusted_damage_surplus)),
    body_adjusted_frequency_surplus = suppressWarnings(as.numeric(.data$body_adjusted_frequency_surplus)),
    physical_attribute_move_contribution_baseline = suppressWarnings(as.numeric(.data$physical_attribute_move_contribution_baseline)),
    physical_attribute_move_damage_baseline = suppressWarnings(as.numeric(.data$physical_attribute_move_damage_baseline)),
    physical_attribute_move_frequency_baseline = suppressWarnings(as.numeric(.data$physical_attribute_move_frequency_baseline)),
    observed_move_attempts_phase28 = suppressWarnings(as.numeric(.data$observed_move_attempts)),
    observed_games_phase28 = suppressWarnings(as.numeric(.data$observed_games)),
    move_attempts_per_game_phase28 = suppressWarnings(as.numeric(.data$move_attempts_per_game))
  ) %>%
  dplyr::filter(.data$signal_domain == "offensive_move", !is.na(.data$player_id), !is.na(.data$move_name)) %>%
  dplyr::select(
    "player_id",
    "player_name_phase28",
    "move_name",
    "physical_attribute",
    "body_adjusted_contribution_surplus",
    "body_adjusted_damage_surplus",
    "body_adjusted_frequency_surplus",
    "physical_attribute_move_contribution_baseline",
    "physical_attribute_move_damage_baseline",
    "physical_attribute_move_frequency_baseline",
    "observed_move_attempts_phase28",
    "observed_games_phase28",
    "move_attempts_per_game_phase28"
  )

card_audit_context <- safe_read_gap_parquet(card_assembly_audit_path) %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "team_abbreviation",
    "physical_attribute",
    "atk_v2_score",
    "high_atk_no_moves_flag",
    "diagnostic_moves_no_eligible_moves_flag",
    "probable_sample_coverage_issue_flag"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name_card_audit = as.character(.data$player_name),
    team_abbreviation = as.character(.data$team_abbreviation),
    physical_attribute_card_audit = as.character(.data$physical_attribute),
    ATK_v2_score_card_audit = suppressWarnings(as.numeric(.data$atk_v2_score)),
    high_atk_no_moves_player_flag = to_logical_flag(.data$high_atk_no_moves_flag),
    diagnostic_moves_no_eligible_moves_player_flag = to_logical_flag(.data$diagnostic_moves_no_eligible_moves_flag),
    probable_sample_coverage_issue_player_flag = to_logical_flag(.data$probable_sample_coverage_issue_flag)
  ) %>%
  dplyr::select(
    "player_id",
    "player_name_card_audit",
    "team_abbreviation",
    "physical_attribute_card_audit",
    "ATK_v2_score_card_audit",
    "high_atk_no_moves_player_flag",
    "diagnostic_moves_no_eligible_moves_player_flag",
    "probable_sample_coverage_issue_player_flag"
  ) %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::distinct(.data$player_id, .keep_all = TRUE)

atk_v2_move_context <- safe_read_gap_parquet(atk_v2_path) %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "move_name",
    "atk_v2_score",
    "sample_size_tier",
    "move_available",
    "observed_move_attempts",
    "observed_games",
    "move_attempts_per_game"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name_atk_v2 = as.character(.data$player_name),
    move_name = as.character(.data$move_name),
    ATK_v2_score_atk_source = suppressWarnings(as.numeric(.data$atk_v2_score)),
    sample_size_tier = as.character(.data$sample_size_tier),
    move_available = to_logical_flag(.data$move_available),
    observed_move_attempts_atk_v2 = suppressWarnings(as.numeric(.data$observed_move_attempts)),
    observed_games_atk_v2 = suppressWarnings(as.numeric(.data$observed_games)),
    move_attempts_per_game_atk_v2 = suppressWarnings(as.numeric(.data$move_attempts_per_game))
  ) %>%
  dplyr::filter(!is.na(.data$player_id), !is.na(.data$move_name)) %>%
  dplyr::group_by(.data$player_id, .data$move_name) %>%
  dplyr::summarise(
    player_name_atk_v2 = first_non_missing(.data$player_name_atk_v2),
    ATK_v2_score_atk_source = first_non_missing(.data$ATK_v2_score_atk_source),
    sample_size_tier = first_non_missing(.data$sample_size_tier),
    move_available = any(dplyr::coalesce(.data$move_available, FALSE), na.rm = TRUE),
    observed_move_attempts_atk_v2 = finite_max(.data$observed_move_attempts_atk_v2),
    observed_games_atk_v2 = finite_max(.data$observed_games_atk_v2),
    move_attempts_per_game_atk_v2 = finite_max(.data$move_attempts_per_game_atk_v2),
    .groups = "drop"
  )

move_eligibility_gap_audit <- dplyr::full_join(
  player_card_moves,
  body_adjusted_moves,
  by = c("player_id", "move_name")
) %>%
  dplyr::left_join(card_audit_context, by = "player_id") %>%
  dplyr::left_join(atk_v2_move_context, by = c("player_id", "move_name")) %>%
  dplyr::mutate(
    player_name = dplyr::coalesce(.data$player_name_phase16c, .data$player_name_phase28, .data$player_name_card_audit, .data$player_name_atk_v2),
    physical_attribute = dplyr::coalesce(.data$physical_attribute, .data$physical_attribute_card_audit),
    ATK_v2_score = dplyr::coalesce(.data$ATK_v2_score_card_audit, .data$ATK_v2_score_atk_source),
    observed_move_attempts = dplyr::coalesce(.data$observed_move_attempts_phase16c, .data$observed_move_attempts_phase28, .data$observed_move_attempts_atk_v2),
    observed_games = dplyr::coalesce(.data$observed_games_phase16c, .data$observed_games_phase28, .data$observed_games_atk_v2),
    move_attempts_per_game = dplyr::coalesce(.data$move_attempts_per_game_phase16c, .data$move_attempts_per_game_phase28, .data$move_attempts_per_game_atk_v2),
    evidence_tier_rank = evidence_tier_rank(.data$evidence_tier),
    rejected_for_low_evidence = is.na(.data$evidence_tier_rank) | .data$evidence_tier_rank < 3L,
    rejected_for_nonpositive_sample_surplus = is.na(.data$contribution_surplus) | .data$contribution_surplus <= 0,
    rejected_for_missing_card_eligibility = !(.data$is_card_eligible %in% TRUE) &
      !.data$rejected_for_low_evidence &
      !.data$rejected_for_nonpositive_sample_surplus,
    rejected_despite_positive_body_surplus = !(.data$is_card_eligible %in% TRUE) &
      !is.na(.data$body_adjusted_contribution_surplus) &
      .data$body_adjusted_contribution_surplus > 0,
    rejected_high_atk_body_signal = !(.data$is_card_eligible %in% TRUE) &
      !is.na(.data$ATK_v2_score) &
      .data$ATK_v2_score >= 70 &
      !is.na(.data$body_adjusted_contribution_surplus) &
      .data$body_adjusted_contribution_surplus > 0,
    move_eligibility_gap_reason = combine_gap_reasons(
      .data$rejected_for_low_evidence,
      .data$rejected_for_nonpositive_sample_surplus,
      .data$rejected_for_missing_card_eligibility,
      .data$rejected_despite_positive_body_surplus,
      .data$rejected_high_atk_body_signal
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "move_name",
    "physical_attribute",
    "team_abbreviation",
    "ATK_v2_score",
    "move_classification",
    "is_card_eligible",
    "evidence_tier",
    "evidence_score",
    "move_contribution_total",
    "contribution_surplus",
    "body_adjusted_contribution_surplus",
    "body_adjusted_damage_surplus",
    "body_adjusted_frequency_surplus",
    "physical_attribute_move_contribution_baseline",
    "physical_attribute_move_damage_baseline",
    "physical_attribute_move_frequency_baseline",
    "observed_move_attempts",
    "observed_games",
    "move_attempts_per_game",
    "sample_size_tier",
    "move_available",
    "rejected_for_low_evidence",
    "rejected_for_nonpositive_sample_surplus",
    "rejected_for_missing_card_eligibility",
    "rejected_despite_positive_body_surplus",
    "rejected_high_atk_body_signal",
    "move_eligibility_gap_reason",
    tidyselect::any_of(c(
      "high_atk_no_moves_player_flag",
      "diagnostic_moves_no_eligible_moves_player_flag",
      "probable_sample_coverage_issue_player_flag"
    ))
  ) %>%
  dplyr::arrange(
    dplyr::desc(.data$rejected_high_atk_body_signal),
    dplyr::desc(.data$body_adjusted_contribution_surplus),
    dplyr::desc(.data$ATK_v2_score),
    .data$player_name,
    .data$move_name
  )

write_project_parquet(move_eligibility_gap_audit, move_eligibility_gap_audit_path)

message("Phase 31 move eligibility gap audit diagnostics:")

message("Top rejected high-ATK body-signal rows:")
print(
  move_eligibility_gap_audit %>%
    dplyr::filter(.data$rejected_high_atk_body_signal) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "move_name",
      "ATK_v2_score",
      "is_card_eligible",
      "evidence_tier",
      "evidence_score",
      "contribution_surplus",
      "body_adjusted_contribution_surplus",
      "move_eligibility_gap_reason"
    ) %>%
    dplyr::slice_head(n = 25)
)

message("All move rows for Shai Gilgeous-Alexander:")
print(
  move_eligibility_gap_audit %>%
    dplyr::filter(.data$player_name == "Shai Gilgeous-Alexander") %>%
    dplyr::arrange(dplyr::desc(.data$body_adjusted_contribution_surplus)) %>%
    dplyr::select(
      "player_name",
      "move_name",
      "ATK_v2_score",
      "move_classification",
      "is_card_eligible",
      "evidence_tier",
      "evidence_score",
      "move_contribution_total",
      "contribution_surplus",
      "body_adjusted_contribution_surplus",
      "observed_move_attempts",
      "move_eligibility_gap_reason"
    )
)

message("All move rows for Keyonte George:")
print(
  move_eligibility_gap_audit %>%
    dplyr::filter(.data$player_name == "Keyonte George") %>%
    dplyr::arrange(dplyr::desc(.data$body_adjusted_contribution_surplus)) %>%
    dplyr::select(
      "player_name",
      "move_name",
      "ATK_v2_score",
      "move_classification",
      "is_card_eligible",
      "evidence_tier",
      "evidence_score",
      "move_contribution_total",
      "contribution_surplus",
      "body_adjusted_contribution_surplus",
      "observed_move_attempts",
      "move_eligibility_gap_reason"
    )
)

message("Top 50 rejected moves by body-adjusted contribution surplus:")
print(
  move_eligibility_gap_audit %>%
    dplyr::filter(.data$rejected_despite_positive_body_surplus) %>%
    dplyr::arrange(dplyr::desc(.data$body_adjusted_contribution_surplus)) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "move_name",
      "ATK_v2_score",
      "evidence_tier",
      "evidence_score",
      "contribution_surplus",
      "body_adjusted_contribution_surplus",
      "observed_move_attempts",
      "move_eligibility_gap_reason"
    ) %>%
    dplyr::slice_head(n = 50)
)

message("Count of rejection reasons:")
print(
  move_eligibility_gap_audit %>%
    dplyr::filter(!is.na(.data$move_eligibility_gap_reason)) %>%
    tidyr::separate_rows(.data$move_eligibility_gap_reason, sep = " \\| ") %>%
    dplyr::count(.data$move_eligibility_gap_reason, sort = TRUE)
)

message("Count of card-eligible moves by evidence tier:")
print(
  move_eligibility_gap_audit %>%
    dplyr::filter(.data$is_card_eligible %in% TRUE) %>%
    dplyr::count(.data$evidence_tier, sort = TRUE)
)

message("Count of rejected positive body-surplus moves by evidence tier:")
print(
  move_eligibility_gap_audit %>%
    dplyr::filter(.data$rejected_despite_positive_body_surplus) %>%
    dplyr::count(.data$evidence_tier, sort = TRUE)
)

message("Saved move eligibility gap audit to: ", move_eligibility_gap_audit_path)
message("Phase 31 note: audit-only; Phase 16c, 28, and 29 were not modified.")
