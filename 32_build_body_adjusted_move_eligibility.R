# ============================================================
# 32_build_body_adjusted_move_eligibility.R
# Phase 32: Body-adjusted move eligibility prototype.
#
# Goal:
# Prototype a revised offensive move eligibility layer that incorporates
# Phase 28 body-adjusted move surplus without modifying Phase 16c.
#
# This phase is provisional:
# - Original Phase 16c eligible moves are preserved.
# - Rejected moves can be upgraded only with sufficient evidence and positive
#   body-adjusted contribution surplus.
# - Phase 27b force-profile fields are not used.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_card_moves_path <- "outputs/player_card_moves.parquet"
body_adjusted_signals_path <- "outputs/cards/body_adjusted_card_signals.parquet"
move_eligibility_gap_audit_path <- "outputs/cards/move_eligibility_gap_audit.parquet"
atk_v2_path <- "outputs/player_card_ATK_v2.parquet"

cards_output_dir <- "outputs/cards"
body_adjusted_move_eligibility_path <- file.path(cards_output_dir, "body_adjusted_move_eligibility.parquet")

fs::dir_create(cards_output_dir)

required_inputs <- c(
  player_card_moves_path,
  body_adjusted_signals_path,
  move_eligibility_gap_audit_path,
  atk_v2_path
)

missing_required_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_required_inputs) > 0) {
  stop(
    "Missing required Phase 32 input(s): ",
    paste(missing_required_inputs, collapse = ", "),
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

safe_read_phase32_parquet <- function(path) {
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

player_card_moves_source <- safe_read_phase32_parquet(player_card_moves_path) %>%
  add_missing_cols(c("player_id", "move_name", "is_card_eligible"), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    move_name = as.character(.data$move_name),
    original_source_is_card_eligible = to_logical_flag(.data$is_card_eligible)
  ) %>%
  dplyr::filter(!is.na(.data$player_id), !is.na(.data$move_name)) %>%
  dplyr::select("player_id", "move_name", "original_source_is_card_eligible")

body_adjusted_move_source <- safe_read_phase32_parquet(body_adjusted_signals_path) %>%
  add_missing_cols(c("signal_domain", "player_id", "move_name", "body_adjusted_contribution_surplus"), NA) %>%
  dplyr::mutate(
    signal_domain = as.character(.data$signal_domain),
    player_id = as.character(.data$player_id),
    move_name = as.character(.data$move_name),
    body_adjusted_source_contribution_surplus = suppressWarnings(as.numeric(.data$body_adjusted_contribution_surplus))
  ) %>%
  dplyr::filter(.data$signal_domain == "offensive_move", !is.na(.data$player_id), !is.na(.data$move_name)) %>%
  dplyr::select("player_id", "move_name", "body_adjusted_source_contribution_surplus")

atk_v2_player_context <- safe_read_phase32_parquet(atk_v2_path) %>%
  add_missing_cols(c("player_id", "atk_v2_score"), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    ATK_v2_score_source = suppressWarnings(as.numeric(.data$atk_v2_score))
  ) %>%
  dplyr::filter(!is.na(.data$player_id)) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    ATK_v2_score_source = first_non_missing(.data$ATK_v2_score_source),
    .groups = "drop"
  )

gap_audit <- safe_read_phase32_parquet(move_eligibility_gap_audit_path) %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "move_name",
    "physical_attribute",
    "atk_v2_score",
    "move_classification",
    "is_card_eligible",
    "evidence_tier",
    "evidence_score",
    "contribution_surplus",
    "body_adjusted_contribution_surplus",
    "body_adjusted_damage_surplus",
    "body_adjusted_frequency_surplus",
    "move_contribution_total",
    "observed_move_attempts",
    "observed_games",
    "move_attempts_per_game",
    "sample_size_tier",
    "move_available"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name = as.character(.data$player_name),
    move_name = as.character(.data$move_name),
    physical_attribute = as.character(.data$physical_attribute),
    ATK_v2_score = suppressWarnings(as.numeric(.data$atk_v2_score)),
    original_move_classification = as.character(.data$move_classification),
    original_is_card_eligible = to_logical_flag(.data$is_card_eligible),
    evidence_tier = as.character(.data$evidence_tier),
    evidence_score = suppressWarnings(as.numeric(.data$evidence_score)),
    contribution_surplus = suppressWarnings(as.numeric(.data$contribution_surplus)),
    body_adjusted_contribution_surplus = suppressWarnings(as.numeric(.data$body_adjusted_contribution_surplus)),
    body_adjusted_damage_surplus = suppressWarnings(as.numeric(.data$body_adjusted_damage_surplus)),
    body_adjusted_frequency_surplus = suppressWarnings(as.numeric(.data$body_adjusted_frequency_surplus)),
    move_contribution_total = suppressWarnings(as.numeric(.data$move_contribution_total)),
    observed_move_attempts = suppressWarnings(as.numeric(.data$observed_move_attempts)),
    observed_games = suppressWarnings(as.numeric(.data$observed_games)),
    move_attempts_per_game = suppressWarnings(as.numeric(.data$move_attempts_per_game)),
    sample_size_tier = as.character(.data$sample_size_tier),
    move_available = to_logical_flag(.data$move_available)
  )

body_adjusted_move_eligibility_base <- gap_audit %>%
  dplyr::left_join(player_card_moves_source, by = c("player_id", "move_name")) %>%
  dplyr::left_join(body_adjusted_move_source, by = c("player_id", "move_name")) %>%
  dplyr::left_join(atk_v2_player_context, by = "player_id") %>%
  dplyr::mutate(
    original_is_card_eligible = dplyr::coalesce(.data$original_is_card_eligible, .data$original_source_is_card_eligible, FALSE),
    ATK_v2_score = dplyr::coalesce(.data$ATK_v2_score, .data$ATK_v2_score_source),
    body_adjusted_contribution_surplus = dplyr::coalesce(
      .data$body_adjusted_contribution_surplus,
      .data$body_adjusted_source_contribution_surplus
    ),
    evidence_tier_rank = evidence_tier_rank(.data$evidence_tier),
    evidence_at_least_c = .data$evidence_tier_rank >= 3L,
    evidence_ab = .data$evidence_tier %in% c("A", "B"),
    evidence_c = .data$evidence_tier == "C",
    body_adjusted_surplus_positive = !is.na(.data$body_adjusted_contribution_surplus) &
      .data$body_adjusted_contribution_surplus > 0,
    # Heuristic only: "clearly positive" is a prototype body-adjusted surplus
    # threshold for stronger A/B upgrades. Future versions should learn this
    # threshold from full-league baselines.
    body_adjusted_surplus_clearly_positive = !is.na(.data$body_adjusted_contribution_surplus) &
      .data$body_adjusted_contribution_surplus >= 0.50,
    positive_move_contribution = !is.na(.data$move_contribution_total) &
      .data$move_contribution_total > 0,
    body_adjusted_upgrade_candidate = !(.data$original_is_card_eligible %in% TRUE) &
      .data$evidence_at_least_c &
      .data$body_adjusted_surplus_positive &
      .data$positive_move_contribution
  )

body_adjusted_upgrade_ranks <- body_adjusted_move_eligibility_base %>%
  dplyr::filter(.data$body_adjusted_upgrade_candidate) %>%
  dplyr::arrange(.data$player_id, dplyr::desc(.data$body_adjusted_contribution_surplus), dplyr::desc(.data$move_contribution_total), .data$move_name) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::mutate(body_adjusted_upgrade_rank = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select("player_id", "move_name", "body_adjusted_upgrade_rank")

body_adjusted_move_eligibility <- body_adjusted_move_eligibility_base %>%
  dplyr::left_join(body_adjusted_upgrade_ranks, by = c("player_id", "move_name")) %>%
  dplyr::mutate(
    body_adjusted_move_eligible = (.data$original_is_card_eligible %in% TRUE) |
      .data$body_adjusted_upgrade_candidate,
    body_adjusted_move_classification = dplyr::case_when(
      .data$original_is_card_eligible %in% TRUE ~ .data$original_move_classification,
      !.data$body_adjusted_upgrade_candidate ~ "NOT_CARD_ELIGIBLE",
      .data$evidence_c ~ "PROVISIONAL_BODY_ADJUSTED_UTILITY",
      .data$evidence_ab &
        .data$body_adjusted_surplus_clearly_positive &
        !is.na(.data$ATK_v2_score) &
        .data$ATK_v2_score >= 70 &
        .data$body_adjusted_upgrade_rank == 1L ~ "PROVISIONAL_BODY_ADJUSTED_SIGNATURE",
      .data$evidence_ab &
        .data$body_adjusted_surplus_clearly_positive &
        !is.na(.data$ATK_v2_score) &
        .data$ATK_v2_score >= 70 ~ "PROVISIONAL_BODY_ADJUSTED_CORE",
      .data$evidence_ab ~ "PROVISIONAL_BODY_ADJUSTED_UTILITY",
      TRUE ~ "NOT_CARD_ELIGIBLE"
    ),
    body_adjusted_eligibility_reason = dplyr::case_when(
      .data$original_is_card_eligible %in% TRUE ~ "Preserved original Phase 16c card eligibility.",
      .data$body_adjusted_upgrade_candidate &
        .data$body_adjusted_move_classification == "PROVISIONAL_BODY_ADJUSTED_SIGNATURE" ~
        "Upgraded: A/B evidence, positive body-adjusted surplus, positive contribution, high ATK, and top surplus rank for player.",
      .data$body_adjusted_upgrade_candidate &
        .data$body_adjusted_move_classification == "PROVISIONAL_BODY_ADJUSTED_CORE" ~
        "Upgraded: A/B evidence, clearly positive body-adjusted surplus, positive contribution, and high ATK.",
      .data$body_adjusted_upgrade_candidate &
        .data$body_adjusted_move_classification == "PROVISIONAL_BODY_ADJUSTED_UTILITY" &
        .data$evidence_c ~
        "Upgraded: C evidence with positive body-adjusted surplus and positive contribution; capped at utility.",
      .data$body_adjusted_upgrade_candidate &
        .data$body_adjusted_move_classification == "PROVISIONAL_BODY_ADJUSTED_UTILITY" ~
        "Upgraded: A/B evidence with positive body-adjusted surplus and positive contribution; not strong enough for core/signature.",
      is.na(.data$evidence_tier_rank) ~ "Rejected: missing evidence tier.",
      .data$evidence_tier_rank < 3L ~ "Rejected: evidence tier below C.",
      !.data$body_adjusted_surplus_positive ~ "Rejected: body-adjusted contribution surplus is not positive.",
      !.data$positive_move_contribution ~ "Rejected: move contribution total is not positive.",
      TRUE ~ "Rejected: did not satisfy prototype body-adjusted upgrade rule."
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "move_name",
    "physical_attribute",
    "ATK_v2_score",
    "original_move_classification",
    "original_is_card_eligible",
    "evidence_tier",
    "evidence_score",
    "contribution_surplus",
    "body_adjusted_contribution_surplus",
    "body_adjusted_damage_surplus",
    "body_adjusted_frequency_surplus",
    "move_contribution_total",
    "observed_move_attempts",
    "observed_games",
    "move_attempts_per_game",
    "sample_size_tier",
    "move_available",
    "body_adjusted_move_eligible",
    "body_adjusted_move_classification",
    "body_adjusted_eligibility_reason",
    tidyselect::any_of(c(
      "body_adjusted_upgrade_candidate",
      "body_adjusted_upgrade_rank",
      "body_adjusted_surplus_clearly_positive",
      "evidence_tier_rank"
    ))
  ) %>%
  dplyr::arrange(
    dplyr::desc(.data$body_adjusted_move_eligible),
    dplyr::desc(!.data$original_is_card_eligible & .data$body_adjusted_move_eligible),
    dplyr::desc(.data$body_adjusted_contribution_surplus),
    dplyr::desc(.data$ATK_v2_score),
    .data$player_name,
    .data$move_name
  )

write_project_parquet(body_adjusted_move_eligibility, body_adjusted_move_eligibility_path)

newly_upgraded_moves <- body_adjusted_move_eligibility %>%
  dplyr::filter(!(.data$original_is_card_eligible %in% TRUE), .data$body_adjusted_move_eligible)

message("Phase 32 body-adjusted move eligibility diagnostics:")

message("Number originally eligible: ", sum(body_adjusted_move_eligibility$original_is_card_eligible, na.rm = TRUE))
message("Number body-adjusted eligible: ", sum(body_adjusted_move_eligibility$body_adjusted_move_eligible, na.rm = TRUE))
message("Number newly upgraded: ", nrow(newly_upgraded_moves))

message("Newly upgraded moves by evidence tier:")
print(
  newly_upgraded_moves %>%
    dplyr::count(.data$evidence_tier, sort = TRUE)
)

message("Newly upgraded moves by player:")
print(
  newly_upgraded_moves %>%
    dplyr::count(.data$player_name, sort = TRUE) %>%
    dplyr::slice_head(n = 50)
)

diagnostic_player_rows <- function(player_pattern) {
  body_adjusted_move_eligibility %>%
    dplyr::filter(stringr::str_detect(.data$player_name, player_pattern)) %>%
    dplyr::arrange(dplyr::desc(.data$body_adjusted_move_eligible), dplyr::desc(.data$body_adjusted_contribution_surplus)) %>%
    dplyr::select(
      "player_name",
      "move_name",
      "ATK_v2_score",
      "original_is_card_eligible",
      "body_adjusted_move_eligible",
      "body_adjusted_move_classification",
      "evidence_tier",
      "evidence_score",
      "contribution_surplus",
      "body_adjusted_contribution_surplus",
      "move_contribution_total",
      "observed_move_attempts",
      "body_adjusted_eligibility_reason"
    )
}

message("All rows for Shai Gilgeous-Alexander:")
print(diagnostic_player_rows("Shai Gilgeous-Alexander"))

message("All rows for Keyonte George:")
print(diagnostic_player_rows("Keyonte George"))

message("All rows for Luka Doncic / Luka Dončić:")
print(diagnostic_player_rows("Luka Don|Doncic|Dončić"))

message("All rows for LeBron James:")
print(diagnostic_player_rows("LeBron James"))

message("Top 50 newly upgraded moves by body-adjusted contribution surplus:")
print(
  newly_upgraded_moves %>%
    dplyr::arrange(dplyr::desc(.data$body_adjusted_contribution_surplus)) %>%
    dplyr::select(
      "player_name",
      "move_name",
      "ATK_v2_score",
      "body_adjusted_move_classification",
      "evidence_tier",
      "evidence_score",
      "contribution_surplus",
      "body_adjusted_contribution_surplus",
      "move_contribution_total",
      "observed_move_attempts",
      "body_adjusted_eligibility_reason"
    ) %>%
    dplyr::slice_head(n = 50)
)

message("Saved body-adjusted move eligibility prototype to: ", body_adjusted_move_eligibility_path)
message("Phase 32 note: prototype eligibility only. Phase 16c, 28, 29, 30, and 31 were not modified.")
