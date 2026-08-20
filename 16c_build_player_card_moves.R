# ============================================================
# 16c_build_player_card_moves.R
# Phase 16c: Provisional Card Move Eligibility & Classification.
#
# Purpose:
# Create the first card-generation layer that determines which offensive moves
# deserve to appear on a player's card.
#
# This phase does NOT determine final card moves. It only creates provisional
# move classifications from the currently observed sample.
#
# Philosophy:
# Not every player should have move text. Some players should currently be
# classified as Normal Cards. Move text should be earned by moves that are:
# - valuable
# - frequently used
# - characteristic of the player
# - supported by sufficient evidence
#
# Important:
# Baselines here are observed-sample baselines only. They are NOT full-league
# NBA baselines and should be replaced after all teams are processed.
#
# This phase does not modify ATK_v2, move contributions, damage calculations,
# confidence intervals, significance testing, or Bayesian shrinkage.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

atk_v2_path <- "outputs/player_card_ATK_v2.parquet"
player_card_moves_path <- "outputs/player_card_moves.parquet"

if (!file.exists(atk_v2_path)) {
  stop(
    "Missing Phase 16c input: ",
    atk_v2_path,
    ". Run 16b_build_ATK_v2.R first.",
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

safe_divide <- function(num, den) {
  num <- as.numeric(num)
  den <- as.numeric(den)
  out <- num / den
  out[is.na(num) | is.na(den) | den == 0] <- NA_real_
  out
}

evidence_score_from_sample <- function(observed_move_attempts, observed_games) {
  attempts <- pmax(0, suppressWarnings(as.numeric(observed_move_attempts)))
  games <- pmax(0, suppressWarnings(as.numeric(observed_games)))

  # Heuristic only. This intentionally avoids significance testing,
  # confidence intervals, and Bayesian posteriors.
  attempt_component <- 1 - exp(-attempts / 45)
  game_component <- pmin(sqrt(games / 20), 1)

  100 * attempt_component * game_component
}

evidence_tier_from_score <- function(evidence_score) {
  dplyr::case_when(
    is.na(evidence_score) ~ "F",
    evidence_score >= 80 ~ "A",
    evidence_score >= 60 ~ "B",
    evidence_score >= 40 ~ "C",
    evidence_score >= 20 ~ "D",
    TRUE ~ "F"
  )
}

evidence_tier_is_at_least_c <- function(evidence_tier) {
  evidence_tier %in% c("C", "B", "A")
}

atk_v2 <- read_project_parquet(atk_v2_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "move_name",
    "move_contribution",
    "observed_move_attempts",
    "observed_games",
    "move_attempts_per_game",
    "adjusted_expected_damage",
    "activation_probability",
    "move_available",
    "weapon_identity_score"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name = as.character(.data$player_name),
    move_name = as.character(.data$move_name),
    move_contribution = suppressWarnings(as.numeric(.data$move_contribution)),
    observed_move_attempts = suppressWarnings(as.numeric(.data$observed_move_attempts)),
    observed_games = suppressWarnings(as.numeric(.data$observed_games)),
    move_attempts_per_game = suppressWarnings(as.numeric(.data$move_attempts_per_game)),
    adjusted_expected_damage = suppressWarnings(as.numeric(.data$adjusted_expected_damage)),
    activation_probability = suppressWarnings(as.numeric(.data$activation_probability)),
    move_available = dplyr::coalesce(as.logical(.data$move_available), FALSE),
    weapon_identity_score = suppressWarnings(as.numeric(.data$weapon_identity_score))
  )

validate_columns(
  atk_v2,
  c(
    "player_id",
    "player_name",
    "move_name",
    "move_contribution",
    "move_available"
  )
)

player_move_summary <- atk_v2 %>%
  dplyr::group_by(.data$player_id, .data$player_name, .data$move_name) %>%
  dplyr::summarise(
    move_contribution_total = sum(.data$move_contribution, na.rm = TRUE),
    observed_move_attempts = suppressWarnings(max(.data$observed_move_attempts, na.rm = TRUE)),
    observed_games = suppressWarnings(max(.data$observed_games, na.rm = TRUE)),
    move_attempts_per_game = suppressWarnings(max(.data$move_attempts_per_game, na.rm = TRUE)),
    mean_adjusted_expected_damage = mean(.data$adjusted_expected_damage[.data$move_available], na.rm = TRUE),
    mean_activation_probability = mean(.data$activation_probability[.data$move_available], na.rm = TRUE),
    weapon_identity_score = suppressWarnings(max(.data$weapon_identity_score, na.rm = TRUE)),
    available_move = any(.data$move_available, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    observed_move_attempts = dplyr::if_else(is.infinite(.data$observed_move_attempts), NA_real_, .data$observed_move_attempts),
    observed_games = dplyr::if_else(is.infinite(.data$observed_games), NA_real_, .data$observed_games),
    move_attempts_per_game = dplyr::if_else(is.infinite(.data$move_attempts_per_game), NA_real_, .data$move_attempts_per_game),
    mean_adjusted_expected_damage = dplyr::if_else(is.nan(.data$mean_adjusted_expected_damage), NA_real_, .data$mean_adjusted_expected_damage),
    mean_activation_probability = dplyr::if_else(is.nan(.data$mean_activation_probability), NA_real_, .data$mean_activation_probability),
    weapon_identity_score = dplyr::if_else(is.infinite(.data$weapon_identity_score), NA_real_, .data$weapon_identity_score)
  )

sample_move_baselines <- player_move_summary %>%
  dplyr::filter(.data$available_move) %>%
  dplyr::group_by(.data$move_name) %>%
  dplyr::summarise(
    sample_move_damage = mean(.data$mean_adjusted_expected_damage, na.rm = TRUE),
    sample_move_frequency = mean(.data$move_attempts_per_game, na.rm = TRUE),
    sample_move_contribution = mean(.data$move_contribution_total, na.rm = TRUE),
    baseline_player_count = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    sample_move_damage = dplyr::if_else(is.nan(.data$sample_move_damage), NA_real_, .data$sample_move_damage),
    sample_move_frequency = dplyr::if_else(is.nan(.data$sample_move_frequency), NA_real_, .data$sample_move_frequency),
    sample_move_contribution = dplyr::if_else(is.nan(.data$sample_move_contribution), NA_real_, .data$sample_move_contribution),
    baseline_note = "Observed-sample move baseline only; not a final full-league NBA baseline."
  )

player_card_moves <- player_move_summary %>%
  dplyr::left_join(sample_move_baselines, by = "move_name") %>%
  dplyr::mutate(
    damage_surplus = .data$mean_adjusted_expected_damage - .data$sample_move_damage,
    frequency_surplus = .data$move_attempts_per_game - .data$sample_move_frequency,
    contribution_surplus = .data$move_contribution_total - .data$sample_move_contribution,
    evidence_score = evidence_score_from_sample(.data$observed_move_attempts, .data$observed_games),
    evidence_tier = evidence_tier_from_score(.data$evidence_score),
    evidence_methodology = paste(
      "Heuristic evidence score from observed move attempts and observed games.",
      "No significance tests, confidence intervals, or Bayesian posteriors."
    ),
    is_card_eligible = .data$available_move &
      !is.na(.data$contribution_surplus) &
      .data$contribution_surplus > 0 &
      evidence_tier_is_at_least_c(.data$evidence_tier)
  ) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::mutate(
    eligible_move_rank = dplyr::if_else(
      .data$is_card_eligible,
      rank(-.data$move_contribution_total, ties.method = "first"),
      NA_real_
    ),
    move_classification = dplyr::case_when(
      .data$is_card_eligible & .data$eligible_move_rank == 1 ~ "PROVISIONAL_SIGNATURE",
      .data$is_card_eligible & .data$eligible_move_rank >= 2 & .data$eligible_move_rank <= 4 ~ "PROVISIONAL_CORE",
      .data$is_card_eligible ~ "PROVISIONAL_UTILITY",
      TRUE ~ "NOT_CARD_ELIGIBLE"
    ),
    is_normal_card = !any(.data$is_card_eligible, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    move_card_note = dplyr::case_when(
      .data$is_card_eligible ~ paste(
        "Provisionally card-eligible against observed-sample baseline.",
        "Final eligibility should be recalculated with full-league baselines."
      ),
      .data$is_normal_card ~ paste(
        "Player currently has no provisionally eligible offensive moves and is classified as a Normal Card.",
        "This reflects observed pulled data only."
      ),
      !.data$available_move ~ "Move unavailable or below ATK_v2 sample threshold; not card-eligible.",
      !evidence_tier_is_at_least_c(.data$evidence_tier) ~ "Move has insufficient evidence tier for provisional card text.",
      is.na(.data$contribution_surplus) | .data$contribution_surplus <= 0 ~ "Move does not exceed observed-sample contribution baseline.",
      TRUE ~ "Move not provisionally card-eligible."
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "move_name",
    "move_classification",
    "is_card_eligible",
    "move_contribution_total",
    "move_attempts_per_game",
    "mean_adjusted_expected_damage",
    "damage_surplus",
    "frequency_surplus",
    "contribution_surplus",
    "evidence_score",
    "evidence_tier",
    "weapon_identity_score",
    "is_normal_card",
    "move_card_note",
    tidyselect::any_of(c(
      "observed_move_attempts",
      "observed_games",
      "mean_activation_probability",
      "sample_move_damage",
      "sample_move_frequency",
      "sample_move_contribution",
      "baseline_player_count",
      "baseline_note",
      "evidence_methodology",
      "eligible_move_rank"
    ))
  ) %>%
  dplyr::arrange(.data$player_name, .data$is_normal_card, .data$move_classification, dplyr::desc(.data$move_contribution_total))

write_project_parquet(player_card_moves, player_card_moves_path)

message("Phase 16c provisional card move diagnostics:")

requested_player_pattern <- "Luka Don|Doncic|LeBron James|Austin Reaves|Deandre Ayton|DeAndre Ayton|Jaxson Hayes|Luke Kennard|Shai Gilgeous-Alexander"

message("Requested player examples:")
print(
  player_card_moves %>%
    dplyr::filter(stringr::str_detect(.data$player_name, requested_player_pattern)) %>%
    dplyr::select(
      "player_name",
      "move_name",
      "move_classification",
      "is_card_eligible",
      "move_contribution_total",
      "move_attempts_per_game",
      "mean_adjusted_expected_damage",
      "contribution_surplus",
      "evidence_score",
      "evidence_tier",
      "is_normal_card",
      "move_card_note"
    ) %>%
    dplyr::arrange(.data$player_name, dplyr::desc(.data$is_card_eligible), dplyr::desc(.data$move_contribution_total))
)

message("Players with most eligible moves:")
print(
  player_card_moves %>%
    dplyr::filter(.data$is_card_eligible) %>%
    dplyr::count(.data$player_id, .data$player_name, name = "eligible_moves", sort = TRUE) %>%
    dplyr::slice_head(n = 25)
)

message("Players currently classified as normal cards:")
print(
  player_card_moves %>%
    dplyr::group_by(.data$player_id, .data$player_name) %>%
    dplyr::summarise(is_normal_card = dplyr::first(.data$is_normal_card), .groups = "drop") %>%
    dplyr::filter(.data$is_normal_card) %>%
    dplyr::arrange(.data$player_name) %>%
    dplyr::slice_head(n = 50)
)

message("Top provisional signature moves:")
print(
  player_card_moves %>%
    dplyr::filter(.data$move_classification == "PROVISIONAL_SIGNATURE") %>%
    dplyr::arrange(dplyr::desc(.data$move_contribution_total)) %>%
    dplyr::select(
      "player_name",
      "move_name",
      "move_contribution_total",
      "move_attempts_per_game",
      "mean_adjusted_expected_damage",
      "evidence_tier"
    ) %>%
    dplyr::slice_head(n = 30)
)

message("Most common provisional core moves:")
print(
  player_card_moves %>%
    dplyr::filter(.data$move_classification == "PROVISIONAL_CORE") %>%
    dplyr::count(.data$move_name, sort = TRUE)
)

message("Most common provisional utility moves:")
print(
  player_card_moves %>%
    dplyr::filter(.data$move_classification == "PROVISIONAL_UTILITY") %>%
    dplyr::count(.data$move_name, sort = TRUE)
)

message("Saved provisional card move classifications to: ", player_card_moves_path)
message("Phase 16c note: provisional card move eligibility only; final eligibility awaits full-league baselines.")
