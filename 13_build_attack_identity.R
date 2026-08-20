# ============================================================
# 13_build_attack_identity.R
# Phase 13: Attack identity / card-facing weapon selection.
#
# This phase converts raw attack movesets into descriptive, card-facing
# offensive weapon identity. It does NOT build final ATK/AP/DEF scores.
#
# Terminology:
# - Primary weapon = most used eligible attack from the moveset ranking.
# - Signature weapon = strongest meaningful weapon, requiring enough attempts,
#   enough attempt share, and strong expected damage/hit rate.
# - This is descriptive card identity, not final ATK.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_attack_library_path <- "outputs/attacks/player_attack_library.parquet"
player_movesets_path <- "outputs/attacks/player_movesets.parquet"
player_attack_identity_path <- "outputs/attacks/player_attack_identity.parquet"

if (!file.exists(player_attack_library_path)) {
  stop("Missing player attack library: ", player_attack_library_path, ". Run 11_build_player_attacks.R first.", call. = FALSE)
}

if (!file.exists(player_movesets_path)) {
  stop("Missing player movesets: ", player_movesets_path, ". Run 12_build_player_movesets.R first.", call. = FALSE)
}

safe_divide <- function(num, den) {
  dplyr::if_else(!is.na(den) & den > 0, num / den, NA_real_)
}

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

sample_size_multiplier <- function(sample_size_tier) {
  dplyr::case_when(
    sample_size_tier == "very_low" ~ 0.45,
    sample_size_tier == "low" ~ 0.70,
    sample_size_tier == "medium" ~ 0.90,
    sample_size_tier == "high" ~ 1.00,
    TRUE ~ 0.60
  )
}

attack_library <- read_project_parquet(player_attack_library_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    attempts = as.numeric(.data$attempts),
    expected_damage_per_attempt = as.numeric(.data$expected_damage_per_attempt),
    efficiency = as.numeric(.data$efficiency),
    reliability_sample_index = as.numeric(.data$reliability_sample_index)
  )

movesets <- read_project_parquet(player_movesets_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c("player_nickname"), NA_character_) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    total_attack_attempts = as.numeric(.data$total_attack_attempts),
    weighted_expected_damage = as.numeric(.data$weighted_expected_damage),
    weighted_hit_rate = as.numeric(.data$weighted_hit_rate),
    attack_diversity = as.numeric(.data$attack_diversity)
  )

validate_columns(
  attack_library,
  c(
    "player_id",
    "attack_family",
    "attack_variant",
    "attempts",
    "sample_game_count",
    "expected_damage_per_attempt",
    "efficiency",
    "reliability_sample_index"
  )
)

validate_columns(
  movesets,
  c(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "total_attack_attempts",
    "sample_size_tier",
    "weighted_expected_damage",
    "weighted_hit_rate",
    "attack_diversity",
    "primary_attack_family",
    "primary_attack_variant",
    "secondary_attack_family",
    "secondary_attack_variant",
    "tertiary_attack_family",
    "tertiary_attack_variant"
  )
)

weapon_rows <- attack_library %>%
  dplyr::left_join(
    movesets %>%
      dplyr::select("player_id", "total_attack_attempts"),
    by = "player_id"
  ) %>%
  dplyr::mutate(
    attempt_share = safe_divide(.data$attempts, .data$total_attack_attempts),
    # Transparent descriptive score:
    # damage per attempt * hit rate * square-rooted usage share. This rewards
    # efficient weapons with real presence without letting raw volume dominate.
    weapon_score = .data$expected_damage_per_attempt * .data$efficiency * sqrt(.data$attempt_share)
  )

observed_sample <- attack_library %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    # This is the summed attack-row game coverage available in the attack
    # library. It is a diagnostic sample indicator for the current PBP pull,
    # not a full-season games-played value.
    observed_game_count = sum(.data$sample_game_count, na.rm = TRUE),
    .groups = "drop"
  )

primary_attempt_shares <- weapon_rows %>%
  dplyr::inner_join(
    movesets %>%
      dplyr::select(
        "player_id",
        primary_weapon_family = "primary_attack_family",
        primary_weapon_variant = "primary_attack_variant"
      ),
    by = "player_id"
  ) %>%
  dplyr::filter(
    .data$attack_family == .data$primary_weapon_family,
    .data$attack_variant == .data$primary_weapon_variant
  ) %>%
  dplyr::select(
    "player_id",
    "primary_weapon_family",
    "primary_weapon_variant",
    primary_weapon_attempt_share = "attempt_share",
    primary_weapon_expected_damage = "expected_damage_per_attempt"
  )

eligible_signature_weapons <- weapon_rows %>%
  dplyr::filter(.data$attempts >= 25, .data$attempt_share >= 0.05)

signature_weapons <- if (nrow(eligible_signature_weapons) == 0) {
  tibble::tibble(
    player_id = character(),
    signature_weapon_family = character(),
    signature_weapon_variant = character(),
    signature_weapon_attempts = numeric(),
    signature_weapon_attempt_share = numeric(),
    signature_weapon_expected_damage = numeric(),
    signature_weapon_hit_rate = numeric(),
    signature_weapon_reliability = numeric(),
    signature_weapon_score = numeric()
  )
} else {
  eligible_signature_weapons %>%
    dplyr::arrange(
      .data$player_id,
      dplyr::desc(.data$weapon_score),
      dplyr::desc(.data$expected_damage_per_attempt),
      dplyr::desc(.data$attempts),
      .data$attack_family,
      .data$attack_variant
    ) %>%
    dplyr::group_by(.data$player_id) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "player_id",
      signature_weapon_family = "attack_family",
      signature_weapon_variant = "attack_variant",
      signature_weapon_attempts = "attempts",
      signature_weapon_attempt_share = "attempt_share",
      signature_weapon_expected_damage = "expected_damage_per_attempt",
      signature_weapon_hit_rate = "efficiency",
      signature_weapon_reliability = "reliability_sample_index",
      signature_weapon_score = "weapon_score"
    )
}

player_attack_identity <- movesets %>%
  dplyr::left_join(primary_attempt_shares, by = "player_id") %>%
  dplyr::left_join(signature_weapons, by = "player_id") %>%
  dplyr::left_join(observed_sample, by = "player_id") %>%
  dplyr::mutate(
    primary_weapon_family = dplyr::coalesce(.data$primary_weapon_family, .data$primary_attack_family),
    primary_weapon_variant = dplyr::coalesce(.data$primary_weapon_variant, .data$primary_attack_variant),
    secondary_weapon_family = .data$secondary_attack_family,
    secondary_weapon_variant = .data$secondary_attack_variant,
    tertiary_weapon_family = .data$tertiary_attack_family,
    tertiary_weapon_variant = .data$tertiary_attack_variant,
    observed_game_count = dplyr::coalesce(.data$observed_game_count, 0),
    observed_attack_sample_warning = dplyr::case_when(
      .data$total_attack_attempts < 25 ~ "very limited observation",
      .data$total_attack_attempts < 100 ~ "limited observation",
      .data$total_attack_attempts < 300 ~ "partial observation",
      TRUE ~ "substantial observation"
    ),
    weapon_diversity_score = .data$attack_diversity * sample_size_multiplier(.data$sample_size_tier),
    weapon_identity_score = 0.50 * dplyr::coalesce(.data$signature_weapon_score, 0) +
      0.30 * .data$weighted_expected_damage +
      0.20 * .data$weapon_diversity_score
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "total_attack_attempts",
    "observed_game_count",
    "observed_attack_sample_warning",
    "sample_size_tier",
    "weighted_expected_damage",
    "weighted_hit_rate",
    "attack_diversity",
    "primary_weapon_family",
    "primary_weapon_variant",
    "primary_weapon_attempt_share",
    "primary_weapon_expected_damage",
    "secondary_weapon_family",
    "secondary_weapon_variant",
    "tertiary_weapon_family",
    "tertiary_weapon_variant",
    "signature_weapon_family",
    "signature_weapon_variant",
    "signature_weapon_attempts",
    "signature_weapon_attempt_share",
    "signature_weapon_expected_damage",
    "signature_weapon_hit_rate",
    "signature_weapon_reliability",
    "signature_weapon_score",
    "weapon_diversity_score",
    "weapon_identity_score"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$weapon_identity_score), .data$player_name)

write_project_parquet(player_attack_identity, player_attack_identity_path)

message("Phase 13 attack identity diagnostics:")

message("Top signature weapons:")
print(
  player_attack_identity %>%
    dplyr::filter(!is.na(.data$signature_weapon_family)) %>%
    dplyr::arrange(dplyr::desc(.data$signature_weapon_score)) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "signature_weapon_family",
      "signature_weapon_variant",
      "signature_weapon_attempts",
      "signature_weapon_attempt_share",
      "signature_weapon_expected_damage",
      "signature_weapon_hit_rate",
      "signature_weapon_score"
    ) %>%
    utils::head(20)
)

players_without_signature <- player_attack_identity %>%
  dplyr::filter(is.na(.data$signature_weapon_family))

message("Players without eligible signature weapons: ", nrow(players_without_signature))
print(
  players_without_signature %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "total_attack_attempts",
      "observed_game_count",
      "observed_attack_sample_warning",
      "sample_size_tier"
    ) %>%
    utils::head(30)
)

message("Requested player examples, if available:")
example_pattern <- "Luka|Doncic|Dončić|LeBron|Austin Reaves|Shai|Gilgeous|Stephen Curry|Steph"
print(
  player_attack_identity %>%
    dplyr::filter(stringr::str_detect(.data$player_name, example_pattern) |
      stringr::str_detect(dplyr::coalesce(.data$player_nickname, ""), example_pattern)) %>%
    dplyr::select(
      "player_name",
      "player_nickname",
      "team_abbreviation",
      "total_attack_attempts",
      "observed_game_count",
      "observed_attack_sample_warning",
      "sample_size_tier",
      "primary_weapon_family",
      "primary_weapon_variant",
      "signature_weapon_family",
      "signature_weapon_variant",
      "signature_weapon_score",
      "weapon_diversity_score",
      "weapon_identity_score"
    )
)

message("Phase 13 note: attack identity is descriptive card-facing weapon selection only. No final ATK/AP/DEF scores were built.")
message("Phase 13 sample note: Current attack identities reflect only games available in the current PBP pull and should not be interpreted as full-season player cards.")
