# ============================================================
# 12_build_player_movesets.R
# Phase 12: Prototype player moveset construction.
#
# This phase rolls the player attack library up to one row per player. It is
# deliberately descriptive: it summarizes observed attack mix, expected damage,
# hit rate, reliability, diversity, and top/signature attacks.
#
# It does NOT build final ATK/AP/DEF scores, lineup/effect text metrics,
# fusion logic, attribute interactions, or Bayesian shrinkage.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_attack_library_path <- "outputs/attacks/player_attack_library.parquet"
player_movesets_path <- "outputs/attacks/player_movesets.parquet"
player_master_path <- glue("data/processed/player/player_master_{season}.parquet")

if (!file.exists(player_attack_library_path)) {
  stop(
    "Missing player attack library: ",
    player_attack_library_path,
    ". Run 11_build_player_attacks.R first.",
    call. = FALSE
  )
}

if (!file.exists(player_master_path)) {
  stop(
    "Missing player master data: ",
    player_master_path,
    ". Run 02_player_season.R first.",
    call. = FALSE
  )
}

mean_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

weighted_mean_or_na <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0

  if (sum(valid) == 0) {
    return(NA_real_)
  }

  stats::weighted.mean(x[valid], w[valid])
}

safe_divide <- function(num, den) {
  dplyr::if_else(!is.na(den) & den > 0, num / den, NA_real_)
}

normalized_entropy <- function(w) {
  w <- as.numeric(w)
  w <- w[!is.na(w) & w > 0]

  if (length(w) == 0) {
    return(NA_real_)
  }

  if (length(w) == 1) {
    return(0)
  }

  p <- w / sum(w)
  -sum(p * log(p)) / log(length(p))
}

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

attack_library <- read_project_parquet(player_attack_library_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c("raw_pbp_player_name", "player_nickname"), NA_character_) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    raw_pbp_player_name = dplyr::coalesce(.data$raw_pbp_player_name, as.character(.data$player_name)),
    attempts = as.numeric(.data$attempts),
    points = as.numeric(.data$points),
    expected_damage_per_attempt = as.numeric(.data$expected_damage_per_attempt),
    efficiency = as.numeric(.data$efficiency),
    reliability_sample_index = as.numeric(.data$reliability_sample_index)
  )

player_metadata <- read_project_parquet(player_master_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  dplyr::mutate(player_id = as.character(.data$player_id))

validate_columns(player_metadata, c("player_id", "player_name", "team_abbreviation"))

player_metadata <- player_metadata %>%
  dplyr::select(tidyselect::any_of(c(
    "player_id",
    "player_name",
    "nickname",
    "team_abbreviation"
  ))) %>%
  dplyr::rename(
    canonical_player_name = "player_name",
    canonical_team_abbreviation = "team_abbreviation"
  ) %>%
  dplyr::rename_with(
    ~ "player_nickname_master",
    tidyselect::any_of("nickname")
  ) %>%
  add_missing_cols(c("player_nickname_master"), NA_character_)

attack_library <- attack_library %>%
  dplyr::left_join(player_metadata, by = "player_id") %>%
  dplyr::mutate(
    player_name = dplyr::coalesce(.data$canonical_player_name, as.character(.data$player_name)),
    player_nickname = dplyr::coalesce(
      as.character(.data$player_nickname_master),
      as.character(.data$player_nickname)
    ),
    team_abbreviation = dplyr::coalesce(.data$canonical_team_abbreviation, as.character(.data$team_abbreviation))
  )

validate_columns(
  attack_library,
  c(
    "player_id",
    "player_name",
    "raw_pbp_player_name",
    "player_nickname",
    "team_abbreviation",
    "attack_family",
    "attack_variant",
    "attempts",
    "points",
    "expected_damage_per_attempt",
    "efficiency",
    "reliability_sample_index"
  )
)

player_base <- attack_library %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    player_name = dplyr::first(stats::na.omit(.data$player_name), default = NA_character_),
    player_nickname = dplyr::first(stats::na.omit(.data$player_nickname), default = NA_character_),
    raw_pbp_player_name = dplyr::first(stats::na.omit(.data$raw_pbp_player_name), default = NA_character_),
    team_abbreviation = dplyr::first(stats::na.omit(.data$team_abbreviation), default = NA_character_),
    total_attack_attempts = sum(.data$attempts, na.rm = TRUE),
    total_attack_points = sum(.data$points, na.rm = TRUE),
    weighted_expected_damage = weighted_mean_or_na(.data$expected_damage_per_attempt, .data$attempts),
    weighted_hit_rate = weighted_mean_or_na(.data$efficiency, .data$attempts),
    weighted_reliability = weighted_mean_or_na(.data$reliability_sample_index, .data$attempts),
    attack_diversity = normalized_entropy(.data$attempts),
    attack_count = sum(.data$attempts > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    sample_size_tier = dplyr::case_when(
      .data$total_attack_attempts < 50 ~ "very_low",
      .data$total_attack_attempts < 150 ~ "low",
      .data$total_attack_attempts < 400 ~ "medium",
      TRUE ~ "high"
    )
  )

ranked_attacks <- attack_library %>%
  dplyr::filter(.data$attempts >= 10) %>%
  dplyr::arrange(
    .data$player_id,
    dplyr::desc(.data$attempts),
    dplyr::desc(.data$expected_damage_per_attempt),
    .data$attack_family,
    .data$attack_variant
  ) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::mutate(attack_rank = dplyr::row_number()) %>%
  dplyr::ungroup()

top_attacks <- if (nrow(ranked_attacks) == 0) {
  tibble::tibble(
    player_id = character(),
    primary_attack_family = character(),
    primary_attack_variant = character(),
    primary_attack_attempts = numeric(),
    primary_attack_expected_damage = numeric(),
    secondary_attack_family = character(),
    secondary_attack_variant = character(),
    tertiary_attack_family = character(),
    tertiary_attack_variant = character()
  )
} else {
  ranked_attacks %>%
    dplyr::filter(.data$attack_rank <= 3) %>%
    dplyr::mutate(
      rank_label = dplyr::case_when(
        .data$attack_rank == 1 ~ "primary",
        .data$attack_rank == 2 ~ "secondary",
        .data$attack_rank == 3 ~ "tertiary",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(
      "player_id",
      "rank_label",
      "attack_family",
      "attack_variant",
      "attempts",
      "expected_damage_per_attempt"
    ) %>%
    tidyr::pivot_wider(
      names_from = "rank_label",
      values_from = c("attack_family", "attack_variant", "attempts", "expected_damage_per_attempt"),
      names_glue = "{rank_label}_{.value}"
    ) %>%
    add_missing_cols(c(
      "primary_attack_family",
      "primary_attack_variant",
      "primary_attempts",
      "primary_expected_damage_per_attempt",
      "secondary_attack_family",
      "secondary_attack_variant",
      "tertiary_attack_family",
      "tertiary_attack_variant"
    )) %>%
    dplyr::rename(
      primary_attack_family = "primary_attack_family",
      primary_attack_variant = "primary_attack_variant",
      primary_attack_attempts = "primary_attempts",
      primary_attack_expected_damage = "primary_expected_damage_per_attempt",
      secondary_attack_family = "secondary_attack_family",
      secondary_attack_variant = "secondary_attack_variant",
      tertiary_attack_family = "tertiary_attack_family",
      tertiary_attack_variant = "tertiary_attack_variant"
    )
}

signature_attacks <- attack_library %>%
  dplyr::filter(.data$attempts >= 25)

signature_attacks <- if (nrow(signature_attacks) == 0) {
  tibble::tibble(
    player_id = character(),
    signature_attack_family = character(),
    signature_attack_variant = character(),
    signature_attack_expected_damage = numeric(),
    signature_attack_attempts = numeric()
  )
} else {
  signature_attacks %>%
    dplyr::arrange(
      .data$player_id,
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
      signature_attack_family = "attack_family",
      signature_attack_variant = "attack_variant",
      signature_attack_expected_damage = "expected_damage_per_attempt",
      signature_attack_attempts = "attempts"
    )
}

player_movesets <- player_base %>%
  dplyr::left_join(top_attacks, by = "player_id") %>%
  dplyr::left_join(signature_attacks, by = "player_id") %>%
  dplyr::mutate(
    signature_attack_frequency = safe_divide(.data$signature_attack_attempts, .data$total_attack_attempts)
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "raw_pbp_player_name",
    "team_abbreviation",
    "total_attack_attempts",
    "total_attack_points",
    "weighted_expected_damage",
    "weighted_hit_rate",
    "weighted_reliability",
    "attack_diversity",
    "attack_count",
    "sample_size_tier",
    "primary_attack_family",
    "primary_attack_variant",
    "primary_attack_attempts",
    "primary_attack_expected_damage",
    "secondary_attack_family",
    "secondary_attack_variant",
    "tertiary_attack_family",
    "tertiary_attack_variant",
    "signature_attack_family",
    "signature_attack_variant",
    "signature_attack_expected_damage",
    "signature_attack_frequency"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$total_attack_attempts), .data$player_name)

write_project_parquet(player_movesets, player_movesets_path)

message("Phase 12 player moveset diagnostics:")
message("- Number of players: ", nrow(player_movesets))

message("Attack count distribution:")
print(
  player_movesets %>%
    dplyr::count(.data$attack_count, sort = TRUE, name = "players")
)

message("Sample size tier distribution:")
print(
  player_movesets %>%
    dplyr::count(.data$sample_size_tier, sort = TRUE, name = "players")
)

message("Top primary attacks:")
print(
  player_movesets %>%
    dplyr::count(.data$primary_attack_family, .data$primary_attack_variant, sort = TRUE, name = "players")
)

message("Requested player examples, if available:")
example_players <- c("Luka Dončić", "Luka Doncic", "LeBron James", "Austin Reaves", "Stephen Curry")
print(
  player_movesets %>%
    dplyr::filter(.data$player_name %in% example_players) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "total_attack_attempts",
      "sample_size_tier",
      "weighted_expected_damage",
      "attack_diversity",
      "primary_attack_family",
      "primary_attack_variant",
      "secondary_attack_family",
      "secondary_attack_variant",
      "signature_attack_family",
      "signature_attack_variant",
      "signature_attack_expected_damage",
      "signature_attack_frequency"
    )
)

message("Phase 12 note: player movesets are descriptive only. No final ATK/AP/DEF scores or lineup/effect text metrics were built.")
