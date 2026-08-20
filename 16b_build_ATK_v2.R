# ============================================================
# 16b_build_ATK_v2.R
# Phase 16b: Build move-context ATK v2.
#
# Phase 16 remains ATK v1 / diagnostic prototype. This script is intentionally
# separate and does not overwrite or modify 16_build_ATK_score.R.
#
# Core question:
# "If I hand this player the ball and ask them to create offense, how much
# damage can they realistically generate?"
#
# ATK_v2 comes from the player's observed move-context arsenal, not from a
# broad component blend:
#
# ATK_v2_raw =
#   sum(expected_frequency *
#       activation_probability *
#       success_probability *
#       adjusted_expected_damage)
#
# Important modeling notes:
# - Missing moves are not assumed average. If the player does not demonstrate a
#   move above the minimum sample threshold, contribution is 0.
# - Foul pressure is included as a damage modifier when measurable/proxied; it
#   is not a separate free-throw system.
# - Foul pressure is represented as a context label in this first pass, but a
#   future version should model it as a modifier layered onto moves.
# - CR, energy, possession/hustle effects, ecosystem/continuous effects, and
#   card display text are deliberately excluded.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_attack_library_path <- "outputs/attacks/player_attack_library.parquet"
player_movesets_path <- "outputs/attacks/player_movesets.parquet"
player_attack_identity_path <- "outputs/attacks/player_attack_identity.parquet"
move_variant_library_path <- "outputs/attacks/move_variant_library.parquet"
player_shot_context_path <- "outputs/attacks/player_shot_context.parquet"
player_shot_making_path <- "outputs/attacks/player_shot_making.parquet"
atk_v1_path <- "outputs/player_card_ATK.parquet"
atk_v2_output_path <- "outputs/player_card_ATK_v2.parquet"

required_inputs <- c(
  player_attack_library_path,
  player_movesets_path,
  player_attack_identity_path
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "Missing Phase 16b input(s): ",
    paste(missing_inputs, collapse = ", "),
    ". Run Phases 11, 12, and 13 first.",
    call. = FALSE
  )
}

min_move_attempts <- 10

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

safe_read_optional <- function(path, fallback_cols = "player_id") {
  if (!file.exists(path)) {
    out <- tibble::tibble()
    for (col in fallback_cols) {
      out[[col]] <- character()
    }
    return(out)
  }

  read_project_parquet(path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols() %>%
    add_missing_cols("player_id", NA_character_) %>%
    dplyr::mutate(player_id = as.character(.data$player_id))
}

coalesce_character_cols <- function(df, candidates) {
  matches <- candidates[candidates %in% names(df)]

  if (length(matches) == 0) {
    return(rep(NA_character_, nrow(df)))
  }

  values <- lapply(matches, function(col) as.character(df[[col]]))
  Reduce(dplyr::coalesce, values)
}

canonicalize_player_metadata_cols <- function(df) {
  df %>%
    dplyr::mutate(
      player_name = coalesce_character_cols(., c("player_name", "player_name.x", "player_name.y")),
      player_nickname = coalesce_character_cols(., c("player_nickname", "player_nickname.x", "player_nickname.y", "nickname", "nickname.x", "nickname.y")),
      team_abbreviation = coalesce_character_cols(., c(
        "team_abbreviation",
        "team_abbreviation.x",
        "team_abbreviation.y",
        "canonical_team_abbreviation",
        "team_abbr",
        "team_abbr.x",
        "team_abbr.y",
        "team_alias",
        "team_alias.x",
        "team_alias.y",
        "team"
      ))
    )
}

safe_divide <- function(num, den) {
  num <- as.numeric(num)
  den <- as.numeric(den)
  out <- num / den
  out[is.na(num) | is.na(den) | den == 0] <- NA_real_
  out
}

clamp <- function(x, lo = 0, hi = 1) {
  pmax(lo, pmin(hi, as.numeric(x)))
}

scale_0_100 <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  valid <- !is.na(x)

  if (sum(valid) == 0) {
    return(out)
  }

  x_valid <- x[valid]
  spread <- max(x_valid) - min(x_valid)

  if (spread == 0) {
    out[valid] <- 50
    return(out)
  }

  out[valid] <- (x_valid - min(x_valid)) / spread * 100
  out
}

infer_distance_tier <- function(attack_variant, shot_distance, three_point_attempts, attempts) {
  variant <- stringr::str_to_lower(as.character(attack_variant))
  dist <- suppressWarnings(as.numeric(shot_distance))
  three_share <- safe_divide(three_point_attempts, attempts)

  dplyr::case_when(
    !is.na(dist) & dist <= 3 ~ "rim",
    !is.na(dist) & dist <= 8 ~ "short_paint",
    !is.na(dist) & dist <= 16 ~ "short_midrange",
    !is.na(dist) & dist < 27 ~ "long_midrange",
    !is.na(dist) & dist >= 27 ~ "deep_three",
    !is.na(three_share) & three_share >= 0.50 & !is.na(dist) & dist >= 27 ~ "deep_three",
    !is.na(three_share) & three_share >= 0.50 ~ "three",
    stringr::str_detect(variant, "3|three|spot-up|catch") ~ "three",
    variant %in% c("dunk", "layup", "driving layup", "alley oop", "putback", "tip", "cut") ~ "rim",
    variant %in% c("floater", "hook", "short jump shot") ~ "short_paint",
    variant %in% c("midrange jump shot", "pullup jumper", "stepback jumper", "fadeaway jumper") ~ "long_midrange",
    TRUE ~ NA_character_
  )
}

activation_probability_for_move <- function(move_name, move_context) {
  move <- stringr::str_to_lower(as.character(move_name))
  context <- stringr::str_to_lower(as.character(move_context))

  base_activation <- dplyr::case_when(
    move %in% c("stepback jumper", "pullup jumper", "driving layup", "floater", "fadeaway jumper") ~ 0.92,
    move %in% c("midrange jump shot", "short jump shot", "hook") ~ 0.78,
    move %in% c("layup", "dunk") ~ 0.70,
    move %in% c("jump shot 3", "spot-up 3", "catch-and-shoot 3") ~ 0.52,
    move %in% c("cut", "alley oop") ~ 0.38,
    move %in% c("putback", "tip") ~ 0.24,
    TRUE ~ 0.50
  )

  context_multiplier <- dplyr::case_when(
    context == "half_court" ~ 1.00,
    context == "transition" ~ 0.68,
    context == "offensive_rebound_scramble" ~ 0.45,
    context == "foul_pressure" ~ 0.82,
    TRUE ~ 1.00
  )

  clamp(base_activation * context_multiplier)
}

context_for_move <- function(move_name) {
  move <- stringr::str_to_lower(as.character(move_name))

  dplyr::case_when(
    move %in% c("putback", "tip") ~ "offensive_rebound_scramble",
    stringr::str_detect(move, "transition") ~ "transition",
    move %in% c("driving layup", "dunk", "layup", "floater") ~ "foul_pressure",
    TRUE ~ "half_court"
  )
}

foul_pressure_value_for_move <- function(move_name, move_context) {
  move <- stringr::str_to_lower(as.character(move_name))
  context <- stringr::str_to_lower(as.character(move_context))

  dplyr::case_when(
    context != "foul_pressure" ~ 0,
    move %in% c("driving layup", "dunk") ~ 0.18,
    move %in% c("layup", "floater") ~ 0.10,
    TRUE ~ 0.05
  )
}

context_damage_multiplier <- function(move_context) {
  dplyr::case_when(
    move_context == "transition" ~ 1.05,
    move_context == "offensive_rebound_scramble" ~ 0.95,
    move_context == "foul_pressure" ~ 1.00,
    TRUE ~ 1.00
  )
}

dependency_note_for_move <- function(move_name, move_context, move_available, attempts) {
  move <- stringr::str_to_lower(as.character(move_name))
  context <- stringr::str_to_lower(as.character(move_context))

  dplyr::case_when(
    !move_available ~ paste0("Move unavailable below ", min_move_attempts, "-attempt sample threshold; contribution set to 0."),
    move %in% c("stepback jumper", "pullup jumper", "driving layup", "floater", "fadeaway jumper") ~ "High activation: player can usually access this move independently.",
    move %in% c("alley oop", "putback", "tip", "catch-and-shoot 3", "spot-up 3") | context == "offensive_rebound_scramble" ~ "Low activation: move depends on upstream creation, pass timing, or scramble conditions.",
    move == "jump shot 3" ~ "Moderate-low activation: generic threes may include assisted or setup-dependent attempts.",
    TRUE ~ paste0("Observed move-context with ", attempts, " attempts.")
  )
}

attack_library <- read_project_parquet(player_attack_library_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "attack_family",
    "attack_variant",
    "attempts",
    "makes",
    "points",
    "expected_damage_per_attempt",
    "points_per_attempt",
    "efficiency",
    "ppp",
    "frequency",
    "average_shot_distance",
    "median_shot_distance",
    "three_point_attempts",
    "two_point_attempts",
    "sample_game_count",
    "reliability_sample_index"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name = as.character(.data$player_name),
    player_nickname = as.character(.data$player_nickname),
    team_abbreviation = as.character(.data$team_abbreviation),
    attack_family = as.character(.data$attack_family),
    attack_variant = stringr::str_to_lower(as.character(.data$attack_variant)),
    attempts = suppressWarnings(as.numeric(.data$attempts)),
    makes = suppressWarnings(as.numeric(.data$makes)),
    points = suppressWarnings(as.numeric(.data$points)),
    expected_damage_per_attempt = dplyr::coalesce(
      suppressWarnings(as.numeric(.data$expected_damage_per_attempt)),
      suppressWarnings(as.numeric(.data$points_per_attempt)),
      suppressWarnings(as.numeric(.data$ppp))
    ),
    observed_success_probability = dplyr::coalesce(
      suppressWarnings(as.numeric(.data$efficiency)),
      safe_divide(.data$makes, .data$attempts)
    ),
    shot_distance = dplyr::coalesce(
      suppressWarnings(as.numeric(.data$average_shot_distance)),
      suppressWarnings(as.numeric(.data$median_shot_distance))
    ),
    three_point_attempts = suppressWarnings(as.numeric(.data$three_point_attempts)),
    two_point_attempts = suppressWarnings(as.numeric(.data$two_point_attempts)),
    sample_game_count = suppressWarnings(as.numeric(.data$sample_game_count)),
    distance_tier = infer_distance_tier(
      .data$attack_variant,
      .data$shot_distance,
      .data$three_point_attempts,
      .data$attempts
    )
  )

validate_columns(
  attack_library,
  c("player_id", "player_name", "attack_variant", "attempts", "expected_damage_per_attempt")
)

movesets <- read_project_parquet(player_movesets_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "team_abbr",
    "team_alias",
    "team",
    "total_attack_attempts"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    team_abbreviation = coalesce_character_cols(., c("team_abbreviation", "team_abbr", "team_alias", "team")),
    total_attack_attempts = suppressWarnings(as.numeric(.data$total_attack_attempts))
  )

attack_identity <- read_project_parquet(player_attack_identity_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  dplyr::mutate(player_id = as.character(.data$player_id))

move_variant_library <- safe_read_optional(move_variant_library_path, c("base_move", "move_variant")) %>%
  add_missing_cols(c("base_move", "move_variant", "observed_variant", "future_variant"), NA) %>%
  dplyr::mutate(
    base_move = stringr::str_to_lower(as.character(.data$base_move)),
    move_variant = stringr::str_to_lower(as.character(.data$move_variant)),
    observed_variant = dplyr::coalesce(as.logical(.data$observed_variant), FALSE),
    future_variant = dplyr::coalesce(as.logical(.data$future_variant), FALSE)
  )

shot_context <- safe_read_optional(player_shot_context_path) %>%
  add_missing_cols(c("player_id", "context_difficulty_score", "tight_defense_frequency", "late_clock_burden"), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    context_difficulty_score = suppressWarnings(as.numeric(.data$context_difficulty_score)),
    tight_defense_frequency = suppressWarnings(as.numeric(.data$tight_defense_frequency)),
    late_clock_burden = suppressWarnings(as.numeric(.data$late_clock_burden))
  )

shot_making <- safe_read_optional(player_shot_making_path) %>%
  add_missing_cols(c("player_id", "observed_hit_rate", "expected_hit_rate", "shot_making_surplus"), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    observed_hit_rate = suppressWarnings(as.numeric(.data$observed_hit_rate)),
    expected_hit_rate = suppressWarnings(as.numeric(.data$expected_hit_rate)),
    shot_making_surplus = suppressWarnings(as.numeric(.data$shot_making_surplus))
  )

atk_v1 <- safe_read_optional(atk_v1_path) %>%
  add_missing_cols(c("player_id", "observed_atk_score", "atk_score", "context_adjusted_atk_score"), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    ATK_v1_score = dplyr::coalesce(
      suppressWarnings(as.numeric(.data$observed_atk_score)),
      suppressWarnings(as.numeric(.data$atk_score)),
      suppressWarnings(as.numeric(.data$context_adjusted_atk_score))
    )
  ) %>%
  dplyr::select("player_id", "ATK_v1_score")

player_metadata <- movesets %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "total_attack_attempts"
  ) %>%
  dplyr::mutate(total_attack_attempts = suppressWarnings(as.numeric(.data$total_attack_attempts))) %>%
  dplyr::left_join(
    attack_identity %>%
      add_missing_cols(c("player_id", "weapon_identity_score", "sample_size_tier"), NA) %>%
      dplyr::select("player_id", "weapon_identity_score", "sample_size_tier"),
    by = "player_id"
  )

observed_move_universe <- attack_library %>%
  dplyr::filter(!is.na(.data$attack_variant), .data$attack_variant != "na") %>%
  dplyr::distinct(move_name = .data$attack_variant)

variant_universe <- move_variant_library %>%
  dplyr::filter(.data$observed_variant, !is.na(.data$move_variant), .data$move_variant != "na") %>%
  dplyr::transmute(move_name = .data$move_variant)

move_universe <- dplyr::bind_rows(observed_move_universe, variant_universe) %>%
  dplyr::distinct(.data$move_name) %>%
  dplyr::filter(!is.na(.data$move_name), .data$move_name != "")

if (nrow(move_universe) == 0) {
  stop("No moves found in current attack outputs; cannot build ATK_v2.", call. = FALSE)
}

distance_baselines <- attack_library %>%
  dplyr::filter(!is.na(.data$distance_tier)) %>%
  dplyr::group_by(.data$distance_tier) %>%
  dplyr::summarise(
    distance_tier_makes = sum(.data$makes, na.rm = TRUE),
    distance_tier_attempts = sum(.data$attempts, na.rm = TRUE),
    distance_tier_success_baseline = safe_divide(.data$distance_tier_makes, .data$distance_tier_attempts),
    distance_tier_damage_baseline = safe_divide(sum(.data$points, na.rm = TRUE), .data$distance_tier_attempts),
    .groups = "drop"
  )

overall_success_baseline <- safe_divide(sum(attack_library$makes, na.rm = TRUE), sum(attack_library$attempts, na.rm = TRUE))
overall_damage_baseline <- safe_divide(sum(attack_library$points, na.rm = TRUE), sum(attack_library$attempts, na.rm = TRUE))

player_observed_games <- attack_library %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    observed_games = suppressWarnings(max(.data$sample_game_count, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    observed_games = dplyr::if_else(is.infinite(.data$observed_games), NA_real_, .data$observed_games)
  )

move_observations <- attack_library %>%
  dplyr::group_by(
    .data$player_id,
    move_name = .data$attack_variant
  ) %>%
  dplyr::summarise(
    observed_move_attempts = sum(.data$attempts, na.rm = TRUE),
    observed_makes = sum(.data$makes, na.rm = TRUE),
    observed_points = sum(.data$points, na.rm = TRUE),
    shot_distance = stats::weighted.mean(.data$shot_distance, dplyr::if_else(is.na(.data$attempts), 0, .data$attempts), na.rm = TRUE),
    three_point_attempts = sum(.data$three_point_attempts, na.rm = TRUE),
    two_point_attempts = sum(.data$two_point_attempts, na.rm = TRUE),
    observed_expected_damage = stats::weighted.mean(
      .data$expected_damage_per_attempt,
      dplyr::if_else(is.na(.data$attempts), 0, .data$attempts),
      na.rm = TRUE
    ),
    observed_success_probability = safe_divide(.data$observed_makes, .data$observed_move_attempts),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    observed_attempts = .data$observed_move_attempts,
    shot_distance = dplyr::if_else(is.nan(.data$shot_distance), NA_real_, .data$shot_distance),
    observed_expected_damage = dplyr::if_else(is.nan(.data$observed_expected_damage), NA_real_, .data$observed_expected_damage),
    distance_tier = infer_distance_tier(
      .data$move_name,
      .data$shot_distance,
      .data$three_point_attempts,
      .data$observed_move_attempts
    )
  )

player_move_grid <- tidyr::crossing(
  player_id = player_metadata$player_id,
  move_universe
) %>%
  dplyr::left_join(player_metadata, by = "player_id") %>%
  dplyr::left_join(move_observations, by = c("player_id", "move_name")) %>%
  dplyr::left_join(distance_baselines, by = "distance_tier") %>%
  dplyr::left_join(player_observed_games, by = "player_id") %>%
  dplyr::left_join(shot_context, by = "player_id") %>%
  dplyr::left_join(shot_making, by = "player_id") %>%
  canonicalize_player_metadata_cols() %>%
  dplyr::mutate(
    observed_move_attempts = dplyr::coalesce(.data$observed_move_attempts, 0),
    observed_attempts = dplyr::coalesce(.data$observed_attempts, 0),
    observed_makes = dplyr::coalesce(.data$observed_makes, 0),
    observed_points = dplyr::coalesce(.data$observed_points, 0),
    move_context = context_for_move(.data$move_name),
    move_available = .data$observed_move_attempts >= min_move_attempts,
    move_attempt_share = dplyr::if_else(
      !is.na(.data$total_attack_attempts) & .data$total_attack_attempts > 0,
      .data$observed_move_attempts / .data$total_attack_attempts,
      NA_real_
    ),
    move_attempts_per_game = safe_divide(.data$observed_move_attempts, .data$observed_games),
    expected_frequency_source = dplyr::case_when(
      .data$move_available & !is.na(.data$move_attempts_per_game) ~ "move_attempts_per_observed_player_game",
      .data$move_available ~ "unavailable_absolute_frequency_proxy",
      TRUE ~ paste0("below_", min_move_attempts, "_attempt_sample_threshold")
    ),
    expected_frequency = dplyr::if_else(
      .data$move_available & !is.na(.data$move_attempts_per_game),
      .data$move_attempts_per_game,
      0
    ),
    activation_probability = dplyr::if_else(
      .data$move_available,
      activation_probability_for_move(.data$move_name, .data$move_context),
      0
    ),
    distance_tier_success_baseline = dplyr::coalesce(.data$distance_tier_success_baseline, overall_success_baseline),
    distance_tier_damage_baseline = dplyr::coalesce(.data$distance_tier_damage_baseline, overall_damage_baseline),
    success_over_distance_expectation = safe_divide(.data$observed_success_probability, .data$distance_tier_success_baseline),
    distance_adjusted_success_probability = dplyr::if_else(
      .data$move_available,
      clamp(
        dplyr::coalesce(.data$distance_tier_success_baseline, overall_success_baseline) *
          dplyr::coalesce(.data$success_over_distance_expectation, 1),
        0,
        1
      ),
      NA_real_
    ),
    success_probability = dplyr::if_else(
      .data$move_available,
      .data$distance_adjusted_success_probability,
      0
    ),
    inferred_made_shot_value = dplyr::case_when(
      safe_divide(.data$three_point_attempts, .data$observed_attempts) >= 0.50 ~ 3,
      .data$distance_tier %in% c("three", "deep_three") ~ 3,
      TRUE ~ 2
    ),
    foul_pressure_value = foul_pressure_value_for_move(.data$move_name, .data$move_context),
    pre_fix_adjusted_expected_damage_reconstructed = dplyr::if_else(
      .data$move_available,
      (.data$inferred_made_shot_value + .data$foul_pressure_value) *
        context_damage_multiplier(.data$move_context) *
        dplyr::case_when(
          !is.na(.data$context_difficulty_score) ~ 1 + clamp(.data$context_difficulty_score / 100, 0, 1) * 0.08,
          TRUE ~ 1
        ) *
        dplyr::case_when(
          !is.na(.data$shot_making_surplus) ~ 1 + clamp(.data$shot_making_surplus, -0.20, 0.20),
          TRUE ~ 1
        ),
      NA_real_
    ),
    # adjusted_expected_damage must stay on expected-points / PPP scale. The
    # attack library's observed_expected_damage already captures shot value
    # and make probability, so do not rebuild it from raw 2PT/3PT shot value.
    # Context difficulty is a 0-100 descriptive burden score, not PPP, so it is
    # rescaled into a very small additive expected-points credit. Shot-making
    # surplus is a hit-rate surplus and is converted to expected-points scale by
    # multiplying by the inferred made-shot value.
    context_difficulty_modifier = dplyr::case_when(
      !is.na(.data$context_difficulty_score) ~ clamp(.data$context_difficulty_score / 100, 0, 1) * 0.08,
      TRUE ~ 0
    ),
    shot_making_modifier = dplyr::case_when(
      !is.na(.data$shot_making_surplus) ~ clamp(.data$shot_making_surplus, -0.20, 0.20) * .data$inferred_made_shot_value,
      TRUE ~ 0
    ),
    adjusted_expected_damage = dplyr::if_else(
      .data$move_available,
      .data$observed_expected_damage +
        .data$foul_pressure_value +
        .data$context_difficulty_modifier +
        .data$shot_making_modifier,
      NA_real_
    ),
    move_contribution = dplyr::if_else(
      .data$move_available,
      .data$expected_frequency *
        .data$activation_probability *
        .data$success_probability *
        .data$adjusted_expected_damage,
      0
    ),
    dependency_note = dependency_note_for_move(
      .data$move_name,
      .data$move_context,
      .data$move_available,
      .data$observed_attempts
    )
  )

player_atk_v2_totals <- player_move_grid %>%
  dplyr::group_by(
    .data$player_id,
    .data$player_name,
    .data$player_nickname,
    .data$team_abbreviation
  ) %>%
  dplyr::summarise(
    ATK_v2_raw = sum(.data$move_contribution, na.rm = TRUE),
    available_move_count = sum(.data$move_available, na.rm = TRUE),
    cumulative_available_frequency = sum(.data$expected_frequency, na.rm = TRUE),
    total_move_attempts_per_game = sum(.data$move_attempts_per_game[.data$move_available], na.rm = TRUE),
    mean_damage = mean(.data$adjusted_expected_damage[.data$move_available], na.rm = TRUE),
    max_single_move_contribution = suppressWarnings(max(.data$move_contribution, na.rm = TRUE)),
    total_attack_attempts = dplyr::first(.data$total_attack_attempts),
    observed_games = dplyr::first(.data$observed_games),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    mean_damage = dplyr::if_else(is.nan(.data$mean_damage), NA_real_, .data$mean_damage),
    max_single_move_contribution = dplyr::if_else(is.infinite(.data$max_single_move_contribution), NA_real_, .data$max_single_move_contribution),
    ATK_v2_score = scale_0_100(.data$ATK_v2_raw),
    ATK_v2_note = paste(
      "ATK_v2 is cumulative move-context offensive damage potential.",
      paste0("Moves below ", min_move_attempts, " attempts contribute 0."),
      "No neutral defaults, CR, energy, possession/hustle, or ecosystem effects are included."
    )
  )

player_card_ATK_v2 <- player_move_grid %>%
  dplyr::left_join(
    player_atk_v2_totals %>%
      dplyr::select(
        "player_id",
        "ATK_v2_raw",
        "ATK_v2_score",
        "ATK_v2_note",
        "available_move_count",
        "cumulative_available_frequency",
        "total_move_attempts_per_game",
        "mean_damage",
        "max_single_move_contribution"
      ),
    by = "player_id"
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "ATK_v2_raw",
    "ATK_v2_score",
    "ATK_v2_note",
    move_name = "move_name",
    move_context = "move_context",
    move_available = "move_available",
    move_attempt_share = "move_attempt_share",
    expected_frequency = "expected_frequency",
    expected_frequency_source = "expected_frequency_source",
    activation_probability = "activation_probability",
    success_probability = "success_probability",
    adjusted_expected_damage = "adjusted_expected_damage",
    move_contribution = "move_contribution",
    dependency_note = "dependency_note",
    "shot_distance",
    "distance_tier",
    "distance_adjusted_success_probability",
    "observed_move_attempts",
    "observed_games",
    "move_attempts_per_game",
    "observed_attempts",
    "observed_makes",
    "observed_points",
    "observed_expected_damage",
    "distance_tier_success_baseline",
    "success_over_distance_expectation",
    "foul_pressure_value",
    "context_difficulty_modifier",
    "shot_making_modifier",
    "available_move_count",
    "cumulative_available_frequency",
    "total_move_attempts_per_game",
    "mean_damage",
    "max_single_move_contribution",
    tidyselect::any_of(c("sample_size_tier", "weapon_identity_score"))
  ) %>%
  dplyr::arrange(dplyr::desc(.data$ATK_v2_score), .data$player_name, dplyr::desc(.data$move_contribution))

write_project_parquet(player_card_ATK_v2, atk_v2_output_path)

message("Phase 16b ATK_v2 diagnostics:")

message("Top ATK_v2 players:")
print(
  player_atk_v2_totals %>%
    dplyr::arrange(dplyr::desc(.data$ATK_v2_score)) %>%
    dplyr::slice_head(n = 25)
)

requested_player_pattern <- "Luka Don|Dončić|Doncic|LeBron James|Austin Reaves|Deandre Ayton|DeAndre Ayton|Jaxson Hayes|Luke Kennard|Stephen Curry|Giannis Antetokounmpo|Nikola Jokic|Jokić"

message("Requested player examples:")
print(
  player_atk_v2_totals %>%
    dplyr::filter(stringr::str_detect(.data$player_name, requested_player_pattern)) %>%
    dplyr::arrange(dplyr::desc(.data$ATK_v2_score))
)

message("Expected-frequency burden summaries for requested players:")
print(
  player_atk_v2_totals %>%
    dplyr::filter(stringr::str_detect(.data$player_name, "Luka Don|Dončić|Doncic|LeBron James|Austin Reaves|Deandre Ayton|DeAndre Ayton|Jaxson Hayes|Luke Kennard|Shai Gilgeous-Alexander")) %>%
    dplyr::transmute(
      player_name = .data$player_name,
      total_contribution = .data$ATK_v2_raw,
      total_frequency = .data$cumulative_available_frequency,
      move_attempts_per_game_total = .data$total_move_attempts_per_game,
      mean_damage = .data$mean_damage,
      available_move_count = .data$available_move_count,
      observed_games = .data$observed_games,
      ATK_v2_score = .data$ATK_v2_score
    ) %>%
    dplyr::arrange(dplyr::desc(.data$ATK_v2_score))
)

message("Move contribution breakdowns for requested players:")
print(
  player_card_ATK_v2 %>%
    dplyr::filter(stringr::str_detect(.data$player_name, requested_player_pattern), .data$move_contribution > 0) %>%
    dplyr::group_by(.data$player_name) %>%
    dplyr::arrange(dplyr::desc(.data$move_contribution), .by_group = TRUE) %>%
    dplyr::slice_head(n = 8) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "player_name",
      "ATK_v2_score",
      "move_name",
      "move_context",
      "expected_frequency",
      "activation_probability",
      "success_probability",
      "adjusted_expected_damage",
      "move_contribution",
      "dependency_note"
    )
)

message("Before/after damage construction examples for Luka and Jaxson Hayes:")
print(
  player_move_grid %>%
    dplyr::filter(stringr::str_detect(.data$player_name, "Luka Don|Dončić|Doncic|Jaxson Hayes"), .data$move_available) %>%
    dplyr::arrange(.data$player_name, dplyr::desc(.data$move_contribution)) %>%
    dplyr::group_by(.data$player_name) %>%
    dplyr::slice_head(n = 8) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "player_name",
      "move_name",
      "move_context",
      "observed_expected_damage",
      previous_reconstructed_damage = "pre_fix_adjusted_expected_damage_reconstructed",
      "foul_pressure_value",
      "context_difficulty_modifier",
      "shot_making_modifier",
      "adjusted_expected_damage"
    )
)

message("Suspicious adjusted_expected_damage values above 2.2 PPP scale:")
suspicious_adjusted_damage <- player_card_ATK_v2 %>%
  dplyr::filter(.data$move_available, !is.na(.data$adjusted_expected_damage), .data$adjusted_expected_damage > 2.2) %>%
  dplyr::mutate(
    clearly_justified = .data$move_context == "foul_pressure" &
      !is.na(.data$foul_pressure_value) &
      .data$foul_pressure_value > 0 &
      .data$adjusted_expected_damage <= 2.35,
    damage_flag = dplyr::if_else(
      .data$clearly_justified,
      "above 2.2 but plausibly explained by foul-pressure modifier",
      "suspicious: review for double-counted expected points"
    )
  ) %>%
  dplyr::filter(!.data$clearly_justified) %>%
  dplyr::arrange(dplyr::desc(.data$adjusted_expected_damage))

if (nrow(suspicious_adjusted_damage) > 0) {
  warning(
    "Suspicious adjusted_expected_damage values above 2.2 found. Review move damage construction before using ATK_v2 as a card score.",
    call. = FALSE
  )
}

print(
  suspicious_adjusted_damage %>%
    dplyr::select(
      "player_name",
      "move_name",
      "move_context",
      "observed_expected_damage",
      "foul_pressure_value",
      "context_difficulty_modifier",
      "shot_making_modifier",
      "adjusted_expected_damage",
      "damage_flag"
    ) %>%
    dplyr::slice_head(n = 30)
)

if (file.exists(atk_v1_path)) {
  message("Players whose ATK changed most from Phase 16 ATK v1 to ATK_v2:")
  print(
    player_atk_v2_totals %>%
      dplyr::left_join(atk_v1, by = "player_id") %>%
      dplyr::mutate(ATK_change_v2_minus_v1 = .data$ATK_v2_score - .data$ATK_v1_score) %>%
      dplyr::filter(!is.na(.data$ATK_change_v2_minus_v1)) %>%
      dplyr::arrange(dplyr::desc(abs(.data$ATK_change_v2_minus_v1))) %>%
      dplyr::slice_head(n = 25)
  )
} else {
  message("ATK v1 comparison skipped because outputs/player_card_ATK.parquet is unavailable.")
}

message("Most damaging move-context combinations:")
print(
  player_card_ATK_v2 %>%
    dplyr::filter(.data$move_available) %>%
    dplyr::arrange(dplyr::desc(.data$move_contribution)) %>%
    dplyr::select(
      "player_name",
      "move_name",
      "move_context",
      "expected_frequency",
      "activation_probability",
      "success_probability",
      "adjusted_expected_damage",
      "move_contribution"
    ) %>%
    dplyr::slice_head(n = 30)
)

message("Players with high single-move damage but low cumulative ATK_v2:")
print(
  player_atk_v2_totals %>%
    dplyr::filter(!is.na(.data$ATK_v2_score), !is.na(.data$max_single_move_contribution)) %>%
    dplyr::mutate(single_move_dependency_share = safe_divide(.data$max_single_move_contribution, .data$ATK_v2_raw)) %>%
    dplyr::filter(.data$single_move_dependency_share >= 0.60, .data$available_move_count <= 2) %>%
    dplyr::arrange(dplyr::desc(.data$max_single_move_contribution)) %>%
    dplyr::slice_head(n = 25)
)

message("Players with broad move arsenals and high cumulative ATK_v2:")
print(
  player_atk_v2_totals %>%
    dplyr::filter(.data$available_move_count >= 4) %>%
    dplyr::arrange(dplyr::desc(.data$ATK_v2_score), dplyr::desc(.data$available_move_count)) %>%
    dplyr::slice_head(n = 25)
)

message("Ecosystem-dependent moves with high PPP but low activation contribution:")
print(
  player_card_ATK_v2 %>%
    dplyr::filter(
      .data$move_available,
      .data$move_name %in% c("alley oop", "putback", "tip", "catch-and-shoot 3", "spot-up 3", "cut"),
      .data$observed_expected_damage >= 1.15
    ) %>%
    dplyr::arrange(.data$activation_probability, dplyr::desc(.data$observed_expected_damage)) %>%
    dplyr::select(
      "player_name",
      "move_name",
      "move_context",
      "observed_expected_damage",
      "activation_probability",
      "move_contribution",
      "dependency_note"
    ) %>%
    dplyr::slice_head(n = 30)
)

message("Saved ATK_v2 move-context table to: ", atk_v2_output_path)
message("Phase 16b note: ATK_v2 is a move-context prototype only. No previous phase was modified.")
