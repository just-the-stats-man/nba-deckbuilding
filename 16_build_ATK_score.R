# ============================================================
# 16_build_ATK_score.R
# Phase 16: Prototype ATK score.
#
# ATK is a first-pass offensive card score for the overall offensive package.
# It is not an individual moves score: signature attacks remain descriptive
# card-body information from Phase 13.
#
# High-efficiency finishing is valuable, but ATK should not treat rim-only
# assisted finishers as elite all-around scorers. The observed score therefore
# includes range, scalable volume, and a modest rim-dependency penalty.
#
# ATK should primarily reflect self-created one-on-one scoring power. Assisted
# shots still matter because difficult execution can be valuable, but
# assisted-dependent profiles are discounted relative to self-created attacks.
#
# Prototype only. This is not adjusted for teammates, opponent strength,
# lineup context, role, or Bayesian shrinkage. Shot context and shot-making
# coverage are optional because current tracking-shot pulls may cover only a
# small set of requested players.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_attack_library_path <- "outputs/attacks/player_attack_library.parquet"
player_movesets_path <- "outputs/attacks/player_movesets.parquet"
player_attack_identity_path <- "outputs/attacks/player_attack_identity.parquet"
player_shot_context_path <- "outputs/attacks/player_shot_context.parquet"
player_shot_making_path <- "outputs/attacks/player_shot_making.parquet"
atk_output_path <- "outputs/player_card_ATK.parquet"

required_inputs <- c(
  player_attack_library_path,
  player_movesets_path,
  player_attack_identity_path
)

missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "Missing Phase 16 input(s): ",
    paste(missing_inputs, collapse = ", "),
    ". Run Phases 11, 12, and 13 first.",
    call. = FALSE
  )
}

scale_0_100 <- function(x, neutral = 50) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  valid <- !is.na(x)

  if (sum(valid) == 0) {
    return(out)
  }

  x_valid <- x[valid]
  spread <- max(x_valid) - min(x_valid)

  if (spread == 0) {
    out[valid] <- neutral
    return(out)
  }

  out[valid] <- (x_valid - min(x_valid)) / spread * 100
  out
}

clamp_0_100 <- function(x) {
  pmax(0, pmin(100, as.numeric(x)))
}

coalesce_numeric <- function(x, fallback = 0) {
  dplyr::coalesce(as.numeric(x), fallback)
}

safe_divide <- function(num, den) {
  dplyr::if_else(!is.na(den) & den > 0, num / den, NA_real_)
}

weighted_mean_or_na <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0

  if (sum(valid) == 0) {
    return(NA_real_)
  }

  stats::weighted.mean(x[valid], w[valid])
}

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

optional_input_exists <- function(path) {
  file.exists(path)
}

attack_library <- read_project_parquet(player_attack_library_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c(
    "expected_damage_per_attempt",
    "points_per_attempt",
    "attempts",
    "attack_variant",
    "assisted_attempts",
    "self_created_attempts",
    "assisted_attempt_rate",
    "self_created_attempt_rate",
    "creation_signal_available"
  ), NA_real_) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    attack_variant = stringr::str_to_lower(as.character(.data$attack_variant)),
    attempts = suppressWarnings(as.numeric(.data$attempts)),
    attack_expected_damage = dplyr::coalesce(
      suppressWarnings(as.numeric(.data$expected_damage_per_attempt)),
      suppressWarnings(as.numeric(.data$points_per_attempt))
    ),
    assisted_attempts = suppressWarnings(as.numeric(.data$assisted_attempts)),
    self_created_attempts = suppressWarnings(as.numeric(.data$self_created_attempts)),
    assisted_attempt_rate = suppressWarnings(as.numeric(.data$assisted_attempt_rate)),
    self_created_attempt_rate = suppressWarnings(as.numeric(.data$self_created_attempt_rate)),
    creation_signal_available = dplyr::coalesce(as.logical(.data$creation_signal_available), FALSE),
    assisted_attempt_rate = dplyr::coalesce(
      .data$assisted_attempt_rate,
      safe_divide(.data$assisted_attempts, .data$attempts),
      1 - .data$self_created_attempt_rate
    ),
    self_created_attempt_rate = dplyr::coalesce(
      .data$self_created_attempt_rate,
      safe_divide(.data$self_created_attempts, .data$attempts),
      1 - .data$assisted_attempt_rate
    ),
    assisted_attempt_rate = dplyr::if_else(
      .data$creation_signal_available,
      pmax(0, pmin(1, .data$assisted_attempt_rate)),
      NA_real_
    ),
    self_created_attempt_rate = dplyr::if_else(
      .data$creation_signal_available,
      pmax(0, pmin(1, .data$self_created_attempt_rate)),
      NA_real_
    ),
    assisted_attempts_estimated = .data$attempts * .data$assisted_attempt_rate,
    self_created_attempts_estimated = .data$attempts * .data$self_created_attempt_rate
  )

movesets <- read_project_parquet(player_movesets_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  dplyr::mutate(player_id = as.character(.data$player_id))

attack_identity <- read_project_parquet(player_attack_identity_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  dplyr::mutate(player_id = as.character(.data$player_id))

shot_context_file_available <- optional_input_exists(player_shot_context_path)
shot_making_file_available <- optional_input_exists(player_shot_making_path)

shot_context <- if (shot_context_file_available) {
  read_project_parquet(player_shot_context_path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols() %>%
    dplyr::mutate(player_id = as.character(.data$player_id))
} else {
  tibble::tibble(
    player_id = character(),
    context_difficulty_score = numeric()
  )
}

shot_making <- if (shot_making_file_available) {
  read_project_parquet(player_shot_making_path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols() %>%
    dplyr::mutate(player_id = as.character(.data$player_id)) %>%
    add_missing_cols(c(
      "observed_hit_rate",
      "expected_hit_rate",
      "shot_making_surplus",
      "contest_resistance_raw",
      "creation_burden_raw",
      "shot_making_component"
    ), NA_real_)
} else {
  tibble::tibble(
    player_id = character(),
    observed_hit_rate = numeric(),
    expected_hit_rate = numeric(),
    shot_making_surplus = numeric(),
    contest_resistance_raw = numeric(),
    creation_burden_raw = numeric(),
    shot_making_component = numeric()
  )
}

validate_columns(
  attack_library,
  c(
    "player_id",
    "attack_variant",
    "attempts"
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
    "weighted_expected_damage",
    "weighted_hit_rate",
    "attack_diversity"
  )
)

validate_columns(
  attack_identity,
  c(
    "player_id",
    "weapon_identity_score",
    "signature_weapon_family",
    "signature_weapon_variant",
    "signature_weapon_score"
  )
)

if (shot_context_file_available) {
  validate_columns(
    shot_context,
    c(
      "player_id",
      "context_difficulty_score"
    )
  )
}

if (shot_making_file_available) {
  validate_columns(
    shot_making,
    c(
      "player_id",
      "observed_hit_rate",
      "expected_hit_rate",
      "shot_making_surplus",
      "contest_resistance_raw",
      "creation_burden_raw"
    )
  )
}

shot_making_player_count <- if (shot_making_file_available) {
  shot_making %>%
    dplyr::filter(!is.na(.data$shot_making_surplus)) %>%
    dplyr::summarise(players = dplyr::n_distinct(.data$player_id)) %>%
    dplyr::pull(.data$players)
} else {
  0L
}

if (length(shot_making_player_count) == 0 || is.na(shot_making_player_count)) {
  shot_making_player_count <- 0L
}

context_adjusted_ATK_pool_available <- shot_making_player_count >= 30
context_adjusted_ATK_insufficient_note <- "Insufficient tracking-shot player pool for normalized context-adjusted ATK."

range_attack_variants <- c(
  "jump shot 3",
  "midrange jump shot",
  "short jump shot",
  "pullup jumper",
  "stepback jumper",
  "fadeaway jumper",
  "floater",
  "hook"
)

rim_dependency_variants <- c(
  "dunk",
  "alley oop",
  "putback",
  "tip",
  "layup"
)

high_ATK_variants <- c(
  "stepback jumper",
  "pullup jumper",
  "driving layup",
  "floater",
  "fadeaway jumper"
)

dependency_heavy_variants <- c(
  "alley oop",
  "putback",
  "tip"
)

attack_mix_components <- attack_library %>%
  dplyr::mutate(
    is_range_attack = .data$attack_variant %in% range_attack_variants,
    is_rim_dependency_attack = .data$attack_variant %in% rim_dependency_variants,
    is_high_ATK_variant = .data$attack_variant %in% high_ATK_variants,
    is_dependency_heavy_variant = .data$attack_variant %in% dependency_heavy_variants |
      (.data$attack_variant == "jump shot 3" & dplyr::coalesce(.data$assisted_attempt_rate, 0) >= 0.65)
  ) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    attack_library_attempts = sum(.data$attempts, na.rm = TRUE),
    self_created_attempts_modeled = sum(.data$self_created_attempts_estimated, na.rm = TRUE),
    assisted_attempts_modeled = sum(.data$assisted_attempts_estimated, na.rm = TRUE),
    self_created_expected_damage = weighted_mean_or_na(.data$attack_expected_damage, .data$self_created_attempts_estimated),
    high_ATK_self_created_attempts = sum(
      dplyr::if_else(.data$is_high_ATK_variant, .data$self_created_attempts_estimated, 0),
      na.rm = TRUE
    ),
    dependency_heavy_attempts = sum(
      dplyr::if_else(.data$is_dependency_heavy_variant, .data$attempts, 0),
      na.rm = TRUE
    ),
    range_attempts = sum(dplyr::if_else(.data$is_range_attack, .data$attempts, 0), na.rm = TRUE),
    range_expected_damage = weighted_mean_or_na(
      dplyr::if_else(.data$is_range_attack, .data$attack_expected_damage, NA_real_),
      dplyr::if_else(.data$is_range_attack, .data$attempts, 0)
    ),
    rim_dependency_attempts = sum(dplyr::if_else(.data$is_rim_dependency_attack, .data$attempts, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    creation_signal_available = .data$self_created_attempts_modeled > 0 |
      .data$assisted_attempts_modeled > 0,
    self_created_attack_share = safe_divide(.data$self_created_attempts_modeled, .data$attack_library_attempts),
    assisted_attack_share = safe_divide(.data$assisted_attempts_modeled, .data$attack_library_attempts),
    high_ATK_self_created_share = safe_divide(.data$high_ATK_self_created_attempts, .data$attack_library_attempts),
    dependency_heavy_attempt_share = safe_divide(.data$dependency_heavy_attempts, .data$attack_library_attempts),
    range_attempt_share = safe_divide(.data$range_attempts, .data$attack_library_attempts),
    rim_dependency_share = safe_divide(.data$rim_dependency_attempts, .data$attack_library_attempts)
  )

atk_base <- movesets %>%
  dplyr::left_join(
    attack_identity %>%
      dplyr::select(
        "player_id",
        "weapon_identity_score",
        "signature_weapon_family",
        "signature_weapon_variant",
        "signature_weapon_score",
        tidyselect::any_of(c(
          "primary_weapon_family",
          "primary_weapon_variant",
          "observed_attack_sample_warning"
        ))
      ),
    by = "player_id"
  ) %>%
  dplyr::left_join(
    attack_mix_components,
    by = "player_id"
  ) %>%
  dplyr::left_join(
    shot_context %>%
      dplyr::select("player_id", "context_difficulty_score") %>%
      dplyr::mutate(shot_context_row_available = TRUE),
    by = "player_id"
  ) %>%
  dplyr::left_join(
    shot_making %>%
      dplyr::select(
        "player_id",
        "observed_hit_rate",
        "expected_hit_rate",
        "shot_making_surplus",
        "contest_resistance_raw",
        "creation_burden_raw",
        "shot_making_component"
      ) %>%
      dplyr::mutate(shot_making_row_available = TRUE),
    by = "player_id"
  )

atk_scores <- atk_base %>%
  dplyr::mutate(
    shot_context_available = dplyr::coalesce(.data$shot_context_row_available, FALSE) &
      !is.na(.data$context_difficulty_score),
    shot_making_available = dplyr::coalesce(.data$shot_making_row_available, FALSE) &
      !is.na(.data$shot_making_surplus),
    context_adjusted_ATK_available = context_adjusted_ATK_pool_available &
      .data$shot_context_available &
      .data$shot_making_available &
      !is.na(.data$shot_making_component),
    context_adjusted_ATK_note = dplyr::if_else(
      !context_adjusted_ATK_pool_available,
      context_adjusted_ATK_insufficient_note,
      NA_character_
    ),
    damage_component = scale_0_100(.data$weighted_expected_damage),
    hit_component = scale_0_100(.data$weighted_hit_rate),
    diversity_component = clamp_0_100(100 * coalesce_numeric(.data$attack_diversity)),
    identity_component = scale_0_100(.data$weapon_identity_score),
    self_created_volume_component = clamp_0_100(100 * sqrt(coalesce_numeric(.data$self_created_attack_share))),
    self_created_efficiency_component = scale_0_100(.data$self_created_expected_damage),
    high_ATK_self_created_component = clamp_0_100(100 * sqrt(coalesce_numeric(.data$high_ATK_self_created_share))),
    self_created_ATK_component = clamp_0_100(
      0.50 * .data$self_created_volume_component +
        0.25 * .data$self_created_efficiency_component +
        0.25 * .data$high_ATK_self_created_component
    ),
    self_created_ATK_component = dplyr::if_else(
      dplyr::coalesce(.data$creation_signal_available, FALSE),
      .data$self_created_ATK_component,
      0
    ),
    assisted_dependency_penalty = clamp_0_100(
      0.60 * (((coalesce_numeric(.data$assisted_attack_share) - 0.55) / 0.45) * 100) +
        0.40 * (100 * sqrt(coalesce_numeric(.data$dependency_heavy_attempt_share)))
    ),
    assisted_dependency_penalty = dplyr::if_else(
      dplyr::coalesce(.data$creation_signal_available, FALSE),
      .data$assisted_dependency_penalty,
      0
    ),
    range_volume_component = clamp_0_100(100 * sqrt(coalesce_numeric(.data$range_attempt_share))),
    range_efficiency_component = dplyr::if_else(
      coalesce_numeric(.data$range_attempts) > 0,
      scale_0_100(.data$range_expected_damage),
      0
    ),
    range_component = clamp_0_100(
      0.55 * .data$range_volume_component +
        0.45 * .data$range_efficiency_component
    ),
    attack_volume_component = clamp_0_100(
      100 * pmin(sqrt(coalesce_numeric(.data$total_attack_attempts) / 500), 1)
    ),
    scalable_volume_component = clamp_0_100(
      0.55 * .data$attack_volume_component +
        0.45 * .data$damage_component
    ),
    rim_dependency_penalty = clamp_0_100(
      ((coalesce_numeric(.data$rim_dependency_share) - 0.45) / 0.55) * 100
    ),
    context_component = dplyr::if_else(
      .data$shot_context_available,
      clamp_0_100(.data$context_difficulty_score),
      NA_real_
    ),
    shot_making_component = dplyr::if_else(
      .data$context_adjusted_ATK_available,
      clamp_0_100(.data$shot_making_component),
      NA_real_
    ),
    # observed_raw_ATK uses only observed attack/moveset identity components.
    # The positive weights sum to 1.00 before dependency deductions:
    # damage 16%, hit rate 12%, diversity 10%, weapon identity 12%, range 14%,
    # scalable volume 16%, self-created ATK 20%. We then subtract 8% of the
    # rim-only finishing penalty and 14% of the assisted-dependency penalty.
    observed_raw_ATK_before_rim_adjustment =
      0.16 * .data$damage_component +
      0.12 * .data$hit_component +
      0.10 * .data$diversity_component +
      0.12 * .data$identity_component +
      0.14 * .data$range_component +
      0.16 * .data$scalable_volume_component +
      0.20 * .data$self_created_ATK_component,
    observed_raw_ATK = clamp_0_100(
      .data$observed_raw_ATK_before_rim_adjustment -
        0.08 * .data$rim_dependency_penalty -
        0.14 * .data$assisted_dependency_penalty
    ),
    # context_adjusted_raw_ATK is only meaningful when both Phase 15 and Phase
    # 17 data are present for the player and the shot-making pool is large
    # enough to support normalized context-adjusted components. Current
    # tracking-shot coverage is limited to the players pulled in the
    # tracking-shot audit/build phases.
    context_adjusted_raw_ATK = dplyr::if_else(
      .data$context_adjusted_ATK_available,
      0.20 * .data$damage_component +
        0.15 * .data$hit_component +
        0.10 * .data$diversity_component +
        0.20 * .data$identity_component +
        0.15 * .data$context_component +
        0.20 * .data$shot_making_component,
      NA_real_
    ),
    confidence_component = pmin(sqrt(coalesce_numeric(.data$total_attack_attempts) / 500), 1),
    observed_ATK_before_rim_adjustment = clamp_0_100(
      .data$observed_raw_ATK_before_rim_adjustment * .data$confidence_component
    ),
    observed_ATK_score = clamp_0_100(.data$observed_raw_ATK * .data$confidence_component),
    context_adjusted_ATK_score = dplyr::if_else(
      .data$context_adjusted_ATK_available,
      clamp_0_100(.data$context_adjusted_raw_ATK * .data$confidence_component),
      NA_real_
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "total_attack_attempts",
    tidyselect::any_of(c("sample_size_tier", "observed_attack_sample_warning")),
    "shot_context_available",
    "shot_making_available",
    "context_adjusted_ATK_available",
    "context_adjusted_ATK_note",
    "observed_hit_rate",
    "expected_hit_rate",
    "shot_making_surplus",
    "contest_resistance_raw",
    "creation_burden_raw",
    "damage_component",
    "hit_component",
    "diversity_component",
    "identity_component",
    "creation_signal_available",
    "self_created_attack_share",
    "assisted_attack_share",
    "self_created_ATK_component",
    "self_created_volume_component",
    "self_created_efficiency_component",
    "high_ATK_self_created_component",
    "high_ATK_self_created_share",
    "assisted_dependency_penalty",
    "dependency_heavy_attempt_share",
    "range_component",
    "range_volume_component",
    "range_efficiency_component",
    "range_attempts",
    "range_attempt_share",
    "range_expected_damage",
    "scalable_volume_component",
    "attack_volume_component",
    "rim_dependency_penalty",
    "rim_dependency_attempts",
    "rim_dependency_share",
    "context_component",
    "shot_making_component",
    "confidence_component",
    "observed_raw_ATK_before_rim_adjustment",
    "observed_ATK_before_rim_adjustment",
    "observed_raw_ATK",
    "observed_ATK_score",
    "context_adjusted_raw_ATK",
    "context_adjusted_ATK_score",
    tidyselect::any_of(c(
      "primary_weapon_family",
      "primary_weapon_variant",
      "signature_weapon_family",
      "signature_weapon_variant",
      "signature_weapon_score"
    ))
  ) %>%
  dplyr::arrange(dplyr::desc(.data$observed_ATK_score), .data$player_name)

write_project_parquet(atk_scores, atk_output_path)

message("Phase 16 prototype ATK diagnostics:")

message("Top observed ATK players before rim dependency adjustment:")
print(
  atk_scores %>%
    dplyr::arrange(dplyr::desc(.data$observed_ATK_before_rim_adjustment)) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "observed_ATK_before_rim_adjustment",
      "observed_ATK_score",
      "rim_dependency_penalty",
      "assisted_dependency_penalty",
      "self_created_ATK_component",
      "self_created_attack_share",
      "assisted_attack_share",
      "range_component",
      "scalable_volume_component",
      "confidence_component",
      "total_attack_attempts"
    ) %>%
    utils::head(25)
)

message("Top observed ATK players after rim dependency adjustment:")
print(
  atk_scores %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "observed_ATK_score",
      "observed_ATK_before_rim_adjustment",
      "observed_raw_ATK",
      "rim_dependency_penalty",
      "assisted_dependency_penalty",
      "confidence_component",
      "total_attack_attempts",
      "damage_component",
      "hit_component",
      "diversity_component",
      "identity_component",
      "self_created_ATK_component",
      "range_component",
      "scalable_volume_component"
    ) %>%
    utils::head(25)
)

message("Top rim_dependency_penalty players:")
print(
  atk_scores %>%
    dplyr::arrange(dplyr::desc(.data$rim_dependency_penalty)) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "rim_dependency_penalty",
      "rim_dependency_share",
      "rim_dependency_attempts",
      "range_attempt_share",
      "range_component",
      "observed_ATK_before_rim_adjustment",
      "observed_ATK_score"
    ) %>%
    utils::head(25)
)

message("Top assisted_dependency_penalty players:")
print(
  atk_scores %>%
    dplyr::arrange(dplyr::desc(.data$assisted_dependency_penalty)) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "assisted_dependency_penalty",
      "assisted_attack_share",
      "self_created_attack_share",
      "dependency_heavy_attempt_share",
      "self_created_ATK_component",
      "observed_ATK_before_rim_adjustment",
      "observed_ATK_score"
    ) %>%
    utils::head(25)
)

message("Top context-adjusted ATK players among available tracking-shot players only:")
print(
  atk_scores %>%
    dplyr::filter(.data$context_adjusted_ATK_available) %>%
    dplyr::arrange(dplyr::desc(.data$context_adjusted_ATK_score)) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "context_adjusted_ATK_score",
      "context_adjusted_raw_ATK",
      "observed_ATK_score",
      "confidence_component",
      "total_attack_attempts",
      "damage_component",
      "hit_component",
      "diversity_component",
      "identity_component",
      "context_component",
      "shot_making_component"
    ) %>%
    utils::head(25)
)

message("Component breakdown:")
print(
  atk_scores %>%
    dplyr::summarise(
      players = dplyr::n(),
      shot_context_available_players = sum(.data$shot_context_available, na.rm = TRUE),
      shot_making_available_players = sum(.data$shot_making_available, na.rm = TRUE),
      shot_making_player_count = shot_making_player_count,
      context_adjusted_available_players = sum(.data$context_adjusted_ATK_available, na.rm = TRUE),
      median_observed_ATK_before_rim_adjustment = stats::median(.data$observed_ATK_before_rim_adjustment, na.rm = TRUE),
      median_observed_ATK_score = stats::median(.data$observed_ATK_score, na.rm = TRUE),
      median_context_adjusted_ATK_score = stats::median(.data$context_adjusted_ATK_score, na.rm = TRUE),
      median_damage_component = stats::median(.data$damage_component, na.rm = TRUE),
      median_hit_component = stats::median(.data$hit_component, na.rm = TRUE),
      median_diversity_component = stats::median(.data$diversity_component, na.rm = TRUE),
      median_identity_component = stats::median(.data$identity_component, na.rm = TRUE),
      median_self_created_attack_share = stats::median(.data$self_created_attack_share, na.rm = TRUE),
      median_assisted_attack_share = stats::median(.data$assisted_attack_share, na.rm = TRUE),
      median_self_created_ATK_component = stats::median(.data$self_created_ATK_component, na.rm = TRUE),
      median_assisted_dependency_penalty = stats::median(.data$assisted_dependency_penalty, na.rm = TRUE),
      median_range_component = stats::median(.data$range_component, na.rm = TRUE),
      median_scalable_volume_component = stats::median(.data$scalable_volume_component, na.rm = TRUE),
      median_rim_dependency_penalty = stats::median(.data$rim_dependency_penalty, na.rm = TRUE),
      median_context_component = stats::median(.data$context_component, na.rm = TRUE),
      median_shot_making_component = stats::median(.data$shot_making_component, na.rm = TRUE),
      median_confidence_component = stats::median(.data$confidence_component, na.rm = TRUE)
    )
)

message("Luka / LeBron / Austin / Ayton / Jaxson Hayes / Luke Kennard examples, if available:")
print(
  atk_scores %>%
    dplyr::filter(stringr::str_detect(.data$player_name, "Luka|Doncic|Dončić|LeBron|Austin Reaves|Ayton|Jaxson Hayes|Luke Kennard")) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "observed_ATK_before_rim_adjustment",
      "observed_ATK_score",
      "context_adjusted_ATK_score",
      "confidence_component",
      "total_attack_attempts",
      "range_component",
      "range_attempt_share",
      "scalable_volume_component",
      "self_created_ATK_component",
      "self_created_attack_share",
      "assisted_attack_share",
      "assisted_dependency_penalty",
      "dependency_heavy_attempt_share",
      "rim_dependency_penalty",
      "rim_dependency_share",
      "shot_context_available",
      "shot_making_available",
      "context_adjusted_ATK_available",
      "context_adjusted_ATK_note",
      "damage_component",
      "hit_component",
      "diversity_component",
      "identity_component",
      "context_component",
      "shot_making_component",
      "observed_hit_rate",
      "expected_hit_rate",
      "shot_making_surplus",
      "contest_resistance_raw",
      "creation_burden_raw",
      tidyselect::any_of(c("signature_weapon_family", "signature_weapon_variant"))
    )
)

if (!context_adjusted_ATK_pool_available) {
  message(context_adjusted_ATK_insufficient_note)
}

message("Phase 16 note: observed_ATK_score is the current prototype self-created scoring score from movesets, weapon identity, range, scalable volume, self-created attack share, and dependency adjustments. Assisted shots still matter, but assisted-dependent profiles are discounted because ATK is meant to primarily reflect self-created one-on-one scoring power. context_adjusted_ATK_score is only calculated when at least 30 players have shot-making data and the player has both shot context and shot-making rows. Current shot context coverage is limited to pulled tracking-shot players. Signature attacks remain card-body information. These scores are not adjusted for teammates or opponent strength.")
