# ============================================================
# 26_build_possession_effects.R
# Phase 26: Build possession and hustle effects.
#
# Goal:
# Extract non-damage basketball actions that create possession advantage,
# pressure, or ecosystem value.
#
# These actions are not direct ATK or DEF. They behave more like status effects,
# passives, utility moves, or continuous effects:
# - offensive rebound -> second-chance pressure
# - defensive rebound -> possession termination
# - loose-ball recovery -> possession retention
# - screen assist -> teammate attack boost
# - charge drawn -> turnover pressure
# - deflection -> offensive disruption
# - steal -> possession swing
#
# This phase does not modify previous phases and does not convert effects into
# ATK or DEF.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

effects_output_dir <- "outputs/effects"
player_possession_effects_path <- file.path(effects_output_dir, "player_possession_effects.parquet")

player_master_path <- glue("data/processed/player/player_master_{season}.parquet")
player_base_path <- glue("data/raw/player/player_base_{season}.parquet")
player_misc_path <- glue("data/raw/player/player_misc_{season}.parquet")
player_attack_library_path <- "outputs/attacks/player_attack_library.parquet"
def_components_path <- "outputs/defense/player_DEF_components.parquet"
def_proxy_components_path <- "outputs/defense/player_DEF_proxy_components.parquet"

fs::dir_create(effects_output_dir)

if (!file.exists(player_master_path)) {
  stop("Missing player master input: ", player_master_path, ". Run 02_player_season.R first.", call. = FALSE)
}

safe_read_parquet <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(player_id = character()))
  }

  df <- read_project_parquet(path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols()

  if (!("player_id" %in% names(df))) {
    df$player_id <- NA_character_
  }

  df %>%
    dplyr::mutate(player_id = as.character(.data$player_id))
}

add_missing_cols <- function(df, cols, value = NA_real_) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

coalesce_numeric_cols <- function(df, candidates) {
  matches <- candidates[candidates %in% names(df)]

  if (length(matches) == 0) {
    return(rep(NA_real_, nrow(df)))
  }

  values <- lapply(matches, function(col) suppressWarnings(as.numeric(df[[col]])))
  Reduce(dplyr::coalesce, values)
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
    return(out)
  }

  out[valid] <- (x_valid - min(x_valid)) / spread * 100
  out
}

row_mean_available <- function(...) {
  pieces <- list(...)
  mat <- do.call(cbind, lapply(pieces, as.numeric))
  out <- rowMeans(mat, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

effect_row <- function(df, effect_name, effect_type, effect_strength_col, effect_note, proxy_flag_col) {
  df %>%
    dplyr::transmute(
      player_id = .data$player_id,
      player_name = .data$player_name,
      effect_name = effect_name,
      effect_type = effect_type,
      effect_strength = .data[[effect_strength_col]],
      effect_note = dplyr::case_when(
        !is.na(.data[[effect_strength_col]]) & .data[[proxy_flag_col]] ~ paste(effect_note, "Proxy-built effect."),
        !is.na(.data[[effect_strength_col]]) ~ effect_note,
        TRUE ~ paste(effect_note, "Unavailable with current pulled data.")
      ),
      availability_flag = !is.na(.data[[effect_strength_col]]),
      proxy_built_effect = dplyr::coalesce(.data[[proxy_flag_col]], FALSE)
    )
}

player_master <- safe_read_parquet(player_master_path) %>%
  add_missing_cols(c("player_name", "nickname", "player_nickname", "team_abbreviation", "team_abbr"), NA_character_) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_nickname = dplyr::coalesce(as.character(.data$player_nickname), as.character(.data$nickname)),
    team_abbreviation = dplyr::coalesce(as.character(.data$team_abbreviation), as.character(.data$team_abbr))
  ) %>%
  dplyr::select("player_id", "player_name", "player_nickname", "team_abbreviation")

validate_columns(player_master, c("player_id", "player_name"))

player_base <- safe_read_parquet(player_base_path)
player_misc <- safe_read_parquet(player_misc_path)
attack_library <- safe_read_parquet(player_attack_library_path)
def_components <- safe_read_parquet(def_components_path)
def_proxy_components <- safe_read_parquet(def_proxy_components_path)

base_effects <- player_base %>%
  dplyr::transmute(
    player_id = as.character(.data$player_id),
    offensive_rebounds = coalesce_numeric_cols(., c("oreb", "offensive_rebounds", "orb")),
    defensive_rebounds = coalesce_numeric_cols(., c("dreb", "defensive_rebounds", "drb")),
    total_rebounds = coalesce_numeric_cols(., c("reb", "rebounds", "total_rebounds")),
    steals = coalesce_numeric_cols(., c("stl", "steals"))
  )

misc_effects <- player_misc %>%
  dplyr::transmute(
    player_id = as.character(.data$player_id),
    screen_assists = coalesce_numeric_cols(., c("screen_assists", "screen_ast", "screen_assist")),
    screen_assist_points = coalesce_numeric_cols(., c("screen_assist_points", "screen_ast_pts")),
    secondary_assists = coalesce_numeric_cols(., c("secondary_assists", "secondary_ast", "secondary_assist")),
    loose_balls_recovered = coalesce_numeric_cols(., c("loose_balls_recovered", "loose_ball_recoveries", "loose_balls")),
    charges_drawn = coalesce_numeric_cols(., c("charges_drawn", "charge_drawn", "charges")),
    deflections = coalesce_numeric_cols(., c("deflections", "deflect")),
    box_outs = coalesce_numeric_cols(., c("box_outs", "boxouts")),
    contested_shots = coalesce_numeric_cols(., c("contested_shots", "shots_contested"))
  )

attack_effects <- if (nrow(attack_library) > 0) {
  attack_library %>%
    add_missing_cols(c("attack_variant", "attempts", "possessions"), NA) %>%
    dplyr::mutate(
      attack_variant = stringr::str_to_lower(as.character(.data$attack_variant)),
      attempts = suppressWarnings(as.numeric(.data$attempts))
    ) %>%
    dplyr::group_by(.data$player_id) %>%
    dplyr::summarise(
      putback_attempts = sum(dplyr::if_else(.data$attack_variant %in% c("putback", "tip"), .data$attempts, 0), na.rm = TRUE),
      putback_dunk_or_finish_attempts = sum(dplyr::if_else(.data$attack_variant %in% c("putback", "tip", "dunk"), .data$attempts, 0), na.rm = TRUE),
      .groups = "drop"
    )
} else {
  tibble::tibble(
    player_id = character(),
    putback_attempts = numeric(),
    putback_dunk_or_finish_attempts = numeric()
  )
}

def_effects <- def_components %>%
  add_missing_cols(c(
    "disruption_component",
    "rim_protection_component",
    "steals_total",
    "deflections_total",
    "loose_balls_total",
    "charges_total"
  ), NA_real_) %>%
  dplyr::select(
    "player_id",
    phase25_disruption_component = "disruption_component",
    phase25_rim_protection_component = "rim_protection_component",
    phase25_steals_total = "steals_total",
    phase25_deflections_total = "deflections_total",
    phase25_loose_balls_total = "loose_balls_total",
    phase25_charges_total = "charges_total"
  )

def_proxy_effects <- def_proxy_components %>%
  add_missing_cols(c("on_ball_proxy_component", "proxy_source"), NA) %>%
  dplyr::select(
    "player_id",
    defensive_activity_proxy = "on_ball_proxy_component",
    defensive_proxy_source = "proxy_source"
  )

effect_base <- player_master %>%
  dplyr::left_join(base_effects, by = "player_id") %>%
  dplyr::left_join(misc_effects, by = "player_id") %>%
  dplyr::left_join(attack_effects, by = "player_id") %>%
  dplyr::left_join(def_effects, by = "player_id") %>%
  dplyr::left_join(def_proxy_effects, by = "player_id") %>%
  dplyr::mutate(
    offensive_rebound_component = scale_0_100(.data$offensive_rebounds),
    putback_component = scale_0_100(.data$putback_attempts),
    second_chance_component = row_mean_available(
      .data$offensive_rebound_component,
      .data$putback_component
    ),
    extra_possession_pressure = .data$second_chance_component,
    second_chance_proxy = is.na(.data$offensive_rebounds) & !is.na(.data$putback_attempts),
    defensive_rebound_component = scale_0_100(.data$defensive_rebounds),
    box_out_component = scale_0_100(.data$box_outs),
    loose_ball_component = scale_0_100(dplyr::coalesce(.data$loose_balls_recovered, .data$phase25_loose_balls_total)),
    possession_control_component = row_mean_available(
      0.70 * .data$defensive_rebound_component,
      .data$box_out_component,
      .data$loose_ball_component
    ),
    possession_termination_component = row_mean_available(
      .data$defensive_rebound_component,
      .data$box_out_component
    ),
    possession_control_proxy = is.na(.data$defensive_rebounds) & (!is.na(.data$box_outs) | !is.na(.data$loose_balls_recovered)),
    steal_component = scale_0_100(dplyr::coalesce(.data$steals, .data$phase25_steals_total)),
    deflection_component = scale_0_100(dplyr::coalesce(.data$deflections, .data$phase25_deflections_total)),
    charge_component = scale_0_100(dplyr::coalesce(.data$charges_drawn, .data$phase25_charges_total)),
    disruption_effect_component = row_mean_available(
      .data$steal_component,
      .data$deflection_component,
      .data$charge_component
    ),
    turnover_pressure_component = row_mean_available(
      .data$steal_component,
      .data$charge_component
    ),
    disruption_proxy = is.na(.data$steals) & (!is.na(.data$phase25_disruption_component) | !is.na(.data$defensive_activity_proxy)),
    screen_assist_component = scale_0_100(.data$screen_assists),
    screen_assist_points_component = scale_0_100(.data$screen_assist_points),
    hockey_assist_component = scale_0_100(.data$secondary_assists),
    teammate_shot_quality_proxy_component = scale_0_100(.data$defensive_activity_proxy),
    ecosystem_effect_component = row_mean_available(
      .data$screen_assist_component,
      .data$screen_assist_points_component,
      .data$hockey_assist_component,
      .data$teammate_shot_quality_proxy_component
    ),
    ecosystem_proxy = is.na(.data$screen_assists) & is.na(.data$secondary_assists) & !is.na(.data$defensive_activity_proxy)
  )

effect_rows <- dplyr::bind_rows(
  effect_row(
    effect_base,
    "second-chance pressure",
    "Second Chance Effects",
    "second_chance_component",
    "Offensive rebounds, putbacks, and putback attempts create extra possession pressure.",
    "second_chance_proxy"
  ),
  effect_row(
    effect_base,
    "extra possession pressure",
    "Second Chance Effects",
    "extra_possession_pressure",
    "Second-chance pressure proxy for forcing defenses to survive additional actions.",
    "second_chance_proxy"
  ),
  effect_row(
    effect_base,
    "possession control",
    "Possession Control Effects",
    "possession_control_component",
    "Defensive rebounds, box-out proxies, and loose-ball recoveries help control possession outcomes.",
    "possession_control_proxy"
  ),
  effect_row(
    effect_base,
    "possession termination",
    "Possession Control Effects",
    "possession_termination_component",
    "Defensive rebounds and box-out proxies end opponent possessions.",
    "possession_control_proxy"
  ),
  effect_row(
    effect_base,
    "offensive disruption",
    "Disruption Effects",
    "disruption_effect_component",
    "Steals, deflections, and charges drawn disrupt offensive flow.",
    "disruption_proxy"
  ),
  effect_row(
    effect_base,
    "turnover pressure",
    "Disruption Effects",
    "turnover_pressure_component",
    "Steals and charges drawn create turnover pressure and possession swings.",
    "disruption_proxy"
  ),
  effect_row(
    effect_base,
    "ecosystem utility",
    "Ecosystem Utility Effects",
    "ecosystem_effect_component",
    "Screen assists, hockey assists, and teammate shot-quality proxies describe non-scoring utility.",
    "ecosystem_proxy"
  )
)

player_possession_effects <- effect_rows %>%
  dplyr::arrange(.data$player_name, .data$effect_type, .data$effect_name)

write_project_parquet(player_possession_effects, player_possession_effects_path)

message("Phase 26 possession/hustle effects diagnostics:")

message("Effect coverage:")
print(
  player_possession_effects %>%
    dplyr::group_by(.data$effect_type, .data$effect_name) %>%
    dplyr::summarise(
      players = dplyr::n(),
      available_players = sum(.data$availability_flag, na.rm = TRUE),
      proxy_built_players = sum(.data$proxy_built_effect, na.rm = TRUE),
      missing_pct = 100 * mean(is.na(.data$effect_strength)),
      .groups = "drop"
    )
)

message("Effect availability:")
print(
  player_possession_effects %>%
    dplyr::count(.data$effect_type, .data$effect_name, .data$availability_flag, sort = TRUE)
)

message("Proxy reliance:")
print(
  player_possession_effects %>%
    dplyr::filter(.data$proxy_built_effect) %>%
    dplyr::count(.data$effect_type, .data$effect_name, sort = TRUE)
)

message("Requested player examples:")
print(
  player_possession_effects %>%
    dplyr::filter(stringr::str_detect(.data$player_name, "Steven Adams|Josh Hart|Alex Caruso|Draymond Green|Gobert|Nikola Jokic|Jokić|LeBron James")) %>%
    dplyr::select(
      "player_name",
      "effect_name",
      "effect_type",
      "effect_strength",
      "availability_flag",
      "proxy_built_effect",
      "effect_note"
    )
)

message("Top players by each effect:")
print(
  player_possession_effects %>%
    dplyr::filter(.data$availability_flag) %>%
    dplyr::group_by(.data$effect_type, .data$effect_name) %>%
    dplyr::arrange(dplyr::desc(.data$effect_strength), .by_group = TRUE) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "effect_type",
      "effect_name",
      "player_name",
      "effect_strength",
      "proxy_built_effect"
    )
)

message("Saved possession and hustle effects to: ", player_possession_effects_path)
message("Phase 26 note: effects only. These are not direct ATK or DEF and no card score was built.")
