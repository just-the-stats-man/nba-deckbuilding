# ============================================================
# 25_build_DEF_components.R
# Phase 25: Build defensive component metrics.
#
# Goal:
# Build observed defensive component scores without creating a final DEF score.
#
# DEF philosophy:
# DEF should represent defensive pressure and resistance created by a player.
# It should NOT be defined as blocks + steals + defensive rebounds. Defensive
# rebounds are retained only as a minor proxy where no better rim/jump context
# exists, and are not a major component.
#
# This phase is prototype / limited-coverage only. Missing unavailable
# components remain NA; no neutral 50 defaults are used.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

defense_output_dir <- "outputs/defense"
def_components_path <- file.path(defense_output_dir, "player_DEF_components.parquet")

defensive_signal_audit_path <- "outputs/defense/defensive_signal_audit.parquet"
pbp_path <- glue("data/raw/pbp/{team_abbr}_pbp_sample_{season}.parquet")
player_master_path <- glue("data/processed/player/player_master_{season}.parquet")
player_base_path <- glue("data/raw/player/player_base_{season}.parquet")
player_misc_path <- glue("data/raw/player/player_misc_{season}.parquet")
player_advanced_path <- glue("data/raw/player/player_advanced_{season}.parquet")
tracking_shots_dir <- "data/raw/tracking_shots"
tracking_creation_audit_path <- "outputs/attacks/tracking_creation_audit.parquet"

fs::dir_create(defense_output_dir)

required_inputs <- c(defensive_signal_audit_path, pbp_path, player_master_path)
missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "Missing Phase 25 input(s): ",
    paste(missing_inputs, collapse = ", "),
    ". Run Phase 21 and the base data pulls first.",
    call. = FALSE
  )
}

safe_read_parquet <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(player_id = character()))
  }

  read_project_parquet(path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols()
}

add_missing_cols <- function(df, cols, value = NA) {
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

scale_0_100 <- function(x, reverse = FALSE) {
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

  scaled <- (x_valid - min(x_valid)) / spread * 100

  if (reverse) {
    scaled <- 100 - scaled
  }

  out[valid] <- scaled
  out
}

safe_divide <- function(numerator, denominator) {
  numerator <- as.numeric(numerator)
  denominator <- as.numeric(denominator)
  out <- numerator / denominator
  out[is.na(numerator) | is.na(denominator) | denominator == 0] <- NA_real_
  out
}

row_mean_available <- function(...) {
  pieces <- list(...)
  mat <- do.call(cbind, lapply(pieces, as.numeric))
  out <- rowMeans(mat, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

collapse_flags <- function(...) {
  flags <- list(...)
  names <- names(flags)

  paste(
    paste0(names, "=", ifelse(unlist(flags), "TRUE", "FALSE")),
    collapse = " | "
  )
}

text_columns <- function(df) {
  candidates <- c(
    "description",
    "home_description",
    "visitor_description",
    "neutral_description",
    "action_description",
    "event_type",
    "event_msg_type_description",
    "action_type",
    "sub_type"
  )

  intersect(candidates, names(df))
}

event_text_blob <- function(df) {
  cols <- text_columns(df)

  if (length(cols) == 0) {
    return(rep("", nrow(df)))
  }

  df %>%
    dplyr::select(tidyselect::all_of(cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::unite("event_text", dplyr::everything(), sep = " | ", na.rm = TRUE) %>%
    dplyr::pull(.data$event_text) %>%
    stringr::str_to_lower()
}

extract_player_event_counts <- function(pbp, regex, count_name) {
  player_id_cols <- intersect(
    c("player_id", "person_id", "athlete_id", "player1_id", "player2_id", "player3_id", "person1_id", "person2_id", "person3_id"),
    names(pbp)
  )

  if (length(player_id_cols) == 0 || nrow(pbp) == 0) {
    out <- tibble::tibble(player_id = character())
    out[[count_name]] <- numeric()
    return(out)
  }

  pbp_text <- event_text_blob(pbp)

  pbp %>%
    dplyr::mutate(.event_text = pbp_text) %>%
    dplyr::filter(stringr::str_detect(.data$.event_text, regex)) %>%
    dplyr::select(tidyselect::all_of(player_id_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "source_player_id_column",
      values_to = "player_id"
    ) %>%
    dplyr::filter(!is.na(.data$player_id), .data$player_id != "", .data$player_id != "0") %>%
    dplyr::count(.data$player_id, name = count_name)
}

read_tracking_shot_tables <- function() {
  if (!dir.exists(tracking_shots_dir)) {
    return(tibble::tibble(player_id = character()))
  }

  paths <- list.files(tracking_shots_dir, pattern = "\\.parquet$", full.names = TRUE)

  if (length(paths) == 0) {
    return(tibble::tibble(player_id = character()))
  }

  dplyr::bind_rows(lapply(paths, function(path) {
    tbl <- safe_read_parquet(path)
    tbl$source_file <- basename(path)
    tbl
  }))
}

player_master <- safe_read_parquet(player_master_path) %>%
  add_missing_cols(c("player_id", "player_name", "nickname", "player_nickname", "team_abbreviation", "team_abbr"), NA_character_) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_nickname = dplyr::coalesce(as.character(.data$player_nickname), as.character(.data$nickname)),
    team_abbreviation = dplyr::coalesce(as.character(.data$team_abbreviation), as.character(.data$team_abbr))
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    tidyselect::any_of(c("height", "height_inches", "weight", "position"))
  )

validate_columns(player_master, c("player_id", "player_name", "team_abbreviation"))

defensive_signal_audit <- safe_read_parquet(defensive_signal_audit_path) %>%
  add_missing_cols(c("signal", "confidence", "available_columns", "evidence_count"), NA_character_)

pbp <- safe_read_parquet(pbp_path) %>%
  dplyr::mutate(dplyr::across(tidyselect::any_of(c("player_id", "person_id", "player1_id", "player2_id", "player3_id", "game_id")), as.character))

player_base <- safe_read_parquet(player_base_path) %>%
  dplyr::mutate(player_id = as.character(.data$player_id))

player_misc <- safe_read_parquet(player_misc_path) %>%
  dplyr::mutate(player_id = as.character(.data$player_id))

player_advanced <- safe_read_parquet(player_advanced_path) %>%
  dplyr::mutate(player_id = as.character(.data$player_id))

base_defense <- player_base %>%
  dplyr::transmute(
    player_id = as.character(.data$player_id),
    minutes = coalesce_numeric_cols(., c("min", "minutes", "minutes_played")),
    steals = coalesce_numeric_cols(., c("stl", "steals")),
    blocks = coalesce_numeric_cols(., c("blk", "blocks")),
    defensive_rebounds = coalesce_numeric_cols(., c("dreb", "defensive_rebounds")),
    personal_fouls = coalesce_numeric_cols(., c("pf", "personal_fouls"))
  )

misc_defense <- player_misc %>%
  dplyr::transmute(
    player_id = as.character(.data$player_id),
    charges_drawn = coalesce_numeric_cols(., c("charges_drawn", "charge_drawn", "charges")),
    loose_balls_recovered = coalesce_numeric_cols(., c("loose_balls_recovered", "loose_ball_recoveries", "loose_balls")),
    deflections = coalesce_numeric_cols(., c("deflections", "deflect")),
    fouls_drawn = coalesce_numeric_cols(., c("pfd", "fouls_drawn", "personal_fouls_drawn"))
  )

advanced_defense <- player_advanced %>%
  dplyr::transmute(
    player_id = as.character(.data$player_id),
    defensive_rating = coalesce_numeric_cols(., c("def_rating", "defensive_rating", "d_rating")),
    defensive_win_shares = coalesce_numeric_cols(., c("def_ws", "defensive_win_shares", "dws")),
    defensive_box_plus_minus = coalesce_numeric_cols(., c("dbpm", "defensive_box_plus_minus"))
  )

pbp_steals <- extract_player_event_counts(pbp, "steal|stolen", "pbp_steals")
pbp_blocks <- extract_player_event_counts(pbp, "\\bblock|blocked", "pbp_blocks")
pbp_charges <- extract_player_event_counts(pbp, "charge drawn|draws charge|offensive charge|charging", "pbp_charges_drawn")
pbp_loose <- extract_player_event_counts(pbp, "loose ball|recovery|recovers", "pbp_loose_balls")
pbp_fouls <- extract_player_event_counts(pbp, "personal foul|shooting foul|offensive foul|blocking foul|foul", "pbp_fouls")
pbp_contests <- extract_player_event_counts(pbp, "contest|contested|close defender|blocked", "pbp_contests")

tracking_shots <- read_tracking_shot_tables() %>%
  add_missing_cols(c("player_id", "source_file"), NA_character_) %>%
  dplyr::mutate(player_id = as.character(.data$player_id))

tracking_shot_context <- if (nrow(tracking_shots) > 0) {
  tracking_shots %>%
    dplyr::mutate(
      fga = coalesce_numeric_cols(., c("fga", "fg_a", "field_goal_attempts")),
      fg_pct = coalesce_numeric_cols(., c("fg_pct", "fg_percent", "field_goal_percentage")),
      contested_row = stringr::str_detect(
        stringr::str_to_lower(dplyr::coalesce(.data$source_file, "")),
        "closestdefender|shotclock|dribble|touchtime"
      ),
      tight_row = stringr::str_detect(
        stringr::str_to_lower(do.call(paste, c(dplyr::across(dplyr::everything()), sep = " "))),
        "0-2|2-4|very tight|tight"
      )
    ) %>%
    dplyr::group_by(.data$player_id) %>%
    dplyr::summarise(
      tracking_context_rows = dplyr::n(),
      contested_tracking_rows = sum(.data$contested_row, na.rm = TRUE),
      tight_tracking_rows = sum(.data$tight_row, na.rm = TRUE),
      tracking_fga = sum(.data$fga, na.rm = TRUE),
      tracking_fg_pct_proxy = stats::weighted.mean(.data$fg_pct, dplyr::if_else(is.na(.data$fga), 0, .data$fga), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(tracking_fg_pct_proxy = dplyr::if_else(is.nan(.data$tracking_fg_pct_proxy), NA_real_, .data$tracking_fg_pct_proxy))
} else {
  tibble::tibble(
    player_id = character(),
    tracking_context_rows = numeric(),
    contested_tracking_rows = numeric(),
    tight_tracking_rows = numeric(),
    tracking_fga = numeric(),
    tracking_fg_pct_proxy = numeric()
  )
}

tracking_creation_audit <- safe_read_parquet(tracking_creation_audit_path) %>%
  add_missing_cols(c("player_id", "has_matchup", "matched_keyword_columns", "row_count"), NA) %>%
  dplyr::mutate(player_id = as.character(.data$player_id))

matchup_proxy <- tracking_creation_audit %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    matchup_rows_available = sum(dplyr::coalesce(as.numeric(.data$row_count), 0), na.rm = TRUE),
    matchup_signal_available = any(dplyr::coalesce(as.logical(.data$has_matchup), FALSE), na.rm = TRUE) |
      any(stringr::str_detect(stringr::str_to_lower(dplyr::coalesce(.data$matched_keyword_columns, "")), "matchup|defender|opponent"), na.rm = TRUE),
    .groups = "drop"
  )

player_def_raw <- player_master %>%
  dplyr::left_join(base_defense, by = "player_id") %>%
  dplyr::left_join(misc_defense, by = "player_id") %>%
  dplyr::left_join(advanced_defense, by = "player_id") %>%
  dplyr::left_join(pbp_steals, by = "player_id") %>%
  dplyr::left_join(pbp_blocks, by = "player_id") %>%
  dplyr::left_join(pbp_charges, by = "player_id") %>%
  dplyr::left_join(pbp_loose, by = "player_id") %>%
  dplyr::left_join(pbp_fouls, by = "player_id") %>%
  dplyr::left_join(pbp_contests, by = "player_id") %>%
  dplyr::left_join(tracking_shot_context, by = "player_id") %>%
  dplyr::left_join(matchup_proxy, by = "player_id") %>%
  add_missing_cols(c(
    "pbp_steals",
    "pbp_blocks",
    "pbp_charges_drawn",
    "pbp_loose_balls",
    "pbp_fouls",
    "pbp_contests",
    "tracking_context_rows",
    "contested_tracking_rows",
    "tight_tracking_rows",
    "tracking_fga",
    "tracking_fg_pct_proxy",
    "matchup_rows_available",
    "matchup_signal_available"
  ), NA_real_) %>%
  dplyr::mutate(
    steals_total = dplyr::coalesce(.data$steals, .data$pbp_steals),
    blocks_total = dplyr::coalesce(.data$blocks, .data$pbp_blocks),
    fouls_total = dplyr::coalesce(.data$personal_fouls, .data$pbp_fouls),
    charges_total = dplyr::coalesce(.data$charges_drawn, .data$pbp_charges_drawn),
    loose_balls_total = dplyr::coalesce(.data$loose_balls_recovered, .data$pbp_loose_balls),
    deflections_total = dplyr::coalesce(.data$deflections, 0),
    defensive_events_per_min = safe_divide(
      dplyr::coalesce(.data$steals_total, 0) +
        dplyr::coalesce(.data$blocks_total, 0) +
        dplyr::coalesce(.data$charges_total, 0) +
        dplyr::coalesce(.data$loose_balls_total, 0) +
        dplyr::coalesce(.data$deflections_total, 0),
      .data$minutes
    ),
    foul_rate = safe_divide(.data$fouls_total, .data$minutes),
    contest_tracking_rate = safe_divide(.data$contested_tracking_rows + .data$tight_tracking_rows, .data$tracking_context_rows)
  )

player_def_components <- player_def_raw %>%
  dplyr::mutate(
    on_ball_opponent_fg_proxy_component = scale_0_100(.data$tracking_fg_pct_proxy, reverse = TRUE),
    on_ball_contest_component = scale_0_100(.data$contest_tracking_rate),
    matchup_component = dplyr::if_else(.data$matchup_signal_available %in% TRUE, scale_0_100(.data$matchup_rows_available), NA_real_),
    on_ball_component = row_mean_available(
      .data$on_ball_opponent_fg_proxy_component,
      .data$on_ball_contest_component,
      .data$matchup_component
    ),
    steal_component = scale_0_100(.data$steals_total),
    deflection_component = scale_0_100(.data$deflections_total),
    loose_ball_component = scale_0_100(.data$loose_balls_total),
    charge_component = scale_0_100(.data$charges_total),
    disruption_component = row_mean_available(
      .data$steal_component,
      .data$deflection_component,
      .data$loose_ball_component,
      .data$charge_component
    ),
    block_component = scale_0_100(.data$blocks_total),
    rim_contest_component = scale_0_100(.data$pbp_contests),
    defensive_rebound_minor_component = scale_0_100(.data$defensive_rebounds),
    rim_protection_component = row_mean_available(
      .data$block_component,
      .data$rim_contest_component,
      0.20 * .data$defensive_rebound_minor_component
    ),
    size_component = row_mean_available(
      scale_0_100(coalesce_numeric_cols(., c("height_inches", "height"))),
      scale_0_100(coalesce_numeric_cols(., c("weight")))
    ),
    matchup_diversity_component = dplyr::if_else(.data$matchup_signal_available %in% TRUE, scale_0_100(.data$matchup_rows_available), NA_real_),
    versatility_component = row_mean_available(
      .data$matchup_diversity_component,
      .data$size_component
    ),
    defensive_cost_component = scale_0_100(.data$foul_rate),
    on_ball_available = !is.na(.data$on_ball_component),
    disruption_available = !is.na(.data$disruption_component),
    rim_protection_available = !is.na(.data$rim_protection_component),
    versatility_available = !is.na(.data$versatility_component),
    defensive_cost_available = !is.na(.data$defensive_cost_component),
    component_availability_flags = mapply(
      collapse_flags,
      on_ball = .data$on_ball_available,
      disruption = .data$disruption_available,
      rim_protection = .data$rim_protection_available,
      versatility = .data$versatility_available,
      defensive_cost = .data$defensive_cost_available,
      SIMPLIFY = TRUE
    ),
    proxy_only_flag = (
      (.data$on_ball_available & is.na(.data$on_ball_opponent_fg_proxy_component)) |
        (.data$rim_protection_available & is.na(.data$block_component)) |
        (.data$versatility_available & is.na(.data$matchup_diversity_component))
    ),
    DEF_component_note = dplyr::case_when(
      .data$proxy_only_flag ~ "Prototype limited-coverage defensive components. At least one available component relies only on proxies.",
      TRUE ~ "Prototype limited-coverage defensive components. No final DEF_score is calculated."
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "on_ball_component",
    "disruption_component",
    "rim_protection_component",
    "versatility_component",
    "defensive_cost_component",
    "component_availability_flags",
    "DEF_component_note",
    "on_ball_available",
    "disruption_available",
    "rim_protection_available",
    "versatility_available",
    "defensive_cost_available",
    "proxy_only_flag",
    "steals_total",
    "blocks_total",
    "deflections_total",
    "loose_balls_total",
    "charges_total",
    "fouls_total",
    "foul_rate",
    "tracking_fg_pct_proxy",
    "contest_tracking_rate",
    "matchup_signal_available",
    "defensive_rating",
    "defensive_win_shares",
    "defensive_box_plus_minus"
  ) %>%
  dplyr::arrange(.data$player_name)

write_project_parquet(player_def_components, def_components_path)

available_signals <- defensive_signal_audit %>%
  dplyr::filter(.data$confidence != "unavailable") %>%
  dplyr::distinct(.data$signal, .data$source_name, .data$confidence)

unavailable_signals <- defensive_signal_audit %>%
  dplyr::group_by(.data$signal) %>%
  dplyr::summarise(any_available = any(.data$confidence != "unavailable", na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(!.data$any_available)

message("Phase 25 defensive component diagnostics:")

message("Component summaries:")
print(
  player_def_components %>%
    dplyr::summarise(
      players = dplyr::n(),
      on_ball_available_players = sum(.data$on_ball_available, na.rm = TRUE),
      disruption_available_players = sum(.data$disruption_available, na.rm = TRUE),
      rim_protection_available_players = sum(.data$rim_protection_available, na.rm = TRUE),
      versatility_available_players = sum(.data$versatility_available, na.rm = TRUE),
      defensive_cost_available_players = sum(.data$defensive_cost_available, na.rm = TRUE),
      proxy_only_players = sum(.data$proxy_only_flag, na.rm = TRUE),
      median_on_ball_component = stats::median(.data$on_ball_component, na.rm = TRUE),
      median_disruption_component = stats::median(.data$disruption_component, na.rm = TRUE),
      median_rim_protection_component = stats::median(.data$rim_protection_component, na.rm = TRUE),
      median_versatility_component = stats::median(.data$versatility_component, na.rm = TRUE),
      median_defensive_cost_component = stats::median(.data$defensive_cost_component, na.rm = TRUE)
    )
)

message("Requested player examples:")
print(
  player_def_components %>%
    dplyr::filter(stringr::str_detect(.data$player_name, "Luka|Doncic|Dončić|LeBron|Austin Reaves|Alex Caruso|Gobert|Wembanyama|Jrue Holiday|Dyson Daniels")) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "on_ball_component",
      "disruption_component",
      "rim_protection_component",
      "versatility_component",
      "defensive_cost_component",
      "component_availability_flags",
      "proxy_only_flag",
      "DEF_component_note"
    )
)

message("Available defensive signals:")
print(available_signals)

message("Unavailable defensive signals:")
print(unavailable_signals)

message("Missingness by component:")
print(
  player_def_components %>%
    dplyr::summarise(
      missing_on_ball_pct = 100 * mean(is.na(.data$on_ball_component)),
      missing_disruption_pct = 100 * mean(is.na(.data$disruption_component)),
      missing_rim_protection_pct = 100 * mean(is.na(.data$rim_protection_component)),
      missing_versatility_pct = 100 * mean(is.na(.data$versatility_component)),
      missing_defensive_cost_pct = 100 * mean(is.na(.data$defensive_cost_component))
    )
)

message("Components relying only on proxies:")
print(
  player_def_components %>%
    dplyr::filter(.data$proxy_only_flag) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "component_availability_flags",
      "DEF_component_note"
    ) %>%
    utils::head(30)
)

message("Saved defensive component metrics to: ", def_components_path)
message("Phase 25 note: defensive components only. No final DEF_score was built.")
