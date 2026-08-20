# ============================================================
# 25b_build_DEF_proxy_components.R
# Phase 25b: Build defensive proxy components.
#
# Goal:
# Repair missing defensive component coverage with explicitly labeled proxy
# components. These are not direct DEF components and do not overwrite Phase 25.
#
# This phase does NOT build DEF_score. Missing unavailable proxy inputs remain
# NA, and every proxy-built value is flagged through proxy_source/proxy_note.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

defense_output_dir <- "outputs/defense"
def_proxy_components_path <- file.path(defense_output_dir, "player_DEF_proxy_components.parquet")

player_master_path <- glue("data/processed/player/player_master_{season}.parquet")
player_base_path <- glue("data/raw/player/player_base_{season}.parquet")
player_misc_path <- glue("data/raw/player/player_misc_{season}.parquet")
player_advanced_path <- glue("data/raw/player/player_advanced_{season}.parquet")
def_components_path <- "outputs/defense/player_DEF_components.parquet"
tracking_creation_audit_path <- "outputs/attacks/tracking_creation_audit.parquet"

fs::dir_create(defense_output_dir)

if (!file.exists(player_master_path)) {
  stop("Missing player master input: ", player_master_path, ". Run 02_player_season.R first.", call. = FALSE)
}

safe_read_parquet <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(player_id = character()))
  }

  read_project_parquet(path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols() %>%
    dplyr::mutate(player_id = as.character(.data$player_id))
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

collapse_available_sources <- function(...) {
  values <- list(...)
  names <- names(values)
  available <- names[!is.na(unlist(values))]

  if (length(available) == 0) {
    return(NA_character_)
  }

  paste(available, collapse = " | ")
}

player_master <- safe_read_parquet(player_master_path) %>%
  add_missing_cols(c(
    "player_name",
    "nickname",
    "player_nickname",
    "team_abbreviation",
    "team_abbr",
    "height",
    "height_inches",
    "weight",
    "wingspan",
    "standing_reach",
    "position"
  ), NA) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_nickname = dplyr::coalesce(as.character(.data$player_nickname), as.character(.data$nickname)),
    team_abbreviation = dplyr::coalesce(as.character(.data$team_abbreviation), as.character(.data$team_abbr)),
    height_value = coalesce_numeric_cols(., c("height_inches", "height")),
    weight_value = coalesce_numeric_cols(., c("weight")),
    wingspan_value = coalesce_numeric_cols(., c("wingspan", "wingspan_inches")),
    standing_reach_value = coalesce_numeric_cols(., c("standing_reach", "standing_reach_inches"))
  )

validate_columns(player_master, c("player_id", "player_name"))

player_base <- safe_read_parquet(player_base_path)
player_misc <- safe_read_parquet(player_misc_path)
player_advanced <- safe_read_parquet(player_advanced_path)
phase25_components <- safe_read_parquet(def_components_path)
tracking_creation_audit <- safe_read_parquet(tracking_creation_audit_path)

base_proxy <- player_base %>%
  dplyr::transmute(
    player_id = as.character(.data$player_id),
    minutes = coalesce_numeric_cols(., c("min", "minutes", "minutes_played")),
    steals = coalesce_numeric_cols(., c("stl", "steals")),
    blocks = coalesce_numeric_cols(., c("blk", "blocks")),
    fouls_committed = coalesce_numeric_cols(., c("pf", "personal_fouls"))
  )

misc_proxy <- player_misc %>%
  dplyr::transmute(
    player_id = as.character(.data$player_id),
    deflections = coalesce_numeric_cols(., c("deflections", "deflect")),
    loose_balls_recovered = coalesce_numeric_cols(., c("loose_balls_recovered", "loose_ball_recoveries", "loose_balls")),
    charges_drawn = coalesce_numeric_cols(., c("charges_drawn", "charge_drawn", "charges"))
  )

advanced_proxy <- player_advanced %>%
  dplyr::transmute(
    player_id = as.character(.data$player_id),
    defensive_rating = coalesce_numeric_cols(., c("def_rating", "defensive_rating", "d_rating")),
    defensive_box_plus_minus = coalesce_numeric_cols(., c("dbpm", "defensive_box_plus_minus"))
  )

phase25_proxy <- phase25_components %>%
  add_missing_cols(c(
    "on_ball_component",
    "versatility_component",
    "on_ball_available",
    "versatility_available",
    "contest_tracking_rate",
    "matchup_signal_available",
    "defensive_cost_component",
    "proxy_only_flag"
  ), NA) %>%
  dplyr::select(
    "player_id",
    phase25_on_ball_component = "on_ball_component",
    phase25_versatility_component = "versatility_component",
    phase25_on_ball_available = "on_ball_available",
    phase25_versatility_available = "versatility_available",
    "contest_tracking_rate",
    "matchup_signal_available",
    "defensive_cost_component",
    "proxy_only_flag"
  )

matchup_proxy <- tracking_creation_audit %>%
  add_missing_cols(c("has_matchup", "matched_keyword_columns", "row_count"), NA) %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    defensive_matchup_frequency = sum(suppressWarnings(as.numeric(.data$row_count)), na.rm = TRUE),
    matchup_information_available = any(dplyr::coalesce(as.logical(.data$has_matchup), FALSE), na.rm = TRUE) |
      any(stringr::str_detect(stringr::str_to_lower(dplyr::coalesce(as.character(.data$matched_keyword_columns), "")), "matchup|defender|opponent"), na.rm = TRUE),
    .groups = "drop"
  )

def_proxy_components <- player_master %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "height_value",
    "weight_value",
    "wingspan_value",
    "standing_reach_value",
    tidyselect::any_of("position")
  ) %>%
  dplyr::left_join(base_proxy, by = "player_id") %>%
  dplyr::left_join(misc_proxy, by = "player_id") %>%
  dplyr::left_join(advanced_proxy, by = "player_id") %>%
  dplyr::left_join(phase25_proxy, by = "player_id") %>%
  dplyr::left_join(matchup_proxy, by = "player_id") %>%
  dplyr::mutate(
    foul_rate = safe_divide(.data$fouls_committed, .data$minutes),
    activity_events = dplyr::coalesce(.data$steals, 0) +
      dplyr::coalesce(.data$blocks, 0) +
      dplyr::coalesce(.data$deflections, 0) +
      dplyr::coalesce(.data$loose_balls_recovered, 0) +
      dplyr::coalesce(.data$charges_drawn, 0),
    activity_rate = safe_divide(.data$activity_events, .data$minutes),
    steal_component = scale_0_100(.data$steals),
    activity_component = scale_0_100(.data$activity_rate),
    foul_pressure_component = scale_0_100(.data$foul_rate),
    matchup_component = dplyr::if_else(
      .data$matchup_information_available %in% TRUE,
      scale_0_100(.data$defensive_matchup_frequency),
      NA_real_
    ),
    contest_component = scale_0_100(.data$contest_tracking_rate),
    defensive_rating_component = scale_0_100(.data$defensive_rating, reverse = TRUE),
    dbpm_component = scale_0_100(.data$defensive_box_plus_minus),
    on_ball_proxy_component = row_mean_available(
      .data$steal_component,
      .data$activity_component,
      .data$foul_pressure_component,
      .data$matchup_component,
      .data$contest_component,
      .data$defensive_rating_component,
      .data$dbpm_component
    ),
    height_component = scale_0_100(.data$height_value),
    weight_component = scale_0_100(.data$weight_value),
    wingspan_component = scale_0_100(.data$wingspan_value),
    standing_reach_component = scale_0_100(.data$standing_reach_value),
    positional_diversity_component = dplyr::if_else(
      !is.na(.data$position) & stringr::str_detect(as.character(.data$position), "-|/|,"),
      100,
      NA_real_
    ),
    matchup_diversity_component = dplyr::if_else(
      .data$matchup_information_available %in% TRUE,
      scale_0_100(.data$defensive_matchup_frequency),
      NA_real_
    ),
    mobility_indicator_component = scale_0_100(.data$activity_rate),
    versatility_proxy_component = row_mean_available(
      .data$height_component,
      .data$weight_component,
      .data$wingspan_component,
      .data$standing_reach_component,
      .data$matchup_diversity_component,
      .data$mobility_indicator_component,
      .data$positional_diversity_component
    ),
    on_ball_proxy_built = !is.na(.data$on_ball_proxy_component),
    versatility_proxy_built = !is.na(.data$versatility_proxy_component),
    proxy_source = mapply(
      collapse_available_sources,
      steals = .data$steal_component,
      fouls_committed = .data$foul_pressure_component,
      matchup_information = .data$matchup_component,
      shot_contests = .data$contest_component,
      defensive_activity = .data$activity_component,
      defensive_rating = .data$defensive_rating_component,
      dbpm = .data$dbpm_component,
      height = .data$height_component,
      weight = .data$weight_component,
      wingspan = .data$wingspan_component,
      standing_reach = .data$standing_reach_component,
      matchup_diversity = .data$matchup_diversity_component,
      mobility_indicator = .data$mobility_indicator_component,
      positional_diversity = .data$positional_diversity_component,
      SIMPLIFY = TRUE
    ),
    proxy_note = dplyr::case_when(
      .data$on_ball_proxy_built | .data$versatility_proxy_built ~
        "Proxy-built defensive components only. Do not treat as direct DEF or overwrite Phase 25 components.",
      TRUE ~ "No sufficient proxy inputs available; proxy components remain NA."
    )
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "on_ball_proxy_component",
    "versatility_proxy_component",
    "proxy_source",
    "proxy_note",
    "on_ball_proxy_built",
    "versatility_proxy_built",
    "steals",
    "fouls_committed",
    "foul_rate",
    "activity_rate",
    "defensive_matchup_frequency",
    "matchup_information_available",
    "contest_tracking_rate",
    "height_value",
    "weight_value",
    "wingspan_value",
    "standing_reach_value",
    tidyselect::any_of(c(
      "phase25_on_ball_component",
      "phase25_versatility_component",
      "phase25_on_ball_available",
      "phase25_versatility_available",
      "proxy_only_flag"
    ))
  ) %>%
  dplyr::arrange(.data$player_name)

write_project_parquet(def_proxy_components, def_proxy_components_path)

message("Phase 25b defensive proxy diagnostics:")

message("Coverage:")
print(
  def_proxy_components %>%
    dplyr::summarise(
      players = dplyr::n(),
      on_ball_proxy_built_players = sum(.data$on_ball_proxy_built, na.rm = TRUE),
      versatility_proxy_built_players = sum(.data$versatility_proxy_built, na.rm = TRUE),
      phase25_missing_on_ball_with_proxy = sum(!dplyr::coalesce(.data$phase25_on_ball_available, FALSE) & .data$on_ball_proxy_built, na.rm = TRUE),
      phase25_missing_versatility_with_proxy = sum(!dplyr::coalesce(.data$phase25_versatility_available, FALSE) & .data$versatility_proxy_built, na.rm = TRUE)
    )
)

message("Missingness:")
print(
  def_proxy_components %>%
    dplyr::summarise(
      missing_on_ball_proxy_pct = 100 * mean(is.na(.data$on_ball_proxy_component)),
      missing_versatility_proxy_pct = 100 * mean(is.na(.data$versatility_proxy_component)),
      missing_proxy_source_pct = 100 * mean(is.na(.data$proxy_source))
    )
)

message("Proxy reliance:")
print(
  def_proxy_components %>%
    dplyr::count(.data$proxy_source, sort = TRUE) %>%
    utils::head(25)
)

message("Requested player examples:")
print(
  def_proxy_components %>%
    dplyr::filter(stringr::str_detect(.data$player_name, "Luka|Doncic|Dončić|LeBron|Caruso|Gobert|Wembanyama|Jrue|Holiday|Dyson|Daniels")) %>%
    dplyr::select(
      "player_name",
      "team_abbreviation",
      "on_ball_proxy_component",
      "versatility_proxy_component",
      "proxy_source",
      "proxy_note",
      "phase25_on_ball_available",
      "phase25_versatility_available"
    )
)

message("Saved defensive proxy components to: ", def_proxy_components_path)
message("Phase 25b note: proxy components only. Phase 25 components were not overwritten and no DEF_score was built.")
