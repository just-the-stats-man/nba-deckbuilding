# ============================================================
# 11_build_player_attacks.R
# Phase 9: Prototype offensive attack / moveset framework.
#
# This phase establishes the first architecture for player offensive attacks.
# It intentionally builds a transparent library of repeatable attack events and
# player-level prototype metrics, but it does NOT create final ATK scores, AP
# scores, fusion logic, attribute interactions, defensive models, or Bayesian
# shrinkage.
#
# Important modeling note:
# These attack labels are scaffolding. They are rule-based definitions derived
# from currently available hoopR play-by-play text plus any shot/play-type
# fields that happen to be present in local raw files. Future versions should
# replace or augment these rules with richer NBA tracking/Synergy-style play
# type tables, possession parsing, opponent context, and player-specific attack
# clustering. The taxonomy below is expected to evolve.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

attacks_dir <- "outputs/attacks"
fs::dir_create(attacks_dir)

pbp_path <- glue("data/raw/pbp/{team_abbr}_pbp_sample_{season}.parquet")
player_master_path <- glue("data/processed/player/player_master_{season}.parquet")
player_scoring_path <- glue("data/raw/player/player_scoring_{season}.parquet")
player_usage_path <- glue("data/raw/player/player_usage_{season}.parquet")

player_attack_library_path <- file.path(attacks_dir, "player_attack_library.parquet")
attack_summary_path <- file.path(attacks_dir, "attack_summary.parquet")

if (!file.exists(pbp_path)) {
  stop("Missing play-by-play data: ", pbp_path, ". Run 03_pull_games_pbp_rotations.R first.", call. = FALSE)
}

if (!file.exists(player_master_path)) {
  stop("Missing player master data: ", player_master_path, ". Run 02_player_season.R first.", call. = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    y
  } else {
    x
  }
}

first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)][1]

  if (is.na(hit)) {
    return(NULL)
  }

  hit
}

value_or_na <- function(df, candidates, type = c("character", "numeric", "logical")) {
  type <- rlang::arg_match(type)
  col <- first_existing(df, candidates)

  if (is.null(col)) {
    if (type == "numeric") {
      return(rep(NA_real_, nrow(df)))
    }

    if (type == "logical") {
      return(rep(NA, nrow(df)))
    }

    return(rep(NA_character_, nrow(df)))
  }

  x <- df[[col]]

  if (type == "numeric") {
    return(suppressWarnings(as.numeric(x)))
  }

  if (type == "logical") {
    return(as.logical(x))
  }

  as.character(x)
}

mean_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

quantile_or_na <- function(x, prob) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

sum_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  sum(x, na.rm = TRUE)
}

safe_divide <- function(num, den) {
  dplyr::if_else(!is.na(den) & den > 0, num / den, NA_real_)
}

add_missing_numeric_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- NA_real_
  }

  df
}

add_missing_character_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- NA_character_
  }

  df
}

normalize_id_cols <- function(df, cols = c("player_id", "team_id", "game_id")) {
  existing_cols <- intersect(cols, names(df))

  df %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(existing_cols), as.character))
}

coalesce_character <- function(...) {
  dplyr::coalesce(!!!rlang::list2(...))
}

standardize_attack_name <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

# Prototype taxonomy:
# The v0.1 attack taxonomy is intentionally limited to labels that the audit
# can observe cleanly. Classification checks structured sub_type first, then
# falls back to description text only when sub_type does not identify a variant.
# Transition, handoff, screen, and isolation are not attack variants in v0.1.
classify_attack_variant_from_text <- function(text) {
  text <- stringr::str_to_lower(dplyr::coalesce(text, ""))

  dplyr::case_when(
    stringr::str_detect(text, "catch[ -]?and[ -]?shoot|catch.?shoot|\\bc&s\\b") &
      stringr::str_detect(text, "3pt|3-pt|three point|three-point|3 point") ~ "catch-and-shoot 3",
    stringr::str_detect(text, "spot[ -]?up") &
      stringr::str_detect(text, "3pt|3-pt|three point|three-point|3 point") ~ "spot-up 3",
    stringr::str_detect(text, "alley[ -]?oop|lob") ~ "alley oop",
    stringr::str_detect(text, "putback|put back") ~ "putback",
    stringr::str_detect(text, "\\btip|tip-in|tip in") ~ "tip",
    stringr::str_detect(text, "driving.*layup|layup.*driving") ~ "driving layup",
    stringr::str_detect(text, "dunk") ~ "dunk",
    stringr::str_detect(text, "layup") ~ "layup",
    stringr::str_detect(text, "floating|floater|runner") ~ "floater",
    stringr::str_detect(text, "hook") ~ "hook",
    stringr::str_detect(text, "step[ -]?back|stepback") ~ "stepback jumper",
    stringr::str_detect(text, "pull[ -]?up|pullup") ~ "pullup jumper",
    stringr::str_detect(text, "fadeaway|fade away") ~ "fadeaway jumper",
    stringr::str_detect(text, "\\bcut\\b|cutting") ~ "cut",
    TRUE ~ NA_character_
  )
}

attack_family_for_variant <- function(attack_variant) {
  dplyr::case_when(
    attack_variant %in% c("layup", "driving layup", "dunk", "alley oop", "putback", "tip") ~ "Rim Pressure",
    attack_variant %in% c("floater", "hook") ~ "Touch / In-Between",
    attack_variant %in% c("pullup jumper", "stepback jumper", "fadeaway jumper") ~ "Pull-Up / Jumper",
    attack_variant == "cut" ~ "Off-Ball",
    attack_variant %in% c(
      "catch-and-shoot 3",
      "spot-up 3",
      "jump shot 3",
      "midrange jump shot",
      "short jump shot"
    ) ~ "Spot-Up / Set Shot",
    TRUE ~ NA_character_
  )
}

classify_generic_jump_shot <- function(shot_value, shot_distance, sub_type, description_text) {
  text <- stringr::str_to_lower(dplyr::coalesce(sub_type, ""))
  fallback_text <- stringr::str_to_lower(dplyr::coalesce(description_text, ""))
  combined_text <- paste(text, fallback_text)
  dist <- suppressWarnings(as.numeric(shot_distance))

  dplyr::case_when(
    shot_value == 3 & stringr::str_detect(combined_text, "catch[ -]?and[ -]?shoot|catch.?shoot|\\bc&s\\b") ~ "catch-and-shoot 3",
    shot_value == 3 & stringr::str_detect(combined_text, "spot[ -]?up") ~ "spot-up 3",
    shot_value == 3 & stringr::str_detect(combined_text, "jump|jumper|shot") ~ "jump shot 3",
    shot_value == 2 & stringr::str_detect(combined_text, "jump|jumper|shot") &
      !is.na(dist) & dist <= 8 ~ "short jump shot",
    shot_value == 2 & stringr::str_detect(combined_text, "jump|jumper|shot") ~ "midrange jump shot",
    TRUE ~ NA_character_
  )
}

classify_attack_taxonomy <- function(sub_type, description_text, shot_value, shot_distance) {
  sub_type_variant <- classify_attack_variant_from_text(sub_type)
  description_variant <- classify_attack_variant_from_text(description_text)
  generic_jump_variant <- classify_generic_jump_shot(shot_value, shot_distance, sub_type, description_text)
  attack_variant <- dplyr::coalesce(sub_type_variant, description_variant, generic_jump_variant)

  tibble::tibble(
    attack_family = attack_family_for_variant(attack_variant),
    attack_variant = attack_variant,
    attack_variant_id = standardize_attack_name(attack_variant),
    attack_source_column = dplyr::case_when(
      !is.na(sub_type_variant) ~ "sub_type",
      !is.na(description_variant) ~ "description_fallback",
      !is.na(generic_jump_variant) ~ "shot_value_distance_text_fallback",
      TRUE ~ NA_character_
    )
  )
}

infer_shot_value <- function(event_text, explicit_points, shot_distance) {
  text <- stringr::str_to_lower(dplyr::coalesce(event_text, ""))
  points <- suppressWarnings(as.numeric(explicit_points))
  dist <- suppressWarnings(as.numeric(shot_distance))

  dplyr::case_when(
    !is.na(points) & points >= 3 ~ 3,
    stringr::str_detect(text, "3pt|3-pt|three point|three-point|3 point") ~ 3,
    !is.na(dist) & dist >= 22 ~ 3,
    stringr::str_detect(text, "free throw|freethrow") ~ NA_real_,
    stringr::str_detect(text, "miss|missed|blocked|shot|layup|dunk|hook|jumper|floating|runner") ~ 2,
    TRUE ~ NA_real_
  )
}

infer_made_shot <- function(event_text, event_msg_type, made_flag, explicit_points) {
  text <- stringr::str_to_lower(dplyr::coalesce(event_text, ""))
  msg_type <- suppressWarnings(as.numeric(event_msg_type))
  flag <- suppressWarnings(as.numeric(made_flag))
  points <- suppressWarnings(as.numeric(explicit_points))

  dplyr::case_when(
    !is.na(msg_type) & msg_type == 1 ~ TRUE,
    !is.na(msg_type) & msg_type == 2 ~ FALSE,
    !is.na(points) & points > 0 ~ TRUE,
    !is.na(flag) & flag == 1 ~ TRUE,
    !is.na(flag) & flag == 0 ~ FALSE,
    stringr::str_detect(text, "\\bmiss|missed|blocked") ~ FALSE,
    stringr::str_detect(text, "makes|made|scores|dunk|layup") ~ TRUE,
    TRUE ~ NA
  )
}

infer_field_goal_attempt <- function(event_text, event_type, event_msg_type, shot_value, made_shot) {
  text <- stringr::str_to_lower(dplyr::coalesce(event_text, ""))
  event <- stringr::str_to_lower(dplyr::coalesce(event_type, ""))
  msg_type <- suppressWarnings(as.numeric(event_msg_type))

  dplyr::case_when(
    !is.na(msg_type) & msg_type %in% c(1, 2) ~ TRUE,
    stringr::str_detect(event, "made shot|missed shot|field goal") ~ TRUE,
    stringr::str_detect(text, "\\bmiss|missed|blocked|makes|made|shot|layup|dunk|hook|jumper|floating|runner") &
      !stringr::str_detect(text, "free throw|freethrow|turnover") ~ TRUE,
    !is.na(shot_value) & made_shot %in% c(TRUE, FALSE) ~ TRUE,
    TRUE ~ FALSE
  )
}

normalize_pbp_for_attacks <- function(pbp) {
  # hoopR/NBA Stats API column names have changed across endpoints and package
  # versions. This function isolates that volatility so the attack architecture
  # can stay stable while input schemas mature.
  description_cols <- c(
    "event_type",
    "shot_result",
    "shot_status",
    "description",
    "play_description",
    "action_type",
    "sub_type",
    "qualifiers",
    "homedescription",
    "visitor_description",
    "home_description",
    "visitor_description",
    "neutral_description"
  )
  description_cols <- description_cols[description_cols %in% names(pbp)]
  fallback_description_cols <- setdiff(
    description_cols,
    c("sub_type", "action_type", "event_type", "event_msg_type", "eventmsgtype")
  )

  event_text <- if (length(description_cols) > 0) {
    pbp %>%
      dplyr::select(tidyselect::all_of(description_cols)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      tidyr::unite("event_text", dplyr::everything(), sep = " | ", na.rm = TRUE) %>%
      dplyr::pull(.data$event_text)
  } else {
    rep(NA_character_, nrow(pbp))
  }

  description_text <- if (length(fallback_description_cols) > 0) {
    pbp %>%
      dplyr::select(tidyselect::all_of(fallback_description_cols)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      tidyr::unite("description_text", dplyr::everything(), sep = " | ", na.rm = TRUE) %>%
      dplyr::pull(.data$description_text)
  } else {
    rep(NA_character_, nrow(pbp))
  }

  sub_type <- value_or_na(pbp, c("sub_type"), "character")

  event_type <- value_or_na(
    pbp,
    c("event_type", "event_msg_type", "eventmsgtype", "action_type"),
    "character"
  )
  event_msg_type <- value_or_na(
    pbp,
    c("event_msg_type", "eventmsgtype"),
    "numeric"
  )
  event_type_lower <- stringr::str_to_lower(dplyr::coalesce(event_type, ""))
  event_text_lower <- stringr::str_to_lower(dplyr::coalesce(event_text, ""))

  explicit_points <- value_or_na(
    pbp,
    c("points", "pts", "score_value", "points_scored"),
    "numeric"
  )
  explicit_shot_value <- value_or_na(
    pbp,
    c("shot_value", "shot_type_value"),
    "numeric"
  )
  explicit_shot_value <- dplyr::if_else(
    explicit_shot_value %in% c(2, 3),
    explicit_shot_value,
    NA_real_
  )
  shot_distance <- value_or_na(
    pbp,
    c("shot_distance", "shot_distance_ft", "distance", "shot_distance_feet"),
    "numeric"
  )
  made_flag <- value_or_na(
    pbp,
    c("shot_made_flag", "made", "is_made", "made_shot"),
    "numeric"
  )

  shot_value <- dplyr::coalesce(
    explicit_shot_value,
    infer_shot_value(event_text, explicit_points, shot_distance)
  )
  made_shot <- infer_made_shot(event_text, event_msg_type, made_flag, explicit_points)
  is_field_goal_attempt <- infer_field_goal_attempt(
    event_text,
    event_type,
    event_msg_type,
    shot_value,
    made_shot
  )
  made_shot <- dplyr::if_else(
    is_field_goal_attempt & is.na(made_shot),
    FALSE,
    made_shot
  )
  shot_value <- dplyr::case_when(
    is_field_goal_attempt & is.na(shot_value) &
      stringr::str_detect(event_text_lower, "3pt|3-pt|three point|three-point|3 point") ~ 3,
    is_field_goal_attempt & is.na(shot_value) ~ 2,
    TRUE ~ shot_value
  )
  rebound_event <- stringr::str_detect(event_type_lower, "rebound") |
    stringr::str_detect(event_text_lower, "offensive rebound|putback|tip")
  attack_taxonomy <- classify_attack_taxonomy(sub_type, description_text, shot_value, shot_distance)

  tibble::tibble(
    game_id = value_or_na(pbp, c("game_id"), "character"),
    event_id = coalesce_character(
      value_or_na(pbp, c("action_number", "event_num", "eventnum", "event_id"), "character"),
      as.character(seq_len(nrow(pbp)))
    ),
    period = value_or_na(pbp, c("period"), "numeric"),
    clock = value_or_na(pbp, c("clock", "pctimestring", "game_clock"), "character"),
    player_id = value_or_na(
      pbp,
      c("person_id", "player1_id", "player_id", "athlete_id", "person1_id"),
      "character"
    ),
    raw_pbp_player_name = value_or_na(
      pbp,
      c("player_name", "player1_name", "athlete_display_name", "person1_name"),
      "character"
    ),
    team_id = value_or_na(pbp, c("team_id", "player1_team_id"), "character"),
    team_abbreviation = value_or_na(
      pbp,
      c("team_abbreviation", "team_tricode", "player1_team_abbreviation", "player1_team_tricode"),
      "character"
    ),
    event_type = event_type,
    event_msg_type = event_msg_type,
    sub_type = sub_type,
    description_text = description_text,
    event_text = event_text,
    shot_value = shot_value,
    is_field_goal_attempt = is_field_goal_attempt,
    shot_distance = shot_distance,
    made_shot = made_shot,
    points = dplyr::case_when(
      is_field_goal_attempt & made_shot %in% TRUE & shot_value == 3 ~ 3,
      is_field_goal_attempt & made_shot %in% TRUE & shot_value == 2 ~ 2,
      TRUE ~ 0
    ),
    # Creation attribution is intentionally unavailable in Phase 11. Earlier
    # prototypes inferred assisted/self-created flags from PBP text/player2
    # columns, but that is not reliable enough for ATK or CR. Later phases
    # should use real tracking/assist/matchup endpoints for creation signals.
    assisted = NA,
    x = value_or_na(pbp, c("x", "loc_x", "x_legacy"), "numeric"),
    y = value_or_na(pbp, c("y", "loc_y", "y_legacy"), "numeric"),
    is_offensive_attack_event = is_field_goal_attempt &
      !stringr::str_detect(event_text_lower, "free throw|turnover")
  ) %>%
    dplyr::bind_cols(attack_taxonomy) %>%
    dplyr::mutate(
      attack_definition_version = "v0.1_rule_scaffold",
      source_detail = "hoopR play-by-play sub_type-first rule scaffold"
    ) %>%
    dplyr::filter(
      .data$is_offensive_attack_event,
      !is.na(.data$player_id)
    )
}

read_optional_player_profile <- function(path, label) {
  if (!file.exists(path)) {
    message("Optional player profile not found: ", path)
    return(tibble::tibble(player_id = character()))
  }

  read_project_parquet(path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols() %>%
    normalize_id_cols(c("player_id", "team_id")) %>%
    dplyr::rename_with(~ paste(label, .x, sep = "_"), -tidyselect::any_of(c(
      "player_id",
      "player_name",
      "team_id",
      "team_abbreviation"
    )))
}

pbp <- read_project_parquet(pbp_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  normalize_id_cols(c(
    "player_id",
    "person_id",
    "player1_id",
    "athlete_id",
    "person1_id",
    "team_id",
    "player1_team_id",
    "game_id"
  ))

player_master <- read_project_parquet(player_master_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  normalize_id_cols(c("player_id", "team_id"))

player_scoring <- read_optional_player_profile(player_scoring_path, "scoring")
player_usage <- read_optional_player_profile(player_usage_path, "usage")

validate_columns(player_master, c("player_id", "player_name", "team_abbreviation"))

attack_events <- normalize_pbp_for_attacks(pbp)

if (nrow(attack_events) == 0) {
  stop(
    "No offensive field goal attempts could be read from PBP. Inspect columns in ",
    pbp_path,
    " and extend normalize_pbp_for_attacks() with the returned hoopR schema.",
    call. = FALSE
  )
}

classified_attack_events <- attack_events %>%
  dplyr::filter(!is.na(.data$attack_family), !is.na(.data$attack_variant))

if (nrow(classified_attack_events) == 0) {
  stop(
    "No offensive field goal attempts could be assigned to attack variants. ",
    "Run analysis/02_audit_attack_source_fields.R and inspect sub_type/description evidence.",
    call. = FALSE
  )
}

unclassified_attack_events <- attack_events %>%
  dplyr::filter(is.na(.data$attack_family) | is.na(.data$attack_variant))

attack_classification_diagnostics <- tibble::tibble(
  total_field_goal_attempts_in_pbp = sum(attack_events$is_field_goal_attempt, na.rm = TRUE),
  classified_field_goal_attempts = sum(classified_attack_events$is_field_goal_attempt, na.rm = TRUE),
  unclassified_field_goal_attempts = sum(unclassified_attack_events$is_field_goal_attempt, na.rm = TRUE),
  classified_attempt_rate = safe_divide(
    sum(classified_attack_events$is_field_goal_attempt, na.rm = TRUE),
    sum(attack_events$is_field_goal_attempt, na.rm = TRUE)
  )
)

unclassified_sub_type_examples <- unclassified_attack_events %>%
  dplyr::count(sub_type = as.character(.data$sub_type), sort = TRUE, name = "attempts") %>%
  dplyr::slice_head(n = 20)

unclassified_description_examples <- unclassified_attack_events %>%
  dplyr::count(description_text = as.character(.data$description_text), sort = TRUE, name = "attempts") %>%
  dplyr::slice_head(n = 20)

player_totals <- classified_attack_events %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    player_attack_possessions = sum(.data$is_field_goal_attempt, na.rm = TRUE),
    player_attack_points = sum(.data$points, na.rm = TRUE),
    .groups = "drop"
  )

player_reference <- player_master %>%
  dplyr::select(
    tidyselect::any_of(c(
      "player_id",
      "player_name",
      "nickname",
      "team_id",
      "team_abbreviation",
      "gp",
      "min",
      "poss",
      "pts",
      "fga",
      "fg3a",
      "fg3m",
      "fg_pct",
      "fg3_pct",
      "pct_ast_2pm",
      "pct_uast_2pm",
      "pct_ast_3pm",
      "pct_uast_3pm",
      "usg_pct"
    ))
  ) %>%
  dplyr::rename(
    canonical_player_name = "player_name",
    canonical_team_abbreviation = "team_abbreviation"
  ) %>%
  dplyr::rename_with(
    ~ "player_nickname",
    tidyselect::any_of("nickname")
  ) %>%
  dplyr::left_join(
    player_scoring %>%
      dplyr::select(tidyselect::any_of(c(
        "player_id",
        "scoring_pct_ast_2pm",
        "scoring_pct_uast_2pm",
        "scoring_pct_ast_3pm",
        "scoring_pct_uast_3pm",
        "scoring_pct_pts_2pt_mr",
        "scoring_pct_pts_fb",
        "scoring_pct_pts_off_tov",
        "scoring_pct_pts_paint",
        "scoring_pct_fga_2pt",
        "scoring_pct_fga_3pt"
      ))),
    by = "player_id"
  ) %>%
  dplyr::left_join(
    player_usage %>%
      dplyr::select(tidyselect::any_of(c(
        "player_id",
        "usage_usage_pct",
        "usage_pct_fga",
        "usage_pct_fg3a",
        "usage_pct_pts",
        "usage_pct_ast",
        "usage_pct_poss"
      ))),
    by = "player_id"
  ) %>%
  add_missing_character_cols(c(
    "canonical_player_name",
    "player_nickname",
    "team_id",
    "canonical_team_abbreviation"
  )) %>%
  add_missing_numeric_cols(c(
    "pct_ast_2pm",
    "pct_uast_2pm",
    "pct_ast_3pm",
    "pct_uast_3pm",
    "scoring_pct_ast_2pm",
    "scoring_pct_uast_2pm",
    "scoring_pct_ast_3pm",
    "scoring_pct_uast_3pm",
    "usage_usage_pct"
  ))

player_attack_library <- classified_attack_events %>%
  dplyr::group_by(.data$player_id, .data$attack_family, .data$attack_variant, .data$attack_variant_id) %>%
  dplyr::summarise(
    raw_pbp_player_name = dplyr::first(stats::na.omit(.data$raw_pbp_player_name), default = NA_character_),
    team_id = dplyr::first(stats::na.omit(.data$team_id), default = NA_character_),
    team_abbreviation = dplyr::first(stats::na.omit(.data$team_abbreviation), default = NA_character_),
    attempts = sum(.data$is_field_goal_attempt, na.rm = TRUE),
    makes = sum(.data$made_shot %in% TRUE, na.rm = TRUE),
    misses = sum(.data$made_shot %in% FALSE, na.rm = TRUE),
    made_2s = sum(.data$made_shot %in% TRUE & .data$shot_value == 2, na.rm = TRUE),
    made_3s = sum(.data$made_shot %in% TRUE & .data$shot_value == 3, na.rm = TRUE),
    missed_2s = sum(.data$made_shot %in% FALSE & .data$shot_value == 2, na.rm = TRUE),
    missed_3s = sum(.data$made_shot %in% FALSE & .data$shot_value == 3, na.rm = TRUE),
    three_point_attempts = sum(.data$is_field_goal_attempt & .data$shot_value == 3, na.rm = TRUE),
    two_point_attempts = sum(.data$is_field_goal_attempt & .data$shot_value == 2, na.rm = TRUE),
    assisted_attempts = NA_real_,
    self_created_attempts = NA_real_,
    average_shot_distance = mean_or_na(.data$shot_distance),
    median_shot_distance = stats::median(.data$shot_distance, na.rm = TRUE),
    min_shot_distance = suppressWarnings(min(.data$shot_distance, na.rm = TRUE)),
    max_shot_distance = suppressWarnings(max(.data$shot_distance, na.rm = TRUE)),
    made_shot_rate = mean_or_na(.data$made_shot),
    ppp_event_variance = stats::var(.data$points, na.rm = TRUE),
    sample_game_count = dplyr::n_distinct(.data$game_id, na.rm = TRUE),
    attack_source_column = paste(sort(unique(stats::na.omit(.data$attack_source_column))), collapse = " | "),
    attack_definition_version = dplyr::first(.data$attack_definition_version),
    source_detail = dplyr::first(.data$source_detail),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    min_shot_distance = dplyr::if_else(is.infinite(.data$min_shot_distance), NA_real_, .data$min_shot_distance),
    max_shot_distance = dplyr::if_else(is.infinite(.data$max_shot_distance), NA_real_, .data$max_shot_distance),
    points = 2 * .data$made_2s + 3 * .data$made_3s,
    # TODO: possessions are field goal attempts for this prototype. Future
    # versions should count true attack possessions, including turnovers,
    # shooting fouls, and and-ones.
    possessions = .data$attempts,
    points_per_attempt = safe_divide(.data$points, .data$attempts),
    expected_damage_per_attempt = .data$points_per_attempt,
    # Compatibility alias only. This is shot-attempt based and should not be
    # interpreted as true possession PPP until turnovers, shooting fouls, and
    # and-ones are included.
    ppp = .data$points_per_attempt,
    frequency = NA_real_,
    efficiency = safe_divide(.data$makes, .data$attempts),
    effective_fg_pct = safe_divide(.data$made_2s + 1.5 * .data$made_3s, .data$attempts),
    creation_signal_available = FALSE,
    assisted_attempt_rate = NA_real_,
    self_created_attempt_rate = NA_real_,
    usage_volume = .data$possessions,
    reliability_sample_index = .data$possessions / (.data$possessions + 20),
    estimated_variance = dplyr::if_else(
      .data$possessions > 1,
      .data$ppp_event_variance / .data$possessions,
      NA_real_
    ),
    notes = paste(
      "Prototype attack definition only.",
      "Damage metrics are shot-attempt based points per attempt, not true possession PPP.",
      "Creation attribution is unavailable in this PBP scaffold; assisted/self-created fields are NA and must not feed ATK or CR.",
      "Rule-based scaffold; no final ATK/AP/DEF model, fusion logic, interactions, or Bayesian shrinkage."
    )
  ) %>%
  dplyr::left_join(player_totals, by = "player_id") %>%
  dplyr::mutate(
    frequency = safe_divide(.data$possessions, .data$player_attack_possessions)
  ) %>%
  dplyr::left_join(player_reference, by = "player_id", suffix = c("", "_reference")) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    team_id = as.character(.data$team_id),
    team_id_reference = as.character(.data$team_id_reference),
    player_name = dplyr::coalesce(.data$canonical_player_name, .data$raw_pbp_player_name),
    team_id = dplyr::coalesce(.data$team_id, .data$team_id_reference),
    team_abbreviation = dplyr::coalesce(.data$canonical_team_abbreviation, .data$team_abbreviation),
    assisted_tendency_context = NA_real_
  ) %>%
  dplyr::select(
    tidyselect::any_of(c(
      "player_id",
      "player_name",
      "player_nickname",
      "raw_pbp_player_name",
      "team_id",
      "team_abbreviation",
      "attack_family",
      "attack_variant",
      "attack_variant_id",
      "attack_source_column",
      "attack_definition_version",
      "source_detail",
      "possessions",
      "points",
      "points_per_attempt",
      "expected_damage_per_attempt",
      "ppp",
      "frequency",
      "efficiency",
      "effective_fg_pct",
      "estimated_variance",
      "reliability_sample_index",
      "creation_signal_available",
      "assisted_attempt_rate",
      "self_created_attempt_rate",
      "assisted_tendency_context",
      "average_shot_distance",
      "median_shot_distance",
      "min_shot_distance",
      "max_shot_distance",
      "usage_volume",
      "sample_game_count",
      "makes",
      "misses",
      "attempts",
      "made_2s",
      "made_3s",
      "missed_2s",
      "missed_3s",
      "three_point_attempts",
      "two_point_attempts",
      "player_attack_possessions",
      "player_attack_points",
      "gp",
      "min",
      "poss",
      "usg_pct",
      "usage_usage_pct",
      "notes"
    ))
  ) %>%
  dplyr::arrange(.data$player_name, .data$attack_family, dplyr::desc(.data$possessions), .data$attack_variant)

# attack_summary is intentionally taxonomy-first. It describes how often each
# prototype attack appears in the currently available data and which inputs
# backed it. It is not a leaderboard and should not be consumed as a rating.
attack_summary <- player_attack_library %>%
  dplyr::group_by(.data$attack_family, .data$attack_variant, .data$attack_variant_id, .data$attack_definition_version) %>%
  dplyr::summarise(
    players_with_attack = dplyr::n_distinct(.data$player_id),
    total_possessions = sum(.data$possessions, na.rm = TRUE),
    total_points = sum(.data$points, na.rm = TRUE),
    points_per_attempt = safe_divide(.data$total_points, .data$total_possessions),
    expected_damage_per_attempt = .data$points_per_attempt,
    ppp = .data$points_per_attempt,
    average_player_frequency = mean_or_na(.data$frequency),
    median_player_frequency = stats::median(.data$frequency, na.rm = TRUE),
    average_efficiency = mean_or_na(.data$efficiency),
    average_assisted_attempt_rate = mean_or_na(.data$assisted_attempt_rate),
    average_self_created_attempt_rate = mean_or_na(.data$self_created_attempt_rate),
    average_shot_distance = mean_or_na(.data$average_shot_distance),
    median_reliability_sample_index = stats::median(.data$reliability_sample_index, na.rm = TRUE),
    attack_source_column = paste(sort(unique(stats::na.omit(.data$attack_source_column))), collapse = " | "),
    source_detail = paste(sort(unique(.data$source_detail)), collapse = " | "),
    notes = paste(
      "Prototype taxonomy summary only.",
      "Damage metrics are shot-attempt based points per attempt, not true possession PPP.",
      "Future versions may cluster attacks, add tracking/play-type sources, and revise labels."
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(.data$attack_family, dplyr::desc(.data$total_possessions), .data$attack_variant)

attack_efficiency_distribution <- player_attack_library %>%
  dplyr::summarise(
    player_attack_rows = dplyr::n(),
    min_efficiency = suppressWarnings(min(.data$efficiency, na.rm = TRUE)),
    p25_efficiency = quantile_or_na(.data$efficiency, 0.25),
    median_efficiency = stats::median(.data$efficiency, na.rm = TRUE),
    p75_efficiency = quantile_or_na(.data$efficiency, 0.75),
    max_efficiency = suppressWarnings(max(.data$efficiency, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    min_efficiency = dplyr::if_else(is.infinite(.data$min_efficiency), NA_real_, .data$min_efficiency),
    max_efficiency = dplyr::if_else(is.infinite(.data$max_efficiency), NA_real_, .data$max_efficiency)
  )

perfect_efficiency_attack_flags <- player_attack_library %>%
  dplyr::group_by(.data$attack_family, .data$attack_variant) %>%
  dplyr::summarise(
    players_with_attack = dplyr::n_distinct(.data$player_id),
    rows_with_efficiency = sum(!is.na(.data$efficiency)),
    all_players_efficiency_one = sum(!is.na(.data$efficiency)) > 0 &
      all(.data$efficiency[!is.na(.data$efficiency)] == 1),
    .groups = "drop"
  ) %>%
  dplyr::filter(.data$all_players_efficiency_one)

write_project_parquet(player_attack_library, player_attack_library_path)
write_project_parquet(attack_summary, attack_summary_path)

message("Phase 9 attack classification coverage diagnostics:")
print(attack_classification_diagnostics)

message("Top sub_type values among unclassified field goal attempts:")
print(unclassified_sub_type_examples)

message("Top description examples among unclassified field goal attempts:")
print(unclassified_description_examples)

message("Phase 9 attack shot accounting diagnostics:")
attack_accounting_diagnostics <- player_attack_library %>%
  dplyr::summarise(
    total_attempts = sum(.data$attempts, na.rm = TRUE),
    total_makes = sum(.data$makes, na.rm = TRUE),
    total_misses = sum(.data$misses, na.rm = TRUE),
    makes_less_than_or_equal_attempts = all(.data$makes <= .data$attempts, na.rm = TRUE),
    attempts_equal_makes_plus_misses = all(.data$attempts == .data$makes + .data$misses, na.rm = TRUE),
    .groups = "drop"
  )
print(attack_accounting_diagnostics)

message("Phase 9 attack efficiency distribution:")
print(attack_efficiency_distribution)

if (nrow(perfect_efficiency_attack_flags) > 0) {
  message("Phase 9 diagnostic warning: these attack variants have efficiency = 1 for all player rows:")
  print(perfect_efficiency_attack_flags)
} else {
  message("Phase 9 diagnostic: no attack variant has efficiency = 1 for all player rows.")
}

message("Phase 9 attack framework summary:")
print(
  attack_summary %>%
    dplyr::select(
      "attack_family",
      "attack_variant",
      "players_with_attack",
      "total_possessions",
      "points_per_attempt",
      "expected_damage_per_attempt",
      "average_player_frequency",
      "average_efficiency"
    )
)

message("Phase 9 note: created prototype offensive attack library only. No final ATK/AP/DEF scores, fusion logic, interactions, or Bayesian shrinkage were built.")
