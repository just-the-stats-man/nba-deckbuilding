# ============================================================
# 19b_extract_tracking_creation_metrics.R
# Phase 19b: Extract raw tracking creation metrics.
#
# Phase 19 audited endpoint/schema availability. This phase pulls actual
# returned tables from successful creation/tracking endpoints and extracts
# player-level numeric metrics that can feed a future CR model.
#
# Exact player_id matching only. Failures are retained as diagnostics and do not
# stop the script unless target player IDs fail validation.
#
# This phase does not modify previous phases.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_master_path <- "data/processed/player/player_master_2025-26.parquet"
tracking_creation_audit_path <- "outputs/attacks/tracking_creation_audit.parquet"
tracking_creation_raw_dir <- "data/raw/tracking_creation"
player_tracking_creation_metrics_path <- "outputs/attacks/player_tracking_creation_metrics.parquet"
pull_diagnostics_path <- "outputs/attacks/tracking_creation_pull_diagnostics.parquet"

fs::dir_create(tracking_creation_raw_dir)
fs::dir_create("outputs/attacks")

if (!file.exists(player_master_path)) {
  stop("Missing player master input: ", player_master_path, ". Run 02_player_season.R first.", call. = FALSE)
}

clean_slug <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

standardize_table_types <- function(df) {
  df <- df %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character)
    )

  text_cols <- intersect(
    c("comment", "description", "notes", "status", "message"),
    names(df)
  )

  for (col in text_cols) {
    df[[col]] <- as.character(df[[col]])
  }

  df
}

first_existing_col <- function(df, candidates) {
  out <- candidates[candidates %in% names(df)][1]

  if (length(out) == 0 || is.na(out)) {
    return(NA_character_)
  }

  out
}

coalesce_numeric_cols <- function(df, candidates) {
  matches <- candidates[candidates %in% names(df)]

  if (length(matches) == 0) {
    return(rep(NA_real_, nrow(df)))
  }

  values <- purrr::map(matches, ~ suppressWarnings(as.numeric(df[[.x]])))
  purrr::reduce(values, dplyr::coalesce)
}

first_non_na <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  x[[1]]
}

extract_named_tables <- function(x, prefix = "") {
  if (is.data.frame(x)) {
    nm <- ifelse(prefix == "", "result", prefix)
    out <- list(tibble::as_tibble(x))
    names(out) <- nm
    return(out)
  }

  if (!is.list(x)) {
    return(list())
  }

  pieces <- purrr::imap(
    x,
    function(item, nm) {
      nm <- ifelse(is.null(nm) || nm == "", "unnamed", nm)
      child_prefix <- ifelse(prefix == "", nm, paste(prefix, nm, sep = "_"))
      extract_named_tables(item, child_prefix)
    }
  )

  purrr::flatten(pieces)
}

endpoint_available <- function(endpoint_name) {
  exists(endpoint_name, where = asNamespace("hoopR"), inherits = FALSE)
}

call_creation_endpoint <- function(endpoint_name, game_id, team_abbreviation) {
  if (!endpoint_available(endpoint_name)) {
    return(list(
      success = FALSE,
      error = paste0(endpoint_name, "() is not available in the installed hoopR package."),
      tables = list()
    ))
  }

  message(
    "Calling ",
    endpoint_name,
    "(game_id=", game_id,
    ", team_abbreviation=", team_abbreviation,
    ")"
  )

  tryCatch(
    {
      raw_result <- switch(
        endpoint_name,
        nba_boxscoreplayertrackv3 = hoopR::nba_boxscoreplayertrackv3(
          game_id = game_id,
          team_abbreviation = team_abbreviation
        ),
        nba_boxscoreusagev3 = hoopR::nba_boxscoreusagev3(
          game_id = game_id,
          team_abbreviation = team_abbreviation
        ),
        nba_assisttracker = hoopR::nba_assisttracker(
          game_id = game_id
        ),
        stop("Unhandled endpoint: ", endpoint_name, call. = FALSE)
      )

      list(
        success = TRUE,
        error = NA_character_,
        tables = extract_named_tables(raw_result)
      )
    },
    error = function(e) {
      list(
        success = FALSE,
        error = conditionMessage(e),
        tables = list()
      )
    }
  )
}

normalize_player_id_col <- function(df) {
  id_col <- first_existing_col(df, c(
    "player_id",
    "person_id",
    "nba_player_id",
    "athlete_id"
  ))

  if (is.na(id_col)) {
    df$player_id <- NA_character_
  } else {
    df$player_id <- as.character(df[[id_col]])
  }

  df
}

existing_raw_table_files <- function(player_name, player_id, endpoint_name) {
  if (!dir.exists(tracking_creation_raw_dir)) {
    return(character())
  }

  file_prefix <- glue(
    "{clean_slug(player_name)}_{player_id}_{clean_slug(endpoint_name)}_"
  )

  fs::dir_ls(tracking_creation_raw_dir, regexp = "\\.parquet$") %>%
    purrr::keep(~ stringr::str_starts(basename(.x), file_prefix))
}

read_existing_raw_tables <- function(paths) {
  purrr::map(
    paths,
    ~ read_project_parquet(.x) %>%
      janitor::clean_names() %>%
      standardize_table_types() %>%
      normalize_player_id_col()
  )
}

player_master <- read_project_parquet(player_master_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c("nickname", "player_nickname", "team_abbreviation", "team_abbr"), NA_character_) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_nickname = dplyr::coalesce(as.character(.data$player_nickname), as.character(.data$nickname)),
    team_abbreviation = dplyr::coalesce(as.character(.data$team_abbreviation), as.character(.data$team_abbr))
  )

validate_columns(player_master, c("player_id", "player_name", "team_abbreviation"))

target_players_expected <- tibble::tribble(
  ~requested_player_name, ~expected_player_id, ~expected_name_pattern,
  "Luka Dončić", "1629029", "luka don|luka donc",
  "LeBron James", "2544", "lebron james",
  "Austin Reaves", "1630559", "austin reaves",
  "Deandre Ayton", "1629028", "deandre ayton|ayton",
  "Jaxson Hayes", "1629637", "jaxson hayes",
  "Luke Kennard", "1628379", "luke kennard"
)

target_players <- target_players_expected %>%
  dplyr::left_join(
    player_master %>%
      dplyr::select("player_id", "player_name", "player_nickname", "team_abbreviation"),
    by = c("expected_player_id" = "player_id")
  ) %>%
  dplyr::mutate(
    player_id = as.character(.data$expected_player_id),
    name_check_text = stringr::str_to_lower(dplyr::coalesce(.data$player_name, .data$player_nickname, ""))
  )

target_validation <- target_players %>%
  dplyr::mutate(
    player_found = !is.na(.data$player_name),
    id_matches_expected = .data$player_id == .data$expected_player_id,
    name_matches_expected = stringr::str_detect(.data$name_check_text, .data$expected_name_pattern)
  )

if (any(!target_validation$player_found | !target_validation$id_matches_expected | !target_validation$name_matches_expected)) {
  message("Phase 19b target player validation failed:")
  print(target_validation)
  stop(
    "Phase 19b target player IDs did not match expected IDs. ",
    "Expected Luka=1629029, LeBron=2544, Austin=1630559, Deandre Ayton=1629028, ",
    "Jaxson Hayes=1629637, Luke Kennard=1628379.",
    call. = FALSE
  )
}

tracking_creation_audit <- if (file.exists(tracking_creation_audit_path)) {
  read_project_parquet(tracking_creation_audit_path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols() %>%
    add_missing_cols(c("player_id", "game_id", "team_abbreviation", "endpoint", "pull_success"), NA_character_) %>%
    dplyr::mutate(
      player_id = as.character(.data$player_id),
      game_id = as.character(.data$game_id),
      team_abbreviation = as.character(.data$team_abbreviation)
    )
} else {
  tibble::tibble(
    player_id = character(),
    game_id = character(),
    team_abbreviation = character(),
    endpoint = character(),
    pull_success = logical()
  )
}

successful_endpoints <- tracking_creation_audit %>%
  dplyr::filter(
    .data$endpoint %in% c("nba_boxscoreplayertrackv3", "nba_boxscoreusagev3", "nba_assisttracker"),
    .data$pull_success %in% TRUE
  ) %>%
  dplyr::distinct(.data$endpoint) %>%
  dplyr::pull(.data$endpoint)

if (length(successful_endpoints) == 0) {
  successful_endpoints <- c("nba_boxscoreplayertrackv3", "nba_boxscoreusagev3", "nba_assisttracker")
}

fallback_audit_games <- tracking_creation_audit %>%
  dplyr::filter(!is.na(.data$game_id), .data$game_id != "") %>%
  dplyr::distinct(.data$player_id, .data$game_id, .data$team_abbreviation)

target_pull_context <- target_players %>%
  dplyr::left_join(
    fallback_audit_games %>%
      dplyr::group_by(.data$player_id) %>%
      dplyr::summarise(
        game_id = dplyr::first(.data$game_id),
        audit_team_abbreviation = dplyr::first(stats::na.omit(.data$team_abbreviation), default = NA_character_),
        .groups = "drop"
      ),
    by = "player_id"
  ) %>%
  dplyr::mutate(
    canonical_team_abbreviation = dplyr::coalesce(
      as.character(.data$team_abbreviation),
      as.character(.data$audit_team_abbreviation)
    )
  )

target_pull_context <- target_pull_context %>%
  dplyr::mutate(
    missing_game_id = is.na(.data$game_id) | .data$game_id == "",
    missing_team_abbreviation = is.na(.data$canonical_team_abbreviation) |
      .data$canonical_team_abbreviation == "",
    skip_reason = dplyr::case_when(
      .data$missing_game_id & .data$missing_team_abbreviation ~ "Missing game_id/team_abbreviation",
      .data$missing_game_id ~ "Missing game_id/team_abbreviation",
      .data$missing_team_abbreviation ~ "Missing game_id/team_abbreviation",
      TRUE ~ NA_character_
    )
  )

pullable_targets <- target_pull_context %>%
  dplyr::filter(is.na(.data$skip_reason))

skipped_targets <- target_pull_context %>%
  dplyr::filter(!is.na(.data$skip_reason))

if (nrow(skipped_targets) > 0) {
  message("Phase 19b skipped targets:")
  print(
    skipped_targets %>%
      dplyr::select(
        "requested_player_name",
        "player_id",
        "player_name",
        "game_id",
        "canonical_team_abbreviation",
        "skip_reason"
      )
  )
}

pull_diagnostics <- list()
raw_table_pieces <- list()

if (nrow(skipped_targets) > 0) {
  for (skipped_idx in seq_len(nrow(skipped_targets))) {
    skipped_row <- skipped_targets[skipped_idx, ]

    for (endpoint_name in successful_endpoints) {
      pull_diagnostics[[length(pull_diagnostics) + 1]] <- tibble::tibble(
        requested_player_name = skipped_row$requested_player_name[[1]],
        player_id = skipped_row$player_id[[1]],
        player_name = skipped_row$player_name[[1]],
        team_abbreviation = skipped_row$canonical_team_abbreviation[[1]],
        game_id = skipped_row$game_id[[1]],
        endpoint = endpoint_name,
        pull_success = FALSE,
        error = skipped_row$skip_reason[[1]],
        table_count = 0L
      )
    }
  }
}

for (player_idx in seq_len(nrow(pullable_targets))) {
  player_row <- pullable_targets[player_idx, ]

  for (endpoint_name in successful_endpoints) {
    existing_files <- existing_raw_table_files(
      player_name = player_row$player_name[[1]],
      player_id = player_row$player_id[[1]],
      endpoint_name = endpoint_name
    )

    if (length(existing_files) > 0) {
      message(
        "Using existing raw parquet for ",
        endpoint_name,
        " | player=",
        player_row$player_name[[1]],
        " | files=",
        length(existing_files)
      )

      existing_tables <- read_existing_raw_tables(existing_files)

      pull_diagnostics[[length(pull_diagnostics) + 1]] <- tibble::tibble(
        requested_player_name = player_row$requested_player_name[[1]],
        player_id = player_row$player_id[[1]],
        player_name = player_row$player_name[[1]],
        team_abbreviation = player_row$canonical_team_abbreviation[[1]],
        game_id = player_row$game_id[[1]],
        endpoint = endpoint_name,
        pull_success = TRUE,
        error = NA_character_,
        table_count = length(existing_tables)
      )

      for (existing_table in existing_tables) {
        raw_table_pieces[[length(raw_table_pieces) + 1]] <- existing_table
      }

      next
    }

    result <- call_creation_endpoint(
      endpoint_name = endpoint_name,
      game_id = player_row$game_id[[1]],
      team_abbreviation = player_row$canonical_team_abbreviation[[1]]
    )

    pull_diagnostics[[length(pull_diagnostics) + 1]] <- tibble::tibble(
      requested_player_name = player_row$requested_player_name[[1]],
      player_id = player_row$player_id[[1]],
      player_name = player_row$player_name[[1]],
      team_abbreviation = player_row$canonical_team_abbreviation[[1]],
      game_id = player_row$game_id[[1]],
      endpoint = endpoint_name,
      pull_success = isTRUE(result$success),
      error = result$error,
      table_count = length(result$tables)
    )

    if (!isTRUE(result$success) || length(result$tables) == 0) {
      next
    }

    for (table_name in names(result$tables)) {
      raw_table <- tibble::as_tibble(result$tables[[table_name]]) %>%
        janitor::clean_names() %>%
        convert_numeric_cols() %>%
        standardize_table_types() %>%
        normalize_player_id_col() %>%
        dplyr::mutate(
          requested_player_name = player_row$requested_player_name[[1]],
          requested_player_id = player_row$player_id[[1]],
          requested_team_abbreviation = player_row$canonical_team_abbreviation[[1]],
          requested_game_id = player_row$game_id[[1]],
          endpoint = endpoint_name,
          table_name = table_name
        )

      output_file <- file.path(
        tracking_creation_raw_dir,
        glue("{clean_slug(player_row$player_name[[1]])}_{player_row$player_id[[1]]}_{clean_slug(endpoint_name)}_{clean_slug(table_name)}.parquet")
      )

      write_project_parquet(raw_table, output_file)
      raw_table_pieces[[length(raw_table_pieces) + 1]] <- raw_table
    }
  }
}

pull_diagnostics_tbl <- dplyr::bind_rows(pull_diagnostics)
write_project_parquet(pull_diagnostics_tbl, pull_diagnostics_path)

raw_tracking_creation <- dplyr::bind_rows(raw_table_pieces)

if (nrow(raw_tracking_creation) == 0) {
  player_tracking_creation_metrics <- target_pull_context %>%
    dplyr::transmute(
      player_id = .data$player_id,
      player_name = .data$player_name,
      player_nickname = .data$player_nickname,
      team_abbreviation = .data$canonical_team_abbreviation,
      touches = NA_real_,
      front_court_touches = NA_real_,
      time_of_possession = NA_real_,
      avg_sec_per_touch = NA_real_,
      avg_drib_per_touch = NA_real_,
      points_per_touch = NA_real_,
      usage_percentage = NA_real_,
      possessions = NA_real_,
      assists = NA_real_,
      secondary_assists = NA_real_,
      potential_assists = NA_real_
    )
} else {
  raw_tracking_creation <- raw_tracking_creation %>%
    dplyr::mutate(player_id = as.character(.data$player_id)) %>%
    dplyr::filter(.data$player_id %in% target_players$player_id)

  metric_rows <- raw_tracking_creation %>%
    dplyr::mutate(
      touches = coalesce_numeric_cols(., c("touches", "touch")),
      front_court_touches = coalesce_numeric_cols(., c("front_court_touches", "frontcourt_touches", "fc_touches")),
      time_of_possession = coalesce_numeric_cols(., c("time_of_possession", "time_of_poss", "time_poss")),
      avg_sec_per_touch = coalesce_numeric_cols(., c("avg_sec_per_touch", "average_seconds_per_touch", "sec_per_touch")),
      avg_drib_per_touch = coalesce_numeric_cols(., c("avg_drib_per_touch", "avg_dribbles_per_touch", "dribbles_per_touch")),
      points_per_touch = coalesce_numeric_cols(., c("points_per_touch", "pts_per_touch")),
      usage_percentage = coalesce_numeric_cols(., c("usage_percentage", "usage_pct", "usg_pct", "usage")),
      possessions = coalesce_numeric_cols(., c("possessions", "poss")),
      assists = coalesce_numeric_cols(., c("assists", "ast")),
      secondary_assists = coalesce_numeric_cols(., c("secondary_assists", "secondary_ast", "secondary_assist")),
      potential_assists = coalesce_numeric_cols(., c("potential_assists", "potential_ast", "potential_assist"))
    )

  player_tracking_creation_metrics <- metric_rows %>%
    dplyr::group_by(.data$player_id) %>%
    dplyr::summarise(
      touches = first_non_na(.data$touches),
      front_court_touches = first_non_na(.data$front_court_touches),
      time_of_possession = first_non_na(.data$time_of_possession),
      avg_sec_per_touch = first_non_na(.data$avg_sec_per_touch),
      avg_drib_per_touch = first_non_na(.data$avg_drib_per_touch),
      points_per_touch = first_non_na(.data$points_per_touch),
      usage_percentage = first_non_na(.data$usage_percentage),
      possessions = first_non_na(.data$possessions),
      assists = first_non_na(.data$assists),
      secondary_assists = first_non_na(.data$secondary_assists),
      potential_assists = first_non_na(.data$potential_assists),
      source_endpoints = paste(sort(unique(stats::na.omit(.data$endpoint))), collapse = " | "),
      source_tables = paste(sort(unique(stats::na.omit(.data$table_name))), collapse = " | "),
      .groups = "drop"
    ) %>%
    dplyr::right_join(
      target_pull_context %>%
        dplyr::select(
          "player_id",
          "player_name",
          "player_nickname",
          team_abbreviation = "canonical_team_abbreviation"
        ),
      by = "player_id"
    ) %>%
    dplyr::select(
      "player_id",
      "player_name",
      "player_nickname",
      "team_abbreviation",
      "touches",
      "front_court_touches",
      "time_of_possession",
      "avg_sec_per_touch",
      "avg_drib_per_touch",
      "points_per_touch",
      "usage_percentage",
      "possessions",
      "assists",
      "secondary_assists",
      "potential_assists",
      tidyselect::any_of(c("source_endpoints", "source_tables"))
    )
}

write_project_parquet(player_tracking_creation_metrics, player_tracking_creation_metrics_path)

message("Phase 19b tracking creation extraction diagnostics:")

message("Successful endpoint pulls:")
print(
  pull_diagnostics_tbl %>%
    dplyr::filter(.data$pull_success) %>%
    dplyr::arrange(.data$player_name, .data$endpoint)
)

message("Failed endpoint pulls/errors:")
print(
  pull_diagnostics_tbl %>%
    dplyr::filter(!.data$pull_success) %>%
    dplyr::arrange(.data$player_name, .data$endpoint)
)

message("Columns found in raw returned tables:")
if (nrow(raw_tracking_creation) > 0) {
  print(
    raw_tracking_creation %>%
      dplyr::group_by(.data$endpoint, .data$table_name) %>%
      dplyr::summarise(
        rows = dplyr::n(),
        column_names = paste(names(dplyr::cur_data()), collapse = ", "),
        .groups = "drop"
      )
  )
} else {
  message("No raw returned tables were available.")
}

message("Missing metric counts:")
print(
  player_tracking_creation_metrics %>%
    dplyr::summarise(
      players = dplyr::n(),
      missing_touches = sum(is.na(.data$touches)),
      missing_front_court_touches = sum(is.na(.data$front_court_touches)),
      missing_time_of_possession = sum(is.na(.data$time_of_possession)),
      missing_avg_sec_per_touch = sum(is.na(.data$avg_sec_per_touch)),
      missing_avg_drib_per_touch = sum(is.na(.data$avg_drib_per_touch)),
      missing_points_per_touch = sum(is.na(.data$points_per_touch)),
      missing_usage_percentage = sum(is.na(.data$usage_percentage)),
      missing_possessions = sum(is.na(.data$possessions)),
      missing_assists = sum(is.na(.data$assists)),
      missing_secondary_assists = sum(is.na(.data$secondary_assists)),
      missing_potential_assists = sum(is.na(.data$potential_assists))
    )
)

message("Luka / LeBron / Austin / Ayton / Jaxson / Kennard examples:")
print(
  player_tracking_creation_metrics %>%
    dplyr::filter(.data$player_id %in% target_players$player_id) %>%
    dplyr::arrange(.data$player_name)
)

message("Saved raw endpoint tables to: ", tracking_creation_raw_dir)
message("Saved player tracking creation metrics to: ", player_tracking_creation_metrics_path)
message("Phase 19b note: extraction only. No previous phases were modified.")
