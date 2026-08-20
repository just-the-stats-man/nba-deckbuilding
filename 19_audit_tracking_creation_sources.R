# ============================================================
# 19_audit_tracking_creation_sources.R
# Phase 19: Audit player tracking and creation endpoints.
#
# This phase checks whether additional NBA/hoopR endpoints expose useful
# creation signals such as drives, touches, potential assists, secondary
# assists, usage, possessions, matchup context, paint/elbow/post touches, etc.
#
# Audit only. This script does not modify previous phases or scoring outputs.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

pbp_path <- glue("data/raw/pbp/{team_abbr}_pbp_sample_{season}.parquet")
player_master_path <- glue("data/processed/player/player_master_{season}.parquet")
games_path <- glue("data/raw/games/{team_abbr}_games_{season}.parquet")
tracking_creation_audit_path <- "outputs/attacks/tracking_creation_audit.parquet"

fs::dir_create("outputs/attacks")

if (!file.exists(player_master_path)) {
  stop("Missing player master input: ", player_master_path, ". Run 02_player_season.R first.", call. = FALSE)
}

if (!file.exists(pbp_path)) {
  stop("Missing PBP input: ", pbp_path, ". Run 03_pull_games_pbp_rotations.R first.", call. = FALSE)
}

make_hoopr_call <- function(fun, args) {
  formal_names <- names(formals(fun))

  if ("..." %in% formal_names) {
    return(do.call(fun, args))
  }

  do.call(fun, args[names(args) %in% formal_names])
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

safe_first_existing_col <- function(df, candidates, required = TRUE) {
  out <- candidates[candidates %in% names(df)][1]

  if (length(out) == 0 || is.na(out)) {
    if (required) {
      stop("Missing expected column. Tried: ", paste(candidates, collapse = ", "), call. = FALSE)
    }

    return(NA_character_)
  }

  out
}

has_keyword_col <- function(cols, keywords) {
  cols_lower <- stringr::str_to_lower(cols)
  matched <- keywords[purrr::map_lgl(keywords, ~ any(stringr::str_detect(cols_lower, .x)))]
  paste(matched, collapse = ", ")
}

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

find_player_id_cols <- function(df) {
  id_candidates <- c(
    "player_id",
    "person_id",
    "athlete_id",
    "nba_player_id",
    "player1_id",
    "player2_id",
    "player3_id",
    "person1_id",
    "person2_id",
    "person3_id"
  )

  intersect(id_candidates, names(df))
}

endpoint_available <- function(endpoint_name) {
  exists(endpoint_name, where = asNamespace("hoopR"), inherits = FALSE)
}

call_tracking_endpoint_direct <- function(endpoint_name, game_id, team_abbreviation, player_id) {
  if (!endpoint_available(endpoint_name)) {
    return(list(
      success = FALSE,
      error = paste0(endpoint_name, "() is not available in the installed hoopR package."),
      tables = list(),
      passed_args = character()
    ))
  }

  message(
    "Calling ",
    endpoint_name,
    "(game_id=", game_id,
    ", team_abbreviation=", team_abbreviation,
    ", player_id=", player_id,
    ")"
  )

  tryCatch(
    {
      raw_result <- switch(
        endpoint_name,
        nba_boxscoreplayertrackv2 = hoopR::nba_boxscoreplayertrackv2(
          game_id = game_id,
          team_abbreviation = team_abbreviation
        ),
        nba_boxscoreplayertrackv3 = hoopR::nba_boxscoreplayertrackv3(
          game_id = game_id,
          team_abbreviation = team_abbreviation
        ),
        nba_boxscoreusagev2 = hoopR::nba_boxscoreusagev2(
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
        nba_boxscorematchups = hoopR::nba_boxscorematchups(
          game_id = game_id
        ),
        stop("Unhandled endpoint: ", endpoint_name, call. = FALSE)
      )

      list(
        success = TRUE,
        error = NA_character_,
        tables = extract_named_tables(raw_result),
        passed_args = dplyr::case_when(
          endpoint_name %in% c(
            "nba_boxscoreplayertrackv2",
            "nba_boxscoreplayertrackv3",
            "nba_boxscoreusagev2",
            "nba_boxscoreusagev3"
          ) ~ "game_id, team_abbreviation",
          TRUE ~ "game_id"
        )
      )
    },
    error = function(e) {
      list(
        success = FALSE,
        error = conditionMessage(e),
        tables = list(),
        passed_args = dplyr::case_when(
          endpoint_name %in% c(
            "nba_boxscoreplayertrackv2",
            "nba_boxscoreplayertrackv3",
            "nba_boxscoreusagev2",
            "nba_boxscoreusagev3"
          ) ~ "game_id, team_abbreviation",
          TRUE ~ "game_id"
        )
      )
    }
  )
}

player_master <- read_project_parquet(player_master_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name_search = stringr::str_to_lower(.data$player_name)
  )

validate_columns(player_master, c("player_id", "player_name", "team_abbreviation"))

pbp <- read_project_parquet(pbp_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  dplyr::mutate(game_id = as.character(.data$game_id))

validate_columns(pbp, c("game_id"))

games <- if (file.exists(games_path)) {
  read_project_parquet(games_path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols() %>%
    dplyr::mutate(game_id = as.character(.data$game_id)) %>%
    dplyr::select(tidyselect::any_of(c("game_id", "matchup", "game_date", "team_abbreviation", "team_abbr"))) %>%
    add_missing_cols(c("team_abbreviation", "team_abbr"), NA_character_)
} else {
  tibble::tibble(game_id = unique(pbp$game_id))
}

expected_test_players <- tibble::tribble(
  ~requested_player_name, ~expected_player_id, ~expected_name_pattern,
  "Luka Dončić", "1629029", "luka don|luka donc",
  "LeBron James", "2544", "lebron james",
  "Austin Reaves", "1630559", "austin reaves",
  "Deandre Ayton", "1629028", "deandre ayton|ayton",
  "Jaxson Hayes", "1629637", "jaxson hayes"
)

player_master_for_targets <- player_master %>%
  dplyr::select(
    "player_id",
    "player_name",
    tidyselect::any_of(c("nickname", "player_nickname", "team_abbreviation", "team_abbr"))
  ) %>%
  add_missing_cols(c("nickname", "player_nickname", "team_abbreviation", "team_abbr"), NA_character_) %>%
  dplyr::mutate(
    player_nickname = dplyr::coalesce(as.character(.data$player_nickname), as.character(.data$nickname)),
    team_abbreviation = dplyr::coalesce(as.character(.data$team_abbreviation), as.character(.data$team_abbr))
  ) %>%
  dplyr::select(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation"
  )

test_players <- expected_test_players %>%
  dplyr::left_join(
    player_master_for_targets,
    by = c("expected_player_id" = "player_id")
  ) %>%
  dplyr::mutate(
    player_id = as.character(.data$expected_player_id)
  ) %>%
  dplyr::select(
    "requested_player_name",
    "expected_player_id",
    "expected_name_pattern",
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation"
  )

selected_id_check <- test_players %>%
  dplyr::mutate(
    id_matches_expected = .data$player_id == .data$expected_player_id,
    player_found = !is.na(.data$player_name),
    player_name_check_text = stringr::str_to_lower(
      dplyr::coalesce(.data$player_name, .data$player_nickname, "")
    ),
    player_name_matches_expected = stringr::str_detect(
      .data$player_name_check_text,
      .data$expected_name_pattern
    )
  )

if (any(!selected_id_check$player_found | !selected_id_check$id_matches_expected | !selected_id_check$player_name_matches_expected)) {
  message("Selected player ID validation failed:")
  print(selected_id_check)
  stop(
    "Phase 19 target player IDs did not match expected IDs. ",
    "Expected Luka=1629029, LeBron=2544, Austin=1630559, Deandre Ayton=1629028, Jaxson Hayes=1629637, ",
    "and selected player_name/player_nickname must match the requested target.",
    call. = FALSE
  )
}

pbp_player_id_cols <- find_player_id_cols(pbp)

player_game_map <- if (length(pbp_player_id_cols) > 0) {
  pbp %>%
    dplyr::select("game_id", tidyselect::all_of(pbp_player_id_cols)) %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(pbp_player_id_cols),
      names_to = "source_player_id_column",
      values_to = "player_id"
    ) %>%
    dplyr::mutate(player_id = as.character(.data$player_id)) %>%
    dplyr::filter(!is.na(.data$player_id), .data$player_id != "") %>%
    dplyr::distinct(.data$player_id, .data$game_id, .data$source_player_id_column)
} else {
  tibble::tibble(
    player_id = character(),
    game_id = character(),
    source_player_id_column = character()
  )
}

fallback_game_id <- pbp %>%
  dplyr::filter(!is.na(.data$game_id)) %>%
  dplyr::distinct(.data$game_id) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::pull(.data$game_id)

test_player_games <- test_players %>%
  dplyr::left_join(
    player_game_map %>%
      dplyr::group_by(.data$player_id) %>%
      dplyr::summarise(
        game_id = dplyr::first(.data$game_id),
        player_found_in_pbp = TRUE,
        pbp_player_id_column = dplyr::first(.data$source_player_id_column),
        .groups = "drop"
      ),
    by = "player_id"
  ) %>%
  dplyr::mutate(
    player_found_in_pbp = dplyr::coalesce(.data$player_found_in_pbp, FALSE),
    game_id = dplyr::coalesce(.data$game_id, fallback_game_id)
  ) %>%
  dplyr::left_join(
    games %>%
      dplyr::distinct(.data$game_id, .keep_all = TRUE),
    by = "game_id"
  ) %>%
  add_missing_cols(c("team_abbreviation", "team_abbreviation.x", "team_abbreviation.y", "team_abbr"), NA_character_) %>%
  dplyr::mutate(
    team_abbreviation = dplyr::coalesce(
      as.character(.data$team_abbreviation.x),
      as.character(.data$team_abbreviation.y),
      as.character(.data$team_abbr),
      as.character(.data$team_abbreviation)
    )
  )

message("Selected Phase 19 player map:")
print(
  test_player_games %>%
    dplyr::select(
      "requested_player_name",
      "expected_player_id",
      "player_id",
      "player_name",
      "player_nickname",
      "team_abbreviation",
      "game_id",
      "player_found_in_pbp",
      "pbp_player_id_column"
    )
)

if (any(is.na(test_player_games$team_abbreviation) | test_player_games$team_abbreviation == "")) {
  print(test_player_games)
  stop("Phase 19 selected player map contains blank canonical team_abbreviation.", call. = FALSE)
}

endpoints_to_test <- c(
  "nba_boxscoreplayertrackv2",
  "nba_boxscoreplayertrackv3",
  "nba_boxscoreusagev2",
  "nba_boxscoreusagev3",
  "nba_assisttracker",
  "nba_boxscorematchups"
)

creation_keywords <- c(
  "drive",
  "touch",
  "time",
  "assist",
  "potential",
  "secondary",
  "paint",
  "elbow",
  "post",
  "usage",
  "possession",
  "matchup"
)

audit_row_pieces <- list()

for (player_idx in seq_len(nrow(test_player_games))) {
  player_row <- test_player_games[player_idx, ]

  requested_player_name <- player_row$requested_player_name[[1]]
  player_id <- as.character(player_row$player_id[[1]])
  player_name <- player_row$player_name[[1]]
  team_abbreviation_i <- as.character(player_row$team_abbreviation[[1]])
  selected_game_id <- as.character(player_row$game_id[[1]])
  player_found_in_pbp <- player_row$player_found_in_pbp[[1]]
  pbp_player_id_column <- player_row$pbp_player_id_column[[1]]

  for (endpoint_name in endpoints_to_test) {
    result <- call_tracking_endpoint_direct(
      endpoint_name = endpoint_name,
      game_id = selected_game_id,
      team_abbreviation = team_abbreviation_i,
      player_id = player_id
    )

    if (!isTRUE(result$success) || length(result$tables) == 0) {
      audit_row_pieces[[length(audit_row_pieces) + 1]] <- tibble::tibble(
        requested_player_name = requested_player_name,
        player_id = player_id,
        player_name = player_name,
        team_abbreviation = team_abbreviation_i,
        game_id = selected_game_id,
        player_found_in_pbp = player_found_in_pbp,
        pbp_player_id_column = pbp_player_id_column,
        endpoint = endpoint_name,
        passed_args = result$passed_args,
        table_name = NA_character_,
        pull_success = isTRUE(result$success),
        error = result$error,
        row_count = 0L,
        column_names = NA_character_,
        matched_keyword_columns = NA_character_,
        has_drive = FALSE,
        has_touch = FALSE,
        has_time = FALSE,
        has_assist = FALSE,
        has_potential = FALSE,
        has_secondary = FALSE,
        has_paint = FALSE,
        has_elbow = FALSE,
        has_post = FALSE,
        has_usage = FALSE,
        has_possession = FALSE,
        has_matchup = FALSE
      )

      next
    }

    for (table_name in names(result$tables)) {
      tbl_clean <- tibble::as_tibble(result$tables[[table_name]]) %>%
        janitor::clean_names()

      cols <- names(tbl_clean)
      cols_lower <- stringr::str_to_lower(cols)

      keyword_flags <- purrr::set_names(
        purrr::map_lgl(creation_keywords, ~ any(stringr::str_detect(cols_lower, .x))),
        paste0("has_", creation_keywords)
      )

      audit_row_pieces[[length(audit_row_pieces) + 1]] <- tibble::tibble(
        requested_player_name = requested_player_name,
        player_id = player_id,
        player_name = player_name,
        team_abbreviation = team_abbreviation_i,
        game_id = selected_game_id,
        player_found_in_pbp = player_found_in_pbp,
        pbp_player_id_column = pbp_player_id_column,
        endpoint = endpoint_name,
        passed_args = result$passed_args,
        table_name = table_name,
        pull_success = TRUE,
        error = NA_character_,
        row_count = nrow(tbl_clean),
        column_names = paste(cols, collapse = ", "),
        matched_keyword_columns = paste(cols[stringr::str_detect(cols_lower, paste(creation_keywords, collapse = "|"))], collapse = ", ")
      ) %>%
        dplyr::bind_cols(tibble::as_tibble_row(keyword_flags))
    }
  }
}

audit_rows <- dplyr::bind_rows(audit_row_pieces)

write_project_parquet(audit_rows, tracking_creation_audit_path)

message("Phase 19 tracking/creation endpoint audit diagnostics:")

message("Test player game map:")
print(test_player_games)

message("Returned tables and row counts:")
print(
  audit_rows %>%
    dplyr::select(
      "player_name",
      "game_id",
      "endpoint",
      "passed_args",
      "table_name",
      "pull_success",
      "row_count",
      "error"
    ) %>%
    dplyr::arrange(.data$player_name, .data$endpoint, .data$table_name)
)

message("Column names:")
print(
  audit_rows %>%
    dplyr::select(
      "player_name",
      "endpoint",
      "table_name",
      "column_names"
    )
)

message("Creation keyword column matches:")
print(
  audit_rows %>%
    dplyr::select(
      "player_name",
      "endpoint",
      "table_name",
      "matched_keyword_columns",
      tidyselect::starts_with("has_")
    ) %>%
    dplyr::arrange(.data$player_name, .data$endpoint, .data$table_name)
)

message("Endpoint availability summary:")
print(
  audit_rows %>%
    dplyr::group_by(.data$endpoint) %>%
    dplyr::summarise(
      pulls_attempted = dplyr::n_distinct(paste(.data$player_id, .data$game_id)),
      successful_tables = sum(.data$pull_success & !is.na(.data$table_name)),
      total_rows_returned = sum(.data$row_count, na.rm = TRUE),
      any_drive = any(.data$has_drive, na.rm = TRUE),
      any_touch = any(.data$has_touch, na.rm = TRUE),
      any_time = any(.data$has_time, na.rm = TRUE),
      any_assist = any(.data$has_assist, na.rm = TRUE),
      any_potential = any(.data$has_potential, na.rm = TRUE),
      any_secondary = any(.data$has_secondary, na.rm = TRUE),
      any_paint = any(.data$has_paint, na.rm = TRUE),
      any_elbow = any(.data$has_elbow, na.rm = TRUE),
      any_post = any(.data$has_post, na.rm = TRUE),
      any_usage = any(.data$has_usage, na.rm = TRUE),
      any_possession = any(.data$has_possession, na.rm = TRUE),
      any_matchup = any(.data$has_matchup, na.rm = TRUE),
      errors = paste(unique(stats::na.omit(.data$error)), collapse = " | "),
      .groups = "drop"
    )
)

message("Saved tracking/creation source audit to: ", tracking_creation_audit_path)
message("Phase 19 note: audit only. No previous phases were modified.")
