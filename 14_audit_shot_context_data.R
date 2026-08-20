# ============================================================
# 14_audit_shot_context_data.R
# Phase 14: Shot context / openness data availability audit.
#
# This phase checks whether NBA Player Tracking Shots data is available through
# hoopR::nba_playerdashptshots(), which wraps the NBA Stats playerdashptshots
# endpoint. It does not scrape NBA.com tables manually and does not modify the
# attack library, movesets, attack identity, or ATK scoring.
#
# This is only a data availability audit before any ATK modeling work.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_master_path <- "data/processed/player/player_master_2025-26.parquet"
tracking_raw_dir <- "data/raw/tracking_shots"
schema_audit_path <- "outputs/attacks/shot_context_schema_audit.parquet"

fs::dir_create(tracking_raw_dir)
fs::dir_create("outputs/attacks")

if (!file.exists(player_master_path)) {
  stop("Missing player master input: ", player_master_path, ". Run 02_player_season.R first.", call. = FALSE)
}

if (!exists("nba_playerdashptshots", where = asNamespace("hoopR"), inherits = FALSE)) {
  stop(
    "hoopR::nba_playerdashptshots() is not available in the installed hoopR package. ",
    "Update hoopR before running this data availability audit.",
    call. = FALSE
  )
}

clean_slug <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
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
      nm <- ifelse(is.null(nm) || nm == "", as.character(length(prefix) + 1), nm)
      child_prefix <- ifelse(prefix == "", nm, paste(prefix, nm, sep = "_"))
      extract_named_tables(item, child_prefix)
    }
  )

  purrr::flatten(pieces)
}

has_any_col <- function(cols, patterns) {
  any(stringr::str_detect(cols, patterns))
}

table_feature_flags <- function(table_name, cols) {
  cols <- stringr::str_to_lower(cols)
  table_name_clean <- stringr::str_to_lower(table_name)

  tibble::tibble(
    has_closest_defender_distance = stringr::str_detect(table_name_clean, "closestdefender") |
      has_any_col(cols, "close_def|closest.*def|def.*dist"),
    has_closest_defender_10ft_plus = stringr::str_detect(table_name_clean, "10ft|10_ft|10_plus|plus") |
      has_any_col(cols, "10ft|10_ft|10_plus|plus"),
    has_shot_clock = stringr::str_detect(table_name_clean, "shotclock") |
      has_any_col(cols, "shot_clock"),
    has_dribbles = stringr::str_detect(table_name_clean, "dribble") |
      has_any_col(cols, "dribble"),
    has_touch_time = stringr::str_detect(table_name_clean, "touchtime") |
      has_any_col(cols, "touch_time"),
    has_general_shooting_splits = stringr::str_detect(table_name_clean, "general|overall") |
      has_any_col(cols, "shot_type|fgm|fga|fg_pct|efg_pct|fg3a")
  )
}

pull_player_tracking_shots <- function(player_id, player_name) {
  message("Pulling player tracking shots for ", player_name, " (", player_id, ")")

  args <- list(
    player_id = as.character(player_id),
    season = season,
    season_type = "Regular Season"
  )

  make_hoopr_call(hoopR::nba_playerdashptshots, args)
}

player_master <- read_project_parquet(player_master_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_name_search = stringr::str_to_lower(.data$player_name)
  )

validate_columns(player_master, c("player_id", "player_name", "team_abbreviation"))

test_player_patterns <- tibble::tribble(
  ~requested_player_name, ~name_pattern,
  "Luka Dončić", "luka don|luka donc",
  "LeBron James", "lebron james",
  "Austin Reaves", "austin reaves"
)

test_players <- purrr::pmap_dfr(
  test_player_patterns,
  function(requested_player_name, name_pattern) {
    player_master %>%
      dplyr::filter(stringr::str_detect(.data$player_name_search, name_pattern)) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::mutate(requested_player_name = requested_player_name) %>%
      dplyr::select(
        "requested_player_name",
        "player_id",
        "player_name",
        "team_abbreviation"
      )
  }
)

missing_requested_players <- test_player_patterns %>%
  dplyr::anti_join(test_players, by = "requested_player_name")

if (nrow(missing_requested_players) > 0) {
  message("Requested test players missing from player_master:")
  print(missing_requested_players)
}

if (nrow(test_players) == 0) {
  stop("None of the requested test players were found in player_master.", call. = FALSE)
}

pull_results <- purrr::pmap(
  test_players,
  function(requested_player_name, player_id, player_name, team_abbreviation) {
    tryCatch(
      {
        raw_result <- pull_player_tracking_shots(player_id, player_name)
        tables <- extract_named_tables(raw_result)

        list(
          requested_player_name = requested_player_name,
          player_id = player_id,
          player_name = player_name,
          team_abbreviation = team_abbreviation,
          success = TRUE,
          error = NA_character_,
          tables = tables
        )
      },
      error = function(e) {
        message("Failed tracking shot pull for ", player_name, ": ", conditionMessage(e))

        list(
          requested_player_name = requested_player_name,
          player_id = player_id,
          player_name = player_name,
          team_abbreviation = team_abbreviation,
          success = FALSE,
          error = conditionMessage(e),
          tables = list()
        )
      }
    )
  }
)

schema_rows <- purrr::map_dfr(
  pull_results,
  function(result) {
    if (!isTRUE(result$success) || length(result$tables) == 0) {
      return(tibble::tibble(
        requested_player_name = result$requested_player_name,
        player_id = as.character(result$player_id),
        player_name = result$player_name,
        team_abbreviation = result$team_abbreviation,
        table_name = NA_character_,
        pull_success = isTRUE(result$success),
        error = result$error,
        row_count = 0L,
        column_names = NA_character_,
        player_id_can_join_player_master = result$player_id %in% player_master$player_id,
        has_closest_defender_distance = FALSE,
        has_closest_defender_10ft_plus = FALSE,
        has_shot_clock = FALSE,
        has_dribbles = FALSE,
        has_touch_time = FALSE,
        has_general_shooting_splits = FALSE
      ))
    }

    purrr::imap_dfr(
      result$tables,
      function(tbl, table_name) {
        table_clean <- tbl %>%
          janitor::clean_names() %>%
          convert_numeric_cols() %>%
          dplyr::mutate(
            requested_player_name = result$requested_player_name,
            requested_player_id = as.character(result$player_id),
            requested_player_name_resolved = result$player_name,
            requested_team_abbreviation = result$team_abbreviation,
            table_name = table_name
          )

        output_file <- file.path(
          tracking_raw_dir,
          glue("{clean_slug(result$player_name)}_{result$player_id}_{clean_slug(table_name)}.parquet")
        )
        write_project_parquet(table_clean, output_file)

        cols <- names(table_clean)
        flags <- table_feature_flags(table_name, cols)
        returned_player_id <- if ("player_id" %in% cols) {
          unique(as.character(stats::na.omit(table_clean$player_id)))
        } else {
          character()
        }

        tibble::tibble(
          requested_player_name = result$requested_player_name,
          player_id = as.character(result$player_id),
          player_name = result$player_name,
          team_abbreviation = result$team_abbreviation,
          table_name = table_name,
          pull_success = TRUE,
          error = NA_character_,
          row_count = nrow(table_clean),
          column_names = paste(cols, collapse = ", "),
          player_id_can_join_player_master = length(returned_player_id) == 0 ||
            any(returned_player_id %in% player_master$player_id)
        ) %>%
          dplyr::bind_cols(flags)
      }
    )
  }
)

write_project_parquet(schema_rows, schema_audit_path)

message("Phase 14 tracking shot context diagnostics:")

message("Returned tables and row counts:")
print(
  schema_rows %>%
    dplyr::select(
      "requested_player_name",
      "player_name",
      "table_name",
      "pull_success",
      "row_count",
      "player_id_can_join_player_master",
      "error"
    ) %>%
    dplyr::arrange(.data$requested_player_name, .data$table_name)
)

message("Column names by returned table:")
print(
  schema_rows %>%
    dplyr::select("requested_player_name", "table_name", "column_names")
)

message("Shot context feature availability:")
print(
  schema_rows %>%
    dplyr::summarise(
      tables_returned_successfully = sum(.data$pull_success & !is.na(.data$table_name)),
      has_closest_defender_distance = any(.data$has_closest_defender_distance, na.rm = TRUE),
      has_closest_defender_10ft_plus = any(.data$has_closest_defender_10ft_plus, na.rm = TRUE),
      has_shot_clock = any(.data$has_shot_clock, na.rm = TRUE),
      has_dribbles = any(.data$has_dribbles, na.rm = TRUE),
      has_touch_time = any(.data$has_touch_time, na.rm = TRUE),
      has_general_shooting_splits = any(.data$has_general_shooting_splits, na.rm = TRUE)
    )
)

message("Phase 14 note: data availability audit only. No attack library, moveset, attack identity, or ATK scoring outputs were modified.")
