# ============================================================
# R/helpers.R
# Shared helper functions for the NBA deckbuilding project.
# This file should be sourced by 01_setup_project.R only.
# ============================================================

extract_all_hoopr_tables <- function(x) {
  # hoopR/NBA Stats API outputs are not always shaped identically.
  # Sometimes the result is a data frame; sometimes a list of tables.
  if (is.data.frame(x)) {
    return(tibble::as_tibble(x))
  }

  if (is.list(x)) {
    dfs <- x[purrr::map_lgl(x, is.data.frame)]

    if (length(dfs) > 0) {
      return(dplyr::bind_rows(dfs))
    }

    nested_dfs <- purrr::flatten(x)
    nested_dfs <- nested_dfs[purrr::map_lgl(nested_dfs, is.data.frame)]

    if (length(nested_dfs) > 0) {
      return(dplyr::bind_rows(nested_dfs))
    }
  }

  stop("No data frames found in hoopR object.", call. = FALSE)
}

convert_numeric_cols <- function(df) {
  df %>%
    dplyr::mutate(dplyr::across(
      where(is.character),
      ~ {
        x <- trimws(.x)
        x[x %in% c("", "NA", "NULL", "NaN")] <- NA_character_
        numeric_like <- all(grepl("^-?[0-9]+(\\.[0-9]+)?$", stats::na.omit(x)))
        if (numeric_like) as.numeric(x) else .x
      }
    ))
}

z_score <- function(x) {
  as.numeric(scale(x))
}

observed_data_scope_note <- function() {
  "Current metrics reflect observed pulled data and should not be interpreted as full-season league-wide values."
}

add_observed_data_scope_note <- function(df) {
  if (!is.data.frame(df)) {
    return(df)
  }

  df %>%
    dplyr::mutate(observed_data_scope_note = observed_data_scope_note())
}

validate_columns <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))

  if (length(missing_cols) == 0) {
    message("All required columns exist.")
    return(invisible(TRUE))
  }

  message("Missing columns detected:")
  existing <- names(df)

  for (col in missing_cols) {
    message("\n- ", col)
    distances <- utils::adist(col, existing)
    close_matches <- existing[order(distances)][seq_len(min(5, length(existing)))]
    message("  Closest matches: ", paste(close_matches, collapse = ", "))
  }

  stop("Fix column names before proceeding.", call. = FALSE)
}

ensure_dirs <- function(dirs) {
  purrr::walk(dirs, fs::dir_create)
  invisible(TRUE)
}

write_project_parquet <- function(x, path) {
  fs::dir_create(dirname(path))
  normalized_path <- gsub("\\\\", "/", path)

  if (startsWith(normalized_path, "outputs/")) {
    x <- add_observed_data_scope_note(x)
  }

  arrow::write_parquet(x, path)
  message("Wrote: ", path)
  invisible(path)
}

read_project_parquet <- function(path) {
  if (!file.exists(path)) {
    stop("Missing file: ", path, call. = FALSE)
  }
  arrow::read_parquet(path)
}

pull_pbp_safe <- function(game_id) {
  message("Pulling PBP for game: ", game_id)

  hoopR::nba_playbyplayv3(game_id = game_id) %>%
    extract_all_hoopr_tables() %>%
    janitor::clean_names() %>%
    dplyr::mutate(game_id = as.character(game_id))
}

pull_rotation_safe <- function(game_id) {
  message("Pulling rotations for game: ", game_id)

  hoopR::nba_gamerotation(game_id = game_id) %>%
    extract_all_hoopr_tables() %>%
    janitor::clean_names() %>%
    dplyr::mutate(game_id = as.character(game_id))
}

safe_first_existing_col <- function(df, candidates, required = TRUE) {
  hit <- candidates[candidates %in% names(df)][1]

  if (is.na(hit) && required) {
    stop(
      "None of these columns were found: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }

  hit
}

get_players_on_court <- function(game_id_i, start_time_i, end_time_i, rotation_data) {
  rotation_data %>%
    dplyr::filter(
      .data$game_id == game_id_i,
      .data$in_time <= start_time_i,
      .data$out_time >= end_time_i
    ) %>%
    dplyr::pull(.data$person_id)
}
