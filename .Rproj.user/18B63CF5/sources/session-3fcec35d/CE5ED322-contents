extract_all_hoopr_tables <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
  
  if (is.list(x)) {
    dfs <- x[map_lgl(x, is.data.frame)]
    
    if (length(dfs) > 0) {
      return(bind_rows(dfs))
    }
  }
  
  stop("No data frames found in hoopR object.")
}

convert_numeric_cols <- function(df) {
  df %>%
    mutate(across(
      where(is.character),
      ~ {
        x <- trimws(.x)
        x[x %in% c("", "NA", "NULL")] <- NA
        if (all(grepl("^[-0-9\\.]+$", na.omit(x)))) {
          as.numeric(x)
        } else {
          .x
        }
      }
    ))
}

validate_columns <- function(df, cols) {
  missing_cols <- setdiff(cols, colnames(df))
  
  if (length(missing_cols) == 0) {
    message("✅ All required columns exist.")
    return(invisible(TRUE))
  }
  
  message("❌ Missing columns detected:")
  
  existing <- colnames(df)
  
  for (col in missing_cols) {
    message("\n- ", col)
    
    distances <- adist(col, existing)
    close_matches <- existing[order(distances)][1:5]
    
    message("  Closest matches:")
    print(close_matches)
  }
  
  stop("Fix column names before proceeding.", call. = FALSE)
}

pull_pbp_safe <- function(game_id) {
  message("Pulling PBP for game: ", game_id)
  
  nba_playbyplayv3(game_id = game_id) %>%
    extract_hoopr_table() %>%
    clean_names() %>%
    mutate(game_id = game_id)
}

get_players_on_court <- function(game_id_i, start_time_i, end_time_i, rotation_data) {
  rotation_data %>%
    filter(
      game_id == game_id_i,
      in_time <= start_time_i,
      out_time >= end_time_i
    ) %>%
    pull(person_id)
}