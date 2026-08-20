# ============================================================
# 27a_audit_physical_measurement_sources.R
# Phase 27a: Audit and pull physical measurement data.
#
# Goal:
# Find usable player physical measurements for Phase 27.
#
# This phase is measurement-source audit/pull only. It does not infer physical
# attributes and does not use skill, role, ATK, DEF, or position labels as a
# substitute for body measurements.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

physical_output_dir <- "outputs/physical"
physical_audit_path <- file.path(physical_output_dir, "physical_measurement_source_audit.parquet")
physical_measurements_path <- file.path(physical_output_dir, "player_physical_measurements.parquet")
draft_combine_raw_dir <- "data/raw/physical/draft_combine_stats"
databallr_wingspan_path <- "data/raw/physical/wingspan_all_2026-05-31.csv"
databallr_wingspan_upload_fallback_path <- "data/wingspan_all_2026-05-31.csv"
player_master_path <- glue("data/processed/player/player_master_{season}.parquet")

fs::dir_create(physical_output_dir)
fs::dir_create(draft_combine_raw_dir)

candidate_column_patterns <- c(
  height = "height$|height_inches|player_height|height_wo_shoes|height_w_shoes",
  weight = "weight$|weight_lbs|player_weight",
  wingspan = "wingspan",
  standing_reach = "standing_reach|reach",
  body_fat = "body_fat|bodyfat",
  max_vertical_leap = "max_vertical_leap|max_vert|max_vertical",
  standing_vertical_leap = "standing_vertical_leap|standing_vert|standing_vertical",
  lane_agility_time = "lane_agility_time|lane_agility",
  shuttle_run = "shuttle_run|shuttle",
  three_quarter_sprint = "three_quarter_sprint|three_quarter|three_quarters|threequarter|sprint",
  bench_press = "bench_press|bench",
  draft_combine = "draft_combine|combine",
  position = "position$|roster_position"
)

local_candidate_file_regex <- paste(
  c(
    "player",
    "roster",
    "metadata",
    "master",
    "card",
    "physical",
    "combine"
  ),
  collapse = "|"
)

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

safe_read_parquet <- function(path) {
  tryCatch(
    read_project_parquet(path) %>%
      janitor::clean_names() %>%
      normalize_missing_strings() %>%
      convert_numeric_cols(),
    error = function(e) {
      message("Could not read parquet: ", path, " | ", conditionMessage(e))
      tibble::tibble()
    }
  )
}

normalize_missing_strings <- function(df) {
  df %>%
    dplyr::mutate(dplyr::across(
      where(is.character),
      ~ {
        x <- trimws(.x)
        x[x %in% c("", "--", "-", "NA", "N/A", "NULL", "NaN", "na", "n/a", "null")] <- NA_character_
        x
      }
    ))
}

coalesce_numeric_cols <- function(df, candidates) {
  matches <- candidates[candidates %in% names(df)]

  if (length(matches) == 0) {
    return(rep(NA_real_, nrow(df)))
  }

  values <- lapply(matches, function(col) suppressWarnings(as.numeric(df[[col]])))
  Reduce(dplyr::coalesce, values)
}

coalesce_character_cols <- function(df, candidates) {
  matches <- candidates[candidates %in% names(df)]

  if (length(matches) == 0) {
    return(rep(NA_character_, nrow(df)))
  }

  values <- lapply(matches, function(col) as.character(df[[col]]))
  Reduce(dplyr::coalesce, values)
}

first_non_missing_value <- function(x, default = NA) {
  if (is.character(x)) {
    x <- x[!is.na(x) & trimws(x) != ""]
  } else {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    return(default)
  }

  x[[1]]
}

first_value_by_source <- function(value, source, preferred_source_regex = NULL, default = NA) {
  source <- as.character(source)

  if (!is.null(preferred_source_regex)) {
    preferred_idx <- !is.na(source) & stringr::str_detect(source, preferred_source_regex)
    preferred_value <- value[preferred_idx]

    if (length(preferred_value) > 0) {
      picked <- first_non_missing_value(preferred_value, default = default)

      if (!(length(picked) == 1 && is.na(picked))) {
        return(picked)
      }
    }
  }

  first_non_missing_value(value, default = default)
}

normalize_inches <- function(x) {
  x_chr <- as.character(x)
  x_chr <- stringr::str_replace_all(x_chr, "\"|''|”|“", "")
  x_chr <- stringr::str_replace_all(x_chr, "’", "'")
  x_chr <- trimws(x_chr)
  x_num <- suppressWarnings(as.numeric(x_chr))
  feet_inches_match <- stringr::str_match(x_chr, "^(\\d+)\\s*(?:'|-|\\s)\\s*(\\d+(?:\\.\\d+)?)")
  parsed_feet_inches <- suppressWarnings(as.numeric(feet_inches_match[, 2]) * 12 + as.numeric(feet_inches_match[, 3]))

  dplyr::case_when(
    !is.na(x_num) & x_num > 120 ~ x_num / 2.54,
    !is.na(x_num) ~ x_num,
    !is.na(parsed_feet_inches) ~ parsed_feet_inches,
    TRUE ~ NA_real_
  )
}

normalize_weight_lbs <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))

  dplyr::case_when(
    !is.na(x_num) & x_num < 140 ~ x_num * 2.20462,
    TRUE ~ x_num
  )
}

classify_candidate_measurement <- function(column_name) {
  col <- stringr::str_to_lower(column_name)

  dplyr::case_when(
    stringr::str_detect(col, candidate_column_patterns[["height"]]) ~ "height",
    stringr::str_detect(col, candidate_column_patterns[["weight"]]) ~ "weight",
    stringr::str_detect(col, candidate_column_patterns[["wingspan"]]) ~ "wingspan",
    stringr::str_detect(col, candidate_column_patterns[["standing_reach"]]) ~ "standing_reach",
    stringr::str_detect(col, candidate_column_patterns[["body_fat"]]) ~ "body_fat",
    stringr::str_detect(col, candidate_column_patterns[["max_vertical_leap"]]) ~ "max_vertical_leap",
    stringr::str_detect(col, candidate_column_patterns[["standing_vertical_leap"]]) ~ "standing_vertical_leap",
    stringr::str_detect(col, candidate_column_patterns[["lane_agility_time"]]) ~ "lane_agility_time",
    stringr::str_detect(col, candidate_column_patterns[["shuttle_run"]]) ~ "shuttle_run",
    stringr::str_detect(col, candidate_column_patterns[["three_quarter_sprint"]]) ~ "three_quarter_sprint",
    stringr::str_detect(col, candidate_column_patterns[["bench_press"]]) ~ "bench_press",
    stringr::str_detect(col, candidate_column_patterns[["draft_combine"]]) ~ "draft_combine",
    stringr::str_detect(col, candidate_column_patterns[["position"]]) ~ "position",
    TRUE ~ NA_character_
  )
}

build_column_audit <- function(source_name, source_path_or_endpoint, df, notes) {
  if (ncol(df) == 0) {
    return(tibble::tibble(
      source_name = source_name,
      source_path_or_endpoint = source_path_or_endpoint,
      column_name = NA_character_,
      candidate_measurement = NA_character_,
      available = FALSE,
      non_missing_count = 0L,
      missing_pct = NA_real_,
      notes = notes
    ))
  }

  tibble::tibble(column_name = names(df)) %>%
    dplyr::mutate(
      candidate_measurement = vapply(.data$column_name, classify_candidate_measurement, character(1)),
      source_name = source_name,
      source_path_or_endpoint = source_path_or_endpoint,
      available = !is.na(.data$candidate_measurement),
      non_missing_count = vapply(.data$column_name, function(col) sum(!is.na(df[[col]])), integer(1)),
      missing_pct = vapply(.data$column_name, function(col) 100 * mean(is.na(df[[col]])), numeric(1)),
      notes = notes
    ) %>%
    dplyr::filter(.data$available) %>%
    dplyr::select(
      "source_name",
      "source_path_or_endpoint",
      "column_name",
      "candidate_measurement",
      "available",
      "non_missing_count",
      "missing_pct",
      "notes"
    )
}

standardize_measurement_table <- function(df, source_name) {
  if (nrow(df) == 0) {
    return(tibble::tibble())
  }

  df <- df %>%
    add_missing_cols(c(
      "player_id",
      "person_id",
      "player_name",
      "display_first_last",
      "player",
      "name",
      "height",
      "height_inches",
      "player_height",
      "height_wo_shoes",
      "height_without_shoes",
      "height_w_shoes",
      "height_with_shoes",
      "height_wo_shoes_ft_in",
      "height_w_shoes_ft_in",
      "weight",
      "weight_lbs",
      "player_weight",
      "wingspan",
      "wingspan_inches",
      "wingspan_ft_in",
      "standing_reach",
      "standing_reach_inches",
      "standing_reach_ft_in",
      "reach",
      "body_fat",
      "body_fat_pct",
      "body_fat_percentage",
      "max_vertical_leap",
      "max_vertical",
      "standing_vertical_leap",
      "standing_vertical",
      "lane_agility_time",
      "modified_lane_agility_time",
      "shuttle_run",
      "three_quarter_sprint",
      "three_quarters_sprint",
      "threequarter_sprint",
      "bench_press"
    ), NA)

  df %>%
    dplyr::transmute(
      player_id = coalesce_character_cols(., c("player_id", "person_id")),
      player_name = coalesce_character_cols(., c("player_name", "display_first_last", "player", "name")),
      height = dplyr::coalesce(
        normalize_inches(.data$height_inches),
        normalize_inches(.data$height),
        normalize_inches(.data$player_height),
        normalize_inches(.data$height_w_shoes),
        normalize_inches(.data$height_with_shoes),
        normalize_inches(.data$height_wo_shoes),
        normalize_inches(.data$height_without_shoes),
        normalize_inches(.data$height_w_shoes_ft_in),
        normalize_inches(.data$height_wo_shoes_ft_in)
      ),
      weight = dplyr::coalesce(
        normalize_weight_lbs(.data$weight_lbs),
        normalize_weight_lbs(.data$weight),
        normalize_weight_lbs(.data$player_weight)
      ),
      wingspan = dplyr::coalesce(
        normalize_inches(.data$wingspan_inches),
        normalize_inches(.data$wingspan),
        normalize_inches(.data$wingspan_ft_in)
      ),
      standing_reach = dplyr::coalesce(
        normalize_inches(.data$standing_reach_inches),
        normalize_inches(.data$standing_reach),
        normalize_inches(.data$standing_reach_ft_in),
        normalize_inches(.data$reach)
      ),
      body_fat = coalesce_numeric_cols(., c("body_fat", "body_fat_pct", "body_fat_percentage")),
      max_vertical_leap = coalesce_numeric_cols(., c("max_vertical_leap", "max_vertical")),
      standing_vertical_leap = coalesce_numeric_cols(., c("standing_vertical_leap", "standing_vertical")),
      lane_agility_time = coalesce_numeric_cols(., c("lane_agility_time", "modified_lane_agility_time")),
      shuttle_run = coalesce_numeric_cols(., c("shuttle_run")),
      three_quarter_sprint = coalesce_numeric_cols(., c("three_quarter_sprint", "three_quarters_sprint", "threequarter_sprint")),
      bench_press = coalesce_numeric_cols(., c("bench_press")),
      measurement_source = source_name,
      measurement_note = "Normalized from local candidate metadata column(s); no physical attribute inference performed."
    ) %>%
    dplyr::filter(
      !is.na(.data$player_id) | !is.na(.data$player_name),
      !is.na(.data$height) |
        !is.na(.data$weight) |
        !is.na(.data$wingspan) |
        !is.na(.data$standing_reach) |
        !is.na(.data$body_fat) |
        !is.na(.data$max_vertical_leap) |
        !is.na(.data$standing_vertical_leap) |
        !is.na(.data$lane_agility_time) |
        !is.na(.data$shuttle_run) |
        !is.na(.data$three_quarter_sprint) |
        !is.na(.data$bench_press)
    )
}

build_databallr_wingspan_audit <- function(df, source_path) {
  if (nrow(df) == 0) {
    return(tibble::tibble(
      source_name = "DataBallR wingspan_all_2026-05-31.csv",
      source_path_or_endpoint = source_path,
      column_name = NA_character_,
      candidate_measurement = NA_character_,
      available = FALSE,
      non_missing_count = 0L,
      missing_pct = NA_real_,
      notes = "DataBallR supplemental wingspan file was not available or contained no rows."
    ))
  }

  audit_spec <- tibble::tribble(
    ~column_name, ~candidate_measurement,
    "databallr_height_wo_shoes", "height_wo_shoes",
    "wingspan", "wingspan",
    "height_wingspan_diff", "height_wingspan_diff",
    "databallr_pos2", "position",
    "databallr_primary_pos", "position",
    "databallr_active", "active_status"
  )

  audit_spec %>%
    dplyr::mutate(
      source_name = "DataBallR wingspan_all_2026-05-31.csv",
      source_path_or_endpoint = source_path,
      available = .data$column_name %in% names(df),
      non_missing_count = vapply(
        .data$column_name,
        function(col) if (col %in% names(df)) sum(!is.na(df[[col]])) else 0L,
        integer(1)
      ),
      missing_pct = vapply(
        .data$column_name,
        function(col) if (col %in% names(df)) 100 * mean(is.na(df[[col]])) else NA_real_,
        numeric(1)
      ),
      notes = "Supplemental DataBallR measurement source. Used for preferred wingspan and secondary height-without-shoes validation only; no physical attribute inference performed."
    ) %>%
    dplyr::select(
      "source_name",
      "source_path_or_endpoint",
      "column_name",
      "candidate_measurement",
      "available",
      "non_missing_count",
      "missing_pct",
      "notes"
    )
}

standardize_databallr_wingspan_table <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble())
  }

  tryCatch(
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE) %>%
      tibble::as_tibble() %>%
      janitor::clean_names() %>%
      normalize_missing_strings() %>%
      add_missing_cols(c(
        "nba_id",
        "player",
        "pos2",
        "primary_pos",
        "active",
        "height_wo_shoes_in",
        "wingspan_in",
        "height_wingspan_diff_in",
        "team",
        "d_dpm"
      ), NA) %>%
      dplyr::transmute(
        player_id = as.character(.data$nba_id),
        player_name = as.character(.data$player),
        player_name_databallr = as.character(.data$player),
        height = normalize_inches(.data$height_wo_shoes_in),
        weight = NA_real_,
        wingspan = normalize_inches(.data$wingspan_in),
        standing_reach = NA_real_,
        body_fat = NA_real_,
        max_vertical_leap = NA_real_,
        standing_vertical_leap = NA_real_,
        lane_agility_time = NA_real_,
        modified_lane_agility_time = NA_real_,
        shuttle_run = NA_real_,
        three_quarter_sprint = NA_real_,
        bench_press = NA_real_,
        databallr_height_wo_shoes = normalize_inches(.data$height_wo_shoes_in),
        height_wingspan_diff = suppressWarnings(as.numeric(.data$height_wingspan_diff_in)),
        databallr_pos2 = as.character(.data$pos2),
        databallr_primary_pos = as.character(.data$primary_pos),
        databallr_team = as.character(.data$team),
        databallr_active = as.character(.data$active),
        databallr_d_dpm = suppressWarnings(as.numeric(.data$d_dpm)),
        databallr_measurement_found = !is.na(.data$nba_id) & (
          !is.na(normalize_inches(.data$wingspan_in)) |
            !is.na(normalize_inches(.data$height_wo_shoes_in))
        ),
        measurement_source = "DataBallR wingspan_all_2026-05-31.csv",
        measurement_note = "DataBallR supplemental measurement row; preferred for wingspan, secondary for height-without-shoes validation."
      ) %>%
      dplyr::filter(
        !is.na(.data$player_id) | !is.na(.data$player_name),
        !is.na(.data$wingspan) | !is.na(.data$databallr_height_wo_shoes) | !is.na(.data$height_wingspan_diff)
      ),
    error = function(e) {
      message("Could not read DataBallR wingspan file: ", path, " | ", conditionMessage(e))
      tibble::tibble()
    }
  )
}

infer_latest_combine_season <- function(season_label) {
  parsed <- suppressWarnings(as.integer(stringr::str_extract(as.character(season_label), "\\d{4}")))

  if (is.na(parsed)) {
    return(as.integer(format(Sys.Date(), "%Y")))
  }

  parsed
}

infer_combine_seasons <- function(player_master_df, season_label) {
  draft_year_cols <- c("draft_year", "draft_year_display", "draft_year_number")
  existing_draft_year_cols <- draft_year_cols[draft_year_cols %in% names(player_master_df)]

  if (length(existing_draft_year_cols) > 0 && nrow(player_master_df) > 0) {
    draft_years <- unlist(lapply(existing_draft_year_cols, function(col) suppressWarnings(as.integer(player_master_df[[col]]))))
    draft_years <- sort(unique(draft_years[!is.na(draft_years) & draft_years >= 2000]))

    if (length(draft_years) > 0) {
      return(draft_years)
    }
  }

  2000:infer_latest_combine_season(season_label)
}

extract_draft_combine_stats_table <- function(result) {
  if (is.data.frame(result)) {
    return(tibble::as_tibble(result))
  }

  if (is.list(result) && "DraftCombineStats" %in% names(result) && is.data.frame(result[["DraftCombineStats"]])) {
    return(tibble::as_tibble(result[["DraftCombineStats"]]))
  }

  tables <- extract_all_endpoint_tables(result)

  if (length(tables) == 0) {
    return(tibble::tibble())
  }

  tables[[1]]
}

standardize_combine_table_types <- function(df) {
  if (nrow(df) == 0 && ncol(df) == 0) {
    return(tibble::tibble())
  }

  # Historical DraftCombineStats seasons are not type-stable in hoopR.
  # Keep the raw season tables character-only so bind_rows() cannot fail on
  # columns that are numeric in one draft class and text in another. Numeric
  # measurement parsing happens later in standardize_draft_combine_stats_table().
  df %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    normalize_missing_strings()
}

pull_draft_combine_stats_safely <- function(season_year) {
  raw_path <- file.path(draft_combine_raw_dir, paste0("draft_combine_stats_", season_year, ".parquet"))

  if (file.exists(raw_path)) {
    existing <- safe_read_parquet(raw_path) %>%
      standardize_combine_table_types() %>%
      add_missing_cols("combine_season", season_year) %>%
      dplyr::mutate(combine_season = dplyr::coalesce(as.character(.data$combine_season), as.character(season_year)))

    return(list(
      success = TRUE,
      error = NA_character_,
      table = existing,
      raw_path = raw_path,
      from_cache = TRUE,
      standardized_success = TRUE,
      standardized_columns = names(existing)
    ))
  }

  if (!requireNamespace("hoopR", quietly = TRUE)) {
    return(list(success = FALSE, error = "hoopR package unavailable.", table = tibble::tibble(), raw_path = raw_path, from_cache = FALSE))
  }

  if (!exists("nba_draftcombinestats", envir = asNamespace("hoopR"), inherits = FALSE)) {
    return(list(success = FALSE, error = "hoopR endpoint not found: nba_draftcombinestats", table = tibble::tibble(), raw_path = raw_path, from_cache = FALSE))
  }

  result <- tryCatch(
    hoopR::nba_draftcombinestats(season_year = season_year),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    return(list(success = FALSE, error = conditionMessage(result), table = tibble::tibble(), raw_path = raw_path, from_cache = FALSE))
  }

  combine_table <- extract_draft_combine_stats_table(result) %>%
    standardize_combine_table_types() %>%
    dplyr::mutate(combine_season = as.character(season_year))

  if (nrow(combine_table) > 0) {
    write_project_parquet(combine_table, raw_path)
  }

  list(
    success = TRUE,
    error = NA_character_,
    table = combine_table,
    raw_path = raw_path,
    from_cache = FALSE,
    standardized_success = TRUE,
    standardized_columns = names(combine_table)
  )
}

standardize_draft_combine_stats_table <- function(df) {
  if (nrow(df) == 0) {
    return(tibble::tibble())
  }

  df <- df %>%
    add_missing_cols(c(
      "player_id",
      "player_name",
      "position",
      "height_wo_shoes",
      "height_wo_shoes_ft_in",
      "height_w_shoes",
      "height_w_shoes_ft_in",
      "weight",
      "wingspan",
      "wingspan_ft_in",
      "standing_reach",
      "standing_reach_ft_in",
      "body_fat_pct",
      "standing_vertical_leap",
      "max_vertical_leap",
      "lane_agility_time",
      "modified_lane_agility_time",
      "three_quarter_sprint",
      "bench_press",
      "combine_season"
    ), NA)

  df %>%
    dplyr::transmute(
      player_id = as.character(.data$player_id),
      player_name = as.character(.data$player_name),
      height = dplyr::coalesce(
        normalize_inches(.data$height_w_shoes),
        normalize_inches(.data$height_w_shoes_ft_in),
        normalize_inches(.data$height_wo_shoes),
        normalize_inches(.data$height_wo_shoes_ft_in)
      ),
      weight = normalize_weight_lbs(.data$weight),
      wingspan = dplyr::coalesce(normalize_inches(.data$wingspan), normalize_inches(.data$wingspan_ft_in)),
      standing_reach = dplyr::coalesce(normalize_inches(.data$standing_reach), normalize_inches(.data$standing_reach_ft_in)),
      body_fat = suppressWarnings(as.numeric(.data$body_fat_pct)),
      standing_vertical_leap = suppressWarnings(as.numeric(.data$standing_vertical_leap)),
      max_vertical_leap = suppressWarnings(as.numeric(.data$max_vertical_leap)),
      lane_agility_time = suppressWarnings(as.numeric(.data$lane_agility_time)),
      modified_lane_agility_time = suppressWarnings(as.numeric(.data$modified_lane_agility_time)),
      shuttle_run = NA_real_,
      three_quarter_sprint = suppressWarnings(as.numeric(.data$three_quarter_sprint)),
      bench_press = suppressWarnings(as.numeric(.data$bench_press)),
      combine_height_wo_shoes = dplyr::coalesce(normalize_inches(.data$height_wo_shoes), normalize_inches(.data$height_wo_shoes_ft_in)),
      combine_height_w_shoes = dplyr::coalesce(normalize_inches(.data$height_w_shoes), normalize_inches(.data$height_w_shoes_ft_in)),
      combine_weight = normalize_weight_lbs(.data$weight),
      combine_wingspan = dplyr::coalesce(normalize_inches(.data$wingspan), normalize_inches(.data$wingspan_ft_in)),
      combine_standing_reach = dplyr::coalesce(normalize_inches(.data$standing_reach), normalize_inches(.data$standing_reach_ft_in)),
      combine_body_fat = suppressWarnings(as.numeric(.data$body_fat_pct)),
      combine_standing_vertical_leap = suppressWarnings(as.numeric(.data$standing_vertical_leap)),
      combine_max_vertical_leap = suppressWarnings(as.numeric(.data$max_vertical_leap)),
      combine_lane_agility_time = suppressWarnings(as.numeric(.data$lane_agility_time)),
      combine_modified_lane_agility_time = suppressWarnings(as.numeric(.data$modified_lane_agility_time)),
      combine_three_quarter_sprint = suppressWarnings(as.numeric(.data$three_quarter_sprint)),
      combine_bench_press = suppressWarnings(as.numeric(.data$bench_press)),
      draft_combine_season = suppressWarnings(as.integer(.data$combine_season)),
      measurement_source = paste0("nba_draftcombinestats::", .data$combine_season),
      measurement_note = "Normalized from historical nba_draftcombinestats; no physical attribute inference performed."
    ) %>%
    dplyr::filter(
      !is.na(.data$player_id) | !is.na(.data$player_name),
      !is.na(.data$height) |
        !is.na(.data$weight) |
        !is.na(.data$wingspan) |
        !is.na(.data$standing_reach) |
        !is.na(.data$body_fat) |
        !is.na(.data$standing_vertical_leap) |
        !is.na(.data$max_vertical_leap) |
        !is.na(.data$lane_agility_time) |
        !is.na(.data$modified_lane_agility_time) |
        !is.na(.data$three_quarter_sprint) |
        !is.na(.data$bench_press)
    )
}

extract_all_endpoint_tables <- function(x) {
  if (is.data.frame(x)) {
    return(list(result = tibble::as_tibble(x)))
  }

  if (!is.list(x)) {
    return(list())
  }

  out <- list()

  for (nm in names(x)) {
    item <- x[[nm]]
    if (is.data.frame(item)) {
      out[[nm]] <- tibble::as_tibble(item)
    }
  }

  if (length(out) == 0) {
    flat <- unlist(x, recursive = FALSE)
    for (i in seq_along(flat)) {
      if (is.data.frame(flat[[i]])) {
        out[[paste0("table_", i)]] <- tibble::as_tibble(flat[[i]])
      }
    }
  }

  out
}

pull_endpoint_safely <- function(function_name, args) {
  if (!requireNamespace("hoopR", quietly = TRUE)) {
    return(list(success = FALSE, error = "hoopR package unavailable.", tables = list()))
  }

  if (!exists(function_name, envir = asNamespace("hoopR"), inherits = FALSE)) {
    return(list(success = FALSE, error = paste("hoopR endpoint not found:", function_name), tables = list()))
  }

  fn <- get(function_name, envir = asNamespace("hoopR"))

  result <- tryCatch(
    do.call(fn, args),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    return(list(success = FALSE, error = conditionMessage(result), tables = list()))
  }

  list(success = TRUE, error = NA_character_, tables = extract_all_endpoint_tables(result))
}

local_parquet_files <- list.files(".", pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
local_parquet_files <- local_parquet_files[stringr::str_detect(stringr::str_to_lower(local_parquet_files), local_candidate_file_regex)]

local_audit_rows <- list()
local_measurement_rows <- list()

for (path in local_parquet_files) {
  df <- safe_read_parquet(path)
  source_name <- tools::file_path_sans_ext(basename(path))

  audit <- build_column_audit(
    source_name = source_name,
    source_path_or_endpoint = path,
    df = df,
    notes = "Local parquet metadata scan for physical measurement candidate columns."
  )

  local_audit_rows[[length(local_audit_rows) + 1]] <- audit

  if (nrow(audit) > 0 && any(audit$candidate_measurement %in% c(
    "height",
    "weight",
    "wingspan",
    "standing_reach",
    "body_fat",
    "max_vertical_leap",
    "standing_vertical_leap",
    "lane_agility_time",
    "shuttle_run",
    "three_quarter_sprint",
    "bench_press"
  ))) {
    standardized <- standardize_measurement_table(df, source_name)

    if (nrow(standardized) > 0) {
      local_measurement_rows[[length(local_measurement_rows) + 1]] <- standardized
    }
  }
}

databallr_wingspan_source_path <- dplyr::case_when(
  file.exists(databallr_wingspan_path) ~ databallr_wingspan_path,
  file.exists(databallr_wingspan_upload_fallback_path) ~ databallr_wingspan_upload_fallback_path,
  TRUE ~ databallr_wingspan_path
)

databallr_wingspan_measurements <- standardize_databallr_wingspan_table(databallr_wingspan_source_path)
local_audit_rows[[length(local_audit_rows) + 1]] <- build_databallr_wingspan_audit(
  databallr_wingspan_measurements,
  databallr_wingspan_source_path
)

if (nrow(databallr_wingspan_measurements) > 0) {
  local_measurement_rows[[length(local_measurement_rows) + 1]] <- databallr_wingspan_measurements
}

player_master_metadata_raw <- if (file.exists(player_master_path)) {
  safe_read_parquet(player_master_path)
} else {
  tibble::tibble()
}

player_master_for_endpoint_targets <- if (nrow(player_master_metadata_raw) > 0) {
  player_master_metadata_raw %>%
    add_missing_cols(c("player_id", "player_name", "nickname", "player_nickname"), NA) %>%
    dplyr::mutate(
      player_id = as.character(.data$player_id),
      player_name = as.character(.data$player_name),
      player_nickname = coalesce_character_cols(., c("player_nickname", "nickname"))
    ) %>%
    dplyr::filter(!is.na(.data$player_id)) %>%
    dplyr::distinct(.data$player_id, .data$player_name, .data$player_nickname)
} else {
  tibble::tibble(player_id = character(), player_name = character(), player_nickname = character())
}

sample_player_ids <- player_master_for_endpoint_targets %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::pull(.data$player_id)

endpoint_specs <- list(
  list(function_name = "nba_playerindex", args = list(season = season), note = "NBA Stats player index may expose roster bio height/weight fields."),
  list(function_name = "nba_commonallplayers", args = list(season = season), note = "Common all players endpoint may expose roster/player identity metadata."),
  list(function_name = "nba_draftcombineplayeranthro", args = list(), note = "Draft combine anthropometric endpoint may expose height, weight, wingspan, and standing reach.")
)

if (length(sample_player_ids) > 0) {
  endpoint_specs[[length(endpoint_specs) + 1]] <- list(
    function_name = "nba_commonplayerinfo",
    args = list(player_id = sample_player_ids[[1]]),
    note = "Common player info endpoint may expose roster bio height and weight."
  )
}

endpoint_audit_rows <- list()
endpoint_measurement_rows <- list()

for (spec in endpoint_specs) {
  message("Auditing hoopR endpoint: ", spec$function_name)
  pulled <- pull_endpoint_safely(spec$function_name, spec$args)

  if (!pulled$success) {
    endpoint_audit_rows[[length(endpoint_audit_rows) + 1]] <- tibble::tibble(
      source_name = spec$function_name,
      source_path_or_endpoint = spec$function_name,
      column_name = NA_character_,
      candidate_measurement = NA_character_,
      available = FALSE,
      non_missing_count = 0L,
      missing_pct = NA_real_,
      notes = paste(spec$note, "Endpoint audit failed:", pulled$error)
    )
    next
  }

  if (length(pulled$tables) == 0) {
    endpoint_audit_rows[[length(endpoint_audit_rows) + 1]] <- tibble::tibble(
      source_name = spec$function_name,
      source_path_or_endpoint = spec$function_name,
      column_name = NA_character_,
      candidate_measurement = NA_character_,
      available = FALSE,
      non_missing_count = 0L,
      missing_pct = NA_real_,
      notes = paste(spec$note, "Endpoint returned no data frames.")
    )
    next
  }

  for (table_name in names(pulled$tables)) {
    endpoint_df <- pulled$tables[[table_name]] %>%
      janitor::clean_names() %>%
      normalize_missing_strings() %>%
      convert_numeric_cols()

    source_name <- paste(spec$function_name, table_name, sep = "::")

    endpoint_audit_rows[[length(endpoint_audit_rows) + 1]] <- build_column_audit(
      source_name = source_name,
      source_path_or_endpoint = spec$function_name,
      df = endpoint_df,
      notes = paste(
        spec$note,
        "Endpoint returned successfully.",
        "Returned columns:",
        paste(names(endpoint_df), collapse = ", ")
      )
    )

    endpoint_measurements <- standardize_measurement_table(endpoint_df, source_name)

    if (nrow(endpoint_measurements) > 0) {
      endpoint_measurement_rows[[length(endpoint_measurement_rows) + 1]] <- endpoint_measurements %>%
        dplyr::mutate(measurement_note = "Normalized from hoopR/NBA endpoint candidate measurement columns; no physical attribute inference performed.")
    }
  }
}

combine_seasons <- infer_combine_seasons(player_master_metadata_raw, season)
draft_combine_pull_diagnostics <- list()
draft_combine_tables <- list()

for (yr in combine_seasons) {
  message("Auditing hoopR endpoint: nba_draftcombinestats season_year=", yr)
  combine_pull <- pull_draft_combine_stats_safely(yr)

  draft_combine_pull_diagnostics[[length(draft_combine_pull_diagnostics) + 1]] <- tibble::tibble(
    combine_season = as.integer(yr),
    pull_success = combine_pull$success,
    standardized_success = isTRUE(combine_pull$standardized_success),
    standardized_column_count = length(if (!is.null(combine_pull$standardized_columns)) combine_pull$standardized_columns else character()),
    row_count = nrow(combine_pull$table),
    raw_path = combine_pull$raw_path,
    from_cache = combine_pull$from_cache,
    error = dplyr::if_else(is.na(combine_pull$error), NA_character_, combine_pull$error)
  )

  if (!combine_pull$success) {
    endpoint_audit_rows[[length(endpoint_audit_rows) + 1]] <- tibble::tibble(
      source_name = paste0("nba_draftcombinestats::", yr),
      source_path_or_endpoint = "nba_draftcombinestats",
      column_name = NA_character_,
      candidate_measurement = NA_character_,
      available = FALSE,
      non_missing_count = 0L,
      missing_pct = NA_real_,
      notes = paste("Draft combine stats historical pull failed for season_year", yr, ":", combine_pull$error)
    )
    next
  }

  if (nrow(combine_pull$table) == 0) {
    endpoint_audit_rows[[length(endpoint_audit_rows) + 1]] <- tibble::tibble(
      source_name = paste0("nba_draftcombinestats::", yr),
      source_path_or_endpoint = "nba_draftcombinestats",
      column_name = NA_character_,
      candidate_measurement = NA_character_,
      available = FALSE,
      non_missing_count = 0L,
      missing_pct = NA_real_,
      notes = paste("Draft combine stats returned no rows for season_year", yr)
    )
    next
  }

  draft_combine_tables[[length(draft_combine_tables) + 1]] <- combine_pull$table

  endpoint_audit_rows[[length(endpoint_audit_rows) + 1]] <- build_column_audit(
    source_name = paste0("nba_draftcombinestats::", yr, "::DraftCombineStats"),
    source_path_or_endpoint = "nba_draftcombinestats",
    df = combine_pull$table,
    notes = paste(
      "Historical DraftCombineStats table returned for season_year",
      yr,
      "Returned columns:",
      paste(names(combine_pull$table), collapse = ", ")
    )
  )
}

historical_draft_combine_stats <- dplyr::bind_rows(draft_combine_tables)
draft_combine_pull_diagnostics <- dplyr::bind_rows(draft_combine_pull_diagnostics)
draft_combine_standardized_columns <- sort(unique(unlist(lapply(draft_combine_tables, names), use.names = FALSE)))

historical_draft_combine_measurements <- standardize_draft_combine_stats_table(historical_draft_combine_stats)

if (nrow(historical_draft_combine_measurements) > 0) {
  endpoint_measurement_rows[[length(endpoint_measurement_rows) + 1]] <- historical_draft_combine_measurements
}

physical_measurement_source_audit <- dplyr::bind_rows(local_audit_rows, endpoint_audit_rows) %>%
  dplyr::mutate(
    available = dplyr::coalesce(.data$available, FALSE),
    notes = dplyr::coalesce(.data$notes, "Physical measurement source audit row.")
  ) %>%
  dplyr::arrange(.data$source_name, .data$candidate_measurement, .data$column_name)

write_project_parquet(physical_measurement_source_audit, physical_audit_path)

candidate_measurements <- dplyr::bind_rows(local_measurement_rows, endpoint_measurement_rows)
candidate_measurements <- candidate_measurements %>%
  add_missing_cols(c(
    "height",
    "weight",
    "wingspan",
    "standing_reach",
    "body_fat",
    "max_vertical_leap",
    "standing_vertical_leap",
    "lane_agility_time",
    "modified_lane_agility_time",
    "shuttle_run",
    "three_quarter_sprint",
    "bench_press",
    "combine_height_wo_shoes",
    "combine_height_w_shoes",
    "combine_weight",
    "combine_wingspan",
    "combine_standing_reach",
    "combine_body_fat",
    "combine_standing_vertical_leap",
    "combine_max_vertical_leap",
    "combine_lane_agility_time",
    "combine_modified_lane_agility_time",
    "combine_three_quarter_sprint",
    "combine_bench_press",
    "draft_combine_season",
    "databallr_height_wo_shoes",
    "height_wingspan_diff",
    "databallr_pos2",
    "databallr_primary_pos",
    "databallr_team",
    "databallr_active",
    "databallr_d_dpm",
    "databallr_measurement_found"
  ), NA)

if (nrow(candidate_measurements) > 0) {
  player_physical_measurements <- candidate_measurements %>%
    dplyr::mutate(
      player_id = as.character(.data$player_id),
      endpoint_player_name = as.character(.data$player_name),
      source_priority = dplyr::case_when(
        stringr::str_detect(.data$measurement_source, "commonplayerinfo|playerindex") ~ 1,
        stringr::str_detect(.data$measurement_source, "(?i)databallr") ~ 2,
        stringr::str_detect(.data$measurement_source, "draftcombine") ~ 3,
        TRUE ~ 3
      ),
      measurement_completeness = rowSums(cbind(
        !is.na(.data$height),
        !is.na(.data$weight),
        !is.na(.data$wingspan),
        !is.na(.data$standing_reach),
        !is.na(.data$body_fat),
        !is.na(.data$max_vertical_leap),
        !is.na(.data$standing_vertical_leap),
        !is.na(.data$lane_agility_time),
        !is.na(.data$modified_lane_agility_time),
        !is.na(.data$shuttle_run),
        !is.na(.data$three_quarter_sprint),
        !is.na(.data$bench_press)
      ))
    ) %>%
    dplyr::left_join(
      player_master_for_endpoint_targets %>%
        dplyr::rename(player_master_name = "player_name"),
      by = "player_id"
    ) %>%
    dplyr::mutate(
      player_name = dplyr::coalesce(.data$player_master_name, .data$endpoint_player_name),
      player_key = dplyr::coalesce(.data$player_id, paste0("name:", .data$player_name))
    ) %>%
    dplyr::filter(!is.na(.data$player_key)) %>%
    dplyr::arrange(.data$player_key, .data$source_priority, dplyr::desc(.data$measurement_completeness)) %>%
    dplyr::group_by(.data$player_key) %>%
    dplyr::summarise(
      player_id = dplyr::first(stats::na.omit(.data$player_id), default = NA_character_),
      player_name = dplyr::first(stats::na.omit(.data$player_name), default = NA_character_),
      height = first_value_by_source(.data$height, .data$measurement_source, "commonplayerinfo|playerindex", NA_real_),
      weight = first_value_by_source(.data$weight, .data$measurement_source, "commonplayerinfo|playerindex", NA_real_),
      databallr_height_wo_shoes = first_value_by_source(.data$databallr_height_wo_shoes, .data$measurement_source, "(?i)databallr", NA_real_),
      wingspan = first_value_by_source(.data$wingspan, .data$measurement_source, "(?i)databallr", NA_real_),
      height_wingspan_diff = first_value_by_source(.data$height_wingspan_diff, .data$measurement_source, "(?i)databallr", NA_real_),
      standing_reach = dplyr::first(stats::na.omit(.data$standing_reach), default = NA_real_),
      body_fat = dplyr::first(stats::na.omit(.data$body_fat), default = NA_real_),
      max_vertical_leap = dplyr::first(stats::na.omit(.data$max_vertical_leap), default = NA_real_),
      standing_vertical_leap = dplyr::first(stats::na.omit(.data$standing_vertical_leap), default = NA_real_),
      lane_agility_time = dplyr::first(stats::na.omit(.data$lane_agility_time), default = NA_real_),
      modified_lane_agility_time = dplyr::first(stats::na.omit(.data$modified_lane_agility_time), default = NA_real_),
      shuttle_run = dplyr::first(stats::na.omit(.data$shuttle_run), default = NA_real_),
      three_quarter_sprint = dplyr::first(stats::na.omit(.data$three_quarter_sprint), default = NA_real_),
      bench_press = dplyr::first(stats::na.omit(.data$bench_press), default = NA_real_),
      combine_height_wo_shoes = dplyr::first(stats::na.omit(.data$combine_height_wo_shoes), default = NA_real_),
      combine_height_w_shoes = dplyr::first(stats::na.omit(.data$combine_height_w_shoes), default = NA_real_),
      combine_weight = dplyr::first(stats::na.omit(.data$combine_weight), default = NA_real_),
      combine_wingspan = dplyr::first(stats::na.omit(.data$combine_wingspan), default = NA_real_),
      combine_standing_reach = dplyr::first(stats::na.omit(.data$combine_standing_reach), default = NA_real_),
      combine_body_fat = dplyr::first(stats::na.omit(.data$combine_body_fat), default = NA_real_),
      combine_standing_vertical_leap = dplyr::first(stats::na.omit(.data$combine_standing_vertical_leap), default = NA_real_),
      combine_max_vertical_leap = dplyr::first(stats::na.omit(.data$combine_max_vertical_leap), default = NA_real_),
      combine_lane_agility_time = dplyr::first(stats::na.omit(.data$combine_lane_agility_time), default = NA_real_),
      combine_modified_lane_agility_time = dplyr::first(stats::na.omit(.data$combine_modified_lane_agility_time), default = NA_real_),
      combine_three_quarter_sprint = dplyr::first(stats::na.omit(.data$combine_three_quarter_sprint), default = NA_real_),
      combine_bench_press = dplyr::first(stats::na.omit(.data$combine_bench_press), default = NA_real_),
      draft_combine_season = dplyr::first(stats::na.omit(.data$draft_combine_season), default = NA_integer_),
      databallr_pos2 = first_value_by_source(.data$databallr_pos2, .data$measurement_source, "(?i)databallr", NA_character_),
      databallr_primary_pos = first_value_by_source(.data$databallr_primary_pos, .data$measurement_source, "(?i)databallr", NA_character_),
      databallr_team = first_value_by_source(.data$databallr_team, .data$measurement_source, "(?i)databallr", NA_character_),
      databallr_active = first_value_by_source(.data$databallr_active, .data$measurement_source, "(?i)databallr", NA_character_),
      databallr_d_dpm = first_value_by_source(.data$databallr_d_dpm, .data$measurement_source, "(?i)databallr", NA_real_),
      databallr_measurement_found = any(dplyr::coalesce(as.logical(.data$databallr_measurement_found), FALSE), na.rm = TRUE),
      measurement_source = paste(unique(stats::na.omit(.data$measurement_source)), collapse = " | "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      draft_combine_measurement_found = stringr::str_detect(.data$measurement_source, "draftcombine"),
      databallr_measurement_found = dplyr::coalesce(.data$databallr_measurement_found, FALSE),
      current_height_weight_source_found = stringr::str_detect(.data$measurement_source, "playerindex|commonplayerinfo"),
      combine_athletic_metric_count = rowSums(cbind(
        !is.na(.data$standing_vertical_leap),
        !is.na(.data$max_vertical_leap),
        !is.na(.data$lane_agility_time),
        !is.na(.data$modified_lane_agility_time),
        !is.na(.data$shuttle_run),
        !is.na(.data$three_quarter_sprint),
        !is.na(.data$bench_press)
      )),
      measurement_note = dplyr::case_when(
        .data$current_height_weight_source_found & .data$databallr_measurement_found & .data$draft_combine_measurement_found & .data$combine_athletic_metric_count == 0 ~
          "current height/weight from playerindex; DataBallR wingspan matched; combine row found but athletic testing mostly missing",
        .data$current_height_weight_source_found & .data$databallr_measurement_found & .data$draft_combine_measurement_found ~
          "current height/weight from playerindex; DataBallR wingspan matched; combine data matched",
        .data$current_height_weight_source_found & .data$databallr_measurement_found & !.data$draft_combine_measurement_found ~
          "current height/weight from playerindex; DataBallR wingspan matched; no draft combine row found",
        .data$current_height_weight_source_found & !.data$databallr_measurement_found & .data$draft_combine_measurement_found ~
          "current height/weight from playerindex; DataBallR wingspan unavailable; combine data matched",
        .data$current_height_weight_source_found & .data$draft_combine_measurement_found & .data$combine_athletic_metric_count == 0 ~
          "current height/weight from playerindex; DataBallR wingspan unavailable; combine row found but athletic testing mostly missing",
        .data$current_height_weight_source_found & .data$draft_combine_measurement_found ~
          "current height/weight from playerindex; DataBallR wingspan unavailable; combine data matched",
        .data$current_height_weight_source_found & !.data$draft_combine_measurement_found ~
          "current height/weight from playerindex; DataBallR wingspan unavailable; no draft combine row found",
        .data$databallr_measurement_found ~
          "DataBallR wingspan matched; current playerindex height/weight unavailable",
        .data$draft_combine_measurement_found ~
          "draft combine data matched; DataBallR wingspan unavailable; current playerindex height/weight unavailable",
        TRUE ~ "DataBallR wingspan unavailable; normalized physical measurement audit output; no physical attribute inference performed"
      )
    ) %>%
    dplyr::select(
      "player_id",
      "player_name",
      "height",
      "weight",
      "databallr_height_wo_shoes",
      "wingspan",
      "height_wingspan_diff",
      "standing_reach",
      "body_fat",
      "max_vertical_leap",
      "standing_vertical_leap",
      "lane_agility_time",
      "modified_lane_agility_time",
      "shuttle_run",
      "three_quarter_sprint",
      "bench_press",
      "combine_height_wo_shoes",
      "combine_height_w_shoes",
      "combine_weight",
      "combine_wingspan",
      "combine_standing_reach",
      "combine_body_fat",
      "combine_standing_vertical_leap",
      "combine_max_vertical_leap",
      "combine_lane_agility_time",
      "combine_modified_lane_agility_time",
      "combine_three_quarter_sprint",
      "combine_bench_press",
      "draft_combine_measurement_found",
      "draft_combine_season",
      "databallr_pos2",
      "databallr_primary_pos",
      "databallr_team",
      "databallr_active",
      "databallr_d_dpm",
      "databallr_measurement_found",
      "measurement_source",
      "measurement_note"
    ) %>%
    dplyr::filter(
      !is.na(.data$height) |
        !is.na(.data$weight) |
        !is.na(.data$databallr_height_wo_shoes) |
        !is.na(.data$wingspan) |
        !is.na(.data$height_wingspan_diff) |
        !is.na(.data$standing_reach) |
        !is.na(.data$body_fat) |
        !is.na(.data$max_vertical_leap) |
        !is.na(.data$standing_vertical_leap) |
        !is.na(.data$lane_agility_time) |
        !is.na(.data$modified_lane_agility_time) |
        !is.na(.data$shuttle_run) |
        !is.na(.data$three_quarter_sprint) |
        !is.na(.data$bench_press)
    )

  usable_measurement_available <- nrow(player_physical_measurements) > 0 &&
    any(
      !is.na(player_physical_measurements$height) |
        !is.na(player_physical_measurements$weight) |
        !is.na(player_physical_measurements$databallr_height_wo_shoes) |
        !is.na(player_physical_measurements$wingspan) |
        !is.na(player_physical_measurements$height_wingspan_diff) |
        !is.na(player_physical_measurements$standing_reach) |
        !is.na(player_physical_measurements$body_fat) |
        !is.na(player_physical_measurements$max_vertical_leap) |
        !is.na(player_physical_measurements$standing_vertical_leap) |
        !is.na(player_physical_measurements$lane_agility_time) |
        !is.na(player_physical_measurements$modified_lane_agility_time) |
        !is.na(player_physical_measurements$shuttle_run) |
        !is.na(player_physical_measurements$three_quarter_sprint) |
        !is.na(player_physical_measurements$bench_press)
    )

  if (usable_measurement_available) {
    write_project_parquet(player_physical_measurements, physical_measurements_path)
  } else {
    message("No usable physical or athletic measurements found; player_physical_measurements.parquet was not written.")
  }
} else {
  player_physical_measurements <- tibble::tibble()
  message("No usable physical measurements found; player_physical_measurements.parquet was not written.")
}

message("Phase 27a physical measurement source diagnostics:")

message("Candidate metadata files and matching columns:")
print(
  physical_measurement_source_audit %>%
    dplyr::filter(stringr::str_detect(.data$source_path_or_endpoint, "\\.parquet$")) %>%
    dplyr::select("source_name", "source_path_or_endpoint", "column_name", "candidate_measurement", "non_missing_count", "missing_pct", "notes")
)

message("External CSV / HoopR / NBA endpoint audit rows:")
print(
  physical_measurement_source_audit %>%
    dplyr::filter(!stringr::str_detect(.data$source_path_or_endpoint, "\\.parquet$")) %>%
    dplyr::select("source_name", "source_path_or_endpoint", "column_name", "candidate_measurement", "available", "non_missing_count", "missing_pct", "notes")
)

message("Measurement availability summary:")
print(
  physical_measurement_source_audit %>%
    dplyr::group_by(.data$candidate_measurement) %>%
    dplyr::summarise(
      source_columns = dplyr::n(),
      total_non_missing = sum(.data$non_missing_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$total_non_missing))
)

message("Historical draft combine pull summary:")
print(
  draft_combine_pull_diagnostics %>%
    dplyr::summarise(
      combine_seasons_requested = dplyr::n(),
      combine_seasons_successfully_pulled = sum(.data$pull_success, na.rm = TRUE),
      combine_seasons_successfully_standardized = sum(.data$standardized_success, na.rm = TRUE),
      total_combine_rows = sum(.data$row_count, na.rm = TRUE),
      total_combined_combine_rows = nrow(historical_draft_combine_stats),
      .groups = "drop"
    )
)

message("Historical draft combine columns standardized as character:")
print(tibble::tibble(column_name = draft_combine_standardized_columns))

if (nrow(player_physical_measurements) > 0) {
  current_player_ids <- player_master_for_endpoint_targets %>%
    dplyr::filter(!is.na(.data$player_id)) %>%
    dplyr::pull(.data$player_id)

  message("Current-player draft combine measurement coverage:")
  print(
    player_physical_measurements %>%
      dplyr::filter(.data$player_id %in% current_player_ids) %>%
      dplyr::summarise(
        current_players_with_measurement_rows = dplyr::n_distinct(.data$player_id),
        current_players_matched_to_combine_rows = dplyr::n_distinct(.data$player_id[.data$draft_combine_measurement_found]),
        current_players_matched_to_databallr_rows = dplyr::n_distinct(.data$player_id[.data$databallr_measurement_found]),
        current_players_with_wingspan = dplyr::n_distinct(.data$player_id[!is.na(.data$wingspan)]),
        current_players_with_standing_reach = dplyr::n_distinct(.data$player_id[!is.na(.data$standing_reach)]),
        current_players_with_athletic_testing = dplyr::n_distinct(.data$player_id[
          !is.na(.data$standing_vertical_leap) |
            !is.na(.data$max_vertical_leap) |
            !is.na(.data$lane_agility_time) |
            !is.na(.data$modified_lane_agility_time) |
            !is.na(.data$shuttle_run) |
            !is.na(.data$three_quarter_sprint) |
            !is.na(.data$bench_press)
        ]),
        .groups = "drop"
      )
  )

  message("Normalized measurement preview:")
  print(
    player_physical_measurements %>%
      dplyr::select(
        "player_id",
        "player_name",
        "height",
        "weight",
        "databallr_height_wo_shoes",
        "wingspan",
        "height_wingspan_diff",
        "standing_reach",
        tidyselect::any_of(c(
          "body_fat",
          "max_vertical_leap",
          "standing_vertical_leap",
          "lane_agility_time",
          "modified_lane_agility_time",
          "shuttle_run",
          "three_quarter_sprint",
          "bench_press",
          "databallr_pos2",
          "databallr_primary_pos",
          "databallr_team",
          "databallr_active",
          "databallr_d_dpm",
          "databallr_measurement_found",
          "draft_combine_measurement_found",
          "draft_combine_season"
        )),
        "measurement_source",
        "measurement_note"
      ) %>%
      dplyr::slice_head(n = 25)
  )

  message("Requested player measurement diagnostics:")
  print(
    player_physical_measurements %>%
      dplyr::filter(stringr::str_detect(
        .data$player_name,
        "LeBron James|Luka Don|Doncic|Dončić|Austin Reaves|Jaxson Hayes|Deandre Ayton|DeAndre Ayton|Victor Wembanyama|Rudy Gobert|Chet Holmgren|Jrue Holiday|Trae Young"
      )) %>%
      dplyr::select(
        "player_id",
        "player_name",
        "height",
        "weight",
        "databallr_height_wo_shoes",
        "wingspan",
        "height_wingspan_diff",
        tidyselect::any_of(c(
          "databallr_pos2",
          "databallr_primary_pos",
          "databallr_measurement_found"
        )),
        "measurement_note"
      ) %>%
      dplyr::arrange(.data$player_name)
  )
}

message("Saved physical measurement source audit to: ", physical_audit_path)
if (file.exists(physical_measurements_path)) {
  message("Saved normalized physical measurements to: ", physical_measurements_path)
}
message("Phase 27a note: measurement audit/pull only. Phase 27 was not modified.")
