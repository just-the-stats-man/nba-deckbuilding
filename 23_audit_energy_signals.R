# ============================================================
# 23_audit_energy_signals.R
# Phase 23: Movement, workload, and energy signal audit.
#
# Energy philosophy:
# Energy is NOT calories burned.
# Energy is NOT athleticism.
# Energy represents possession effort cost or move stamina cost.
#
# Future move energy may blend movement burden, creation burden, contact burden,
# jumping burden, and execution burden. This phase only audits whether current
# hoopR/NBA tracking sources expose realistic inputs for that idea.
#
# Audit only. Do not build energy scores. Do not modify previous phases.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

energy_output_dir <- "outputs/energy"
energy_signal_audit_path <- file.path(energy_output_dir, "energy_signal_audit.parquet")

fs::dir_create(energy_output_dir)

candidate_sources <- c(
  "outputs/attacks/player_tracking_creation_metrics.parquet",
  "outputs/attacks/player_creation_signals.parquet",
  "outputs/attacks/player_shot_context.parquet",
  "outputs/attacks/player_shot_making.parquet",
  "outputs/attacks/player_attack_library.parquet",
  "outputs/attacks/tracking_creation_audit.parquet",
  "outputs/attacks/shot_context_schema_audit.parquet",
  list.files("data/raw/tracking_creation", pattern = "\\.parquet$", full.names = TRUE),
  list.files("data/raw/tracking_shots", pattern = "\\.parquet$", full.names = TRUE)
)

candidate_sources <- candidate_sources[file.exists(candidate_sources)]

safe_read_parquet <- function(path) {
  tryCatch(
    {
      read_project_parquet(path) %>%
        janitor::clean_names() %>%
        convert_numeric_cols()
    },
    error = function(e) {
      tibble::tibble(.read_error = conditionMessage(e))
    }
  )
}

first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]

  if (length(hit) == 0) {
    return(character())
  }

  hit
}

collapse_chr <- function(x) {
  x <- sort(unique(stats::na.omit(as.character(x))))

  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(x, collapse = ", ")
}

infer_source_endpoint <- function(path, df) {
  endpoint_cols <- first_existing(df, c("endpoint", "source_endpoints"))

  if (length(endpoint_cols) > 0) {
    values <- unlist(df[endpoint_cols], use.names = FALSE)
    values <- stats::na.omit(as.character(values))

    if (length(values) > 0) {
      return(collapse_chr(values))
    }
  }

  name <- basename(path)
  dplyr::case_when(
    stringr::str_detect(name, "nba_boxscoreplayertrack") ~ "nba_boxscoreplayertrackv3",
    stringr::str_detect(name, "nba_boxscoreusage") ~ "nba_boxscoreusagev3",
    stringr::str_detect(name, "nba_assisttracker") ~ "nba_assisttracker",
    stringr::str_detect(name, "closestdefender|dribble|shotclock|touchtime|generalshooting|overall") ~ "nba_playerdashptshots",
    TRUE ~ tools::file_path_sans_ext(name)
  )
}

player_coverage_count <- function(df) {
  id_cols <- first_existing(df, c("player_id", "person_id", "nba_player_id", "athlete_id", "requested_player_id"))

  if (length(id_cols) == 0) {
    return(NA_integer_)
  }

  ids <- unlist(df[id_cols], use.names = FALSE)
  ids <- stats::na.omit(as.character(ids))
  ids <- ids[ids != ""]

  dplyr::n_distinct(ids)
}

missing_pct_for_col <- function(df, col) {
  if (length(col) == 0 || is.na(col) || !(col %in% names(df)) || nrow(df) == 0) {
    return(NA_real_)
  }

  100 * mean(is.na(df[[col]]))
}

signal_specs <- tibble::tribble(
  ~signal_name, ~burden_family, ~column_regex, ~notes,
  "distance traveled", "movement", "distance|dist_traveled|dist_miles|miles", "Movement workload proxy from player tracking.",
  "average speed", "movement", "avg_speed|average_speed|speed", "Overall movement intensity proxy.",
  "average offensive speed", "movement", "off.*speed|speed.*off", "Would be useful for off-ball offensive movement cost.",
  "average defensive speed", "movement", "def.*speed|speed.*def", "Would help split offensive and defensive energy.",
  "miles run", "movement", "miles|dist_miles|distance_miles", "May appear as distance in miles in tracking boxscore tables.",
  "touches", "movement", "\\btouches\\b|\\btouch\\b", "Touch volume contributes to possession workload.",
  "front court touches", "movement", "front_court_touches|frontcourt_touches|fc_touches", "Front-court touches better approximate offensive creation opportunity.",
  "time of possession", "movement", "time_of_possession|time_of_poss|time_poss", "Ball-control workload proxy.",
  "avg seconds per touch", "movement", "avg_sec_per_touch|seconds_per_touch|sec_per_touch", "Longer touches imply more sustained creation effort.",
  "avg dribbles per touch", "movement", "avg_drib_per_touch|avg_dribbles_per_touch|dribbles_per_touch|drib", "Dribble load proxy.",
  "minutes played", "movement", "\\bmin\\b|minutes|minutes_played", "Workload denominator and fatigue exposure proxy.",
  "possessions", "movement", "possessions|\\bposs\\b", "Possession exposure proxy.",
  "usage percentage", "creation", "usage_percentage|usage_pct|usg_pct|\\busage\\b", "Creation burden proxy.",
  "assists", "creation", "\\bassists\\b|\\bast\\b", "Playmaking burden proxy.",
  "secondary assists", "creation", "secondary_assists|secondary_ast|secondary_assist", "Better captures non-box-score creation.",
  "potential assists", "creation", "potential_assists|potential_ast|potential_assist", "Best available pass-creation opportunity proxy if present.",
  "dribbles per touch", "creation", "avg_drib_per_touch|avg_dribbles_per_touch|dribbles_per_touch", "On-ball creation effort proxy.",
  "drives", "contact", "\\bdrives\\b|drive", "High-contact rim pressure and creation effort proxy.",
  "rim attempts", "contact", "rim|restricted|paint.*attempt|attempt.*paint", "Rim pressure often increases contact cost.",
  "fouls drawn", "contact", "foul.*draw|drawn|\\bpfd\\b", "Contact received proxy.",
  "fouls committed", "contact", "foul|\\bpf\\b|personal_fouls", "Contact given proxy; defensive/offensive split may need context.",
  "paint touches", "contact", "paint.*touch|touch.*paint", "Interior touch/contact proxy.",
  "post touches", "contact", "post.*touch|touch.*post", "Physical creation burden proxy.",
  "jumps", "jumping", "\\bjumps\\b|jump_count|jump", "Direct jumping workload if available.",
  "jump count", "jumping", "jump_count|jumps", "Direct jump volume if available.",
  "contest jumps", "jumping", "contest.*jump|jump.*contest", "Shot contest energy if available.",
  "rebound jumps", "jumping", "rebound.*jump|jump.*rebound", "Boarding jump burden if available.",
  "block attempts", "jumping", "block_attempt|blk_attempt|blocks|\\bblk\\b", "Shot-blocking jump proxy.",
  "dunk attempts", "jumping", "dunk", "Dunk volume can proxy explosive jumping on offense.",
  "layup attempts", "jumping", "layup", "Lower-confidence jumping/contact proxy from attack labels/text.",
  "putback attempts", "jumping", "putback|put_back", "Putback attempts imply rebound/jump/execution sequence.",
  "alley-oop attempts", "jumping", "alley|oop|lob", "Lob finishing jump burden proxy.",
  "vertical contests", "jumping", "vertical.*contest|contest.*vertical", "Ideal contest-jump proxy if available.",
  "shot contests", "jumping", "contest|contested|close_def|closest.*def|def.*dist", "Contest/openness data can proxy defensive or execution burden.",
  "late shot clock attempts", "execution", "shot_clock|late_clock|very_late|clock", "Late-clock attempts increase execution burden.",
  "tightly contested attempts", "execution", "tight|very_tight|closest.*def|close_def|def.*dist", "Tight defense increases execution burden.",
  "difficult shot profile indicators", "execution", "pullup|stepback|fadeaway|floater|hook|dribble|touch_time|shot_clock", "Composite future signal from attack and shot context.",
  "self-created attempt burden", "execution", "self_created|unassisted|uast", "Currently risky unless sourced from real creation attribution."
)

audit_source <- function(path) {
  df <- safe_read_parquet(path)
  cols <- names(df)
  cols_lower <- stringr::str_to_lower(cols)
  endpoint <- infer_source_endpoint(path, df)
  coverage <- player_coverage_count(df)

  signal_specs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      matched_columns = list(cols[stringr::str_detect(cols_lower, .data$column_regex)]),
      column_name = collapse_chr(.data$matched_columns),
      available = length(.data$matched_columns) > 0,
      missing_pct = if (length(.data$matched_columns) == 0) {
        NA_real_
      } else {
        mean(purrr::map_dbl(.data$matched_columns, ~ missing_pct_for_col(df, .x)), na.rm = TRUE)
      },
      player_coverage = coverage,
      source_endpoint = endpoint,
      source_path = path,
      row_count = nrow(df)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      missing_pct = dplyr::if_else(is.nan(.data$missing_pct), NA_real_, .data$missing_pct),
      notes = dplyr::case_when(
        .data$available ~ .data$notes,
        TRUE ~ paste(.data$notes, "No matching current column found.")
      )
    ) %>%
    dplyr::select(
      "signal_name",
      "burden_family",
      "source_endpoint",
      "source_path",
      "column_name",
      "available",
      "missing_pct",
      "player_coverage",
      "row_count",
      "notes"
    )
}

energy_signal_audit <- if (length(candidate_sources) == 0) {
  signal_specs %>%
    dplyr::transmute(
      signal_name = .data$signal_name,
      burden_family = .data$burden_family,
      source_endpoint = NA_character_,
      source_path = NA_character_,
      column_name = NA_character_,
      available = FALSE,
      missing_pct = NA_real_,
      player_coverage = NA_integer_,
      row_count = 0L,
      notes = paste(.data$notes, "No current tracking sources found.")
    )
} else {
  dplyr::bind_rows(lapply(candidate_sources, audit_source))
}

if (!("burden_family" %in% names(energy_signal_audit))) {
  energy_signal_audit <- energy_signal_audit %>%
    dplyr::mutate(burden_family = "uncategorized")
}

energy_signal_audit <- energy_signal_audit %>%
  dplyr::mutate(
    burden_family = dplyr::case_when(
      .data$burden_family %in% c("movement", "creation", "contact", "jumping", "execution") ~ .data$burden_family,
      TRUE ~ "uncategorized"
    )
  )

write_project_parquet(energy_signal_audit, energy_signal_audit_path)

target_player_patterns <- tibble::tribble(
  ~player_label, ~player_regex,
  "Luka Doncic", "luka|don",
  "LeBron James", "lebron",
  "Stephen Curry", "stephen|curry",
  "Giannis Antetokounmpo", "giannis|antetokounmpo",
  "Rudy Gobert", "rudy|gobert",
  "Victor Wembanyama", "victor|wembanyama"
)

player_example_rows <- purrr::map_dfr(
  candidate_sources,
  function(path) {
    df <- safe_read_parquet(path)
    name_cols <- first_existing(df, c("player_name", "player_nickname", "requested_player_name", "name"))

    if (length(name_cols) == 0 || nrow(df) == 0) {
      return(tibble::tibble())
    }

    df %>%
      dplyr::mutate(
        .source_endpoint = infer_source_endpoint(path, df),
        .source_path = path,
        .player_text = do.call(
          paste,
          c(dplyr::across(tidyselect::all_of(name_cols)), sep = " ")
        )
      ) %>%
      dplyr::select(
        ".source_endpoint",
        ".source_path",
        ".player_text",
        tidyselect::any_of(c(
          "player_id",
          "person_id",
          "player_name",
          "player_nickname",
          "requested_player_name",
          "touches",
          "front_court_touches",
          "time_of_possession",
          "avg_sec_per_touch",
          "avg_drib_per_touch",
          "usage_percentage",
          "possessions",
          "assists",
          "secondary_assists",
          "potential_assists",
          "min",
          "minutes",
          "distance",
          "speed"
        ))
      ) %>%
      dplyr::filter(
        purrr::map_lgl(
          stringr::str_to_lower(.data$.player_text),
          ~ any(stringr::str_detect(.x, target_player_patterns$player_regex))
        )
      )
  }
)

message("Phase 23 energy signal audit diagnostics:")

message("Available signals:")
print(
  energy_signal_audit %>%
    dplyr::filter(.data$available) %>%
    dplyr::select(
      "signal_name",
      "burden_family",
      "source_endpoint",
      "column_name",
      "player_coverage",
      "missing_pct"
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$burden_family, .data$signal_name, .data$source_endpoint)
)

message("Unavailable signals:")
print(
  energy_signal_audit %>%
    dplyr::group_by(.data$signal_name, .data$burden_family) %>%
    dplyr::summarise(any_available = any(.data$available, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!.data$any_available) %>%
    dplyr::arrange(.data$burden_family, .data$signal_name)
)

message("Coverage summary:")
print(
  energy_signal_audit %>%
    dplyr::group_by(.data$burden_family, .data$signal_name) %>%
    dplyr::summarise(
      available_sources = sum(.data$available, na.rm = TRUE),
      max_player_coverage = suppressWarnings(max(.data$player_coverage, na.rm = TRUE)),
      best_missing_pct = suppressWarnings(min(.data$missing_pct, na.rm = TRUE)),
      source_endpoints = paste(sort(unique(stats::na.omit(.data$source_endpoint[.data$available]))), collapse = " | "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      max_player_coverage = dplyr::if_else(is.infinite(.data$max_player_coverage), NA_integer_, as.integer(.data$max_player_coverage)),
      best_missing_pct = dplyr::if_else(is.infinite(.data$best_missing_pct), NA_real_, .data$best_missing_pct)
    ) %>%
    dplyr::arrange(.data$burden_family, .data$signal_name)
)

message("Player examples: Luka / LeBron / Stephen Curry / Giannis / Rudy Gobert / Victor Wembanyama")
if (nrow(player_example_rows) == 0) {
  message("No requested player examples found in current tracking sources.")
} else {
  print(player_example_rows)
}

message("Saved energy signal audit to: ", energy_signal_audit_path)
message("Phase 23 note: audit only. Energy cost is not calories and not athleticism; no energy scores were built.")
