# ============================================================
# 21_audit_defensive_signals.R
# Phase 21: Defensive Event Audit.
#
# Goal:
# Determine which defensive signals are available in current play-by-play,
# tracking-shot, tracking-creation, and processed outputs before building DEF.
#
# This phase is audit-only. It does not build DEF and does not modify previous
# phase outputs. The output is a measurement feasibility map, not a model.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

pbp_path <- glue("data/raw/pbp/{team_abbr}_pbp_sample_{season}.parquet")
player_master_path <- glue("data/processed/player/player_master_{season}.parquet")
tracking_shots_dir <- "data/raw/tracking_shots"
tracking_creation_dir <- "data/raw/tracking_creation"
shot_context_path <- "outputs/attacks/player_shot_context.parquet"
tracking_creation_audit_path <- "outputs/attacks/tracking_creation_audit.parquet"
defense_output_dir <- "outputs/defense"
defensive_signal_audit_path <- file.path(defense_output_dir, "defensive_signal_audit.parquet")

fs::dir_create(defense_output_dir)

if (!file.exists(player_master_path)) {
  stop("Missing player master input: ", player_master_path, ". Run 02_player_season.R first.", call. = FALSE)
}

if (!file.exists(pbp_path)) {
  stop("Missing PBP input: ", pbp_path, ". Run 03_pull_games_pbp_rotations.R first.", call. = FALSE)
}

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

safe_percent <- function(num, den) {
  if (is.na(den) || den <= 0) {
    return(NA_real_)
  }

  100 * num / den
}

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

collapse_chr <- function(x) {
  x <- sort(unique(stats::na.omit(as.character(x))))

  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(x, collapse = ", ")
}

matching_columns <- function(cols, regex) {
  cols[stringr::str_detect(stringr::str_to_lower(cols), regex)]
}

text_columns <- function(df) {
  candidates <- c(
    "description",
    "home_description",
    "visitor_description",
    "neutral_description",
    "action_description",
    "action_type",
    "sub_type",
    "event_type",
    "event_msg_type",
    "event_msg_type_description",
    "event_action_type",
    "event_action_type_description"
  )

  intersect(candidates, names(df))
}

count_text_matches <- function(df, regex) {
  cols <- text_columns(df)

  if (length(cols) == 0 || nrow(df) == 0) {
    return(0L)
  }

  text <- df %>%
    dplyr::transmute(
      text_blob = do.call(
        paste,
        c(dplyr::across(tidyselect::all_of(cols)), sep = " ")
      )
    ) %>%
    dplyr::pull(.data$text_blob)

  sum(stringr::str_detect(stringr::str_to_lower(text), regex), na.rm = TRUE)
}

percent_missing_for_cols <- function(df, cols) {
  cols <- intersect(cols, names(df))

  if (length(cols) == 0 || nrow(df) == 0) {
    return(NA_real_)
  }

  mean(purrr::map_dbl(cols, ~ mean(is.na(df[[.x]])))) * 100
}

signal_specs <- tibble::tribble(
  ~signal, ~column_regex, ~keyword_regex, ~notes,
  "Steals", "steal|\\bstl\\b", "steal|stolen", "Usually available in PBP as an event or text action.",
  "Blocks", "block|\\bblk\\b", "\\bblock|blocked", "Usually available in PBP as an event or text action.",
  "Deflections", "deflect", "deflect", "Often unavailable unless a tracking/hustle endpoint is pulled.",
  "Contested shots", "contest|close_def|closest.*def|def.*dist|tight_defense|wide_open", "contest|contested|close defender", "Tracking shot context can support this better than PBP.",
  "Defensive rebounds", "def.*reb|dreb|rebound", "defensive rebound|dreb|rebound", "PBP may need rebound type/team context to separate offensive vs defensive boards.",
  "Charges drawn", "charge", "charge drawn|draws charge|offensive charge|charging", "Often text-only in PBP.",
  "Loose-ball recoveries", "loose.*ball|loose_ball|recovery|recover", "loose ball|recovery|recovers", "Often unavailable unless hustle data is present.",
  "Fouls committed", "foul|\\bpf\\b|personal_fouls", "foul|personal foul|shooting foul|offensive foul", "PBP usually supports foul events, but committed/drawn attribution needs player columns.",
  "Fouls drawn", "foul.*draw|drawn|\\bpfd\\b", "draws foul|foul drawn|shooting foul", "May require player2/opponent attribution in PBP.",
  "Matchup information", "matchup|defender|offender|opponent", "matchup|defender|guarded|against", "Requires matchup endpoint/table availability.",
  "Opponent FG% if available", "opp.*fg|opponent.*fg|dfgm|dfga|dfg|fg_pct|fg_percent", "opponent fg|dfg|defended field goal", "May be available only in tracking/matchup or dashboard outputs."
)

audit_source <- function(df, source_name, source_path, source_type) {
  row_count <- nrow(df)
  cols <- names(df)
  cols_lower <- stringr::str_to_lower(cols)

  signal_specs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      available_columns = collapse_chr(matching_columns(cols, .data$column_regex)),
      column_evidence_count = length(matching_columns(cols, .data$column_regex)),
      text_evidence_count = count_text_matches(df, .data$keyword_regex),
      evidence_count = dplyr::coalesce(.data$text_evidence_count, 0L),
      percent_missing = percent_missing_for_cols(df, matching_columns(cols, .data$column_regex)),
      source_name = source_name,
      source_path = source_path,
      source_type = source_type,
      row_count = row_count,
      all_columns = paste(cols, collapse = ", "),
      confidence = dplyr::case_when(
        .data$column_evidence_count > 0 & .data$text_evidence_count > 0 ~ "high",
        .data$column_evidence_count > 0 ~ "medium",
        .data$text_evidence_count > 0 ~ "medium",
        TRUE ~ "unavailable"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "signal",
      "source_name",
      "source_type",
      "source_path",
      "row_count",
      "available_columns",
      "column_evidence_count",
      "text_evidence_count",
      "evidence_count",
      "percent_missing",
      "confidence",
      "notes",
      "all_columns"
    )
}

player_master <- safe_read_parquet(player_master_path) %>%
  add_missing_cols(c("player_id", "player_name", "nickname", "player_nickname", "team_abbreviation", "team_abbr"), NA_character_) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    player_nickname = dplyr::coalesce(as.character(.data$player_nickname), as.character(.data$nickname)),
    team_abbreviation = dplyr::coalesce(as.character(.data$team_abbreviation), as.character(.data$team_abbr))
  )

target_players <- tibble::tribble(
  ~target_label, ~player_id,
  "Luka", "1629029",
  "LeBron", "2544",
  "Austin", "1630559",
  "Ayton", "1629028",
  "Jaxson", "1629637"
) %>%
  dplyr::left_join(
    player_master %>%
      dplyr::select("player_id", "player_name", "player_nickname", "team_abbreviation"),
    by = "player_id"
  )

pbp <- safe_read_parquet(pbp_path) %>%
  add_missing_cols(c("game_id"), NA_character_) %>%
  dplyr::mutate(game_id = as.character(.data$game_id))

source_audit_rows <- list(
  audit_source(pbp, "play_by_play", pbp_path, "pbp")
)

optional_table_paths <- c(
  shot_context_path,
  tracking_creation_audit_path
)

for (path in optional_table_paths[file.exists(optional_table_paths)]) {
  source_audit_rows[[length(source_audit_rows) + 1]] <- audit_source(
    safe_read_parquet(path),
    tools::file_path_sans_ext(basename(path)),
    path,
    "processed_output"
  )
}

raw_tracking_paths <- c(
  if (dir.exists(tracking_shots_dir)) list.files(tracking_shots_dir, pattern = "\\.parquet$", full.names = TRUE) else character(),
  if (dir.exists(tracking_creation_dir)) list.files(tracking_creation_dir, pattern = "\\.parquet$", full.names = TRUE) else character()
)

for (path in raw_tracking_paths) {
  source_audit_rows[[length(source_audit_rows) + 1]] <- audit_source(
    safe_read_parquet(path),
    tools::file_path_sans_ext(basename(path)),
    path,
    "raw_tracking"
  )
}

defensive_signal_audit <- dplyr::bind_rows(source_audit_rows) %>%
  dplyr::arrange(.data$signal, dplyr::desc(.data$confidence), dplyr::desc(.data$evidence_count), .data$source_name)

write_project_parquet(defensive_signal_audit, defensive_signal_audit_path)

pbp_player_id_cols <- intersect(
  c("player_id", "person_id", "athlete_id", "player1_id", "player2_id", "player3_id", "person1_id", "person2_id", "person3_id"),
  names(pbp)
)

pbp_text_cols <- text_columns(pbp)

target_pbp_rows <- if (length(pbp_player_id_cols) > 0) {
  pbp %>%
    dplyr::select(tidyselect::all_of(c(pbp_player_id_cols, pbp_text_cols))) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(pbp_player_id_cols), as.character)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      matched_player_id = {
        ids <- dplyr::c_across(tidyselect::all_of(pbp_player_id_cols))
        hit <- ids[ids %in% target_players$player_id]
        if (length(hit) == 0) NA_character_ else hit[[1]]
      },
      text_blob = paste(dplyr::c_across(tidyselect::all_of(pbp_text_cols)), collapse = " ")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$matched_player_id)) %>%
    dplyr::left_join(
      target_players %>% dplyr::select(player_id, target_label, player_name),
      by = c("matched_player_id" = "player_id")
    )
} else {
  tibble::tibble(
    matched_player_id = character(),
    target_label = character(),
    player_name = character(),
    text_blob = character()
  )
}

target_defensive_examples <- if (nrow(target_pbp_rows) > 0) {
  signal_specs %>%
    dplyr::select("signal", "keyword_regex") %>%
    tidyr::crossing(target_players %>% dplyr::select("target_label", "player_id", "player_name")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      pbp_text_matches = sum(
        stringr::str_detect(
          stringr::str_to_lower(target_pbp_rows$text_blob[target_pbp_rows$matched_player_id == .data$player_id]),
          .data$keyword_regex
        ),
        na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup()
} else {
  tibble::tibble(
    signal = character(),
    keyword_regex = character(),
    target_label = character(),
    player_id = character(),
    player_name = character(),
    pbp_text_matches = integer()
  )
}

message("Phase 21 defensive signal audit diagnostics:")

message("Available columns by primary source:")
print(
  defensive_signal_audit %>%
    dplyr::group_by(.data$source_name, .data$source_type) %>%
    dplyr::summarise(
      row_count = dplyr::first(.data$row_count),
      column_count = dplyr::n_distinct(unlist(strsplit(dplyr::first(.data$all_columns), ", ", fixed = TRUE))),
      all_columns = dplyr::first(.data$all_columns),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$source_type, .data$source_name)
)

message("Defensive signal availability summary:")
print(
  defensive_signal_audit %>%
    dplyr::group_by(.data$signal) %>%
    dplyr::summarise(
      sources_checked = dplyr::n(),
      sources_with_column_evidence = sum(.data$column_evidence_count > 0, na.rm = TRUE),
      sources_with_text_evidence = sum(.data$text_evidence_count > 0, na.rm = TRUE),
      total_text_evidence = sum(.data$text_evidence_count, na.rm = TRUE),
      best_confidence = dplyr::case_when(
        any(.data$confidence == "high") ~ "high",
        any(.data$confidence == "medium") ~ "medium",
        TRUE ~ "unavailable"
      ),
      best_sources = paste(.data$source_name[.data$confidence != "unavailable"], collapse = " | "),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$signal)
)

message("Missingness for detected defensive columns:")
print(
  defensive_signal_audit %>%
    dplyr::filter(!is.na(.data$available_columns), .data$available_columns != "") %>%
    dplyr::select(
      "signal",
      "source_name",
      "source_type",
      "available_columns",
      "percent_missing",
      "confidence"
    ) %>%
    dplyr::arrange(.data$signal, .data$source_name)
)

message("Luka / LeBron / Austin / Ayton / Jaxson PBP defensive-text examples:")
print(target_defensive_examples)

message("Saved defensive signal audit to: ", defensive_signal_audit_path)
message("Phase 21 note: audit only. Do not interpret this as DEF. No previous phases were modified.")
