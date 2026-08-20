# ============================================================
# analysis/02_audit_attack_source_fields.R
# Diagnostic audit for observable attack taxonomy fields.
#
# Purpose:
# Before expanding attack labels, inspect which distinctions are actually
# visible in the available hoopR play-by-play fields. This script does not
# change the attack framework or write any model scores. It only prints source
# field evidence and saves a lightweight taxonomy audit table.
# ============================================================

source("01_setup_project.R")

pbp_path <- "data/raw/pbp/LAL_pbp_sample_2025-26.parquet"
player_attack_library_path <- "outputs/attacks/player_attack_library.parquet"
audit_output_path <- "outputs/attacks/attack_taxonomy_audit.parquet"

if (!file.exists(pbp_path)) {
  stop("Missing PBP input: ", pbp_path, call. = FALSE)
}

pbp <- read_project_parquet(pbp_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols()

player_attack_library <- if (file.exists(player_attack_library_path)) {
  read_project_parquet(player_attack_library_path) %>%
    janitor::clean_names() %>%
    convert_numeric_cols()
} else {
  NULL
}

message("PBP column names:")
print(names(pbp))

candidate_column_patterns <- c(
  "action_type",
  "sub_type",
  "shot_type",
  "shot_zone_basic",
  "shot_zone_area",
  "shot_zone_range",
  "shot_distance",
  "loc_x",
  "loc_y",
  "description",
  "event_msg_type",
  "eventmsgtype"
)

candidate_columns <- names(pbp)[purrr::map_lgl(
  names(pbp),
  ~ any(stringr::str_detect(.x, candidate_column_patterns))
)]

candidate_columns <- unique(c(
  intersect(
    c(
      "action_type",
      "sub_type",
      "shot_type",
      "shot_zone_basic",
      "shot_zone_area",
      "shot_zone_range",
      "shot_distance",
      "shot_distance_ft",
      "distance",
      "loc_x",
      "loc_y",
      "x",
      "y",
      "description",
      "play_description",
      "home_description",
      "visitor_description",
      "homedescription",
      "neutral_description",
      "event_type",
      "event_msg_type",
      "eventmsgtype",
      "action_number"
    ),
    names(pbp)
  ),
  candidate_columns
))

message("Candidate columns that may support attack classification:")
print(candidate_columns)

count_distinct_values <- function(df, col, max_values = 40) {
  df %>%
    dplyr::count(value = as.character(.data[[col]]), sort = TRUE, name = "n") %>%
    dplyr::mutate(data_source_column = col) %>%
    dplyr::select("data_source_column", "value", "n") %>%
    dplyr::slice_head(n = max_values)
}

shot_action_columns <- candidate_columns[stringr::str_detect(
  candidate_columns,
  "action|sub_type|shot|zone|distance|loc_|^x$|^y$|description|event"
)]

message("Distinct values and counts for candidate shot/action columns:")
if (length(shot_action_columns) == 0) {
  message("No candidate shot/action columns found.")
} else {
  purrr::walk(
    shot_action_columns,
    function(col) {
      message("\nColumn: ", col)
      print(count_distinct_values(pbp, col))
    }
  )
}

text_action_columns <- names(pbp)[purrr::map_lgl(
  pbp,
  ~ is.character(.x) || is.factor(.x)
)]

text_action_columns <- text_action_columns[stringr::str_detect(
  text_action_columns,
  "description|action|type|sub_type|qualifier|shot|event"
)]

message("Text/action columns searched for attack keywords:")
print(text_action_columns)

keyword_map <- tibble::tribble(
  ~attack_label_candidate, ~keyword_regex,
  "dunk", "dunk",
  "alley oop", "alley[ -]?oop|lob",
  "layup", "layup",
  "hook", "hook",
  "fadeaway", "fadeaway|fade away",
  "step back", "step[ -]?back|stepback",
  "pullup", "pull[ -]?up|pullup",
  "floating", "floating|floater|runner",
  "driving", "driving|drive",
  "tip", "\\btip|tip-in|tip in",
  "putback", "putback|put back",
  "isolation", "isolation|\\biso\\b",
  "handoff", "handoff|hand[ -]?off|\\bdho\\b",
  "screen", "screen|off[ -]?screen",
  "cut", "\\bcut\\b|cutting",
  "transition", "transition|fast break|\\bfb\\b"
)

count_keyword_hits <- function(df, col, regex) {
  x <- stringr::str_to_lower(as.character(df[[col]]))
  sum(stringr::str_detect(x, regex), na.rm = TRUE)
}

keyword_hits <- tidyr::crossing(
  keyword_map,
  data_source_column = text_action_columns
) %>%
  dplyr::mutate(
    evidence_count = purrr::map2_int(
      .data$data_source_column,
      .data$keyword_regex,
      ~ count_keyword_hits(pbp, .x, .y)
    )
  ) %>%
  dplyr::filter(.data$evidence_count > 0)

message("Keyword hit counts by text/action column:")
if (nrow(keyword_hits) == 0) {
  message("No keyword evidence found in searched text/action columns.")
} else {
  print(keyword_hits %>% dplyr::arrange(dplyr::desc(.data$evidence_count), .data$attack_label_candidate))
}

column_availability <- tibble::tibble(data_source_column = candidate_columns) %>%
  dplyr::mutate(
    non_empty_rows = purrr::map_int(
      .data$data_source_column,
      ~ sum(!is.na(pbp[[.x]]) & as.character(pbp[[.x]]) != "", na.rm = TRUE)
    )
  ) %>%
  dplyr::arrange(dplyr::desc(.data$non_empty_rows), .data$data_source_column)

message("Column availability summary (not written to attack_taxonomy_audit.parquet):")
print(column_availability)

source_column_quality <- function(col) {
  dplyr::case_when(
    col == "sub_type" ~ "structured_attack_detail",
    col %in% c("shot_type", "shot_zone_basic", "shot_zone_area", "shot_zone_range") ~ "structured_shot_detail",
    stringr::str_detect(col, "description") ~ "free_text_description",
    col == "action_type" ~ "generic_event_type",
    TRUE ~ "other_text"
  )
}

confidence_from_evidence <- function(evidence_count, data_source_column) {
  quality <- source_column_quality(data_source_column)

  dplyr::case_when(
    evidence_count <= 0 ~ "unavailable",
    quality %in% c("structured_attack_detail", "structured_shot_detail") & evidence_count >= 10 ~ "high",
    quality %in% c("structured_attack_detail", "structured_shot_detail") ~ "medium",
    quality == "free_text_description" & evidence_count >= 25 ~ "medium",
    quality == "free_text_description" ~ "low",
    quality == "generic_event_type" ~ "low",
    evidence_count >= 25 ~ "medium",
    TRUE ~ "low"
  )
}

taxonomy_audit <- tidyr::crossing(
  keyword_map,
  data_source_column = text_action_columns
) %>%
  dplyr::mutate(
    evidence_count = purrr::map2_int(
      .data$data_source_column,
      .data$keyword_regex,
      ~ count_keyword_hits(pbp, .x, .y)
    ),
    confidence = confidence_from_evidence(.data$evidence_count, .data$data_source_column)
  ) %>%
  dplyr::filter(.data$evidence_count > 0) %>%
  dplyr::select(
    "attack_label_candidate",
    "keyword_regex",
    "data_source_column",
    "evidence_count",
    "confidence"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$evidence_count), .data$attack_label_candidate, .data$data_source_column)

message("Attack taxonomy candidate evidence table:")
print(taxonomy_audit)

if (!is.null(player_attack_library)) {
  message("Existing player_attack_library attack labels, if useful for comparison:")
  if ("attack_type" %in% names(player_attack_library)) {
    print(
      player_attack_library %>%
        dplyr::count(.data$attack_type, sort = TRUE, name = "player_attack_rows")
    )
  } else {
    message("player_attack_library exists, but no attack_type column was found.")
  }
} else {
  message("Optional player_attack_library not found at: ", player_attack_library_path)
}

write_project_parquet(taxonomy_audit, audit_output_path)

message("Wrote attack taxonomy audit: ", audit_output_path)
