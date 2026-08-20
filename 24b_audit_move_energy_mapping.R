# ============================================================
# 24b_audit_move_energy_mapping.R
# Phase 24b: Audit move-level energy assumptions.
#
# Goal:
# Separate player energy capacity / workload from move energy cost.
#
# This phase reviews move categories in player_attack_library and assigns
# theoretical move burden tags. It does not calculate final energy and does not
# modify previous phases.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

energy_output_dir <- "outputs/energy"
move_energy_mapping_audit_path <- file.path(energy_output_dir, "move_energy_mapping_audit.parquet")
player_attack_library_path <- "outputs/attacks/player_attack_library.parquet"

fs::dir_create(energy_output_dir)

if (!file.exists(player_attack_library_path)) {
  stop(
    "Missing Phase 24b input: ",
    player_attack_library_path,
    ". Run 11_build_player_attacks.R first.",
    call. = FALSE
  )
}

tag_burden <- function(move_name, patterns) {
  text <- stringr::str_to_lower(dplyr::coalesce(move_name, ""))
  purrr::map_lgl(patterns, ~ stringr::str_detect(text, .x)) %>%
    any()
}

burden_note <- function(move_name, movement_burden, creation_burden, contact_burden, explosive_burden) {
  active <- c(
    if (isTRUE(movement_burden)) "movement",
    if (isTRUE(creation_burden)) "creation",
    if (isTRUE(contact_burden)) "contact",
    if (isTRUE(explosive_burden)) "explosive"
  )

  if (length(active) == 0) {
    return("No theoretical high-burden tag assigned from current move taxonomy. Treat as low/unknown move-cost until richer movement tracking exists.")
  }

  paste(
    "Theoretical move-cost tags:",
    paste(active, collapse = ", "),
    ". This maps move cost only; player workload/capacity should remain separate."
  )
}

attack_moves <- read_project_parquet(player_attack_library_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  dplyr::mutate(
    attack_variant = stringr::str_to_lower(as.character(.data$attack_variant)),
    attack_family = as.character(.data$attack_family),
    move_name = .data$attack_variant
  ) %>%
  dplyr::group_by(.data$move_name, .data$attack_family) %>%
  dplyr::summarise(
    players_with_move = dplyr::n_distinct(.data$player_id),
    observed_attempts = sum(suppressWarnings(as.numeric(.data$attempts)), na.rm = TRUE),
    .groups = "drop"
  )

validate_columns(attack_moves, c("move_name", "attack_family"))

future_reference_moves <- tibble::tribble(
  ~move_name, ~attack_family, ~players_with_move, ~observed_attempts,
  "movement three", "Future / Off-Ball", 0L, 0,
  "relocation", "Future / Off-Ball", 0L, 0,
  "off-screen shot", "Future / Off-Ball", 0L, 0,
  "power drive", "Future / Rim Pressure", 0L, 0,
  "post-up", "Future / Interior", 0L, 0,
  "block", "Future / Defense", 0L, 0
)

move_catalog <- attack_moves %>%
  dplyr::bind_rows(future_reference_moves) %>%
  dplyr::distinct(.data$move_name, .data$attack_family, .keep_all = TRUE)

movement_patterns <- c(
  "movement three",
  "relocation",
  "off[ -]?screen",
  "cut",
  "catch[ -]?and[ -]?shoot",
  "spot[ -]?up"
)

creation_patterns <- c(
  "stepback",
  "step[ -]?back",
  "pullup",
  "pull[ -]?up",
  "drive",
  "driving",
  "floater",
  "fadeaway"
)

contact_patterns <- c(
  "power drive",
  "post[ -]?up",
  "putback",
  "put back",
  "driving layup",
  "hook",
  "layup"
)

explosive_patterns <- c(
  "dunk",
  "alley[ -]?oop",
  "\\boop\\b",
  "block",
  "putback",
  "tip"
)

move_energy_mapping_audit <- move_catalog %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    movement_burden = tag_burden(.data$move_name, movement_patterns),
    creation_burden = tag_burden(.data$move_name, creation_patterns),
    contact_burden = tag_burden(.data$move_name, contact_patterns),
    explosive_burden = tag_burden(.data$move_name, explosive_patterns),
    notes = burden_note(
      .data$move_name,
      .data$movement_burden,
      .data$creation_burden,
      .data$contact_burden,
      .data$explosive_burden
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    "move_name",
    "movement_burden",
    "creation_burden",
    "contact_burden",
    "explosive_burden",
    "notes",
    tidyselect::any_of(c("attack_family", "players_with_move", "observed_attempts"))
  ) %>%
  dplyr::arrange(.data$move_name)

write_project_parquet(move_energy_mapping_audit, move_energy_mapping_audit_path)

message("Phase 24b move energy mapping diagnostics:")

message("Move burden tag counts:")
print(
  move_energy_mapping_audit %>%
    dplyr::summarise(
      moves = dplyr::n(),
      movement_burden_moves = sum(.data$movement_burden, na.rm = TRUE),
      creation_burden_moves = sum(.data$creation_burden, na.rm = TRUE),
      contact_burden_moves = sum(.data$contact_burden, na.rm = TRUE),
      explosive_burden_moves = sum(.data$explosive_burden, na.rm = TRUE)
    )
)

message("Current observed move mapping:")
print(
  move_energy_mapping_audit %>%
    dplyr::filter(.data$observed_attempts > 0) %>%
    dplyr::select(
      "move_name",
      "attack_family",
      "movement_burden",
      "creation_burden",
      "contact_burden",
      "explosive_burden",
      "players_with_move",
      "observed_attempts",
      "notes"
    )
)

message("Future/reference-only move mapping:")
print(
  move_energy_mapping_audit %>%
    dplyr::filter(.data$observed_attempts == 0) %>%
    dplyr::select(
      "move_name",
      "movement_burden",
      "creation_burden",
      "contact_burden",
      "explosive_burden",
      "notes"
    )
)

message("Saved move energy mapping audit to: ", move_energy_mapping_audit_path)
message("Phase 24b note: audit only. This separates player workload/capacity from theoretical move energy cost; no final energy was calculated.")
