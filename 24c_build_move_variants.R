# ============================================================
# 24c_build_move_variants.R
# Phase 24c: Move variant expansion audit.
#
# Goal:
# Split generic moves into basketball-relevant variants for future card design.
#
# This phase builds a move variant library only. It does not calculate damage,
# energy, ATK, CR, DEF, or any final card score. It does not modify previous
# phases.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_attack_library_path <- "outputs/attacks/player_attack_library.parquet"
move_variant_library_path <- "outputs/attacks/move_variant_library.parquet"

fs::dir_create("outputs/attacks")

if (!file.exists(player_attack_library_path)) {
  stop(
    "Missing Phase 24c input: ",
    player_attack_library_path,
    ". Run 11_build_player_attacks.R first.",
    call. = FALSE
  )
}

normalize_move <- function(x) {
  stringr::str_to_lower(as.character(x))
}

observed_moves <- read_project_parquet(player_attack_library_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  dplyr::mutate(
    attack_variant = normalize_move(.data$attack_variant),
    attack_family = as.character(.data$attack_family)
  ) %>%
  dplyr::group_by(base_move = .data$attack_variant, .data$attack_family) %>%
  dplyr::summarise(
    players_with_move = dplyr::n_distinct(.data$player_id),
    observed_attempts = sum(suppressWarnings(as.numeric(.data$attempts)), na.rm = TRUE),
    .groups = "drop"
  )

validate_columns(observed_moves, c("base_move", "players_with_move", "observed_attempts"))

variant_seed <- tibble::tribble(
  ~base_move, ~move_variant, ~variant_source, ~notes,
  "dunk", "transition dunk", "future_taxonomy", "Needs transition possession tagging before it can be observed reliably.",
  "dunk", "power drive dunk", "future_taxonomy", "Requires drive/contact context; current PBP can observe dunk but not power-drive creation.",
  "dunk", "alley-oop dunk", "observed_taxonomy", "Current attack taxonomy observes alley-oop separately; this variant links alley-oop finishing back to dunk family.",
  "dunk", "putback dunk", "observed_taxonomy", "Current attack taxonomy observes putback separately; future versions can split putback dunks from putback tips/layups.",
  "dunk", "self-created dunk", "future_taxonomy", "Requires reliable self-created/assisted attribution; currently blocked as scaffolded.",
  "jump shot 3", "catch-and-shoot three", "observed_taxonomy", "Current taxonomy can observe catch-and-shoot 3 when PBP text supports it.",
  "jump shot 3", "pull-up three", "observed_taxonomy", "Current taxonomy observes pullup jumper but not always three-point pullups separately.",
  "jump shot 3", "movement three", "future_taxonomy", "Requires off-ball movement, relocation, or tracking context beyond current PBP labels.",
  "jump shot 3", "stepback three", "observed_taxonomy", "Current taxonomy observes stepback jumper but not always three-point stepbacks separately.",
  "layup", "driving layup", "observed_taxonomy", "Current taxonomy observes driving layup directly when sub_type/text supports it.",
  "layup", "transition layup", "future_taxonomy", "Needs transition possession tagging before it can be observed reliably.",
  "layup", "contested layup", "future_taxonomy", "Requires closest-defender or contest context joined to rim attempts.",
  "layup", "self-created layup", "future_taxonomy", "Requires reliable self-created/assisted attribution; currently blocked as scaffolded.",
  "putback", "putback dunk", "future_taxonomy", "Requires finer text or tracking context to distinguish putback dunk from tip/layup.",
  "putback", "putback layup", "future_taxonomy", "Requires finer text or tracking context to distinguish putback layup from dunk/tip.",
  "putback", "putback tip", "observed_taxonomy", "Current taxonomy observes tip separately; this variant links tip finishing back to putback family.",
  "pullup jumper", "pull-up three", "future_taxonomy", "Needs shot-value split on pullup jumper events.",
  "pullup jumper", "midrange pull-up", "future_taxonomy", "Needs shot-distance split on pullup jumper events.",
  "stepback jumper", "stepback three", "future_taxonomy", "Needs shot-value split on stepback jumper events.",
  "stepback jumper", "midrange stepback", "future_taxonomy", "Needs shot-distance split on stepback jumper events.",
  "cut", "basket cut finish", "observed_taxonomy", "Current taxonomy observes cut when PBP text exposes it.",
  "cut", "off-screen cut", "future_taxonomy", "Requires action/play-type or tracking context for screen involvement.",
  "floater", "driving floater", "future_taxonomy", "Requires drive context plus floater label.",
  "floater", "paint floater", "future_taxonomy", "Requires shot location/distance context.",
  "hook", "post hook", "future_taxonomy", "Requires post touch/play-type context.",
  "fadeaway jumper", "post fadeaway", "future_taxonomy", "Requires post touch/play-type context.",
  "fadeaway jumper", "midrange fadeaway", "future_taxonomy", "Requires shot-distance context."
)

observed_variant_lookup <- observed_moves %>%
  dplyr::transmute(
    observed_move_name = .data$base_move,
    observed_players_with_move = .data$players_with_move,
    observed_attempts_lookup = .data$observed_attempts
  )

move_variant_library <- variant_seed %>%
  dplyr::mutate(
    base_move = normalize_move(.data$base_move),
    move_variant = normalize_move(.data$move_variant)
  ) %>%
  dplyr::left_join(
    observed_moves %>%
      dplyr::select(
        "base_move",
        "attack_family",
        "players_with_move",
        "observed_attempts"
      ),
    by = "base_move"
  ) %>%
  dplyr::left_join(
    observed_variant_lookup,
    by = c("move_variant" = "observed_move_name")
  ) %>%
  dplyr::mutate(
    observed_variant = dplyr::coalesce(.data$observed_attempts_lookup, 0) > 0 |
      .data$variant_source == "observed_taxonomy" & dplyr::coalesce(.data$observed_attempts, 0) > 0,
    future_variant = !.data$observed_variant,
    notes = dplyr::case_when(
      .data$observed_variant ~ paste(.data$notes, "Observed or partially supported by current move taxonomy."),
      TRUE ~ paste(.data$notes, "Future variant; do not score until supporting source data exists.")
    )
  ) %>%
  dplyr::select(
    "base_move",
    "move_variant",
    "variant_source",
    "observed_variant",
    "future_variant",
    "notes",
    tidyselect::any_of(c(
      "attack_family",
      "players_with_move",
      "observed_attempts",
      "observed_players_with_move",
      "observed_attempts_lookup"
    ))
  ) %>%
  dplyr::arrange(.data$base_move, .data$move_variant)

write_project_parquet(move_variant_library, move_variant_library_path)

message("Phase 24c move variant expansion diagnostics:")

message("Variant status summary:")
print(
  move_variant_library %>%
    dplyr::summarise(
      variants = dplyr::n(),
      observed_variants = sum(.data$observed_variant, na.rm = TRUE),
      future_variants = sum(.data$future_variant, na.rm = TRUE)
    )
)

message("Move variant library:")
print(
  move_variant_library %>%
    dplyr::select(
      "base_move",
      "move_variant",
      "variant_source",
      "observed_variant",
      "future_variant",
      "notes"
    )
)

message("Observed current attack moves not yet expanded:")
print(
  observed_moves %>%
    dplyr::anti_join(move_variant_library %>% dplyr::distinct(.data$base_move), by = "base_move") %>%
    dplyr::arrange(.data$base_move)
)

message("Saved move variant library to: ", move_variant_library_path)
message("Phase 24c note: variant audit only. No damage or energy was calculated and no previous phases were modified.")
