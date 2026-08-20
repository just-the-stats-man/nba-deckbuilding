# ============================================================
# 18_audit_creation_signals.R
# Phase 18: Creation Signal Audit.
#
# This phase audits whether current assisted/self-created fields in the player
# attack library carry real information or are mostly placeholder scaffolding.
# It does not modify attack libraries, movesets, attack identity, ATK, CR, or
# any previous phase outputs.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

player_attack_library_path <- "outputs/attacks/player_attack_library.parquet"

if (!file.exists(player_attack_library_path)) {
  stop("Missing attack library input: ", player_attack_library_path, ". Run 11_build_player_attacks.R first.", call. = FALSE)
}

safe_divide <- function(num, den) {
  dplyr::if_else(!is.na(den) & den > 0, num / den, NA_real_)
}

weighted_mean_or_na <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0

  if (sum(valid) == 0) {
    return(NA_real_)
  }

  stats::weighted.mean(x[valid], w[valid])
}

add_missing_cols <- function(df, cols, value = NA) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- value
  }

  df
}

numeric_summary <- function(df, column_name) {
  x <- df[[column_name]]

  tibble::tibble(
    variable = column_name,
    rows = length(x),
    non_na = sum(!is.na(x)),
    percent_na = 100 * mean(is.na(x)),
    unique_values = dplyr::n_distinct(x, na.rm = TRUE),
    min = suppressWarnings(min(x, na.rm = TRUE)),
    p25 = suppressWarnings(stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE)),
    median = stats::median(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    p75 = suppressWarnings(stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE)),
    max = suppressWarnings(max(x, na.rm = TRUE))
  ) %>%
    dplyr::mutate(
      dplyr::across(
        c("min", "p25", "median", "mean", "p75", "max"),
        ~ dplyr::if_else(is.infinite(.x), NA_real_, .x)
      )
    )
}

attack_library <- read_project_parquet(player_attack_library_path) %>%
  janitor::clean_names() %>%
  convert_numeric_cols() %>%
  add_missing_cols(c(
    "player_id",
    "player_name",
    "player_nickname",
    "team_abbreviation",
    "attack_family",
    "attack_variant",
    "attempts",
    "assisted_attempts",
    "self_created_attempts",
    "assisted_attempt_rate",
    "self_created_attempt_rate",
    "assisted_tendency_context"
  ), NA_real_) %>%
  dplyr::mutate(
    player_id = as.character(.data$player_id),
    attempts = suppressWarnings(as.numeric(.data$attempts)),
    assisted_attempts = suppressWarnings(as.numeric(.data$assisted_attempts)),
    self_created_attempts = suppressWarnings(as.numeric(.data$self_created_attempts)),
    assisted_attempt_rate = suppressWarnings(as.numeric(.data$assisted_attempt_rate)),
    self_created_attempt_rate = suppressWarnings(as.numeric(.data$self_created_attempt_rate)),
    assisted_tendency_context = suppressWarnings(as.numeric(.data$assisted_tendency_context))
  )

validate_columns(
  attack_library,
  c(
    "player_id",
    "player_name",
    "attack_variant",
    "attempts",
    "assisted_attempt_rate",
    "self_created_attempt_rate",
    "assisted_tendency_context"
  )
)

row_distribution_summary <- purrr::map_dfr(
  c("assisted_attempt_rate", "self_created_attempt_rate", "assisted_tendency_context"),
  ~ numeric_summary(attack_library, .x)
)

unique_value_counts <- attack_library %>%
  dplyr::select(
    "assisted_attempt_rate",
    "self_created_attempt_rate",
    "assisted_tendency_context"
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  dplyr::count(.data$variable, .data$value, sort = TRUE)

player_creation_summary <- attack_library %>%
  dplyr::group_by(.data$player_id) %>%
  dplyr::summarise(
    player_name = dplyr::first(stats::na.omit(.data$player_name), default = NA_character_),
    player_nickname = dplyr::first(stats::na.omit(.data$player_nickname), default = NA_character_),
    team_abbreviation = dplyr::first(stats::na.omit(.data$team_abbreviation), default = NA_character_),
    attack_rows = dplyr::n(),
    total_attempts = sum(.data$attempts, na.rm = TRUE),
    assisted_attempts = sum(.data$assisted_attempts, na.rm = TRUE),
    self_created_attempts = sum(.data$self_created_attempts, na.rm = TRUE),
    weighted_assisted_attempt_rate = weighted_mean_or_na(.data$assisted_attempt_rate, .data$attempts),
    weighted_self_created_attempt_rate = weighted_mean_or_na(.data$self_created_attempt_rate, .data$attempts),
    weighted_assisted_tendency_context = weighted_mean_or_na(.data$assisted_tendency_context, .data$attempts),
    direct_assisted_attempt_rate = safe_divide(.data$assisted_attempts, .data$total_attempts),
    direct_self_created_attempt_rate = safe_divide(.data$self_created_attempts, .data$total_attempts),
    .groups = "drop"
  )

player_distribution_summary <- purrr::map_dfr(
  c(
    "weighted_assisted_attempt_rate",
    "weighted_self_created_attempt_rate",
    "weighted_assisted_tendency_context",
    "direct_assisted_attempt_rate",
    "direct_self_created_attempt_rate"
  ),
  ~ numeric_summary(player_creation_summary, .x)
)

player_unique_value_counts <- player_creation_summary %>%
  dplyr::select(
    "weighted_assisted_attempt_rate",
    "weighted_self_created_attempt_rate",
    "weighted_assisted_tendency_context",
    "direct_assisted_attempt_rate",
    "direct_self_created_attempt_rate"
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  dplyr::count(.data$variable, .data$value, sort = TRUE)

player_placeholder_check <- player_creation_summary %>%
  dplyr::filter(!is.na(.data$weighted_self_created_attempt_rate) | !is.na(.data$weighted_assisted_attempt_rate)) %>%
  dplyr::summarise(
    players_checked = dplyr::n(),
    players_self_created_equals_one = sum(dplyr::coalesce(.data$weighted_self_created_attempt_rate == 1, FALSE)),
    players_assisted_equals_zero = sum(dplyr::coalesce(.data$weighted_assisted_attempt_rate == 0, FALSE)),
    percent_self_created_equals_one = 100 * safe_divide(.data$players_self_created_equals_one, .data$players_checked),
    percent_assisted_equals_zero = 100 * safe_divide(.data$players_assisted_equals_zero, .data$players_checked),
    placeholder_scaffolding_flag = .data$percent_self_created_equals_one > 95 |
      .data$percent_assisted_equals_zero > 95
  )

target_player_pattern <- "Luka|Doncic|Dončić|LeBron|Austin Reaves|Deandre Ayton|Deandre|Ayton|Jaxson Hayes|Luke Kennard|Stephen Curry|Shai|Gilgeous"

target_player_rows <- player_creation_summary %>%
  dplyr::filter(stringr::str_detect(.data$player_name, target_player_pattern)) %>%
  dplyr::arrange(.data$player_name)

target_attack_rows <- attack_library %>%
  dplyr::filter(stringr::str_detect(.data$player_name, target_player_pattern)) %>%
  dplyr::select(
    "player_name",
    "team_abbreviation",
    "attack_family",
    "attack_variant",
    "attempts",
    "assisted_attempts",
    "self_created_attempts",
    "assisted_attempt_rate",
    "self_created_attempt_rate",
    "assisted_tendency_context"
  ) %>%
  dplyr::arrange(.data$player_name, dplyr::desc(.data$attempts), .data$attack_variant)

message("Phase 18 creation signal audit diagnostics:")

message("Row-level distribution summaries:")
print(row_distribution_summary)

message("Player-level distribution summaries:")
print(player_distribution_summary)

message("Row-level unique value counts:")
print(unique_value_counts)

message("Player-level unique value counts:")
print(player_unique_value_counts)

message("Top players by weighted_self_created_attempt_rate:")
print(
  player_creation_summary %>%
    dplyr::arrange(dplyr::desc(.data$weighted_self_created_attempt_rate), dplyr::desc(.data$total_attempts)) %>%
    utils::head(25)
)

message("Bottom players by weighted_self_created_attempt_rate:")
print(
  player_creation_summary %>%
    dplyr::arrange(.data$weighted_self_created_attempt_rate, dplyr::desc(.data$total_attempts)) %>%
    utils::head(25)
)

message("Top players by weighted_assisted_attempt_rate:")
print(
  player_creation_summary %>%
    dplyr::arrange(dplyr::desc(.data$weighted_assisted_attempt_rate), dplyr::desc(.data$total_attempts)) %>%
    utils::head(25)
)

message("Bottom players by weighted_assisted_attempt_rate:")
print(
  player_creation_summary %>%
    dplyr::arrange(.data$weighted_assisted_attempt_rate, dplyr::desc(.data$total_attempts)) %>%
    utils::head(25)
)

message("Requested player-level inspection:")
print(target_player_rows)

message("Requested player attack-level inspection:")
print(target_attack_rows)

message("Placeholder scaffolding check:")
print(player_placeholder_check)

if (nrow(player_placeholder_check) > 0 && isTRUE(player_placeholder_check$placeholder_scaffolding_flag[[1]])) {
  message("Creation variables appear to be placeholder scaffolding and should not be used for ATK or CR.")
}

message("Phase 18 note: audit only. No previous phases or outputs were modified.")
