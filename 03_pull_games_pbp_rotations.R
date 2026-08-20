# ============================================================
# 03_pull_games_pbp_rotations.R
# Pull team games, play-by-play, and rotations.
#
# By default this processes all available games for the configured team/season.
# Set PROJECT_MAX_GAMES in 00_config.R only for debugging. If
# PROJECT_SAMPLE_GAMES is TRUE, those max games are sampled; otherwise the first
# max games in the returned schedule are used.
# ============================================================

source("01_setup_project.R")

select_requested_games <- function(team_games, max_games, sample_games) {
  requested <- team_games

  if (is.null(max_games)) {
    return(requested)
  }

  if (!is.numeric(max_games) || length(max_games) != 1 || is.na(max_games) || max_games <= 0) {
    stop("PROJECT_MAX_GAMES must be NULL or a positive number.", call. = FALSE)
  }

  max_games <- min(as.integer(max_games), nrow(requested))

  if (isTRUE(sample_games)) {
    requested %>%
      dplyr::slice_sample(n = max_games)
  } else {
    requested %>%
      dplyr::slice_head(n = max_games)
  }
}

pull_game_table <- function(game_ids, pull_fun, label) {
  total_games <- length(game_ids)

  results <- purrr::imap(
    game_ids,
    function(game_id, idx) {
      message("[", idx, "/", total_games, "] Pulling ", label, " for game: ", game_id)

      tryCatch(
        {
          pulled <- pull_fun(game_id)

          list(
            game_id = game_id,
            rows = nrow(pulled),
            success = nrow(pulled) > 0,
            error = NA_character_,
            data = pulled
          )
        },
        error = function(e) {
          message("  Failed ", label, " for game ", game_id, ": ", conditionMessage(e))

          list(
            game_id = game_id,
            rows = 0L,
            success = FALSE,
            error = conditionMessage(e),
            data = tibble::tibble()
          )
        }
      )
    }
  )

  list(
    data = purrr::map_dfr(results, "data"),
    diagnostics = purrr::map_dfr(
      results,
      ~ tibble::tibble(
        game_id = .x$game_id,
        rows = .x$rows,
        success = .x$success,
        error = .x$error
      )
    )
  )
}

games <- hoopR::nba_leaguegamefinder(
  season_nullable = season,
  team_abbreviation_nullable = team_abbr
) %>%
  extract_all_hoopr_tables() %>%
  janitor::clean_names() %>%
  dplyr::mutate(game_id = as.character(.data$game_id))

write_project_parquet(games, glue("data/raw/games/{team_abbr}_games_{season}.parquet"))

validate_columns(games, c("game_id", "team_id", "team_abbreviation", "matchup"))

team_games <- games %>%
  dplyr::filter(.data$team_abbreviation == team_abbr) %>%
  dplyr::distinct(.data$game_id, .data$team_id, .data$team_abbreviation, .data$matchup)

requested_games <- select_requested_games(team_games, max_games, sample_games)
requested_game_ids <- requested_games %>%
  dplyr::pull(.data$game_id)

if (length(requested_game_ids) == 0) {
  stop("No games found. Check season/team_abbr in 00_config.R.", call. = FALSE)
}

message("Game pull configuration:")
message("- Team games available: ", nrow(team_games))
message("- Total games requested: ", length(requested_game_ids))
message("- Max games setting: ", ifelse(is.null(max_games), "NULL (full available schedule)", max_games))
message("- Debug sampling enabled: ", sample_games)

if (length(requested_game_ids) >= 20) {
  message("Large pull safeguard: requesting ", length(requested_game_ids), " games. This can take a while and may hit API limits.")
  message("For debugging, set PROJECT_MAX_GAMES to a small number in 00_config.R.")
}

message("Requested games: ", paste(requested_game_ids, collapse = ", "))

pbp_pull <- pull_game_table(requested_game_ids, pull_pbp_safe, "PBP")
rotation_pull <- pull_game_table(requested_game_ids, pull_rotation_safe, "rotations")

pbp_data <- pbp_pull$data
rotation_data <- rotation_pull$data

write_project_parquet(pbp_data, glue("data/raw/pbp/{team_abbr}_pbp_sample_{season}.parquet"))
write_project_parquet(rotation_data, glue("data/raw/rotations/{team_abbr}_rotation_sample_{season}.parquet"))

pull_diagnostics <- requested_games %>%
  dplyr::select("game_id", "team_id", "team_abbreviation", "matchup") %>%
  dplyr::left_join(
    pbp_pull$diagnostics %>%
      dplyr::rename(
        pbp_rows = "rows",
        pbp_success = "success",
        pbp_error = "error"
      ),
    by = "game_id"
  ) %>%
  dplyr::left_join(
    rotation_pull$diagnostics %>%
      dplyr::rename(
        rotation_rows = "rows",
        rotation_success = "success",
        rotation_error = "error"
      ),
    by = "game_id"
  ) %>%
  dplyr::mutate(
    fully_successful = .data$pbp_success & .data$rotation_success
  )

write_project_parquet(pull_diagnostics, glue("outputs/tables/{team_abbr}_game_pull_diagnostics_{season}.parquet"))

message("Game pull diagnostics:")
print(
  pull_diagnostics %>%
    dplyr::summarise(
      total_games_requested = dplyr::n(),
      pbp_games_successfully_pulled = sum(.data$pbp_success, na.rm = TRUE),
      rotation_games_successfully_pulled = sum(.data$rotation_success, na.rm = TRUE),
      fully_successful_games = sum(.data$fully_successful, na.rm = TRUE),
      games_with_any_error = sum(!.data$fully_successful, na.rm = TRUE)
    )
)

message("Missing games/errors:")
print(
  pull_diagnostics %>%
    dplyr::filter(!.data$fully_successful) %>%
    dplyr::select(
      "game_id",
      "matchup",
      "pbp_success",
      "pbp_error",
      "rotation_success",
      "rotation_error"
    )
)
