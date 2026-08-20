# ============================================================
# 02_player_season.R
# Pull player-season data and create a first player master table.
# ============================================================

source("01_setup_project.R")

pull_player_stats <- function(season = PROJECT_SEASON) {
  measures <- c("Base", "Advanced", "Scoring", "Misc", "Usage")

  stats <- purrr::map(
    measures,
    ~ hoopR::nba_leaguedashplayerstats(
      season = season,
      per_mode_detailed = "Per100Possessions",
      measure_type_detailed_defense = .x
    ) %>%
      extract_all_hoopr_tables() %>%
      janitor::clean_names()
  )

  names(stats) <- tolower(measures)
  stats
}

player_stats_raw <- pull_player_stats(season)

purrr::iwalk(
  player_stats_raw,
  ~ write_project_parquet(.x, glue("data/raw/player/player_{.y}_{season}.parquet"))
)

join_keys <- c("player_id", "player_name", "team_id", "team_abbreviation")

player_master <- player_stats_raw$base %>%
  dplyr::left_join(player_stats_raw$advanced, by = join_keys, suffix = c("", "_advanced")) %>%
  dplyr::left_join(player_stats_raw$scoring,  by = join_keys, suffix = c("", "_scoring")) %>%
  dplyr::left_join(player_stats_raw$misc,     by = join_keys, suffix = c("", "_misc")) %>%
  dplyr::left_join(player_stats_raw$usage,    by = join_keys, suffix = c("", "_usage")) %>%
  dplyr::mutate(season = season) %>%
  convert_numeric_cols()

required_for_mvp <- c("pts", "ast", "reb", "tov", "fga", "fta", "min")
validate_columns(player_master, required_for_mvp)

player_master <- player_master %>%
  dplyr::mutate(
    ts_pct = dplyr::if_else(
      (.data$fga + 0.44 * .data$fta) > 0,
      .data$pts / (2 * (.data$fga + 0.44 * .data$fta)),
      NA_real_
    )
  )

mvp_beta <- player_master %>%
  dplyr::filter(.data$min >= min_minutes) %>%
  dplyr::mutate(
    pts_z = z_score(.data$pts),
    ast_z = z_score(.data$ast),
    reb_z = z_score(.data$reb),
    ts_pct_z = z_score(.data$ts_pct),
    tov_rate = .data$tov / .data$min,
    tov_rate_z = z_score(.data$tov_rate),
    simple_mvp_score =
      0.30 * .data$pts_z +
      0.25 * .data$ast_z +
      0.15 * .data$reb_z +
      0.25 * .data$ts_pct_z -
      0.05 * .data$tov_rate_z
  ) %>%
  dplyr::arrange(dplyr::desc(.data$simple_mvp_score))

write_project_parquet(player_master, glue("data/processed/player/player_master_{season}.parquet"))
write_project_parquet(mvp_beta, glue("data/processed/player/mvp_beta_{season}.parquet"))

message("Top MVP beta rows:")
print(
  mvp_beta %>%
    dplyr::select(player_name, team_abbreviation, min, pts, ast, reb, ts_pct, simple_mvp_score) %>%
    head(20)
)
