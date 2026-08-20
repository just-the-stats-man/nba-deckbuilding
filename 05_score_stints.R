# ============================================================
# 05_score_stints.R
# Attach score changes to stint rows.
# This is a conservative first pass, not final adjusted plus-minus.
# ============================================================

source("01_setup_project.R")

pbp_sample <- read_project_parquet(glue("data/raw/pbp/{team_abbr}_pbp_sample_{season}.parquet"))
stints <- read_project_parquet(glue("data/processed/stints/{team_abbr}_stints_10_players_{season}.parquet"))
games <- read_project_parquet(glue("data/raw/games/{team_abbr}_games_{season}.parquet"))

score_home_col <- safe_first_existing_col(pbp_sample, c("score_home", "home_score", "home_score_total"), required = FALSE)
score_away_col <- safe_first_existing_col(pbp_sample, c("score_away", "away_score", "away_score_total"), required = FALSE)
period_col <- safe_first_existing_col(pbp_sample, c("period", "period_number", "quarter"), required = FALSE)
clock_col <- safe_first_existing_col(pbp_sample, c("clock", "pctimestring", "pc_time_string", "time_remaining"), required = FALSE)
event_col <- safe_first_existing_col(pbp_sample, c("event_num", "event_number", "action_number"), required = FALSE)

if (is.na(score_home_col) || is.na(score_away_col)) {
  stop(
    "Could not find home/away score columns in PBP. Run names(pbp_sample) and ask Codex to map score columns.",
    call. = FALSE
  )
}

sum_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  sum(x, na.rm = TRUE)
}

parse_clock_remaining_seconds <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NULL", "NaN")] <- NA_character_

  out <- suppressWarnings(as.numeric(x_chr))

  iso_hit <- stringr::str_match(x_chr, "^PT(?:(\\d+)M)?(?:(\\d+(?:\\.\\d+)?)S)?$")
  iso_parseable <- !is.na(iso_hit[, 1])
  out[iso_parseable] <- dplyr::coalesce(suppressWarnings(as.numeric(iso_hit[iso_parseable, 2])), 0) * 60 +
    dplyr::coalesce(suppressWarnings(as.numeric(iso_hit[iso_parseable, 3])), 0)

  mmss_hit <- stringr::str_match(x_chr, "^(\\d{1,2}):(\\d{2}(?:\\.\\d+)?)$")
  mmss_parseable <- !is.na(mmss_hit[, 1])
  out[mmss_parseable] <- suppressWarnings(as.numeric(mmss_hit[mmss_parseable, 2])) * 60 +
    suppressWarnings(as.numeric(mmss_hit[mmss_parseable, 3]))

  out
}

period_start_seconds <- function(period) {
  dplyr::if_else(
    period <= 4,
    (period - 1) * 720,
    4 * 720 + (period - 5) * 300
  )
}

period_length_seconds <- function(period) {
  dplyr::if_else(period <= 4, 720, 300)
}

standardize_stint_times <- function(stints) {
  max_time <- suppressWarnings(max(c(stints$start_time, stints$end_time), na.rm = TRUE))
  scale_factor <- dplyr::case_when(
    is.na(max_time) ~ 1,
    max_time > 3600 ~ 10,
    TRUE ~ 1
  )

  stints %>%
    dplyr::mutate(
      stint_time_scale_factor = scale_factor,
      start_time_elapsed_seconds = .data$start_time / scale_factor,
      end_time_elapsed_seconds = .data$end_time / scale_factor
    )
}

build_pbp_scoring <- function(pbp_sample, score_home_col, score_away_col, period_col, clock_col, event_col) {
  out <- pbp_sample %>%
    dplyr::mutate(
      game_id = as.character(.data$game_id)
    ) %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::mutate(
      event_order = dplyr::row_number(),
      pbp_period = if (!is.na(period_col)) suppressWarnings(as.numeric(.data[[period_col]])) else NA_real_,
      pbp_clock_raw = if (!is.na(clock_col)) as.character(.data[[clock_col]]) else NA_character_,
      pbp_clock_remaining_seconds = if (!is.na(clock_col)) parse_clock_remaining_seconds(.data[[clock_col]]) else NA_real_,
      pbp_event_number = if (!is.na(event_col)) suppressWarnings(as.numeric(.data[[event_col]])) else NA_real_,
      pbp_elapsed_seconds = dplyr::if_else(
        !is.na(.data$pbp_period) & !is.na(.data$pbp_clock_remaining_seconds),
        period_start_seconds(.data$pbp_period) +
          period_length_seconds(.data$pbp_period) -
          .data$pbp_clock_remaining_seconds,
        NA_real_
      )
    ) %>%
    dplyr::mutate(
      event_time_proxy = dplyr::if_else(
        !is.na(.data$pbp_elapsed_seconds),
        .data$pbp_elapsed_seconds,
        as.numeric(.data$event_order)
      ),
      pbp_time_source = dplyr::if_else(
        !is.na(.data$pbp_elapsed_seconds),
        "period_clock_elapsed_seconds",
        "event_order_fallback"
      )
    ) %>%
    dplyr::ungroup()

  out %>%
    dplyr::transmute(
      game_id = .data$game_id,
      event_time_proxy = .data$event_time_proxy,
      pbp_time_source = .data$pbp_time_source,
      matched_pbp_event_order = .data$event_order,
      matched_pbp_event_number = .data$pbp_event_number,
      matched_pbp_period = .data$pbp_period,
      matched_pbp_clock = .data$pbp_clock_raw,
      matched_pbp_clock_remaining_seconds = .data$pbp_clock_remaining_seconds,
      score_home = suppressWarnings(as.numeric(.data[[score_home_col]])),
      score_away = suppressWarnings(as.numeric(.data[[score_away_col]]))
    ) %>%
    dplyr::filter(!is.na(.data$score_home), !is.na(.data$score_away)) %>%
    dplyr::arrange(.data$game_id, .data$event_time_proxy, .data$matched_pbp_event_order)
}

print_phase3_time_axis_diagnostics <- function(stints_for_scoring, score_timeline, period_col, clock_col, event_col) {
  message("Phase 3 time-axis diagnostic:")
  print(
    tibble::tibble(
      period_col = dplyr::if_else(is.na(period_col), "missing", period_col),
      clock_col = dplyr::if_else(is.na(clock_col), "missing", clock_col),
      event_col = dplyr::if_else(is.na(event_col), "missing", event_col),
      stint_time_scale_factor = dplyr::first(stints_for_scoring$stint_time_scale_factor),
      stint_start_min = suppressWarnings(min(stints_for_scoring$start_time, na.rm = TRUE)),
      stint_end_max = suppressWarnings(max(stints_for_scoring$end_time, na.rm = TRUE)),
      stint_elapsed_start_min = suppressWarnings(min(stints_for_scoring$start_time_elapsed_seconds, na.rm = TRUE)),
      stint_elapsed_end_max = suppressWarnings(max(stints_for_scoring$end_time_elapsed_seconds, na.rm = TRUE)),
      pbp_time_min = suppressWarnings(min(score_timeline$event_time_proxy, na.rm = TRUE)),
      pbp_time_max = suppressWarnings(max(score_timeline$event_time_proxy, na.rm = TRUE)),
      pbp_time_source = paste(unique(score_timeline$pbp_time_source), collapse = ", "),
      score_match_strategy = "nearest_prior_scoring_snapshot"
    )
  )

  message("PBP clock direction sample by game:")
  print(
    score_timeline %>%
      dplyr::group_by(.data$game_id) %>%
      dplyr::arrange(.data$matched_pbp_event_order, .by_group = TRUE) %>%
      dplyr::summarise(
        first_pbp_time = dplyr::first(.data$event_time_proxy),
        last_pbp_time = dplyr::last(.data$event_time_proxy),
        first_clock = dplyr::first(.data$matched_pbp_clock),
        last_clock = dplyr::last(.data$matched_pbp_clock),
        elapsed_time_descents = sum(diff(.data$event_time_proxy) < 0, na.rm = TRUE),
        .groups = "drop"
      )
  )
}

print_phase3_scoring_diagnostics <- function(stints_scored, score_timeline, score_home_col, score_away_col, period_col, clock_col, event_col) {
  score_example_cols <- c(
    "game_id",
    "stint_id",
    "start_time",
    "end_time",
    "start_time_elapsed_seconds",
    "end_time_elapsed_seconds",
    "start_score_time",
    "end_score_time",
    "start_score_match_lag_seconds",
    "end_score_match_lag_seconds",
    "start_matched_pbp_clock",
    "end_matched_pbp_clock",
    "start_matched_pbp_period",
    "end_matched_pbp_period",
    "start_score_home",
    "start_score_away",
    "end_score_home",
    "end_score_away",
    "home_points_delta",
    "away_points_delta",
    "target_team_is_home",
    "target_margin_change",
    "opponent_margin_change",
    "target_points_for",
    "target_points_against"
  )

  diagnostic_stints <- stints_scored %>%
    dplyr::mutate(
      complete_start_score = !is.na(.data$start_score_home) & !is.na(.data$start_score_away),
      complete_end_score = !is.na(.data$end_score_home) & !is.na(.data$end_score_away),
      home_points_delta = .data$end_score_home - .data$start_score_home,
      away_points_delta = .data$end_score_away - .data$start_score_away,
      score_changed = .data$score_delta_available &
        (.data$home_points_delta != 0 | .data$away_points_delta != 0)
    )

  message("Phase 3 score column mapping diagnostic:")
  print(
    tibble::tibble(
      score_home_col = score_home_col,
      score_away_col = score_away_col,
      period_col = dplyr::if_else(is.na(period_col), "missing", period_col),
      clock_col = dplyr::if_else(is.na(clock_col), "missing", clock_col),
      event_col = dplyr::if_else(is.na(event_col), "missing", event_col),
      score_timeline_rows = nrow(score_timeline),
      score_timeline_games = dplyr::n_distinct(score_timeline$game_id),
      score_home_min = suppressWarnings(min(score_timeline$score_home, na.rm = TRUE)),
      score_home_max = suppressWarnings(max(score_timeline$score_home, na.rm = TRUE)),
      score_away_min = suppressWarnings(min(score_timeline$score_away, na.rm = TRUE)),
      score_away_max = suppressWarnings(max(score_timeline$score_away, na.rm = TRUE))
    )
  )

  message("Phase 3 scored stint availability diagnostic:")
  print(
    diagnostic_stints %>%
      dplyr::summarise(
        stints = dplyr::n(),
        non_missing_start_scores = sum(.data$complete_start_score, na.rm = TRUE),
        non_missing_end_scores = sum(.data$complete_end_score, na.rm = TRUE),
        score_delta_available_stints = sum(.data$score_delta_available, na.rm = TRUE),
        stints_with_score_change = sum(.data$score_changed, na.rm = TRUE),
        stints_without_score_change = sum(.data$score_delta_available & !.data$score_changed, na.rm = TRUE)
      )
  )

  message("Distribution of target_margin_change:")
  print(
    diagnostic_stints %>%
      dplyr::count(.data$target_margin_change, name = "stints") %>%
      dplyr::arrange(.data$target_margin_change)
  )

  message("Target margin summary:")
  print(
    diagnostic_stints %>%
      dplyr::summarise(
        missing_target_margin_change = sum(is.na(.data$target_margin_change)),
        min_target_margin_change = suppressWarnings(min(.data$target_margin_change, na.rm = TRUE)),
        median_target_margin_change = suppressWarnings(stats::median(.data$target_margin_change, na.rm = TRUE)),
        mean_target_margin_change = suppressWarnings(mean(.data$target_margin_change, na.rm = TRUE)),
        max_target_margin_change = suppressWarnings(max(.data$target_margin_change, na.rm = TRUE))
      )
  )

  message("PBP score match lag summary in seconds:")
  print(
    diagnostic_stints %>%
      dplyr::summarise(
        start_lag_min = suppressWarnings(min(.data$start_score_match_lag_seconds, na.rm = TRUE)),
        start_lag_median = suppressWarnings(stats::median(.data$start_score_match_lag_seconds, na.rm = TRUE)),
        start_lag_max = suppressWarnings(max(.data$start_score_match_lag_seconds, na.rm = TRUE)),
        end_lag_min = suppressWarnings(min(.data$end_score_match_lag_seconds, na.rm = TRUE)),
        end_lag_median = suppressWarnings(stats::median(.data$end_score_match_lag_seconds, na.rm = TRUE)),
        end_lag_max = suppressWarnings(max(.data$end_score_match_lag_seconds, na.rm = TRUE))
      )
  )

  message("Examples of stints where score changed:")
  print(
    diagnostic_stints %>%
      dplyr::filter(.data$score_changed) %>%
      dplyr::select(tidyselect::any_of(score_example_cols)) %>%
      dplyr::slice_head(n = 10)
  )

  message("Examples of stints where score did not change:")
  print(
    diagnostic_stints %>%
      dplyr::filter(.data$score_delta_available, !.data$score_changed) %>%
      dplyr::select(tidyselect::any_of(score_example_cols)) %>%
      dplyr::slice_head(n = 10)
  )

  invisible(diagnostic_stints)
}

stints_for_scoring <- standardize_stint_times(stints)

score_timeline <- build_pbp_scoring(
  pbp_sample = pbp_sample,
  score_home_col = score_home_col,
  score_away_col = score_away_col,
  period_col = period_col,
  clock_col = clock_col,
  event_col = event_col
)

print_phase3_time_axis_diagnostics(
  stints_for_scoring = stints_for_scoring,
  score_timeline = score_timeline,
  period_col = period_col,
  clock_col = clock_col,
  event_col = event_col
)

get_score_at_or_before <- function(game_id_i, time_i, score_data) {
  out <- score_data %>%
    dplyr::filter(.data$game_id == game_id_i, .data$event_time_proxy <= time_i) %>%
    dplyr::arrange(dplyr::desc(.data$event_time_proxy), dplyr::desc(.data$matched_pbp_event_order)) %>%
    dplyr::slice_head(n = 1)

  if (nrow(out) == 0) {
    return(tibble::tibble(
      score_time = NA_real_,
      matched_pbp_event_order = NA_real_,
      matched_pbp_event_number = NA_real_,
      matched_pbp_period = NA_real_,
      matched_pbp_clock = NA_character_,
      matched_pbp_clock_remaining_seconds = NA_real_,
      score_home = NA_real_,
      score_away = NA_real_
    ))
  }

  tibble::tibble(
    score_time = out$event_time_proxy,
    matched_pbp_event_order = out$matched_pbp_event_order,
    matched_pbp_event_number = out$matched_pbp_event_number,
    matched_pbp_period = out$matched_pbp_period,
    matched_pbp_clock = out$matched_pbp_clock,
    matched_pbp_clock_remaining_seconds = out$matched_pbp_clock_remaining_seconds,
    score_home = out$score_home,
    score_away = out$score_away
  )
}

start_scores <- purrr::pmap_dfr(
  list(stints_for_scoring$game_id, stints_for_scoring$start_time_elapsed_seconds),
  ~ get_score_at_or_before(..1, ..2, score_timeline)
) %>%
  dplyr::rename(
    start_score_time = "score_time",
    start_matched_pbp_event_order = "matched_pbp_event_order",
    start_matched_pbp_event_number = "matched_pbp_event_number",
    start_matched_pbp_period = "matched_pbp_period",
    start_matched_pbp_clock = "matched_pbp_clock",
    start_matched_pbp_clock_remaining_seconds = "matched_pbp_clock_remaining_seconds",
    start_score_home = "score_home",
    start_score_away = "score_away"
  )

end_scores <- purrr::pmap_dfr(
  list(stints_for_scoring$game_id, stints_for_scoring$end_time_elapsed_seconds),
  ~ get_score_at_or_before(..1, ..2, score_timeline)
) %>%
  dplyr::rename(
    end_score_time = "score_time",
    end_matched_pbp_event_order = "matched_pbp_event_order",
    end_matched_pbp_event_number = "matched_pbp_event_number",
    end_matched_pbp_period = "matched_pbp_period",
    end_matched_pbp_clock = "matched_pbp_clock",
    end_matched_pbp_clock_remaining_seconds = "matched_pbp_clock_remaining_seconds",
    end_score_home = "score_home",
    end_score_away = "score_away"
  )

team_game_context <- games %>%
  dplyr::filter(.data$team_abbreviation == team_abbr) %>%
  dplyr::distinct(.data$game_id, .data$team_id, .data$team_abbreviation, .data$matchup) %>%
  dplyr::mutate(target_team_is_home = stringr::str_detect(.data$matchup, "vs\\.")) %>%
  dplyr::select(
    "game_id",
    target_team_id = "team_id",
    target_team_abbr = "team_abbreviation",
    "matchup",
    "target_team_is_home"
  )

  stints_scored <- dplyr::bind_cols(stints_for_scoring, start_scores, end_scores) %>%
  dplyr::left_join(team_game_context, by = "game_id") %>%
  dplyr::mutate(
    start_score_match_lag_seconds = .data$start_time_elapsed_seconds - .data$start_score_time,
    end_score_match_lag_seconds = .data$end_time_elapsed_seconds - .data$end_score_time,
    score_delta_available = !is.na(.data$start_score_home) &
      !is.na(.data$start_score_away) &
      !is.na(.data$end_score_home) &
      !is.na(.data$end_score_away) &
      !is.na(.data$target_team_is_home) &
      (.data$end_score_home - .data$start_score_home) >= 0 &
      (.data$end_score_away - .data$start_score_away) >= 0,
    start_home_margin = .data$start_score_home - .data$start_score_away,
    end_home_margin = .data$end_score_home - .data$end_score_away,
    home_margin_change = .data$end_home_margin - .data$start_home_margin,
    target_margin_change = dplyr::if_else(
      .data$score_delta_available & .data$target_team_is_home,
      .data$home_margin_change,
      dplyr::if_else(.data$score_delta_available, -.data$home_margin_change, NA_real_)
    ),
    opponent_margin_change = dplyr::if_else(
      !is.na(.data$target_margin_change),
      -.data$target_margin_change,
      NA_real_
    ),
    # These are true scoreboard deltas only when complete start/end home and
    # away scores can be aligned to the stint. They stay NA otherwise; do not
    # replace missing points with zero, because that would fake scoreless stints.
    target_points_for = dplyr::if_else(
      .data$score_delta_available & .data$target_team_is_home,
      .data$end_score_home - .data$start_score_home,
      dplyr::if_else(
        .data$score_delta_available,
        .data$end_score_away - .data$start_score_away,
        NA_real_
      )
    ),
    target_points_against = dplyr::if_else(
      .data$score_delta_available & .data$target_team_is_home,
      .data$end_score_away - .data$start_score_away,
      dplyr::if_else(
        .data$score_delta_available,
        .data$end_score_home - .data$start_score_home,
        NA_real_
      )
    )
  )

print_phase3_scoring_diagnostics(
  stints_scored = stints_scored,
  score_timeline = score_timeline,
  score_home_col = score_home_col,
  score_away_col = score_away_col,
  period_col = period_col,
  clock_col = clock_col,
  event_col = event_col
)

write_project_parquet(stints_scored, glue("data/processed/stints/{team_abbr}_stints_scored_{season}.parquet"))

message("Scored stint summary:")
print(
  stints_scored %>%
    dplyr::summarise(
      stints = dplyr::n(),
      missing_start_scores = sum(is.na(.data$start_score_home) | is.na(.data$start_score_away)),
      missing_end_scores = sum(is.na(.data$end_score_home) | is.na(.data$end_score_away)),
      score_delta_available_stints = sum(.data$score_delta_available, na.rm = TRUE),
      total_target_margin_change = sum_or_na(.data$target_margin_change),
      total_target_points_for = sum_or_na(.data$target_points_for),
      total_target_points_against = sum_or_na(.data$target_points_against)
    )
)
