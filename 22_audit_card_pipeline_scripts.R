# ============================================================
# 22_audit_card_pipeline_scripts.R
# Phase 22: Audit card-stat pipeline scripts against hoopR/NBA
# tracking endpoint discoveries.
#
# Goal:
# Review current card-facing offensive/creation/defensive scripts for
# assumptions that may now be outdated because richer hoopR/NBA tracking
# endpoints are known to exist.
#
# This phase reads scripts as text, classifies known assumption risks, and
# writes an audit table. It does not modify previous phases or model outputs.
# ============================================================

source("01_setup_project.R")
source("R/helpers.R")

script_audit_output_path <- "outputs/card_pipeline_script_audit.parquet"

fs::dir_create("outputs")

scripts_to_audit <- c(
  "11_build_player_attacks.R",
  "12_build_player_movesets.R",
  "13_build_attack_identity.R",
  "14_audit_shot_context_data.R",
  "15_build_player_shot_context.R",
  "16_build_ATK_score.R",
  "17_build_shot_making_surplus.R",
  "18_audit_creation_signals.R",
  "19_audit_tracking_creation_sources.R",
  "19b_extract_tracking_creation_metrics.R",
  "20_build_creation_rating.R",
  "20b_build_creation_opportunity_signals.R",
  "21_audit_defensive_signals.R"
)

read_script_text <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }

  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

has_pattern <- function(text, pattern) {
  if (is.na(text)) {
    return(FALSE)
  }

  stringr::str_detect(stringr::str_to_lower(text), stringr::regex(pattern, ignore_case = TRUE))
}

make_issue <- function(script_name, issue_category, issue_description, risk_level, status, recommended_action) {
  tibble::tibble(
    script_name = script_name,
    issue_category = issue_category,
    issue_description = issue_description,
    risk_level = risk_level,
    status = status,
    recommended_action = recommended_action
  )
}

audit_script <- function(script_name, script_text) {
  if (is.na(script_text)) {
    return(make_issue(
      script_name = script_name,
      issue_category = "missing script",
      issue_description = "Script listed for audit was not found in the workspace.",
      risk_level = "high",
      status = "still active",
      recommended_action = "Restore or remove this script from the Phase 22 audit list."
    ))
  }

  issues <- list()

  if (has_pattern(script_text, "assisted_attempt_rate|self_created_attempt_rate|assisted_tendency_context|assisted_attempts|self_created_attempts")) {
    status <- dplyr::case_when(
      has_pattern(script_text, "placeholder scaffolding|should not be used for atk or cr|creation variables appear") ~ "fixed",
      has_pattern(script_text, "if available|do not completely exclude assisted|creation burden|current cr reflects") ~ "intentionally deferred",
      TRUE ~ "still active"
    )

    risk <- dplyr::case_when(
      status == "fixed" ~ "low",
      status == "intentionally deferred" ~ "medium",
      TRUE ~ "high"
    )

    action <- dplyr::case_when(
      status == "fixed" ~ "Keep the audit guard; avoid using placeholder assisted/self-created fields as final model inputs.",
      status == "intentionally deferred" ~ "Replace scaffolded assisted/self-created tendencies with real tracking or play-by-play attribution before final ATK/CR.",
      TRUE ~ "Audit whether assisted/self-created fields are real; block final scoring usage if they are placeholders."
    )

    issues[[length(issues) + 1]] <- make_issue(
      script_name,
      "fake assisted/self-created fields",
      "Script references assisted/self-created creation fields that may be placeholder scaffolding rather than measured creation.",
      risk,
      status,
      action
    )
  }

  if (has_pattern(script_text, "default 50|neutral = 50|coalesce\\([^\\n]+50|context_component.*50")) {
    status <- if (has_pattern(script_text, "keep context_component as na|not default 50|insufficient tracking|coverage unavailable")) {
      "fixed"
    } else {
      "still active"
    }

    issues[[length(issues) + 1]] <- make_issue(
      script_name,
      "defaulting missing context to 50",
      "Script contains neutral-50 fallback or scaling behavior that can make unavailable context data look average.",
      ifelse(status == "fixed", "low", "high"),
      status,
      ifelse(
        status == "fixed",
        "Keep unavailable context as NA and continue printing coverage diagnostics.",
        "Replace neutral 50 defaults with NA plus explicit availability flags."
      )
    )
  }

  if (has_pattern(script_text, "min\\(|max\\(|scale_0_100|min-max|z-score|z_score|50 \\+ 15|clamp")) {
    status <- dplyr::case_when(
      has_pattern(script_text, "player count <30|fewer than 30|coverage >= 30|insufficient tracking|small sample normalization warning") ~ "fixed",
      has_pattern(script_text, "prototype|descriptive|audit-only|audit only|limited-coverage") ~ "intentionally deferred",
      TRUE ~ "still active"
    )

    risk <- dplyr::case_when(
      status == "fixed" ~ "low",
      status == "intentionally deferred" ~ "medium",
      TRUE ~ "high"
    )

    issues[[length(issues) + 1]] <- make_issue(
      script_name,
      "min-max or bounded scaling on tiny samples",
      "Script uses bounded scaling or min/max-style transforms that may exaggerate values when only a few tracked players are available.",
      risk,
      status,
      dplyr::case_when(
        status == "fixed" ~ "Maintain coverage gates and raw descriptive fields until tracking data is broad enough.",
        status == "intentionally deferred" ~ "Treat scaled values as exploratory diagnostics only; avoid final card rankings until coverage expands.",
        TRUE ~ "Add sample-size gates, raw fields, and warnings before using scaled values in card scores."
      )
    )
  }

  if (has_pattern(script_text, "lal_|team_abbr|pbp_sample|current pbp pull|full-season|full season|not full-season|observed games|current tracking sample")) {
    status <- if (has_pattern(script_text, "not full-season|not full season|observed games|current pbp pull|current tracking sample|limited-coverage|only games available")) {
      "fixed"
    } else {
      "still active"
    }

    issues[[length(issues) + 1]] <- make_issue(
      script_name,
      "Lakers/sample pull interpreted as league-wide",
      "Script reads team/sample-scoped data or current-pull outputs that could be mistaken for league-wide season data.",
      ifelse(status == "fixed", "medium", "high"),
      status,
      ifelse(
        status == "fixed",
        "Keep explicit observed-sample warnings in outputs and diagnostics.",
        "Add observed-sample warnings and avoid league-wide language until the data pull covers the intended population."
      )
    )
  }

  if (has_pattern(script_text, "left_join|inner_join|right_join|anti_join|semi_join|match\\(")) {
    uses_player_id_join <- has_pattern(script_text, "by = \"player_id\"|by = c\\([^\\n]*player_id|expected_player_id")
    uses_name_join <- has_pattern(script_text, "by = \"player_name\"|by = c\\([^\\n]*player_name|nickname as the join key|fuzzy")

    if (uses_name_join || !uses_player_id_join) {
      issues[[length(issues) + 1]] <- make_issue(
        script_name,
        "player name joins instead of player_id joins",
        "Script has joins or matching logic that may rely on names, nicknames, fuzzy matching, or non-player_id keys.",
        ifelse(uses_player_id_join, "medium", "high"),
        ifelse(uses_player_id_join, "intentionally deferred", "still active"),
        "Prefer exact player_id joins. Keep name matching only for diagnostics or endpoint target selection with ID validation."
      )
    } else {
      issues[[length(issues) + 1]] <- make_issue(
        script_name,
        "player name joins instead of player_id joins",
        "Script joins player-level data using player_id or validates expected player_id after name-based target lookup.",
        "low",
        "fixed",
        "Keep player_id as the join key and retain explicit ID validation for diagnostic target players."
      )
    }
  }

  if (has_pattern(script_text, "team_abbreviation|team_abbr|canonical_team_abbreviation")) {
    status <- if (has_pattern(script_text, "canonical_team_abbreviation|coalesce\\([^\\n]*team_abbreviation|team_abbreviation\\.x|team_abbreviation\\.y")) {
      "fixed"
    } else {
      "still active"
    }

    issues[[length(issues) + 1]] <- make_issue(
      script_name,
      "missing canonical team_abbreviation",
      "Script references team abbreviation data that may come from multiple source columns.",
      ifelse(status == "fixed", "low", "medium"),
      status,
      ifelse(
        status == "fixed",
        "Continue coalescing source team fields into one canonical team_abbreviation before endpoint calls or joins.",
        "Coalesce team_abbreviation/team_abbr variants before endpoint calls, joins, and output writes."
      )
    )
  }

  if (has_pattern(script_text, "nba_boxscore|nba_playerdash|nba_assisttracker|tracking endpoint|endpoint")) {
    status <- dplyr::case_when(
      has_pattern(script_text, "write_project_parquet\\(raw|raw endpoint tables|data/raw/tracking|save raw|raw_table") ~ "fixed",
      has_pattern(script_text, "audit only|schema audit|endpoint availability|does not modify") ~ "intentionally deferred",
      TRUE ~ "still active"
    )

    issues[[length(issues) + 1]] <- make_issue(
      script_name,
      "endpoints audited but raw values not saved",
      "Script references NBA/hoopR endpoints or endpoint audits; raw values should be persisted when metrics will feed later card phases.",
      dplyr::case_when(status == "fixed" ~ "low", status == "intentionally deferred" ~ "medium", TRUE ~ "high"),
      status,
      dplyr::case_when(
        status == "fixed" ~ "Keep raw endpoint parquet outputs so downstream phases can be reproduced.",
        status == "intentionally deferred" ~ "If this audit discovers a useful endpoint, add a follow-up extraction phase that saves raw tables.",
        TRUE ~ "Persist raw endpoint tables before deriving card metrics from them."
      )
    )
  }

  if (has_pattern(script_text, "cr_score|atk_score|context_adjusted_atk|observed_cr_score|observed_atk_score|weapon_identity_score|signature_weapon_score")) {
    status <- dplyr::case_when(
      has_pattern(script_text, "coverage >= 30|tracking_metric_coverage_sufficient|cr_score = na|context_adjusted_atk_available = false|insufficient tracking") ~ "fixed",
      has_pattern(script_text, "prototype|descriptive|do not build final|not final") ~ "intentionally deferred",
      TRUE ~ "still active"
    )

    issues[[length(issues) + 1]] <- make_issue(
      script_name,
      "metrics used despite insufficient coverage",
      "Script builds card-facing or score-like metrics that may rely on partial tracking or current-sample data.",
      dplyr::case_when(status == "fixed" ~ "low", status == "intentionally deferred" ~ "medium", TRUE ~ "high"),
      status,
      dplyr::case_when(
        status == "fixed" ~ "Keep final-score fields gated by coverage and expose observed/prototype fields separately.",
        status == "intentionally deferred" ~ "Label outputs as descriptive or prototype until the input pool is broad enough.",
        TRUE ~ "Add coverage flags, sample-size checks, and NA final score behavior before ranking players."
      )
    )
  }

  if (length(issues) == 0) {
    issues[[1]] <- make_issue(
      script_name,
      "no targeted issue detected",
      "No Phase 22 targeted assumption pattern was detected in this script text.",
      "low",
      "fixed",
      "No action from this audit category."
    )
  }

  dplyr::bind_rows(issues)
}

script_texts <- stats::setNames(lapply(scripts_to_audit, read_script_text), scripts_to_audit)

card_pipeline_script_audit <- dplyr::bind_rows(lapply(
  names(script_texts),
  function(script_name) {
    audit_script(script_name, script_texts[[script_name]])
  }
)) %>%
  dplyr::mutate(
    risk_level = factor(.data$risk_level, levels = c("high", "medium", "low"), ordered = TRUE),
    status = factor(.data$status, levels = c("still active", "intentionally deferred", "fixed"), ordered = TRUE)
  ) %>%
  dplyr::arrange(.data$risk_level, .data$status, .data$script_name, .data$issue_category) %>%
  dplyr::mutate(
    risk_level = as.character(.data$risk_level),
    status = as.character(.data$status)
  )

write_project_parquet(card_pipeline_script_audit, script_audit_output_path)

message("Phase 22 card pipeline script audit diagnostics:")

message("Concise summary by script and risk level:")
print(
  card_pipeline_script_audit %>%
    dplyr::count(.data$script_name, .data$risk_level, .data$status, name = "issue_count") %>%
    dplyr::arrange(.data$script_name, .data$risk_level, .data$status)
)

message("High and medium risk active/deferred issues:")
print(
  card_pipeline_script_audit %>%
    dplyr::filter(.data$risk_level %in% c("high", "medium")) %>%
    dplyr::select(
      "script_name",
      "issue_category",
      "risk_level",
      "status",
      "recommended_action"
    ) %>%
    dplyr::arrange(.data$risk_level, .data$status, .data$script_name)
)

message("Saved card pipeline script audit to: ", script_audit_output_path)
message("Phase 22 note: audit only. No previous phases were modified.")
