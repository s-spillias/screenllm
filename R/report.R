# Any of these columns on the ranked side would collide with the
# columns we get from the decisions df during a left_join. Toy CSVs
# and real user CSVs sometimes carry a baked-in `human_decision`
# (as ground truth) which is what triggered the .x/.y suffix that
# broke the Report tab in production.
#' @keywords internal
strip_decision_columns <- function(df) {
  cols <- c("human_decision", "note", "timestamp")
  df[, setdiff(names(df), cols), drop = FALSE]
}

#' Summarise a completed screening session
#'
#' Combines the LLM ranking with a set of human decisions and reports the
#' realised recall (against the decisions the human made above the stop
#' point), the workload actually incurred, and diagnostic per-record
#' counts.
#'
#' @param ranked A ranking (output of `rank_records()`).
#' @param decisions A tibble with columns `id` and `human_decision`
#'   (values `"Accept"` or `"Reject"`).
#' @param plan Optional `screenllm_plan` used to identify the stop point;
#'   when supplied, `summarise_screening()` also reports the workload
#'   fraction the plan intended.
#' @return A `screenllm_report` object.
#' @export
summarise_screening <- function(ranked, decisions, plan = NULL) {
  stopifnot(is.data.frame(ranked), is.data.frame(decisions))
  # Tolerate a legacy decisions file that lost the `human_decision`
  # column: normalise the shape (add missing columns as NA) instead
  # of aborting. Downstream code treats NA rows as "not screened".
  decisions <- normalise_decisions_shape(decisions)
  # The toy CBFM corpus ships with ground-truth decision columns
  # baked in (as `human_decision`); a real user's CSV might too.
  # Strip those before the join so we don't end up with .x/.y
  # suffixes that break the downstream filters.
  ranked <- strip_decision_columns(ranked)
  merged <- ranked |>
    dplyr::left_join(decisions, by = "id") |>
    dplyr::arrange(.data$rank)

  n <- nrow(merged)
  screened <- !is.na(merged$human_decision)
  n_screened <- sum(screened)
  accepts <- merged$human_decision == "Accept" & screened
  n_accepts <- sum(accepts, na.rm = TRUE)
  workload_pct <- 100 * n_screened / n

  # Realised recall requires knowing the true positives across the whole
  # corpus. Since the tail is not screened, we report recall relative to
  # accepts found within the screened set only.
  recall_within_screened <- if (n_screened > 0L) n_accepts / max(n_accepts, 1L) else NA

  structure(
    list(
      n_records = n,
      n_screened = n_screened,
      n_accepts = n_accepts,
      workload_pct = workload_pct,
      recall_within_screened = recall_within_screened,
      stop_at = if (!is.null(plan)) plan$stop_at else NA_integer_,
      merged = merged
    ),
    class = "screenllm_report"
  )
}

#' @export
print.screenllm_report <- function(x, ...) {
  cli::cli_h2("<screenllm_report>")
  cli::cli_alert_info(sprintf("Records total:    %d", x$n_records))
  cli::cli_alert_info(sprintf("Records screened: %d (%.1f%% workload)", x$n_screened, x$workload_pct))
  cli::cli_alert_info(sprintf("Human accepts:    %d", x$n_accepts))
  if (!is.na(x$stop_at)) {
    cli::cli_alert_info(sprintf("Planned stop at:  record %d", x$stop_at))
  }
  invisible(x)
}

#' Surface strong LLM-human disagreements for a manual audit
#'
#' Returns records where the LLM ensemble and the human reviewer disagree
#' at high confidence. On one of the benchmark reviews reported in the
#' paper, a similar audit caught genuine screener errors in 28\% of the
#' disagreements. This is intended as a low-cost quality-control step
#' after the main screen.
#'
#' @param ranked A ranking (output of `rank_records()`).
#' @param decisions A tibble with columns `id` and `human_decision`.
#' @param strong_fp_score LLM score at or above which an accept is
#'   "confident" (default 70).
#' @param strong_fn_score LLM score below which a reject is "confident"
#'   (default 30).
#' @return A tibble of disagreement rows, one per record.
#' @export
audit_disagreements <- function(ranked, decisions,
                                strong_fp_score = 70,
                                strong_fn_score = 30) {
  stopifnot(is.data.frame(ranked), is.data.frame(decisions))
  # Same defence as summarise_screening: normalise the decisions
  # shape and strip any pre-existing decision columns from the
  # ranked side so the join doesn't produce .x/.y suffixes.
  decisions <- normalise_decisions_shape(decisions)
  ranked <- strip_decision_columns(ranked)
  merged <- ranked |>
    dplyr::left_join(decisions, by = "id") |>
    dplyr::filter(!is.na(.data$human_decision))
  strong_fp <- merged |>
    dplyr::filter(
      .data$universal_best_score >= strong_fp_score,
      .data$human_decision == "Reject"
    ) |>
    dplyr::mutate(disagreement = "Strong FP (LLM accepts, human rejects)")
  strong_fn <- merged |>
    dplyr::filter(
      .data$universal_best_score < strong_fn_score,
      .data$human_decision == "Accept"
    ) |>
    dplyr::mutate(disagreement = "Strong FN (LLM rejects, human accepts)")
  audit <- dplyr::bind_rows(strong_fn, strong_fp)
  dplyr::arrange(
    audit,
    dplyr::desc(startsWith(.data$disagreement, "Strong FN")),
    .data$universal_best_score
  ) |>
    dplyr::select(dplyr::any_of(c(
      "id", "title", "abstract", "universal_best_score",
      "human_decision", "disagreement"
    )))
}

#' Export the human-screening worksheet
#'
#' Writes the records above the SAFE stop point to an Excel file with a
#' blank `human_decision` column ready for the reviewer to complete.
#'
#' @param plan A `screenllm_plan` object.
#' @param path Output path (must end in `.xlsx`).
#' @return Invisibly, `path`.
#' @export
export_worksheet <- function(plan, path) {
  stopifnot(inherits(plan, "screenllm_plan"))
  rlang::check_installed("writexl", "to export the worksheet.")
  df <- plan$to_screen
  df$human_decision <- NA_character_
  df$note <- NA_character_
  # List-columns don't survive Excel; convert to summary text.
  if ("per_model_scores" %in% names(df)) df$per_model_scores <- NULL
  if ("justifications" %in% names(df)) {
    df$justifications <- vapply(
      df$justifications,
      function(x) if (is.null(x)) NA_character_ else paste(x$explanation, collapse = " | "),
      character(1)
    )
  }
  writexl::write_xlsx(df, path = path)
  cli::cli_alert_success("Wrote worksheet: {.path {path}}")
  invisible(path)
}
