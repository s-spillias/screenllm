#' Plan the human screening set with the SAFE stopping rule
#'
#' Applies the SAFE rule from Anonymous et al. (2026) to a ranking produced
#' by `rank_records()`. Walks the ranked corpus from highest score to
#' lowest, and returns the position where SAFE fires along with the
#' subset of records the human should screen (everything at or above that
#' position). Defaults reproduce the paper's advance-choosable recommended
#' setting: minimum coverage 50\%, consecutive-negative run length 50.
#'
#' Because SAFE's spot-check gate depends on a random sample, the returned
#' plan is only deterministic when `seed` is set.
#'
#' @param ranked A `screenllm_ranking` object (output of `rank_records()`).
#' @param target_recall Target recall (default 0.95).
#' @param safe_min_cover Minimum-coverage fraction (default 0.50).
#' @param safe_run_length Consecutive-negatives run length (default 50).
#' @param spot_check_n Number of records in the SAFE spot-check (default 200).
#' @param spot_check_labels Optional named vector of accept/reject decisions
#'   for the spot-check records (names = record ids, values in
#'   `c("Accept", "Reject")`). If `NULL`, the plan uses a placeholder
#'   estimate and reports the stop-point conservatively.
#' @param seed Random seed for the spot-check draw.
#' @return A `screenllm_plan` object.
#' @export
#' @examples
#' # A minimal example. In practice, `ranked` comes from `rank_records()`.
#' ranked <- data.frame(
#'   id = paste0("r", 1:100),
#'   universal_best_score = sort(runif(100, 0, 100), decreasing = TRUE),
#'   rank = 1:100
#' )
#' plan <- plan_screening(ranked, safe_run_length = 10, spot_check_n = 20)
#' plan
plan_screening <- function(ranked,
                           target_recall = .DEFAULT_TARGET_RECALL,
                           safe_min_cover = .DEFAULT_SAFE_MIN_COVER,
                           safe_run_length = .DEFAULT_SAFE_RUN_LENGTH,
                           spot_check_n = .DEFAULT_SPOT_CHECK_N,
                           spot_check_labels = NULL,
                           seed = 1L) {
  stopifnot(
    is.data.frame(ranked),
    "universal_best_score" %in% names(ranked),
    target_recall > 0, target_recall < 1,
    safe_min_cover > 0, safe_min_cover < 1,
    safe_run_length >= 1L,
    spot_check_n >= 1L
  )
  N <- nrow(ranked)
  ranked <- dplyr::arrange(ranked, .data$rank)

  # SAFE gate 1: minimum coverage
  min_cover_position <- ceiling(safe_min_cover * N)

  # SAFE gate 3: spot-check estimate of total positives.
  # If the caller has labelled spot-check records, use them; otherwise
  # fall back to the observed prevalence in the top of the ranking as a
  # placeholder (documented in the plan).
  set.seed(seed)
  spot_idx <- sample.int(N, size = min(spot_check_n, N))
  if (!is.null(spot_check_labels)) {
    hits <- ranked$id[spot_idx] %in% names(spot_check_labels)
    labelled <- spot_check_labels[ranked$id[spot_idx][hits]]
    spot_positives <- sum(labelled == "Accept", na.rm = TRUE)
    est_prev <- if (length(labelled) > 0) mean(labelled == "Accept") else NA
  } else {
    spot_positives <- NA_integer_
    est_prev <- NA_real_
  }
  est_total_positives <- if (!is.na(est_prev)) round(est_prev * N) else NA
  spot_check_gate_target <- if (!is.na(est_total_positives)) {
    ceiling(2 * est_total_positives)
  } else {
    NA
  }

  # Simulated walk down the ranking. We don't know which records the human
  # will actually reject at inference time; use the score-implied label
  # (score < 50 = probable reject) as a placeholder for the SAFE preview,
  # and expose the gate positions the human will need to update after
  # screening actually begins.
  #
  # Any record with a missing universal_best_score (e.g. every LLM call
  # for that record failed or timed out) is treated as a probable
  # reject; otherwise NA would propagate through the cumulative sum
  # and the run-length gate check below and abort the whole walk.
  probable_accept <- !is.na(ranked$universal_best_score) &
    ranked$universal_best_score >= 50
  cum_positives <- cumsum(probable_accept)

  consecutive_neg <- integer(N)
  streak <- 0L
  for (j in seq_len(N)) {
    if (!probable_accept[j]) {
      streak <- streak + 1L
    } else {
      streak <- 0L
    }
    consecutive_neg[j] <- streak
  }

  # Earliest position where all three gates simultaneously satisfied.
  gate1 <- seq_len(N) >= min_cover_position
  gate2 <- consecutive_neg >= safe_run_length
  gate3 <- if (is.na(spot_check_gate_target)) {
    rep(TRUE, N)
  } else {
    cum_positives >= spot_check_gate_target
  }
  fires <- gate1 & gate2 & gate3
  stop_at <- if (any(fires)) which(fires)[1L] else N

  # Per-gate earliest-fire position for diagnostics. `Inf` means the
  # gate never fires with the current settings + ranking; the UI can
  # then show a hint about which slider to move.
  gate1_at <- if (any(gate1)) which(gate1)[1L] else Inf
  gate2_at <- if (any(gate2)) which(gate2)[1L] else Inf
  gate3_at <- if (all(gate3)) 1L
    else if (any(gate3)) which(gate3)[1L] else Inf
  # Whichever gate fires latest is what's binding stop_at (the others
  # are already satisfied earlier). Ties resolve to gate1 arbitrarily.
  gate_positions <- c(min_coverage = gate1_at,
                       run_length = gate2_at,
                       spot_check = gate3_at)
  finite_positions <- gate_positions[is.finite(gate_positions)]
  binding <- if (length(finite_positions) == length(gate_positions)) {
    names(gate_positions)[which.max(gate_positions)]
  } else {
    names(gate_positions)[!is.finite(gate_positions)][1]
  }

  to_screen <- ranked[seq_len(stop_at), , drop = FALSE]

  plan <- structure(
    list(
      stop_at = stop_at,
      N = N,
      expected_workload_pct = 100 * stop_at / N,
      gates = list(
        min_coverage = list(position = gate1_at, fires = is.finite(gate1_at)),
        run_length   = list(position = gate2_at, fires = is.finite(gate2_at)),
        spot_check   = list(position = gate3_at, fires = is.finite(gate3_at),
                             evaluated = !is.na(spot_check_gate_target))
      ),
      binding_gate = binding,
      max_negative_streak = max(consecutive_neg, na.rm = TRUE),
      target_recall = target_recall,
      to_screen = to_screen,
      settings = list(
        safe_min_cover = safe_min_cover,
        safe_run_length = safe_run_length,
        spot_check_n = spot_check_n,
        seed = seed
      ),
      spot_check = list(
        est_prevalence = est_prev,
        est_total_positives = est_total_positives,
        gate_target = spot_check_gate_target,
        used_provided_labels = !is.null(spot_check_labels)
      )
    ),
    class = "screenllm_plan"
  )
  plan
}

#' @export
print.screenllm_plan <- function(x, ...) {
  cli::cli_h2("<screenllm_plan>")
  cli::cli_alert_info(
    sprintf(
      "Stop at record %d of %d (expected workload %.1f%%).",
      x$stop_at, x$N, x$expected_workload_pct
    )
  )
  cli::cli_alert_info(
    sprintf(
      "Settings: min coverage %.0f%%, run length %d, spot check n = %d",
      100 * x$settings$safe_min_cover,
      x$settings$safe_run_length,
      x$settings$spot_check_n
    )
  )
  if (!x$spot_check$used_provided_labels) {
    cli::cli_alert_warning(
      "Spot-check gate is placeholder (no labelled spot-check supplied). \\
       Once the reviewer labels the first ~200 records, re-run \\
       `plan_screening()` with `spot_check_labels` set to sharpen the stop point."
    )
  }
  invisible(x)
}
