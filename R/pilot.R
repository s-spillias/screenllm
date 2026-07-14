#' Pilot the ensemble on a small sample before committing to a full run
#'
#' Runs the ensemble on a small sample of records at a single replicate
#' per model, and returns each record's aggregate score together with the
#' per-criterion scores and one-line LLM justifications. Meant as a
#' sanity check for the criteria wording: the user reads a handful of
#' scored records and decides whether the LLM is scoring things the way
#' they expect before triggering a hours-long full run.
#'
#' The pilot deliberately uses `replicates = 1` on the supplied
#' ensemble so the total number of LLM calls is `n * length(models)`.
#' This makes it fast and cheap, at the cost of not showing
#' replicate-to-replicate variation. Use `rank_records()` for the real
#' run.
#'
#' Piloting is a diagnostic tool for the user, not a validated
#' methodological step. The paper does not evaluate criteria-revision
#' as an intervention; iterating criteria on the pilot's output changes
#' both the criteria and the sample of records the reviewer has seen,
#' which is confounded. Use pilot output to catch obvious mis-scoring
#' (a criterion the LLM is systematically ignoring, a criterion that
#' triggers on off-topic records), not to fine-tune criteria against
#' a held-out set.
#'
#' @param records A tibble of records (produced by `read_records()`).
#' @param criteria A `screenllm_criteria` object.
#' @param ensemble A `screenllm_ensemble` object. Defaults to
#'   `default_ensemble()`. The pilot overrides `replicates` to 1
#'   regardless of what the ensemble was configured with.
#' @param n Number of records to score. Defaults to 20. If `records`
#'   has fewer rows, all of them are used.
#' @param sample Whether to sample `n` records at random (default) or
#'   take the first `n` rows in the order they appear in `records`.
#' @param seed Random seed for reproducibility of the sample.
#' @param cache_dir Passed through to `rank_records()`.
#' @param verbose Passed through to `rank_records()`.
#' @return A `screenllm_pilot` object: a tibble with columns `id`,
#'   `title`, `abstract`, `universal_best_score`, `per_model_scores`,
#'   `justifications`. Prints as a compact per-record summary.
#' @export
#' @examples
#' \donttest{
#' records <- data.frame(
#'   id = paste0("r", 1:5),
#'   title = paste("Title", 1:5),
#'   abstract = paste("Abstract", 1:5)
#' )
#' criteria <- define_criteria(
#'   scope = "Demo scope",
#'   inclusions = c("Test criterion A.", "Test criterion B.")
#' )
#' ens <- default_ensemble(backend = backend_mock())
#' pilot(records, criteria, ensemble = ens, n = 3, verbose = FALSE)
#' }
pilot <- function(records,
                  criteria,
                  ensemble = default_ensemble(),
                  n = 20L,
                  sample = TRUE,
                  seed = 1L,
                  cache_dir = getOption("screenllm.cache_dir"),
                  verbose = getOption("screenllm.verbose", TRUE)) {
  stopifnot(
    is.data.frame(records),
    inherits(criteria, "screenllm_criteria"),
    inherits(ensemble, "screenllm_ensemble"),
    length(n) == 1L, is.numeric(n), n >= 1L
  )
  n <- as.integer(min(n, nrow(records)))
  if (isTRUE(sample) && nrow(records) > n) {
    set.seed(seed)
    idx <- sort(sample.int(nrow(records), n))
  } else {
    idx <- seq_len(n)
  }
  subset <- records[idx, , drop = FALSE]

  # Force replicates = 1 for speed. Everything else about the ensemble
  # is preserved.
  ensemble$replicates <- 1L

  if (verbose) {
    cli::cli_alert_info(
      "Piloting {n} record{?s} with {length(ensemble$models)} model{?s} \\
       at 1 replicate each ({n * length(ensemble$models)} LLM call{?s})."
    )
  }
  out <- rank_records(subset, criteria, ensemble = ensemble,
                      cache_dir = cache_dir, verbose = verbose)

  keep <- intersect(
    c("id", "title", "abstract",
      "universal_best_score", "per_model_scores", "justifications"),
    names(out)
  )
  out <- out[, keep, drop = FALSE]
  class(out) <- c("screenllm_pilot", class(out))
  attr(out, "n_pilot") <- n
  attr(out, "n_models") <- length(ensemble$models)
  out
}

#' @export
print.screenllm_pilot <- function(x, max_records = 10L, ...) {
  n <- nrow(x)
  n_models <- attr(x, "n_models") %||% NA_integer_
  cli::cli_h2("<screenllm_pilot> {n} record{?s} scored on {n_models} model{?s}")

  # Order by aggregate score, descending, so the interesting records are
  # near the top.
  x <- x[order(-x$universal_best_score), , drop = FALSE]
  show <- utils::head(x, max_records)
  for (i in seq_len(nrow(show))) {
    rec <- show[i, ]
    score <- rec$universal_best_score
    title <- rec$title %||% "(no title)"
    title <- substr(title, 1, 90)
    cli::cli_h3(sprintf("[%3.0f] %s", score, title))
    js <- rec$justifications[[1]]
    if (!is.null(js) && nrow(js) > 0L) {
      msgs <- unique(js$explanation[nzchar(js$explanation) & !is.na(js$explanation)])
      msgs <- utils::head(msgs, 3L)
      for (m in msgs) {
        m <- substr(gsub("\\s+", " ", m), 1, 200)
        cli::cli_ul(m)
      }
    }
  }
  if (n > max_records) {
    cli::cli_alert_info(
      "Showing top {max_records} of {n}. Access the full table with `as.data.frame(x)`."
    )
  }
  invisible(x)
}
