#' Estimate wall-clock time for a ranking run
#'
#' Rough back-of-the-envelope estimator to tell a user "this is going
#' to take about X hours" before they hit Run. The estimate scales
#' `n_records * n_models * n_replicates` by a per-call cost that
#' depends on model size, and adds a small fixed overhead per model
#' for weight loading.
#'
#' The estimate is intentionally coarse. Real wall-clock depends on
#' hardware (GPU vs CPU), Ollama's model-swapping behaviour, prompt
#' length, and other user-load on the machine. Treat the number as an
#' order-of-magnitude sanity check, not a promise.
#'
#' @param n_records Number of records in the corpus.
#' @param ensemble A `screenllm_ensemble` object.
#' @param seconds_per_call Optional override for the per-call cost.
#'   When `NULL` (the default) a heuristic based on the largest model
#'   in the ensemble is used.
#' @return A `screenllm_estimate` object: a list with `n_calls`,
#'   `seconds_per_call`, `seconds_total`, `human_readable`, and a
#'   `caveats` character vector.
#' @export
#' @examples
#' ens <- default_ensemble(backend = backend_mock())
#' estimate_runtime(n_records = 500, ensemble = ens)
#' estimate_runtime(n_records = 500, ensemble = default_ensemble_light(
#'   backend = backend_mock()))
estimate_runtime <- function(n_records,
                             ensemble,
                             seconds_per_call = NULL) {
  stopifnot(
    length(n_records) == 1L, is.numeric(n_records), n_records >= 0L,
    inherits(ensemble, "screenllm_ensemble")
  )
  n_calls <- as.integer(n_records) *
    length(ensemble$models) * as.integer(ensemble$replicates)

  if (is.null(seconds_per_call)) {
    # Heuristic: biggest model in the ensemble dominates. A 20-30 B
    # Q4 model at Ollama on a modest laptop is roughly ~8 s/call for
    # a screening prompt; a 4-7 B is roughly ~3 s/call. GPU inference
    # can be 5-10x faster.
    sizes <- vapply(ensemble$models, model_size_hint, numeric(1))
    biggest <- max(sizes, na.rm = TRUE)
    seconds_per_call <- if (biggest >= 20) 8 else if (biggest >= 7) 4 else 3
  }

  seconds_total <- n_calls * seconds_per_call

  structure(
    list(
      n_calls = n_calls,
      seconds_per_call = seconds_per_call,
      seconds_total = seconds_total,
      human_readable = format_duration(seconds_total),
      caveats = c(
        "Coarse heuristic; real runtime depends on hardware and Ollama load.",
        "GPU inference is often 5-10x faster than the built-in defaults.",
        "Cached records from a prior run take negligible time."
      )
    ),
    class = "screenllm_estimate"
  )
}

#' @export
print.screenllm_estimate <- function(x, ...) {
  cli::cli_h2("<screenllm_estimate>")
  cli::cli_alert_info(
    "About {x$human_readable} for {format(x$n_calls, big.mark = ',')} \\
     LLM call{?s} at ~{x$seconds_per_call}s each."
  )
  for (c in x$caveats) cli::cli_ul(c)
  invisible(x)
}

# Return a rough parameter-count hint (in billions) for an Ollama tag.
# Parses the ":<size>b" suffix; falls back to NA_real_.
model_size_hint <- function(tag) {
  m <- regmatches(tag, regexpr("[0-9.]+[bB]", tag))
  if (length(m) == 0L || !nzchar(m)) return(NA_real_)
  as.numeric(sub("[bB]$", "", m))
}

# Format a duration in seconds as e.g. "2 h 15 m" or "45 s".
format_duration <- function(seconds) {
  if (is.na(seconds) || !is.finite(seconds)) return("(unknown)")
  if (seconds < 60) return(sprintf("%.0f s", seconds))
  if (seconds < 3600) return(sprintf("%d m", as.integer(round(seconds / 60))))
  hours <- floor(seconds / 3600)
  mins <- as.integer(round((seconds - hours * 3600) / 60))
  if (mins == 0L) return(sprintf("%d h", hours))
  sprintf("%d h %d m", hours, mins)
}
