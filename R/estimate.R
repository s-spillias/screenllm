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
#' order-of-magnitude estimate only.
#'
#' @param n_records Number of records in the corpus.
#' @param ensemble A `screenllm_ensemble` object.
#' @param seconds_per_call Optional override for the per-call cost.
#'   When `NULL` (the default) a heuristic based on the largest model
#'   in the ensemble is used, adjusted for GPU / CPU / throttled
#'   hardware states.
#' @param gpu Whether to assume GPU inference. `NULL` (the default)
#'   calls [detect_gpu()] to auto-detect. Pass `TRUE` or `FALSE` to
#'   override.
#' @param throttled If `TRUE`, assume GPU cores are locked at low
#'   clock speed (see [gpu_status()]). Treated as CPU-equivalent for
#'   throughput. Defaults to `FALSE`; the Shiny app passes the live
#'   [gpu_status()] value.
#' @return A `screenllm_estimate` object: a list with `n_calls`,
#'   `seconds_per_call`, `seconds_total`, `human_readable`, `gpu`,
#'   `hardware` (a string describing the assumed hardware profile),
#'   and a `caveats` character vector.
#' @export
#' @examples
#' ens <- default_ensemble(backend = backend_mock())
#' estimate_runtime(n_records = 500, ensemble = ens, gpu = FALSE)
#' estimate_runtime(n_records = 500, ensemble = ens, gpu = TRUE)
estimate_runtime <- function(n_records,
                             ensemble,
                             seconds_per_call = NULL,
                             gpu = NULL,
                             throttled = FALSE) {
  stopifnot(
    length(n_records) == 1L, is.numeric(n_records), n_records >= 0L,
    inherits(ensemble, "screenllm_ensemble")
  )
  n_calls <- as.integer(n_records) *
    length(ensemble$models) * as.integer(ensemble$replicates)

  # Resolve the GPU flag once so the caveats can reference it.
  if (is.null(gpu)) {
    gpu_info <- detect_gpu()
    gpu <- isTRUE(gpu_info$available)
    gpu_detail <- gpu_info$detail
  } else {
    gpu_detail <- if (isTRUE(gpu)) "GPU assumed (override)." else
      "CPU assumed (override)."
  }
  # A throttled GPU is CPU-equivalent for throughput, so route through
  # the CPU branch below.
  effective_gpu <- isTRUE(gpu) && !isTRUE(throttled)
  hardware <- if (isTRUE(gpu) && isTRUE(throttled)) {
    "GPU throttled (idle-clock)"
  } else if (isTRUE(gpu)) {
    "GPU"
  } else {
    "CPU"
  }

  if (is.null(seconds_per_call)) {
    # Biggest model in the ensemble dominates.
    sizes <- vapply(ensemble$models, model_size_hint, numeric(1))
    biggest <- max(sizes, na.rm = TRUE)
    seconds_per_call <- default_seconds_per_call(biggest, effective_gpu)
  }

  seconds_total <- n_calls * seconds_per_call

  structure(
    list(
      n_calls = n_calls,
      seconds_per_call = seconds_per_call,
      seconds_total = seconds_total,
      human_readable = format_duration(seconds_total),
      gpu = gpu,
      hardware = hardware,
      caveats = c(
        gpu_detail,
        sprintf("Assumed hardware profile: %s.", hardware),
        "Coarse heuristic; real runtime depends on VRAM fit, prompt length, and Ollama load.",
        "Cached records from a prior run take negligible time."
      )
    ),
    class = "screenllm_estimate"
  )
}

# Per-call time in seconds by biggest-model-size (billion params) and
# hardware profile. Values are conservative-realistic based on
# measured runs at Q4_K_M quantisation with a ~500-token prompt +
# ~200-token JSON response:
#
#   GPU healthy (modern laptop dGPU or better, model fits in VRAM):
#     <=4 B:  2s   (typ 40-60 tok/s)
#     5-8 B:  4s   (typ 25-40 tok/s)
#     9-15 B: 8s
#     16-30 B: 15s
#     >30 B:   35s (may not fit; treat as slow)
#
#   CPU or throttled GPU (10x slower is typical):
#     <=4 B:  15s
#     5-8 B:  35s
#     9-15 B: 80s
#     16-30 B: 180s
#     >30 B:   400s
#
# These are order-of-magnitude anchors. Use `seconds_per_call = ...`
# to override when you have machine-specific data.
#' @keywords internal
default_seconds_per_call <- function(biggest_b, use_gpu) {
  size_tier <- if (is.na(biggest_b)) "medium"
    else if (biggest_b <= 4)  "tiny"
    else if (biggest_b <= 8)  "small"
    else if (biggest_b <= 15) "medium"
    else if (biggest_b <= 30) "large"
    else                       "xlarge"
  gpu_table <- c(tiny = 2, small = 4, medium = 8,
                 large = 15, xlarge = 35)
  cpu_table <- c(tiny = 15, small = 35, medium = 80,
                 large = 180, xlarge = 400)
  if (isTRUE(use_gpu)) gpu_table[[size_tier]] else cpu_table[[size_tier]]
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
