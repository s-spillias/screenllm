#' The paper's default LLM ensemble
#'
#' Returns the four-LLM \emph{mean} ensemble reported as the universal
#' ranker in Spillias et al. (2026), with three replicates per LLM. This
#' is the ensemble `rank_records()` uses when `ensemble` is not supplied.
#'
#' @param backend A backend object (default: `backend_ollama()`).
#' @param replicates Integer >= 1. Defaults to three.
#' @param temperature Sampling temperature. Defaults to 0.7 (per the paper).
#' @return A `screenllm_ensemble` object.
#' @export
default_ensemble <- function(backend = backend_ollama(),
                             replicates = .DEFAULT_REPLICATES,
                             temperature = .DEFAULT_TEMPERATURE) {
  custom_ensemble(
    models = .PINNED_DEFAULT_MODELS,
    replicates = replicates,
    aggregator = "mean",
    temperature = temperature,
    backend = backend
  )
}

#' A "light" ensemble that runs on a laptop
#'
#' Returns a four-LLM mean ensemble built from 3-4 B-class open-weights
#' models (`gemma3:4b`, `llama3.2:3b`, `qwen3:4b`, `mistral:7b`). Fits in
#' roughly 10 GB of disk and 8 GB of RAM at Q4 quantisation, so it runs
#' comfortably on an 8-16 GB laptop.
#'
#' Slightly less accurate than [default_ensemble()] (~5-10 percentage-point
#' drop in AP on the paper benchmarks), but useful for exploration, small
#' reviews, or machines that cannot host the paper's 65 GB ensemble.
#'
#' @inheritParams default_ensemble
#' @return A `screenllm_ensemble` object.
#' @export
#' @examples
#' \donttest{
#' light <- default_ensemble_light(backend = backend_mock())
#' print(light)
#' }
default_ensemble_light <- function(backend = backend_ollama(),
                                   replicates = .DEFAULT_REPLICATES,
                                   temperature = .DEFAULT_TEMPERATURE) {
  custom_ensemble(
    models = .PINNED_LIGHT_MODELS,
    replicates = replicates,
    aggregator = "mean",
    temperature = temperature,
    backend = backend
  )
}

#' Define a custom LLM ensemble
#'
#' Lets the user swap in a different set of Ollama-served models, adjust
#' the number of replicates per model, or change the aggregation rule.
#' The paper's findings support 3-4 comparable open-source LLMs with the
#' \emph{mean} aggregator; other choices are supported but not recommended.
#'
#' @param models Character vector of Ollama model tags (or backend-appropriate
#'   identifiers).
#' @param replicates Integer >= 1. Number of replicates per model.
#' @param aggregator One of "mean", "median", "max", "topk_mean".
#' @param temperature Sampling temperature passed to the backend.
#' @param backend A backend object (default: `backend_ollama()`).
#' @return A `screenllm_ensemble` object.
#' @export
custom_ensemble <- function(models,
                            replicates = .DEFAULT_REPLICATES,
                            aggregator = c("mean", "median", "max", "topk_mean"),
                            temperature = .DEFAULT_TEMPERATURE,
                            backend = backend_ollama()) {
  aggregator <- match.arg(aggregator)
  stopifnot(
    is.character(models), length(models) >= 1L, all(nzchar(models)),
    length(replicates) == 1L, is.numeric(replicates), replicates >= 1L,
    length(temperature) == 1L, is.numeric(temperature),
    temperature >= 0, temperature <= 2
  )
  structure(
    list(
      models = models,
      replicates = as.integer(replicates),
      aggregator = aggregator,
      temperature = temperature,
      backend = backend
    ),
    class = "screenllm_ensemble"
  )
}

#' @export
print.screenllm_ensemble <- function(x, ...) {
  cli::cli_h2(sprintf("<screenllm_ensemble> %s, r = %d", x$aggregator, x$replicates))
  cli::cli_ul(x$models)
  cli::cli_alert_info(sprintf("Backend: %s", x$backend$name))
  invisible(x)
}

#' Aggregate scores across models and replicates for one record
#'
#' Applies the ensemble's aggregator to a numeric vector of scores on
#' the 0 to 100 scale. Missing (`NA`) scores are dropped before aggregation.
#'
#' @param scores Numeric vector.
#' @param aggregator One of "mean", "median", "max", "topk_mean".
#' @param topk Integer (only used for "topk_mean").
#' @return Numeric scalar on the 0 to 100 scale, or NA if all inputs are missing.
#' @keywords internal
aggregate_scores <- function(scores, aggregator, topk = 2L) {
  s <- scores[!is.na(scores)]
  if (length(s) == 0L) return(NA_real_)
  switch(
    aggregator,
    mean = mean(s),
    median = stats::median(s),
    max = max(s),
    topk_mean = {
      k <- min(topk, length(s))
      mean(sort(s, decreasing = TRUE)[seq_len(k)])
    },
    cli::cli_abort("Unknown aggregator: {.val {aggregator}}")
  )
}
