# Async pilot runs.
#
# `pilot()` is synchronous; we run it in a background R process and
# stream per-record results into a progress file so the Shiny UI can
# poll and grow the results table one row at a time as records finish
# scoring.

#' Start a background pilot job
#'
#' Spawns a `callr::r_bg` subprocess that samples `n` records from
#' `records`, scores each one against the criteria using every model
#' in the ensemble at 1 replicate, aggregates, and appends the result
#' to a progress file. The caller polls `pilot_job_status()` for
#' updates and `pilot_job_cancel(handle)` to abort.
#'
#' Unlike `pilot()`, this returns immediately with a handle. The
#' progress file is at `pilot_progress_path()` and is overwritten by
#' each new job.
#'
#' @param records A tibble of records (from `read_records()`).
#' @param criteria A `screenllm_criteria` object.
#' @param ensemble A `screenllm_ensemble` object.
#' @param n Number of records to score. Defaults to 20.
#' @param sample Whether to sample at random (default) or take the
#'   first `n` rows.
#' @param seed Random seed for the sample.
#' @return A list with `pid`, `handle`, and `progress_path`.
#' @export
start_pilot_job <- function(records, criteria, ensemble,
                            n = 20L, sample = TRUE, seed = 1L) {
  rlang::check_installed("callr", "to run the pilot in the background.")
  stopifnot(
    is.data.frame(records),
    inherits(criteria, "screenllm_criteria"),
    inherits(ensemble, "screenllm_ensemble")
  )
  n <- as.integer(min(n, nrow(records)))
  if (isTRUE(sample) && nrow(records) > n) {
    set.seed(seed)
    idx <- sort(sample.int(nrow(records), n))
  } else {
    idx <- seq_len(n)
  }
  subset <- records[idx, , drop = FALSE]

  progress_path <- pilot_progress_path()
  input_path <- fs::path(fs::path_dir(progress_path), "input.rds")
  fs::dir_create(fs::path_dir(progress_path), recurse = TRUE)
  saveRDS(list(records = subset, criteria = criteria, ensemble = ensemble),
          input_path)
  saveRDS(list(
    status = "starting",
    processed = 0L,
    total = n,
    results = list(),
    error = NULL,
    started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), progress_path)

  libpaths <- .libPaths()
  handle <- callr::r_bg(
    func = pilot_worker_body,
    args = list(
      input_path = input_path,
      progress_path = progress_path,
      libpaths = libpaths
    ),
    supervise = FALSE
  )
  list(pid = handle$get_pid(),
       handle = handle,
       progress_path = progress_path)
}

#' Status of a background pilot job
#'
#' @return A list with `status` (`"idle"`, `"starting"`, `"running"`,
#'   `"done"`, or `"error"`), `processed`, `total`, `percent`,
#'   `results` (a list of scored records so far), `error`, and
#'   `elapsed_secs`.
#' @export
pilot_job_status <- function() {
  path <- pilot_progress_path()
  if (!fs::file_exists(path)) {
    return(list(status = "idle", processed = 0L, total = NA_integer_,
                percent = 0, results = list()))
  }
  st <- readRDS(path)
  pct <- if (isTRUE(st$total > 0) && !is.na(st$processed)) {
    round(100 * st$processed / st$total, 1)
  } else 0
  elapsed <- if (!is.null(st$started_at)) {
    started <- try(as.POSIXct(st$started_at), silent = TRUE)
    if (inherits(started, "POSIXct")) {
      as.numeric(Sys.time() - started, units = "secs")
    } else NA_real_
  } else NA_real_
  c(st, list(percent = pct, elapsed_secs = elapsed))
}

#' Cancel a running pilot job
#' @param handle Callr process handle (from `start_pilot_job()$handle`).
#' @return Invisibly, `TRUE`.
#' @export
pilot_job_cancel <- function(handle) {
  if (!is.null(handle) && handle$is_alive()) handle$kill()
  invisible(TRUE)
}

#' Convert accumulated pilot results into a `screenllm_pilot` tibble
#'
#' The worker stores partial results as a plain list of per-record
#' entries so it can append cheaply without touching the whole
#' object. Both the UI (during streaming) and the final job step call
#' this helper to hand back the standard tibble.
#'
#' @param results List returned by `pilot_job_status()$results`.
#' @return A `screenllm_pilot` tibble; empty if `results` is empty.
#' @keywords internal
pilot_results_as_tibble <- function(results) {
  if (length(results) == 0L) {
    out <- tibble::tibble(
      id = character(), title = character(), abstract = character(),
      universal_best_score = double(),
      per_model_scores = list(), justifications = list()
    )
  } else {
    out <- tibble::tibble(
      id = vapply(results, function(r) r$id, character(1)),
      title = vapply(results, function(r) r$title, character(1)),
      abstract = vapply(results, function(r) r$abstract, character(1)),
      universal_best_score = vapply(results, function(r) r$universal_best_score,
                                     numeric(1)),
      per_model_scores = lapply(results, function(r) r$per_model_scores),
      justifications = lapply(results, function(r) r$justifications)
    )
  }
  class(out) <- c("screenllm_pilot", class(out))
  attr(out, "n_pilot") <- nrow(out)
  attr(out, "n_models") <- if (nrow(out) > 0L) {
    length(unique(out$per_model_scores[[1]]$model))
  } else 0L
  out
}

# --------------------------------------------------------------------
#' @keywords internal
pilot_worker_body <- function(input_path, progress_path, libpaths) {
  .libPaths(libpaths)
  library(screenllm)

  inp <- readRDS(input_path)
  records <- inp$records
  criteria <- inp$criteria
  ensemble <- inp$ensemble

  n <- nrow(records)
  results <- vector("list", n)

  write_state <- function(status, processed, results_so_far,
                          error = NULL) {
    saveRDS(list(
      status = status,
      processed = processed,
      total = n,
      results = results_so_far,
      error = error,
      started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ), progress_path)
  }

  tryCatch({
    for (i in seq_len(n)) {
      rec <- records[i, ]
      prompt <- build_prompt(criteria, rec)

      # Score against every model in the ensemble at 1 replicate.
      per_model <- lapply(ensemble$models, function(m) {
        out <- ensemble$backend$score_record(m, prompt, ensemble$temperature)
        list(model = m, replicate = 1L,
             score = as.numeric(out$score),
             explanation = out$explanation %||% NA_character_)
      })

      scores <- vapply(per_model, function(x) x$score, numeric(1))
      # Inline the aggregation so the worker doesn't need to reach
      # into the package's non-exported namespace via ::: (which R CMD
      # check flags). Behaviour matches R/ensemble.R aggregate_scores().
      s <- scores[!is.na(scores)]
      agg <- if (length(s) == 0L) {
        NA_real_
      } else {
        switch(
          ensemble$aggregator,
          mean = mean(s),
          median = stats::median(s),
          max = max(s),
          topk_mean = {
            k <- min(2L, length(s))
            mean(sort(s, decreasing = TRUE)[seq_len(k)])
          },
          stop("Unknown aggregator: ", ensemble$aggregator)
        )
      }

      results[[i]] <- list(
        id = as.character(rec$id),
        title = as.character(rec$title),
        abstract = as.character(rec$abstract %||% ""),
        universal_best_score = agg,
        per_model_scores = data.frame(
          model = vapply(per_model, function(x) x$model, character(1)),
          replicate = vapply(per_model, function(x) x$replicate, integer(1)),
          score = vapply(per_model, function(x) x$score, numeric(1)),
          stringsAsFactors = FALSE
        ),
        justifications = data.frame(
          model = vapply(per_model, function(x) x$model, character(1)),
          replicate = vapply(per_model, function(x) x$replicate, integer(1)),
          explanation = vapply(per_model, function(x) x$explanation, character(1)),
          stringsAsFactors = FALSE
        )
      )
      write_state("running", i, results[seq_len(i)])
    }
    write_state("done", n, results)
  }, error = function(e) {
    write_state("error", length(Filter(Negate(is.null), results)),
                Filter(Negate(is.null), results),
                error = conditionMessage(e))
    stop(e)
  })
}

# --------------------------------------------------------------------
#' @keywords internal
pilot_progress_path <- function() {
  fs::path(tools::R_user_dir("screenllm", "cache"),
           "pilots", "progress.rds")
}
