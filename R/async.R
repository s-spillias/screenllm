# Async ranking helpers.
#
# `rank_records()` is long-running (hours on a real corpus), and Shiny's
# default execution model would freeze the UI for the whole run. We wrap
# it in a background R process so the Shiny app can call `start_rank_job()`,
# return immediately, and poll `rank_job_status()` to update a progress bar.
#
# We deliberately avoid `future`/`promises` here because they can hit
# thorny issues around global capture and library reload inside Shiny.
# Instead we use `callr::r_bg`, which spawns a plain R subprocess whose
# stderr we can tail, and a small progress file the subprocess writes to.
#
# The design:
#   - The parent Shiny process calls `start_rank_job(project, ...)`.
#   - `start_rank_job()` spawns a callr::r_bg() worker that:
#       - loads screenllm
#       - reads the project's records + criteria + ensemble
#       - runs rank_records() with a small `on_progress` callback that
#         writes a "processed / total" line to the project's progress file
#       - saves the ranked result and terminates
#   - The Shiny process periodically calls `rank_job_status(project)` to
#     read that progress file and update its UI.
#   - `rank_job_cancel(project)` kills the worker.

#' Start a background ranking job for a project
#'
#' Spawns a `callr::r_bg` subprocess that loads the project's records,
#' criteria, and ensemble from disk and runs `rank_records()`. Returns
#' immediately with a handle the caller can poll or cancel.
#'
#' @param project Project name.
#' @param ensemble A `screenllm_ensemble` object. Saved into the project
#'   directory so the worker can read it.
#' @return A list with `pid` (the worker PID) and `handle` (the callr
#'   process object).
#' @export
start_rank_job <- function(project, ensemble = default_ensemble()) {
  rlang::check_installed("callr", "to run ranking in the background.")

  records <- load_artefact(project, "records")
  criteria <- load_artefact(project, "criteria")
  if (is.null(records)) {
    cli::cli_abort("Cannot start job: no records saved in project {.val {project}}.")
  }
  if (is.null(criteria)) {
    cli::cli_abort("Cannot start job: no criteria saved in project {.val {project}}.")
  }
  save_artefact(project, "ensemble", ensemble)

  # Initialise the progress file so pollers get a defined state even
  # before the worker writes its first update.
  save_artefact(project, "progress", list(
    status = "starting",
    processed = 0L,
    total = nrow(records) * length(ensemble$models) * ensemble$replicates,
    started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    error = NULL
  ))

  proj_dir <- project_dir(project, create = TRUE)
  cache_dir <- project_cache_dir(project)
  progress_path <- fs::path(proj_dir, .project_artefacts$progress)
  ranked_path <- fs::path(proj_dir, .project_artefacts$ranked)
  libpaths <- .libPaths()

  handle <- callr::r_bg(
    func = rank_worker_body,
    args = list(
      project = project,
      cache_dir = cache_dir,
      progress_path = progress_path,
      ranked_path = ranked_path,
      libpaths = libpaths
    ),
    supervise = FALSE,
    stdout = fs::path(proj_dir, "rank_stdout.log"),
    stderr = fs::path(proj_dir, "rank_stderr.log")
  )

  list(pid = handle$get_pid(), handle = handle)
}

#' Read the current status of a ranking job
#'
#' Non-blocking: reads the progress file the worker updates.
#'
#' @param project Project name.
#' @return A list with `status` (`"starting"`, `"running"`, `"done"`, or
#'   `"error"`), `processed`, `total`, `percent`, `current_model`,
#'   `eta_secs`, and optionally `error` and `elapsed_secs`.
#' @export
rank_job_status <- function(project) {
  st <- load_artefact(project, "progress")
  if (is.null(st)) {
    return(list(status = "idle", processed = 0L, total = NA_integer_,
                percent = 0, current_model = NA_character_,
                eta_secs = NA_real_))
  }
  pct <- if (isTRUE(st$total > 0)) round(100 * st$processed / st$total, 1) else 0
  elapsed <- if (!is.null(st$started_at)) {
    started <- try(as.POSIXct(st$started_at), silent = TRUE)
    if (inherits(started, "POSIXct")) {
      as.numeric(Sys.time() - started, units = "secs")
    } else NA_real_
  } else NA_real_
  # ETA = remaining_calls / rate_calls_per_sec.
  # Only report once we have >= a few seconds of data so the estimate
  # doesn't flicker wildly at start-up.
  eta_secs <- NA_real_
  if (!is.na(elapsed) && elapsed > 5 &&
        !is.null(st$processed) && st$processed > 0 &&
        !is.null(st$total) && st$total > st$processed) {
    rate <- st$processed / elapsed
    if (is.finite(rate) && rate > 0) {
      eta_secs <- (st$total - st$processed) / rate
    }
  }
  c(st, list(percent = pct, elapsed_secs = elapsed, eta_secs = eta_secs))
}

#' Cancel a running ranking job
#'
#' Requires the caller to have kept the `handle` returned by
#' `start_rank_job()`.
#'
#' @param handle Callr process object.
#' @return Invisibly, `TRUE`.
#' @export
rank_job_cancel <- function(handle) {
  if (!is.null(handle) && handle$is_alive()) handle$kill()
  invisible(TRUE)
}

#' Function executed inside the background R process
#'
#' Kept as a standalone top-level function so `callr::r_bg` can serialise
#' its body without capturing an environment.
#'
#' @keywords internal
rank_worker_body <- function(project, cache_dir, progress_path, ranked_path, libpaths) {
  .libPaths(libpaths)
  library(screenllm)

  records  <- load_artefact(project, "records")
  criteria <- load_artefact(project, "criteria")
  ensemble <- load_artefact(project, "ensemble")

  n_records  <- nrow(records)
  n_models   <- length(ensemble$models)
  replicates <- ensemble$replicates
  total      <- n_records * n_models * replicates

  # Fixed timestamp so ETA is measured from the actual job start,
  # not from each throttled progress write.
  started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  saveRDS(list(
    status = "running", processed = 0L, total = total,
    started_at = started_at, error = NULL,
    current_model = NA_character_
  ), progress_path)

  # Wrap the ensemble backend so we can count completed scoring calls
  # without editing rank_records() itself.
  progress_counter <- new.env(parent = emptyenv())
  progress_counter$n <- 0L
  progress_counter$last_write <- Sys.time()

  original_score <- ensemble$backend$score_record
  wrapped <- ensemble
  wrapped$backend$score_record <- function(model, prompt, temperature) {
    out <- original_score(model, prompt, temperature)
    progress_counter$n <- progress_counter$n + 1L
    now <- Sys.time()
    # Throttle writes to at most 4/second to avoid disk thrash.
    if (as.numeric(now - progress_counter$last_write, units = "secs") > 0.25) {
      saveRDS(list(
        status = "running",
        processed = progress_counter$n,
        total = total,
        current_model = model,
        started_at = started_at,
        error = NULL
      ), progress_path)
      progress_counter$last_write <- now
    }
    out
  }

  ranked <- tryCatch(
    rank_records(
      records = records, criteria = criteria, ensemble = wrapped,
      cache_dir = cache_dir, verbose = FALSE
    ),
    error = function(e) {
      saveRDS(list(
        status = "error",
        processed = progress_counter$n,
        total = total,
        current_model = NA_character_,
        started_at = started_at,
        error = conditionMessage(e)
      ), progress_path)
      stop(e)
    }
  )

  saveRDS(ranked, ranked_path)
  saveRDS(list(
    status = "done", processed = total, total = total,
    current_model = NA_character_,
    started_at = started_at, error = NULL
  ), progress_path)
  invisible(TRUE)
}
