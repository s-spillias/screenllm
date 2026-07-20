# Parse a "started_at" timestamp string produced by
# format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"). Base R's `as.POSIXct`
# uses a locale-dependent heuristic that on some Windows builds
# fails to recognise the `+HHMM` timezone suffix and silently
# returns NA -- which made the elapsed/ETA display stay blank for
# the whole run. Try the exact ISO-8601-with-numeric-offset format
# first, then fall back to no-offset, then to whatever the heuristic
# picks up. Always returns a POSIXct or NA of that class.
#' @keywords internal
parse_started_at <- function(x) {
  if (is.null(x) || !nzchar(x)) return(as.POSIXct(NA))
  # Format with numeric timezone offset (`%z`).
  t <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S%z"))
  if (!is.na(t)) return(t)
  # Same shape without the offset (some locales emit "%z" as empty).
  t <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S"))
  if (!is.na(t)) return(t)
  # Anything else: last-resort heuristic parse. Base R's method
  # throws on genuinely unparseable input, so catch that too.
  tryCatch(suppressWarnings(as.POSIXct(x)),
           error = function(e) as.POSIXct(NA))
}

# Async ranking helpers.
#
# `rank_records()` is long-running (hours on a real corpus), and Shiny's
# default execution model would freeze the UI for the whole run. We wrap
# it in a background R process so the Shiny app can call `start_rank_job()`,
# return immediately, and poll `rank_job_status()` to update a progress bar
# and stream per-record scores as they land.
#
# We deliberately avoid `future`/`promises` here because they can hit
# thorny issues around global capture and library reload inside Shiny.
# Instead we use `callr::r_bg`, which spawns a plain R subprocess whose
# stderr we can tail, and a small progress file the subprocess writes to.

#' Start a background ranking job for a project
#'
#' Spawns a `callr::r_bg` subprocess that loads the project's records,
#' criteria, and ensemble from disk and runs `rank_records()`. Returns
#' immediately with a handle the caller can poll or cancel.
#'
#' Optionally sub-samples the records so the worker only ranks `n`
#' of them. This is the mechanism the Shiny app uses for "quick pilot"
#' runs against a real Ollama backend: same code path as a full rank,
#' just fewer records.
#'
#' @param project Project name.
#' @param ensemble A `screenllm_ensemble` object. Saved into the project
#'   directory so the worker can read it.
#' @param sample_size Optional integer. If supplied and less than the
#'   number of records, the worker samples `sample_size` records
#'   before ranking. `NULL` (default) or `0` = all records.
#' @param random_sample If sampling, whether to draw at random
#'   (default `TRUE`) or take the first `sample_size` rows.
#' @param seed Random seed used when `random_sample = TRUE`.
#' @param force If `TRUE`, skip the concurrent-job safety check
#'   (which aborts when the project's progress file was updated
#'   within the last 60 s, suggesting another rank worker is live).
#'   Use when you're sure the previous run is dead -- e.g. after a
#'   force-quit R session -- and want to take over the project.
#' @return A list with `pid` (the worker PID) and `handle` (the callr
#'   process object).
#' @export
start_rank_job <- function(project,
                           ensemble = default_ensemble(),
                           sample_size = NULL,
                           random_sample = TRUE,
                           seed = 1L,
                           force = FALSE) {
  rlang::check_installed("callr", "to run ranking in the background.")

  # A second Shiny session on the same project would clobber the
  # shared worker files (records artefact + progress rds) and both
  # status polls would then read garbage. Detect a still-running
  # job by looking at the progress file's status + freshness of its
  # last write. If mtime is within the throttle interval used by the
  # worker (< 60s), it's genuinely alive; if it's older, we assume
  # the previous run died (crash, force-quit) and take over silently.
  # Pass force = TRUE to skip this check.
  if (!isTRUE(force)) {
    st_prev <- load_artefact(project, "progress")
    prog_path <- fs::path(project_dir(project, create = FALSE),
                          .project_artefacts$progress)
    if (!is.null(st_prev) &&
          identical(st_prev$status, "running") &&
          fs::file_exists(prog_path)) {
      mtime <- as.numeric(Sys.time() - fs::file_info(prog_path)$modification_time,
                          units = "secs")
      if (!is.na(mtime) && mtime < 60) {
        cli::cli_abort(c(
          "A ranking job for project {.val {project}} is already running.",
          "i" = "Progress file was updated {round(mtime)}s ago.",
          "i" = paste0(
            "If you're sure it's dead (killed R session, another ",
            "machine, etc.), pass `force = TRUE` to take over."
          )
        ))
      }
    }
  }

  records <- load_artefact(project, "records")
  criteria <- load_artefact(project, "criteria")
  if (is.null(records)) {
    cli::cli_abort("Cannot start job: no records saved in project {.val {project}}.")
  }
  if (is.null(criteria)) {
    cli::cli_abort("Cannot start job: no criteria saved in project {.val {project}}.")
  }
  save_artefact(project, "ensemble", ensemble)

  # Resolve the effective sample. NULL / 0 / >= nrow means all.
  n_target <- if (is.null(sample_size) || is.na(sample_size) ||
                    as.integer(sample_size) <= 0L) {
    nrow(records)
  } else {
    min(as.integer(sample_size), nrow(records))
  }
  if (n_target < nrow(records)) {
    if (isTRUE(random_sample)) {
      set.seed(seed)
      records <- records[sort(sample.int(nrow(records), n_target)), , drop = FALSE]
    } else {
      records <- records[seq_len(n_target), , drop = FALSE]
    }
  }

  # Persist the (possibly subsampled) records the worker will use.
  # Written to a per-job side file so we don't clobber the project's
  # canonical records artefact.
  proj_dir <- project_dir(project, create = TRUE)
  worker_records_path <- fs::path(proj_dir, "rank_job_records.rds")
  saveRDS(records, worker_records_path)

  # Initialise the progress file so pollers get a defined state even
  # before the worker writes its first update.
  save_artefact(project, "progress", list(
    status = "starting",
    processed = 0L,
    total = nrow(records) * length(ensemble$models) * ensemble$replicates,
    n_records = nrow(records),
    started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    error = NULL,
    current_model = NA_character_,
    scores = list()
  ))

  cache_dir <- project_cache_dir(project)
  progress_path <- fs::path(proj_dir, .project_artefacts$progress)
  ranked_path <- fs::path(proj_dir, .project_artefacts$ranked)
  libpaths <- .libPaths()

  handle <- callr::r_bg(
    func = rank_worker_body,
    args = list(
      records_path = worker_records_path,
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
#'   `eta_secs`, `scores` (a per-call data.frame of everything scored
#'   so far), and optionally `error` and `elapsed_secs`.
#' @export
rank_job_status <- function(project) {
  st <- load_artefact(project, "progress")
  if (is.null(st)) {
    return(list(status = "idle", processed = 0L, total = NA_integer_,
                percent = 0, current_model = NA_character_,
                eta_secs = NA_real_, scores = list()))
  }
  pct <- if (isTRUE(st$total > 0)) round(100 * st$processed / st$total, 1) else 0
  elapsed <- if (!is.null(st$started_at)) {
    started <- parse_started_at(st$started_at)
    if (inherits(started, "POSIXct") && !is.na(started)) {
      as.numeric(Sys.time() - started, units = "secs")
    } else NA_real_
  } else NA_real_
  # ETA = remaining_calls / rate_calls_per_sec.
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
rank_worker_body <- function(records_path, project, cache_dir, progress_path,
                             ranked_path, libpaths) {
  # Bootstrap error handler: nothing between here and the definition
  # of write_state() below writes to the progress file, so any early
  # failure (library() fails after an R upgrade, readRDS chokes on
  # a partial file, load_artefact() bombs on a corrupt project) used
  # to leave the UI stuck at "starting" forever. Write a minimal
  # error record directly so the user can see what went wrong.
  bootstrap_fail <- function(e) {
    try(saveRDS(list(
      status = "error",
      processed = 0L,
      total = NA_integer_,
      error = paste0("Worker startup failed: ", conditionMessage(e)),
      started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ), progress_path), silent = TRUE)
    stop(e)
  }
  init <- tryCatch({
    .libPaths(libpaths)
    library(screenllm)
    list(
      records  = readRDS(records_path),
      criteria = load_artefact(project, "criteria"),
      ensemble = load_artefact(project, "ensemble")
    )
  }, error = bootstrap_fail)
  records  <- init$records
  criteria <- init$criteria
  ensemble <- init$ensemble
  rm(init)

  n_records  <- nrow(records)
  n_models   <- length(ensemble$models)
  replicates <- ensemble$replicates
  total      <- n_records * n_models * replicates

  # Fixed timestamp so ETA is measured from the actual job start,
  # not from each throttled progress write.
  started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

  # Streaming state. `scores` accumulates every per-call result as
  # rank_records() completes each (model, replicate, record) tuple.
  # We throttle progress-file writes to ~4/sec so a 4000-call run
  # doesn't hammer the disk.
  ctx <- new.env(parent = emptyenv())
  ctx$processed <- 0L
  ctx$current_model <- NA_character_
  ctx$scores_id <- character()
  ctx$scores_model <- character()
  ctx$scores_replicate <- integer()
  ctx$scores_score <- numeric()
  ctx$scores_explanation <- character()
  ctx$last_write <- Sys.time() - 1

  # Save the record metadata the UI needs to display partial results
  # (title, abstract) alongside per-call scores. Keep it small: only
  # the columns we actually use.
  record_meta <- tibble::tibble(
    id = as.character(records$id),
    title = as.character(records$title),
    abstract = if ("abstract" %in% names(records))
      as.character(records$abstract) else NA_character_
  )

  write_state <- function(status = "running", error = NULL, force = FALSE) {
    now <- Sys.time()
    if (!force &&
          as.numeric(now - ctx$last_write, units = "secs") < 0.25) {
      return(invisible())
    }
    scores_df <- data.frame(
      id = ctx$scores_id,
      model = ctx$scores_model,
      replicate = ctx$scores_replicate,
      score = ctx$scores_score,
      explanation = ctx$scores_explanation,
      stringsAsFactors = FALSE
    )
    saveRDS(list(
      status = status,
      processed = ctx$processed,
      total = total,
      n_records = n_records,
      current_model = ctx$current_model,
      started_at = started_at,
      error = error,
      scores = scores_df,
      record_meta = record_meta,
      ensemble_models = ensemble$models,
      ensemble_replicates = replicates,
      aggregator = ensemble$aggregator
    ), progress_path)
    ctx$last_write <- now
  }

  # Initial "running" state.
  write_state(force = TRUE)

  on_score_cb <- function(id, model, replicate, score, explanation, error,
                          index, total) {
    ctx$processed <- as.integer(index)
    ctx$current_model <- as.character(model)
    ctx$scores_id <- c(ctx$scores_id, as.character(id))
    ctx$scores_model <- c(ctx$scores_model, as.character(model))
    ctx$scores_replicate <- c(ctx$scores_replicate, as.integer(replicate))
    ctx$scores_score <- c(ctx$scores_score, as.numeric(score))
    ctx$scores_explanation <- c(ctx$scores_explanation,
                                 as.character(explanation))
    write_state()
  }

  ranked <- tryCatch(
    rank_records(
      records = records, criteria = criteria, ensemble = ensemble,
      cache_dir = cache_dir, verbose = FALSE,
      on_score = on_score_cb
    ),
    error = function(e) {
      write_state(status = "error", error = conditionMessage(e), force = TRUE)
      stop(e)
    }
  )

  saveRDS(ranked, ranked_path)
  write_state(status = "done", force = TRUE)
  invisible(TRUE)
}
