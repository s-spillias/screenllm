# Async model pulls.
#
# `pull_model()` is synchronous: on typical broadband a Mistral 7B pull
# takes 5-15 minutes, and Shiny is single-threaded, so a click on the
# pull button freezes the whole app until the download completes. The
# functions below run the pull in a background R process and expose a
# progress file the Shiny UI can poll.

#' Start a background pull for an Ollama model
#'
#' Spawns a `callr::r_bg` subprocess that streams progress from
#' Ollama's `/api/pull` endpoint into a progress file. The caller can
#' poll `pull_job_status(model)` for updates and `pull_job_cancel(handle)`
#' to abort.
#'
#' Idempotent per-model within one R session: if a pull is already
#' running for `model`, returns the existing handle instead of
#' launching another.
#'
#' @param model Ollama model tag (e.g. `"mistral:7b"`).
#' @param ollama_url Base URL of the Ollama server.
#' @return A list with `model`, `pid`, `handle`, and `progress_path`.
#' @export
start_pull_job <- function(model,
                           ollama_url = getOption("screenllm.ollama_url")) {
  rlang::check_installed("callr", "to run pulls in the background.")
  stopifnot(is.character(model), length(model) == 1L, nzchar(model))

  progress_path <- pull_progress_path(model)
  fs::dir_create(fs::path_dir(progress_path), recurse = TRUE)

  # Existing running job? Return its handle if we still have it.
  existing <- .pull_handles$get(model)
  if (!is.null(existing) && existing$handle$is_alive()) {
    return(existing)
  }

  saveRDS(list(
    model = model,
    status = "starting",
    completed = 0,
    total = NA_real_,
    detail = "",
    error = NULL,
    started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), progress_path)

  libpaths <- .libPaths()
  handle <- callr::r_bg(
    func = pull_worker_body,
    args = list(
      model = model,
      ollama_url = ollama_url,
      progress_path = progress_path,
      libpaths = libpaths
    ),
    supervise = FALSE
  )
  out <- list(model = model, pid = handle$get_pid(),
              handle = handle, progress_path = progress_path)
  .pull_handles$set(model, out)
  out
}

#' Current status of a background model pull
#'
#' @param model Ollama model tag.
#' @return A list with `status` (one of `"idle"`, `"starting"`,
#'   `"running"`, `"done"`, `"error"`), `completed`, `total`,
#'   `percent`, `detail`, `error`, and `elapsed_secs`. `status = "idle"`
#'   when there is no progress file for `model`.
#' @export
pull_job_status <- function(model) {
  path <- pull_progress_path(model)
  if (!fs::file_exists(path)) {
    return(list(status = "idle", percent = 0, model = model))
  }
  st <- readRDS(path)
  pct <- if (isTRUE(st$total > 0) && !is.na(st$completed)) {
    round(100 * st$completed / st$total, 1)
  } else 0
  elapsed <- if (!is.null(st$started_at)) {
    started <- parse_started_at(st$started_at)
    if (inherits(started, "POSIXct") && !is.na(started)) {
      as.numeric(Sys.time() - started, units = "secs")
    } else NA_real_
  } else NA_real_
  c(st, list(percent = pct, elapsed_secs = elapsed))
}

#' Cancel a running pull job
#'
#' @param handle Callr process object (from `start_pull_job()$handle`).
#' @return Invisibly, `TRUE`.
#' @export
pull_job_cancel <- function(handle) {
  if (!is.null(handle) && handle$is_alive()) handle$kill()
  invisible(TRUE)
}

# --------------------------------------------------------------------
# Worker body (runs in a separate R process).
#' @keywords internal
pull_worker_body <- function(model, ollama_url, progress_path, libpaths) {
  # Bootstrap error handler: if .libPaths / library() fails (user
  # upgraded R mid-session so the child picks up a libpath where the
  # package isn't installed), write an error to the progress file so
  # the Shiny UI shows something instead of hanging at "starting".
  tryCatch({
    .libPaths(libpaths)
    library(screenllm)
  }, error = function(e) {
    try(saveRDS(list(
      model = model, status = "error", completed = 0, total = NA_real_,
      detail = "failed",
      error = paste0("Pull worker startup failed: ", conditionMessage(e)),
      started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ), progress_path), silent = TRUE)
    stop(e)
  })

  # Ollama's /api/pull with stream=TRUE returns newline-delimited JSON:
  #   {"status":"pulling manifest"}
  #   {"status":"downloading digestsha256:xxxx","total":4109865600,"completed":123}
  #   {"status":"success"}
  # We parse each line and throttle progress writes to ~4/sec.
  write_state <- function(status, completed, total, detail, error = NULL) {
    saveRDS(list(
      model = model,
      status = status,
      completed = completed,
      total = total,
      detail = detail,
      error = error,
      started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ), progress_path)
  }

  last_write <- Sys.time()
  # Track whether Ollama actually confirmed the pull succeeded. The
  # transport ending cleanly (proxy timeout, socket reset, corpo net
  # cutting an idle connection, Ollama replying "model not found"
  # then closing) is NOT enough to call a pull done -- silently
  # promoting those to "done" left users thinking a 24 GB model was
  # pulled when only 30 MB had actually landed.
  saw_success <- FALSE
  stream_error <- NULL
  callback <- function(x) {
    lines <- strsplit(rawToChar(x), "\n", fixed = TRUE)[[1]]
    for (ln in lines) {
      if (!nzchar(ln)) next
      obj <- tryCatch(jsonlite::fromJSON(ln), error = function(e) NULL)
      if (is.null(obj)) next
      # Ollama reports pull errors in-band as {"error": "..."} with
      # no status field. Surface that to the progress file and stop.
      if (!is.null(obj$error) && nzchar(as.character(obj$error))) {
        stream_error <<- as.character(obj$error)
        write_state("error", 0, NA_real_,
                    detail = "failed", error = stream_error)
        return(FALSE)
      }
      status <- obj$status %||% "running"
      completed <- as.numeric(obj$completed %||% NA_real_)
      total <- as.numeric(obj$total %||% NA_real_)
      if (identical(status, "success")) {
        saw_success <<- TRUE
        write_state("done", total, total, detail = "complete")
        return(FALSE) # stop
      }
      now <- Sys.time()
      if (as.numeric(now - last_write, units = "secs") > 0.25) {
        write_state("running", completed, total, detail = status)
        last_write <<- now
      }
    }
    TRUE
  }

  ok <- tryCatch({
    httr2::request(paste0(ollama_url, "/api/pull")) |>
      httr2::req_body_json(list(name = model, stream = TRUE)) |>
      httr2::req_timeout(60 * 60) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform_stream(callback, buffer_kb = 16, round = "line")
    TRUE
  }, error = function(e) {
    write_state("error", 0, NA_real_,
                detail = "failed", error = conditionMessage(e))
    FALSE
  })

  # Only report "done" when Ollama itself said "success". A transport
  # that ended without error but never emitted the success chunk is
  # a truncated pull -- flag it so the caller doesn't proceed to
  # ranking with a partial model.
  if (!isTRUE(saw_success) && is.null(stream_error) && isTRUE(ok)) {
    write_state("error", 0, NA_real_, detail = "incomplete",
                error = paste0(
                  "Pull stream ended without a success marker. ",
                  "The model may not be fully installed -- try again, ",
                  "or run `ollama pull ", model, "` in a terminal."
                ))
    ok <- FALSE
  }
  invisible(ok)
}

# --------------------------------------------------------------------
# In-session registry of live pull handles, so a re-clicked pull button
# doesn't spawn a second worker for the same model.
.pull_handles <- local({
  reg <- list()
  list(
    get = function(model) reg[[model]],
    set = function(model, handle) {
      reg[[model]] <<- handle
      invisible(NULL)
    },
    clear = function(model) {
      reg[[model]] <<- NULL
      invisible(NULL)
    }
  )
})

# --------------------------------------------------------------------
# Progress-file location. Per-model file under R_user_dir("cache").
#' @keywords internal
pull_progress_path <- function(model) {
  slug <- gsub("[^A-Za-z0-9._-]", "_", model)
  fs::path(tools::R_user_dir("screenllm", "cache"),
           "pulls", paste0(slug, ".rds"))
}
