#' Rank a corpus with an LLM ensemble
#'
#' Sends every record to every model in the ensemble at every replicate,
#' aggregates the resulting scores per record, and returns the corpus with
#' the aggregated score and rank attached. Progress is displayed with `cli`.
#' All intermediate scores are cached to disk so an interrupted run can be
#' resumed by calling `rank_records()` again with the same `cache_dir`.
#'
#' @param records A tibble of records (produced by `read_records()`).
#' @param criteria A `screenllm_criteria` object.
#' @param ensemble A `screenllm_ensemble` object. Defaults to
#'   `default_ensemble()`.
#' @param cache_dir Directory to write per-call cache files. If `NULL`, a
#'   temporary directory is used and the results are not persisted between
#'   R sessions.
#' @param verbose Show progress messages and bars.
#' @param max_workers Not yet used; reserved for future concurrency.
#' @param on_score Optional callback invoked once per (record, model,
#'   replicate) tuple after each score is available (whether fresh or
#'   restored from cache). Signature:
#'   `function(id, model, replicate, score, explanation, error,
#'   index, total)`. Used by the Shiny app's ranking module to stream
#'   partial scores into the UI as they land; safe to leave `NULL`.
#' @return The input tibble with added columns:
#'   `universal_best_score`, `rank`, `per_model_scores` (list-column),
#'   `justifications` (list-column of per-criterion rationales as returned
#'   by the LLM).
#' @export
#' @examples
#' \donttest{
#' # A no-Ollama, in-R demo using the mock backend.
#' records <- data.frame(
#'   id = c("a", "b"),
#'   title = c("Coral reef restoration outcomes",
#'             "Deep-sea mining impacts on benthic fauna"),
#'   abstract = c("We monitored transplanted corals for 12 months...",
#'                "This modelling study estimates long-term sediment plumes...")
#' )
#' criteria <- define_criteria(
#'   scope = "Field-based coral reef restoration and performance",
#'   inclusions = c("Study is field-based.", "Study describes an intervention.")
#' )
#' ens <- default_ensemble(backend = backend_mock())
#' ranked <- rank_records(records, criteria, ensemble = ens, verbose = FALSE)
#' ranked[, c("id", "universal_best_score", "rank")]
#' }
rank_records <- function(records,
                         criteria,
                         ensemble = default_ensemble(),
                         cache_dir = getOption("screenllm.cache_dir"),
                         verbose = getOption("screenllm.verbose", TRUE),
                         max_workers = getOption("screenllm.max_workers", 1L),
                         on_score = NULL) {
  stopifnot(
    is.data.frame(records),
    inherits(criteria, "screenllm_criteria"),
    inherits(ensemble, "screenllm_ensemble")
  )
  needed <- c("id", "title", "abstract")
  missing <- setdiff(needed, names(records))
  if (length(missing) > 0L) {
    cli::cli_abort("`records` is missing column{?s}: {.val {missing}}")
  }
  # Belt-and-braces: coerce id to character here as well as in
  # read_records(). A numeric id column (e.g. from a CSV that used
  # integer row numbers) would break vapply(character(1)) in the
  # aggregation step at the very END of a run -- wasting hours of
  # LLM scoring for a type mismatch.
  records$id <- as.character(records$id)

  if (is.null(cache_dir)) {
    cache_dir <- fs::path(tempdir(), "screenllm-cache")
  }
  fs::dir_create(cache_dir, recurse = TRUE)
  criteria_hash <- digest::digest(criteria)

  # Drop any pre-existing output columns so the join at the end does not
  # duplicate them with .x/.y suffixes.
  reserved <- c("universal_best_score", "rank", "per_model_scores", "justifications")
  records <- records[, setdiff(names(records), reserved), drop = FALSE]

  n_records <- nrow(records)
  jobs <- expand.grid(
    id = records$id,
    model = ensemble$models,
    replicate = seq_len(ensemble$replicates),
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )
  # Order by (model, replicate, id) so Ollama serves one model at a time and
  # only reloads model weights between replicates or ensemble members.
  jobs <- jobs[order(jobs$model, jobs$replicate, jobs$id), , drop = FALSE]

  if (verbose) {
    cli::cli_h2("Ranking {n_records} records with {length(ensemble$models)} model{?s}, r = {ensemble$replicates}")
    cli::cli_alert_info("Cache: {.path {cache_dir}}")
  }

  pb <- if (verbose) {
    cli::cli_progress_bar("Scoring", total = nrow(jobs), .envir = environment())
  } else {
    NULL
  }

  cached_hits <- 0L
  fresh_hits <- 0L
  scores <- vector("list", nrow(jobs))
  for (i in seq_len(nrow(jobs))) {
    row <- jobs[i, ]
    cache_key <- digest::digest(list(
      criteria_hash, row$model, row$replicate, row$id,
      ensemble$temperature
    ))
    cache_path <- fs::path(cache_dir, paste0(cache_key, ".rds"))
    out <- NULL
    if (fs::file_exists(cache_path)) {
      # A cache file left truncated / zero-byte by an interrupted
      # write (crash, kill, machine restart mid-saveRDS) would abort
      # readRDS with "error reading from connection". Tolerate it:
      # drop the bad file and fall through to a fresh score.
      out <- tryCatch(readRDS(cache_path), error = function(e) NULL)
      if (is.null(out)) {
        try(fs::file_delete(cache_path), silent = TRUE)
      } else {
        cached_hits <- cached_hits + 1L
      }
    }
    if (is.null(out)) {
      rec <- records[records$id == row$id, ][1, ]
      prompt <- build_prompt(criteria, rec)
      out <- ensemble$backend$score_record(
        model = row$model,
        prompt = prompt,
        temperature = ensemble$temperature
      )
      out$id <- row$id
      out$model <- row$model
      out$replicate <- row$replicate
      # Atomic write: save to a tmp file then rename. If we get
      # killed between the two lines the real cache_path is
      # untouched, so the next run will treat this as a cache miss
      # rather than seeing a truncated file.
      save_rds_atomic(out, cache_path)
      fresh_hits <- fresh_hits + 1L
    }
    scores[[i]] <- out
    if (!is.null(pb)) cli::cli_progress_update(id = pb)
    if (!is.null(on_score)) {
      tryCatch(
        on_score(
          id = row$id, model = row$model, replicate = row$replicate,
          score = as.numeric(out$score %||% NA_real_),
          explanation = out$explanation %||% NA_character_,
          error = out$error %||% NA_character_,
          index = i, total = nrow(jobs)
        ),
        error = function(e) {
          # A broken callback should not abort the ranking run.
          if (verbose) cli::cli_alert_warning(
            "on_score callback failed: {conditionMessage(e)}"
          )
        }
      )
    }
  }
  if (!is.null(pb)) cli::cli_progress_done(id = pb)

  if (verbose) {
    cli::cli_alert_info(
      "Scored {fresh_hits} record-model-replicate combo{?s} fresh; \\
       {cached_hits} cached."
    )
  }

  # Build a long tibble and aggregate per record. Any single cache
  # entry missing a field (older package version, malformed backend
  # response persisted, etc.) would have crashed the whole vapply --
  # discarding hours of scoring right at the aggregation step. Use
  # a per-element accessor that returns NA on a missing/wrong-length
  # value instead.
  # Use base coercers directly rather than methods::as() so we don't
  # need to depend on the methods S4 machinery for a tiny helper.
  pick <- function(s, key, type, default) {
    v <- s[[key]]
    if (is.null(v) || length(v) != 1L) return(default)
    tryCatch(
      switch(type,
             character = as.character(v),
             integer   = as.integer(v),
             numeric   = as.numeric(v),
             default),
      error = function(e) default,
      warning = function(w) default
    )
  }
  long <- tibble::tibble(
    id = vapply(scores, function(s) pick(s, "id", "character", NA_character_),
                character(1)),
    model = vapply(scores, function(s) pick(s, "model", "character", NA_character_),
                   character(1)),
    replicate = vapply(scores, function(s) pick(s, "replicate", "integer", NA_integer_),
                       integer(1)),
    score = vapply(scores, function(s) pick(s, "score", "numeric", NA_real_),
                   numeric(1)),
    explanation = vapply(scores,
                         function(s) pick(s, "explanation", "character", NA_character_),
                         character(1)),
    error = vapply(scores, function(s) pick(s, "error", "character", NA_character_),
                   character(1))
  )
  # Drop any all-NA-id rows: those are cache files whose id field
  # went missing, and we can't join them back to a record anyway.
  n_orphan <- sum(is.na(long$id))
  if (n_orphan > 0L) {
    if (verbose) cli::cli_alert_warning(
      "Dropped {n_orphan} score{?s} with missing id (likely from an older \\
       cache; safe to ignore, or clear the cache to remove the warning)."
    )
    long <- long[!is.na(long$id), , drop = FALSE]
  }

  # Warn loudly if a substantial fraction of calls failed. Silence is
  # dangerous here: the previous behaviour let "every model returned
  # HTTP 404" complete as a successful ranking with all-NA scores,
  # which then broke downstream (plan_screening, etc.).
  n_failed <- sum(is.na(long$score))
  n_total <- nrow(long)
  fail_rate <- if (n_total > 0) n_failed / n_total else 0
  if (fail_rate > 0.1) {
    # Grab the first distinct error message so the user sees the cause.
    errs <- unique(long$error[!is.na(long$error)])
    hint <- if (length(errs) > 0L) sprintf(" First error: %s.", errs[1]) else ""
    cli::cli_warn(c(
      sprintf("%d of %d LLM calls (%.0f%%) failed and returned NA.",
              n_failed, n_total, 100 * fail_rate),
      # NB: sprintf takes ONE format string. Concatenate the message
      # into a single format before passing values, otherwise the
      # second string is treated as a value and `hint` never appears.
      "i" = sprintf(
        paste0(
          "This usually means the model isn't installed on Ollama, ",
          "the daemon is unreachable, or the model returns malformed JSON.%s"
        ),
        hint
      )
    ))
  }

  agg <- long |>
    dplyr::group_by(.data$id) |>
    dplyr::summarise(
      universal_best_score = aggregate_scores(
        .data$score, ensemble$aggregator
      ),
      per_model_scores = list(dplyr::pick("model", "replicate", "score")),
      justifications = list(dplyr::pick("model", "replicate", "explanation")),
      .groups = "drop"
    )

  out <- records |>
    dplyr::left_join(agg, by = "id") |>
    dplyr::arrange(dplyr::desc(.data$universal_best_score)) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::relocate(
      dplyr::any_of(c("id", "title", "abstract", "universal_best_score", "rank"))
    )

  attr(out, "ensemble") <- ensemble
  attr(out, "criteria_hash") <- criteria_hash
  attr(out, "cache_dir") <- cache_dir
  class(out) <- c("screenllm_ranking", class(out))
  out
}

#' @importFrom rlang .data
NULL

# Atomic saveRDS: write to a per-process tmp path then rename over
# the target. `file.rename` is atomic on the same filesystem, so
# even if the process is killed between the two calls the target
# either doesn't exist or contains the last-good snapshot. Prevents
# the "0-byte cache file after a crash" class of bug that turns up
# as "error reading from connection" on the next run.
#' @keywords internal
save_rds_atomic <- function(x, path) {
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  saveRDS(x, tmp)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) {
    # Fallback: rename can fail across filesystems. Copy + delete.
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(path)
}
