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
                         max_workers = getOption("screenllm.max_workers", 1L)) {
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
    if (fs::file_exists(cache_path)) {
      out <- readRDS(cache_path)
      cached_hits <- cached_hits + 1L
    } else {
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
      saveRDS(out, cache_path)
      fresh_hits <- fresh_hits + 1L
    }
    scores[[i]] <- out
    if (!is.null(pb)) cli::cli_progress_update(id = pb)
  }
  if (!is.null(pb)) cli::cli_progress_done(id = pb)

  if (verbose) {
    cli::cli_alert_info(
      "Scored {fresh_hits} record-model-replicate combo{?s} fresh; \\
       {cached_hits} cached."
    )
  }

  # Build a long tibble and aggregate per record.
  long <- tibble::tibble(
    id = vapply(scores, function(s) s$id, character(1)),
    model = vapply(scores, function(s) s$model, character(1)),
    replicate = vapply(scores, function(s) s$replicate, integer(1)),
    score = vapply(scores, function(s) as.numeric(s$score), numeric(1)),
    explanation = vapply(
      scores,
      function(s) if (is.null(s$explanation)) NA_character_ else as.character(s$explanation),
      character(1)
    )
  )

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
