# Per-user project directory helpers.
#
# The full-workflow Shiny app persists everything the user produces
# (records, criteria, cache, ranked results, plan, decisions, report)
# under a fixed per-user data directory:
#
#   <tools::R_user_dir("screenllm", "data")>/projects/<project_name>/
#
# Each subdirectory is one "project" (typically one review). Users pick
# or create a project by name in the Setup tab.

#' Root data directory used by the Shiny app
#'
#' Resolves to the platform-appropriate per-user data location
#' (`tools::R_user_dir()`), created if it does not exist. Individual
#' projects live under `<root>/projects/<project_name>/`.
#'
#' @return Character. The absolute path of the root data directory.
#' @keywords internal
data_root <- function() {
  root <- tools::R_user_dir("screenllm", which = "data")
  # On a locked-down system (corporate laptop with a read-only or
  # redirected HOME, some Posit Cloud / RStudio Server images),
  # fs::dir_create() throws and every downstream call -- Setup tab
  # renders, project pickers, save_artefact -- dies with an opaque
  # permission error. Fall back to a session-local directory so
  # the app at least runs; projects won't persist across sessions,
  # but "app works, no persistence" beats "app doesn't launch".
  ok <- tryCatch({
    fs::dir_create(root, recurse = TRUE)
    fs::dir_create(fs::path(root, "projects"), recurse = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    fallback <- fs::path(tempdir(), "screenllm-data")
    fs::dir_create(fallback, recurse = TRUE)
    fs::dir_create(fs::path(fallback, "projects"), recurse = TRUE)
    if (!identical(getOption("screenllm.data_root_warned"), TRUE)) {
      cli::cli_alert_warning(
        "Could not write to {.path {root}}; using {.path {fallback}}. \\
         Projects will not persist across R sessions."
      )
      options(screenllm.data_root_warned = TRUE)
    }
    return(as.character(fallback))
  }
  as.character(root)
}

#' List existing project names
#'
#' @return Character vector of project names (immediate subdirectories of
#'   `<data_root()>/projects/`).
#' @export
list_projects <- function() {
  proj_root <- fs::path(data_root(), "projects")
  entries <- fs::dir_ls(proj_root, type = "directory")
  basename(as.character(entries))
}

#' Return the absolute path of a named project directory
#'
#' Creates the project directory if `create = TRUE` and it does not
#' already exist. Names are normalised to a filesystem-friendly slug
#' (spaces become underscores; unusual characters are dropped) so users
#' can enter freeform names.
#'
#' @param name Project name.
#' @param create Whether to create the directory if it does not exist.
#' @return Character. Absolute path.
#' @keywords internal
project_dir <- function(name, create = FALSE) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  slug <- slugify_project_name(name)
  path <- fs::path(data_root(), "projects", slug)
  if (create) {
    fs::dir_create(path, recurse = TRUE)
    fs::dir_create(fs::path(path, "cache"), recurse = TRUE)
  }
  as.character(path)
}

#' @keywords internal
slugify_project_name <- function(name) {
  name <- trimws(name)
  slug <- gsub("[^A-Za-z0-9_.-]+", "_", name)
  slug <- gsub("_+", "_", slug)
  slug <- gsub("^_|_$", "", slug)
  # A name written entirely in non-Latin script (Chinese, Arabic,
  # Cyrillic, etc.) used to collapse to "" and then to a shared
  # "unnamed_project" bucket -- two different Chinese-named projects
  # silently overwrote each other on disk. Append a short hash of
  # the original name so distinct inputs get distinct slugs.
  if (!nzchar(slug)) {
    slug <- paste0("project_", substr(digest::digest(name), 1, 10))
  }
  slug
}

# ---- Read/write helpers for artefacts inside a project ------------------

# Standard filenames inside a project directory.
.project_artefacts <- list(
  records          = "records.rds",
  records_csv      = "records.csv",
  criteria         = "criteria.rds",
  ensemble         = "ensemble.rds",
  ranked           = "ranked.rds",
  plan             = "plan.rds",
  decisions        = "decisions.csv",
  progress         = "rank_progress.rds",
  report           = "report.rds"
)

#' Save an artefact into a project directory
#'
#' Writes an object under a canonical filename inside the project
#' directory so all downstream tabs / modules can find it.
#'
#' @param project Project name.
#' @param artefact One of the canonical artefact names (see
#'   `list_project_artefacts()`).
#' @param x The object to save.
#' @return Invisibly, the path written to.
#' @export
save_artefact <- function(project, artefact, x) {
  fname <- match_artefact(artefact)
  path <- fs::path(project_dir(project, create = TRUE), fname)
  # Atomic write: temp file + rename. A crash mid-write (laptop lid
  # closed during autosave, R killed, machine shutdown) previously
  # left a truncated file that broke the next project load and greyed
  # out the Setup tab. Same pattern already used in rank.R for the
  # cache, generalised here.
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  if (grepl("\\.csv$", fname)) {
    utils::write.csv(x, tmp, row.names = FALSE, fileEncoding = "UTF-8")
  } else {
    saveRDS(x, tmp)
  }
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) {
    # Fallback for cross-filesystem renames (rare, but happens when
    # R_user_dir is on a network mount and tempfile is on local disk).
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(as.character(path))
}

#' Load an artefact from a project directory
#'
#' @param project Project name.
#' @param artefact One of the canonical artefact names.
#' @param default Value to return if the artefact does not exist.
#' @return The stored object, or `default`.
#' @export
load_artefact <- function(project, artefact, default = NULL) {
  fname <- match_artefact(artefact)
  path <- fs::path(project_dir(project, create = FALSE), fname)
  if (!fs::file_exists(path)) return(default)
  # A truncated / zero-byte artefact left by a crashed prior write
  # used to blow up load_project_into_state() and grey the Setup
  # tab. Return the default silently -- the same behaviour as a
  # missing file -- so the user gets a fresh slate instead of a
  # dead app.
  if (grepl("\\.csv$", fname)) {
    tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8"),
      error = function(e) default
    )
  } else {
    tryCatch(readRDS(path), error = function(e) default)
  }
}

#' Delete an artefact (or a whole project)
#'
#' @param project Project name.
#' @param artefact Either a canonical artefact name, or `"all"` to
#'   remove the entire project directory.
#' @return Invisibly, `TRUE`.
#' @export
delete_artefact <- function(project, artefact = "all") {
  if (identical(artefact, "all")) {
    fs::dir_delete(project_dir(project, create = FALSE))
    return(invisible(TRUE))
  }
  fname <- match_artefact(artefact)
  path <- fs::path(project_dir(project, create = FALSE), fname)
  if (fs::file_exists(path)) fs::file_delete(path)
  invisible(TRUE)
}

#' Clear cached LLM scores for a project
#'
#' Removes score-cache files under a project's cache directory. Use
#' this when a model returned garbage during a ranking run and you
#' want to re-score with fresh calls; the cache would otherwise be
#' hit on the next `rank_records()` invocation.
#'
#' By default clears every cached score (the most conservative
#' "start over" behaviour). Pass `model = "..."` to clear only that
#' model's cached scores across all records / replicates. Optionally
#' also deletes the persisted ranked artefact (`delete_ranked = TRUE`)
#' so a stale ranking doesn't linger.
#'
#' @param project Project name.
#' @param model Optional Ollama tag. When `NULL` (default), clears
#'   every cached score in the project. When supplied, only cached
#'   scores whose stored `$model` field matches are removed. Reading
#'   each cache file is O(n_files) but each file is tiny (~1 KB).
#' @param delete_ranked If `TRUE` (default), also removes the
#'   project's `ranked.rds` artefact so a re-run starts from a
#'   clean slate.
#' @return Invisibly, the number of cache files removed.
#' @export
#' @examples
#' \dontrun{
#' # Clear one model's cached scores after it misbehaved
#' clear_cache("my-review", model = "mistral:7b")
#'
#' # Nuclear option: clear everything for a project
#' clear_cache("my-review")
#' }
clear_cache <- function(project, model = NULL, delete_ranked = TRUE) {
  cache_dir <- project_cache_dir(project)
  if (!fs::dir_exists(cache_dir)) return(invisible(0L))
  files <- fs::dir_ls(cache_dir, glob = "*.rds")
  removed <- 0L
  if (is.null(model)) {
    if (length(files) > 0L) fs::file_delete(files)
    removed <- length(files)
  } else {
    for (f in files) {
      x <- tryCatch(readRDS(f), error = function(e) NULL)
      if (!is.null(x) && identical(as.character(x$model), model)) {
        fs::file_delete(f)
        removed <- removed + 1L
      }
    }
  }
  if (isTRUE(delete_ranked)) {
    ranked_path <- fs::path(project_dir(project, create = FALSE),
                             .project_artefacts$ranked)
    if (fs::file_exists(ranked_path)) fs::file_delete(ranked_path)
  }
  invisible(removed)
}

#' Canonical artefact filenames
#'
#' Returns the mapping from artefact key to filename used inside every
#' project directory. Exposed so power users can find files on disk
#' without loading the package.
#'
#' @return Named character vector.
#' @export
list_project_artefacts <- function() {
  unlist(.project_artefacts)
}

#' @keywords internal
match_artefact <- function(artefact) {
  choice <- match.arg(artefact, names(.project_artefacts))
  .project_artefacts[[choice]]
}

#' Return the cache directory for a project (used by `rank_records`)
#'
#' @param project Project name.
#' @return Character path.
#' @keywords internal
project_cache_dir <- function(project) {
  path <- fs::path(project_dir(project, create = TRUE), "cache")
  fs::dir_create(path, recurse = TRUE)
  as.character(path)
}
