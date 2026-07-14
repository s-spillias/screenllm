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
#' @export
data_root <- function() {
  root <- tools::R_user_dir("screenllm", which = "data")
  fs::dir_create(root, recurse = TRUE)
  fs::dir_create(fs::path(root, "projects"), recurse = TRUE)
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
#' @export
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
  slug <- gsub("[^A-Za-z0-9_.-]+", "_", trimws(name))
  slug <- gsub("_+", "_", slug)
  slug <- gsub("^_|_$", "", slug)
  if (!nzchar(slug)) slug <- "unnamed_project"
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
  report           = "report.rds",
  pilot            = "pilot.rds"
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
  if (grepl("\\.csv$", fname)) {
    utils::write.csv(x, path, row.names = FALSE)
  } else {
    saveRDS(x, path)
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
  if (grepl("\\.csv$", fname)) {
    utils::read.csv(path, stringsAsFactors = FALSE)
  } else {
    readRDS(path)
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
#' @export
project_cache_dir <- function(project) {
  path <- fs::path(project_dir(project, create = TRUE), "cache")
  fs::dir_create(path, recurse = TRUE)
  as.character(path)
}
