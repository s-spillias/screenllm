#' Define screening inclusion criteria
#'
#' Bundles the human-readable inclusion criteria a review uses into an
#' object that can be rendered into the paper's standard screening prompt.
#'
#' @param scope A one-sentence description of what the review is about.
#'   Used at the top of the LLM prompt so the model has orienting context.
#' @param inclusions A character vector of numbered inclusion criteria.
#'   Order matters — criterion 1 is scored first, criterion `n` last.
#' @param exclusion_notes Optional list of the same length as `inclusions`,
#'   giving per-criterion exclusion clarifications. `NULL` at any position
#'   means no notes for that criterion.
#' @param clarifications Optional character vector of general clarifications
#'   (e.g. definitions of terms) rendered as a block after the criteria.
#'   `NULL` (the default) omits the block.
#' @return An object of class `screenllm_criteria`.
#' @export
#' @examples
#' criteria <- define_criteria(
#'   scope = "Field-based coral reef restoration and performance",
#'   inclusions = c(
#'     "The study is conducted at a field-based coral reef restoration site.",
#'     "The study describes a project with an explicit restoration goal.",
#'     "The study describes an active restoration intervention.",
#'     "The study monitors at least one restoration-performance metric."
#'   )
#' )
#' print(criteria)
define_criteria <- function(scope,
                            inclusions,
                            exclusion_notes = NULL,
                            clarifications = NULL) {
  stopifnot(
    is.character(scope), length(scope) == 1L, nzchar(scope),
    is.character(inclusions), length(inclusions) >= 1L, all(nzchar(inclusions))
  )
  if (!is.null(exclusion_notes)) {
    if (length(exclusion_notes) != length(inclusions)) {
      cli::cli_abort(
        "`exclusion_notes` (if supplied) must have the same length as `inclusions`."
      )
    }
  }
  structure(
    list(
      scope = scope,
      inclusions = inclusions,
      exclusion_notes = exclusion_notes,
      clarifications = clarifications
    ),
    class = "screenllm_criteria"
  )
}

#' @export
format.screenllm_criteria <- function(x, ...) {
  out <- c(
    cli::col_grey(sprintf("<screenllm_criteria %d inclusions>", length(x$inclusions))),
    cli::style_bold("Scope: "),
    strwrap(x$scope, indent = 2L, exdent = 2L)
  )
  for (i in seq_along(x$inclusions)) {
    out <- c(out, sprintf("%d. %s", i, x$inclusions[[i]]))
    notes <- x$exclusion_notes[[i]]
    if (!is.null(notes)) {
      for (n in notes) out <- c(out, sprintf("   - Exclude: %s", n))
    }
  }
  if (!is.null(x$clarifications)) {
    out <- c(out, cli::style_bold("Clarifications:"), paste0("  - ", x$clarifications))
  }
  paste(out, collapse = "\n")
}

#' @export
print.screenllm_criteria <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

#' Build a screening prompt for one record
#'
#' Renders the standard prompt template shipped in `inst/prompts/standard.txt`
#' with the criteria and the record's title/abstract substituted in. Users
#' can inspect the returned prompt string to confirm the LLM is being asked
#' the right question, and power users can wrap or replace this function.
#'
#' @param criteria A `screenllm_criteria` object.
#' @param record A one-row data frame or named list with `id`, `title`,
#'   `abstract`.
#' @return A character string ready to be sent as an LLM prompt.
#' @export
build_prompt <- function(criteria, record) {
  stopifnot(inherits(criteria, "screenllm_criteria"))
  if (is.data.frame(record)) {
    if (nrow(record) != 1L) {
      cli::cli_abort("`record` must be a one-row data frame or a named list.")
    }
    record <- as.list(record)
  }
  needed <- c("id", "title", "abstract")
  missing <- setdiff(needed, names(record))
  if (length(missing) > 0L) {
    cli::cli_abort("`record` is missing field{?s}: {.val {missing}}")
  }

  template <- read_prompt_template()

  n_criteria <- length(criteria$inclusions)
  w_points <- round(100 / n_criteria)
  p20 <- round(w_points * 0.20)
  p40 <- round(w_points * 0.40)
  p80 <- round(w_points * 0.80)

  inclusions_rendered <- vapply(
    seq_along(criteria$inclusions),
    function(i) {
      line <- sprintf("%d. %s", i, criteria$inclusions[[i]])
      notes <- criteria$exclusion_notes[[i]]
      if (!is.null(notes)) {
        line <- paste(
          c(line, sprintf("   - Exclusion/boundary: %s", notes)),
          collapse = "\n"
        )
      }
      line
    },
    character(1)
  )

  clarifications_block <- if (is.null(criteria$clarifications)) {
    ""
  } else {
    paste0(
      "Clarifications:\n",
      paste(sprintf("- %s", criteria$clarifications), collapse = "\n")
    )
  }

  record_json <- jsonlite::toJSON(
    list(id = record$id, title = record$title, abstract = record$abstract),
    auto_unbox = TRUE, pretty = TRUE
  )

  glue::glue(
    template,
    SCOPE_SUMMARY = criteria$scope,
    INCLUSION_CRITERIA = paste(inclusions_rendered, collapse = "\n"),
    CLARIFICATIONS_BLOCK = clarifications_block,
    N_CRITERIA = n_criteria,
    W_POINTS = w_points,
    P20 = p20,
    P40 = p40,
    P80 = p80,
    RECORD_ID = record$id,
    RECORD_JSON = as.character(record_json),
    .open = "{{",
    .close = "}}",
    .trim = FALSE
  )
}

#' Load the ready-made criteria for the toy CBFM corpus
#'
#' Reads `inst/extdata/toy_cbfm_criteria.R` (which mirrors the CBFM
#' entry of the screening criteria used in Spillias et al. 2024,
#' Cell Reports Sustainability) and returns a `screenllm_criteria`
#' object ready to feed into `rank_records()`.
#'
#' Kept as a file the user can inspect / edit rather than
#' hard-coding the criteria in the package, so a colleague can
#' iterate on the demo criteria without touching R source.
#'
#' @return A `screenllm_criteria` object.
#' @export
#' @examples
#' criteria <- load_toy_cbfm_criteria()
#' criteria$scope
#' criteria$inclusions
load_toy_cbfm_criteria <- function() {
  path <- system.file("extdata", "toy_cbfm_criteria.R",
                      package = "screenllm")
  if (!nzchar(path) || !file.exists(path)) {
    dev_path <- fs::path_wd("screenllm", "inst", "extdata",
                            "toy_cbfm_criteria.R")
    if (file.exists(dev_path)) path <- dev_path
  }
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort("Could not locate toy_cbfm_criteria.R in the installed package.")
  }
  # Parent must be baseenv() (not emptyenv) so language primitives
  # such as `<-`, `list()`, `c()`, `paste0()` used by the sourced
  # file can be resolved. Using emptyenv() as parent would make the
  # source fail with "could not find function '<-'".
  env <- new.env(parent = baseenv())
  sys.source(path, envir = env, keep.source = FALSE)
  spec <- env$toy_cbfm_criteria
  if (is.null(spec)) {
    cli::cli_abort("toy_cbfm_criteria.R did not define `toy_cbfm_criteria`.")
  }
  define_criteria(scope = spec$scope, inclusions = spec$inclusions)
}

#' @keywords internal
read_prompt_template <- function() {
  path <- system.file("prompts", "standard.txt", package = "screenllm")
  if (!nzchar(path) || !file.exists(path)) {
    # During package development the installed copy may not exist yet;
    # fall back to the source tree.
    dev_path <- fs::path_wd("screenllm", "inst", "prompts", "standard.txt")
    if (file.exists(dev_path)) path <- dev_path
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}
