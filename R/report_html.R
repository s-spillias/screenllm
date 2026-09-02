#' Render the screening report as a self-contained HTML document
#'
#' Knits the packaged R Markdown template with whatever project
#' artefacts are supplied and returns the path to the rendered HTML
#' file. Open it in a browser to view; use the browser's
#' Print > Save as PDF to archive as PDF (this avoids the LaTeX install
#' that a direct PDF backend would need).
#'
#' All artefact arguments default to `NULL`; the template
#' fills in "(not recorded)" for anything missing, so the same call
#' works whether the reviewer is midway through screening or fully
#' finished.
#'
#' @param output_file Path to write the HTML report to. Defaults to a
#'   file in `tempdir()`.
#' @param project Optional project name (for the report header).
#' @param ranked Optional ranked corpus tibble.
#' @param plan Optional `screenllm_plan` object.
#' @param decisions Optional tibble of human decisions.
#' @param criteria Optional `screenllm_criteria` object.
#' @param ensemble Optional `screenllm_ensemble` object.
#' @return Invisibly, the path to the rendered HTML file.
#' @export
#' @examples
#' \dontrun{
#' path <- export_report(project = "my-review",
#'                       ranked = load_artefact("my-review", "ranked"),
#'                       plan = load_artefact("my-review", "plan"),
#'                       decisions = load_artefact("my-review", "decisions"))
#' utils::browseURL(path)
#' }
export_report <- function(output_file = NULL,
                          project = NULL,
                          ranked = NULL,
                          plan = NULL,
                          decisions = NULL,
                          criteria = NULL,
                          ensemble = NULL) {
  rlang::check_installed(
    c("rmarkdown", "knitr"),
    "to render the screening report."
  )
  template <- system.file("reports", "screening_report.Rmd",
                          package = "screenllm")
  if (!nzchar(template)) {
    cli::cli_abort(
      "Could not locate the report template inside the installed package."
    )
  }
  if (is.null(output_file)) {
    output_file <- tempfile(fileext = ".html")
  }
  # rmarkdown::render() writes next to the input by default, so copy the
  # template into a temp directory first to keep the installed package
  # tree untouched.
  work_dir <- tempfile("screenllm-report-")
  fs::dir_create(work_dir)
  work_rmd <- fs::path(work_dir, "screening_report.Rmd")
  fs::file_copy(template, work_rmd, overwrite = TRUE)

  rmarkdown::render(
    input = work_rmd,
    output_file = fs::path_file(output_file),
    output_dir = fs::path_dir(output_file),
    params = list(
      project = project,
      ranked = ranked,
      plan = plan,
      decisions = decisions,
      criteria = criteria,
      ensemble = ensemble
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  invisible(output_file)
}
