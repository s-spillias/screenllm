#' Launch the interactive screening Shiny app
#'
#' Opens a browser-based UI that walks the reviewer through the records
#' at or above the SAFE stop point. Each record is presented with title,
#' abstract, LLM ensemble score, and the per-criterion justifications.
#' Decisions are appended to `out_file` after every click so that a browser
#' crash never loses more than one decision.
#'
#' @param plan A `screenllm_plan` object.
#' @param ranked The full ranking (needed for the per-criterion
#'   justifications).
#' @param out_file Path where decisions are written (`.csv` or `.xlsx`).
#' @param launch_browser Passed to `shiny::runApp()`.
#' @return Invisibly, the path to `out_file`.
#' @export
launch_screening_app <- function(plan, ranked,
                                 out_file = "screening_decisions.csv",
                                 launch_browser = interactive()) {
  rlang::check_installed(c("shiny", "bslib", "DT"), "to launch the screening app.")
  stopifnot(inherits(plan, "screenllm_plan"), is.data.frame(ranked))
  app_dir <- system.file("shiny", "screen_app", package = "screenllm")
  if (!nzchar(app_dir)) {
    app_dir <- fs::path_wd("screenllm", "inst", "shiny", "screen_app")
  }
  if (!fs::dir_exists(app_dir)) {
    cli::cli_abort("Cannot locate the Shiny app directory: {.path {app_dir}}")
  }
  # Pass state via a package-scope environment the app can source.
  .screenllm_shiny_state$plan <- plan
  .screenllm_shiny_state$ranked <- ranked
  .screenllm_shiny_state$out_file <- out_file
  shiny::runApp(app_dir, launch.browser = launch_browser)
  invisible(out_file)
}

# Environment used to hand off state to the Shiny app.
.screenllm_shiny_state <- new.env(parent = emptyenv())
