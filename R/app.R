#' Launch the full end-to-end Shiny app
#'
#' Opens a seven-tab wizard that walks the user from Ollama setup
#' through corpus upload, criteria definition, ensemble ranking, SAFE
#' stopping, human screening, and reporting. Everything the app produces
#' is persisted to the per-user data directory
#' (`data_root()`) so a browser crash never loses more than one decision.
#'
#' @param project Optional project name to open on launch. If `NULL`,
#'   the Setup tab lets the user pick or create one.
#' @param launch_browser Passed to `shiny::runApp()`.
#' @return Invisibly, `NULL`.
#' @export
launch_app <- function(project = NULL, launch_browser = interactive()) {
  rlang::check_installed(c("shiny", "bslib", "DT"), "to launch the full app.")

  ui <- app_ui()
  server <- function(input, output, session) {
    app_server(input, output, session, initial_project = project)
  }
  shiny::shinyApp(ui, server, options = list(launch.browser = launch_browser))
}

#' @keywords internal
app_ui <- function() {
  bslib::page_navbar(
    title = "screenllm",
    id = "main_navbar",
    theme = bslib::bs_theme(preset = "shiny"),
    fillable = TRUE,
    bslib::nav_panel("1. Setup", mod_setup_ui("setup")),
    bslib::nav_panel("2. Corpus", mod_corpus_ui("corpus")),
    bslib::nav_panel("3. Criteria", mod_criteria_ui("criteria")),
    bslib::nav_panel("4. Pilot", mod_pilot_ui("pilot")),
    bslib::nav_panel("5. Rank", mod_rank_ui("rank")),
    bslib::nav_panel("6. Plan", mod_plan_ui("plan")),
    bslib::nav_panel("7. Screen", mod_screen_ui("screen")),
    bslib::nav_panel("8. Report", mod_report_ui("report")),
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::textOutput("project_badge", inline = TRUE)
    )
  )
}

#' @keywords internal
app_server <- function(input, output, session, initial_project = NULL) {
  # Reactive project state shared across every module.
  state <- shiny::reactiveValues(
    project = initial_project,
    records = NULL,
    criteria = NULL,
    ensemble = NULL,
    ranked = NULL,
    plan = NULL,
    decisions = NULL,
    rank_handle = NULL
  )

  # Rehydrate whatever artefacts are already on disk for the initial
  # project, so a returning user immediately sees their prior work.
  if (!is.null(initial_project) && initial_project %in% list_projects()) {
    state$records <- load_artefact(initial_project, "records")
    state$criteria <- load_artefact(initial_project, "criteria")
    state$ensemble <- load_artefact(initial_project, "ensemble")
    state$ranked <- load_artefact(initial_project, "ranked")
    state$plan <- load_artefact(initial_project, "plan")
    state$decisions <- load_artefact(initial_project, "decisions")
  }

  output$project_badge <- shiny::renderText({
    if (is.null(state$project)) "no project" else paste0("project: ", state$project)
  })

  mod_setup_server("setup", state = state)
  mod_corpus_server("corpus", state = state)
  mod_criteria_server("criteria", state = state)
  mod_pilot_server("pilot", state = state)
  mod_rank_server("rank", state = state)
  mod_plan_server("plan", state = state)
  mod_screen_server("screen", state = state)
  mod_report_server("report", state = state)
}
