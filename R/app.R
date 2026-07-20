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

  # Friendly heads-up for first-time users who launch before setting
  # up Ollama. The app itself still opens (Setup tab will show
  # everything the user needs); this just points them at the
  # one-liner that automates the install.
  if (interactive() && !nzchar(find_ollama_binary())) {
    # `cli_alert_info` takes a single `text` argument (positional
    # extras land in `id`, `class`, `wrap` and crash with "argument
    # is not interpretable as logical"). Build one string first.
    cli::cli_alert_info(paste0(
      "Ollama is not installed on this machine yet. The Setup tab in ",
      "the app can help, or run {.code install_prereqs(preset = \"light\")} ",
      "from the R console to install Ollama and pull a laptop-friendly ",
      "ensemble of models (~10 GB)."
    ))
  }

  # Running R as root (sudo R, sudo Rscript) breaks X11 authorisation
  # for launching a browser and reroutes tools::R_user_dir() to
  # /root/... so any projects saved in the sudo session won't be
  # findable from the user's normal account. sudo is only needed for
  # the one-off Ollama install command inside `install_prereqs()`;
  # never for launch_app() itself. Refuse and explain, so the user
  # doesn't stare at a cryptic "cannot open display" or "no
  # authorization" message.
  if (identical(Sys.info()[["effective_user"]], "root") &&
        Sys.info()[["sysname"]] != "Windows") {
    cli::cli_abort(paste0(
      "R is running as root ({.field sudo R}). This breaks X11 for the ",
      "browser and reroutes project storage to {.path /root}, so it will ",
      "fail with confusing errors. ",
      "Exit sudo ({.code exit}) and run {.code launch_app()} from your ",
      "normal user account. If you need to install Ollama, call ",
      "{.code install_prereqs()} from that same normal session -- it will ",
      "prompt for sudo only for the specific install command."
    ))
  }

  # On Linux without DISPLAY (headless, SSH without X11 forwarding),
  # trying to launch a browser will error. Fall back to just
  # printing the URL so the user can open it themselves.
  if (isTRUE(launch_browser) &&
        identical(Sys.info()[["sysname"]], "Linux") &&
        !nzchar(Sys.getenv("DISPLAY"))) {
    cli::cli_alert_info(paste0(
      "No DISPLAY detected (running headless / over SSH). ",
      "The app URL will be printed below -- open it in a browser on your ",
      "local machine (with SSH port forwarding if needed)."
    ))
    launch_browser <- FALSE
  }

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
    # Selectize's default dropdown caps at ~200px, which on a busy
    # user's project list would hide anything past project #6 or 7
    # behind an internal scroll. Lift the cap so the whole list is
    # visible when the popup opens, and let the picker itself be
    # taller when the user opens it. Applies to every selectize
    # instance in the app (project picker, model pull, etc.).
    shiny::tags$head(shiny::tags$style(shiny::HTML(
      ".selectize-dropdown-content { max-height: 400px !important; }"
    ))),
    bslib::nav_panel("1. Setup", mod_setup_ui("setup")),
    bslib::nav_panel("2. Corpus", mod_corpus_ui("corpus")),
    bslib::nav_panel("3. Criteria", mod_criteria_ui("criteria")),
    bslib::nav_panel("4. Rank", mod_rank_ui("rank")),
    bslib::nav_panel("5. Plan", mod_plan_ui("plan")),
    bslib::nav_panel("6. Screen", mod_screen_ui("screen")),
    bslib::nav_panel("7. Report", mod_report_ui("report")),
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

  # The Setup module owns "load a project's artefacts" so it can also
  # restore Setup-tab UI state (ensemble radio + custom checkboxes)
  # from the saved ensemble. We just set state$project here; the Setup
  # module observes it and does the rest.

  output$project_badge <- shiny::renderText({
    if (is.null(state$project)) "no project" else paste0("project: ", state$project)
  })

  mod_setup_server("setup", state = state)
  mod_corpus_server("corpus", state = state)
  mod_criteria_server("criteria", state = state)
  mod_rank_server("rank", state = state)
  mod_plan_server("plan", state = state)
  mod_screen_server("screen", state = state)
  mod_report_server("report", state = state)
}
