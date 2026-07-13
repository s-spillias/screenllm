# Setup tab: pick/create project, verify Ollama, choose ensemble.

#' @keywords internal
mod_setup_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    # -- LEFT: project picker ------------------------------------------------
    bslib::card(
      bslib::card_header("Project"),
      bslib::card_body(
        shiny::selectizeInput(
          ns("project_select"), "Existing project:",
          choices = NULL, options = list(placeholder = "(none)")
        ),
        shiny::textInput(ns("new_project"), "Or create a new project:",
                         placeholder = "e.g. my-review-2026"),
        shiny::actionButton(ns("create_project"), "Create / select",
                            class = "btn-primary"),
        shiny::hr(),
        shiny::helpText(shiny::em("Projects live under:")),
        shiny::verbatimTextOutput(ns("data_root_display"))
      )
    ),
    # -- RIGHT: Ollama status + models + pull + ensemble config -------------
    bslib::layout_column_wrap(
      width = 1,
      heights_equal = "row",
      gap = "0.75rem",
      # Ollama status card
      bslib::card(
        bslib::card_header("Ollama status"),
        bslib::card_body(
          class = "d-flex align-items-center gap-3",
          shiny::uiOutput(ns("ollama_badge"), inline = TRUE),
          shiny::actionButton(ns("refresh_ollama"), "Refresh",
                              class = "btn-sm btn-outline-secondary")
        )
      ),
      # Installed-models card (bounded height so it never overflows)
      bslib::card(
        bslib::card_header("Installed models"),
        bslib::card_body(
          min_height = "200px",
          max_height = "260px",
          DT::DTOutput(ns("models_table"))
        )
      ),
      # Pull-a-model card
      bslib::card(
        bslib::card_header("Pull a model"),
        bslib::card_body(
          shiny::fluidRow(
            shiny::column(
              8,
              shiny::textInput(ns("pull_tag"), NULL,
                               placeholder = "e.g. gemma3:27b",
                               width = "100%")
            ),
            shiny::column(
              4,
              shiny::actionButton(ns("pull_btn"), "Pull",
                                  class = "btn-outline-primary",
                                  width = "100%")
            )
          )
        )
      ),
      # Ensemble config card
      bslib::card(
        bslib::card_header("Choose ensemble"),
        bslib::card_body(
          shiny::radioButtons(
            ns("ensemble_mode"), NULL,
            choices = c(
              "Paper default (4 models, ~65 GB)" = "default",
              "Light (4 small models, ~10 GB)" = "light",
              "Custom (pick from installed)" = "custom"
            ),
            selected = "default", inline = FALSE
          ),
          shiny::uiOutput(ns("ensemble_hint")),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns("ensemble_mode")),
            shiny::selectizeInput(
              ns("custom_models"), "Models:", choices = NULL, multiple = TRUE,
              width = "100%"
            ),
            shiny::numericInput(ns("replicates"), "Replicates per model:",
                                value = 3, min = 1, max = 5, width = "100%")
          ),
          shiny::actionButton(ns("save_ensemble"), "Save ensemble config",
                              class = "btn-success")
        )
      )
    )
  )
}

#' @keywords internal
mod_setup_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$data_root_display <- shiny::renderText(data_root())

    # Populate the project selector on load (and whenever the tab is revisited).
    shiny::observe({
      projects <- list_projects()
      selected <- if (!is.null(state$project) && state$project %in% projects) {
        state$project
      } else if (length(projects) > 0) {
        projects[1]
      } else NULL
      shiny::updateSelectizeInput(
        session, "project_select",
        choices = c("(none)", projects), selected = selected
      )
    })

    # Pick or create a project.
    shiny::observeEvent(input$create_project, {
      new <- trimws(input$new_project)
      if (nzchar(new)) {
        project_dir(new, create = TRUE)
        state$project <- slugify_project_name(new)
        shiny::updateTextInput(session, "new_project", value = "")
      } else {
        sel <- input$project_select
        if (!identical(sel, "(none)") && !is.null(sel) && nzchar(sel)) {
          state$project <- sel
        }
      }
      # Rehydrate saved artefacts, if any.
      if (!is.null(state$project)) {
        state$records <- load_artefact(state$project, "records")
        state$criteria <- load_artefact(state$project, "criteria")
        state$ensemble <- load_artefact(state$project, "ensemble")
        state$ranked <- load_artefact(state$project, "ranked")
        state$plan <- load_artefact(state$project, "plan")
        state$decisions <- load_artefact(state$project, "decisions")
        shiny::showNotification(
          sprintf("Selected project: %s", state$project), duration = 3
        )
      }
    })

    # ---- Ollama panel --------------------------------------------------

    ollama_state <- shiny::reactive({
      shiny::invalidateLater(0)  # only recomputes when refresh_ollama is pressed
      up <- ollama_health(quiet = TRUE)
      installed <- if (up) ollama_installed_models() else character()
      list(up = up, installed = installed)
    }) |> shiny::bindEvent(input$refresh_ollama, ignoreNULL = FALSE, ignoreInit = FALSE)

    output$ollama_badge <- shiny::renderUI({
      s <- ollama_state()
      colour <- if (isTRUE(s$up)) "success" else "danger"
      msg <- if (isTRUE(s$up)) "Ollama reachable" else "Ollama not reachable"
      shiny::tags$span(class = sprintf("badge bg-%s", colour), msg)
    })

    output$models_table <- DT::renderDT({
      s <- ollama_state()
      if (!isTRUE(s$up) || length(s$installed) == 0) {
        return(DT::datatable(
          data.frame(Model = "(none - refresh once Ollama is running)"),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE, selection = "none"
        ))
      }
      DT::datatable(
        data.frame(Model = s$installed),
        options = list(
          dom = "tp",
          pageLength = 6,
          scrollY = "180px",
          scrollCollapse = TRUE,
          paging = TRUE
        ),
        rownames = FALSE, selection = "none", class = "compact"
      )
    })

    # Keep the custom-model picker synced with installed models.
    shiny::observe({
      s <- ollama_state()
      shiny::updateSelectizeInput(session, "custom_models",
                                   choices = s$installed, server = FALSE)
    })

    shiny::observeEvent(input$pull_btn, {
      tag <- trimws(input$pull_tag)
      if (!nzchar(tag)) return(NULL)
      shiny::withProgress(
        message = sprintf("Pulling %s...", tag),
        value = 0.5,
        {
          ok <- try(pull_model(tag, verbose = FALSE), silent = TRUE)
          shiny::showNotification(
            if (isTRUE(ok)) sprintf("Pulled %s.", tag) else "Pull failed.",
            type = if (isTRUE(ok)) "message" else "error", duration = 5
          )
        }
      )
      shiny::updateActionButton(session, "refresh_ollama")
    })

    # ---- Ensemble config ----------------------------------------------

    # Small hint under the ensemble radios warning about missing models.
    output$ensemble_hint <- shiny::renderUI({
      mode <- input$ensemble_mode
      if (is.null(mode) || identical(mode, "custom")) return(NULL)
      wanted <- switch(mode,
                       default = .PINNED_DEFAULT_MODELS,
                       light   = .PINNED_LIGHT_MODELS,
                       character())
      s <- ollama_state()
      if (!isTRUE(s$up)) return(NULL)
      missing <- setdiff(wanted, s$installed)
      if (length(missing) == 0L) {
        shiny::tags$div(
          class = "alert alert-success py-1 my-1",
          shiny::tags$small(sprintf(
            "All %d models for this preset are installed.", length(wanted)
          ))
        )
      } else {
        shiny::tags$div(
          class = "alert alert-warning py-1 my-1",
          shiny::tags$small(
            sprintf("%d of %d models not yet installed: ",
                    length(missing), length(wanted)),
            shiny::tags$code(paste(missing, collapse = ", ")),
            ". Pull them one by one above, or in the R console run: ",
            shiny::tags$code(
              sprintf('install_prereqs(models = c(%s))',
                      paste(sprintf('"%s"', missing), collapse = ", "))
            )
          )
        )
      }
    })

    shiny::observeEvent(input$save_ensemble, {
      shiny::req(state$project)
      ens <- tryCatch({
        switch(
          input$ensemble_mode %||% "default",
          default = default_ensemble(),
          light   = default_ensemble_light(),
          custom  = {
            models <- input$custom_models
            if (length(models) == 0L) {
              cli::cli_abort("Pick at least one model.")
            }
            custom_ensemble(models = models,
                            replicates = as.integer(input$replicates))
          }
        )
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
        NULL
      })
      if (is.null(ens)) return(NULL)
      state$ensemble <- ens
      save_artefact(state$project, "ensemble", ens)
      shiny::showNotification(
        sprintf("Ensemble config saved (%d models x %d replicates).",
                length(ens$models), ens$replicates),
        duration = 3
      )
    })
  })
}
