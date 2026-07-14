# Setup tab: pick/create project, verify Ollama, choose ensemble.

#' @keywords internal
mod_setup_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    # ---- LEFT: project (compact) ----------------------------------------
    bslib::card(
      bslib::card_header("Project"),
      bslib::card_body(
        shiny::selectizeInput(
          ns("project_select"), NULL,
          choices = NULL, options = list(placeholder = "(select existing project)")
        ),
        shiny::fluidRow(
          shiny::column(
            8,
            shiny::textInput(ns("new_project"), NULL,
                             placeholder = "or new project name")
          ),
          shiny::column(
            4,
            shiny::actionButton(ns("create_project"), "Create",
                                class = "btn-primary w-100")
          )
        ),
        shiny::tags$small(
          class = "text-muted d-block mt-2",
          "Data dir: ",
          shiny::tags$code(shiny::textOutput(ns("data_root_display"),
                                              inline = TRUE))
        )
      )
    ),
    # ---- RIGHT: ollama status + pull-any strip, then ensemble --------
    shiny::tags$div(
      class = "d-flex flex-column gap-2 h-100",
      # Slim top strip: Ollama status + GPU badge + inline pull-any-model
      shiny::tags$div(
        class = "d-flex align-items-center gap-2 py-1 px-2 border rounded",
        shiny::tags$small(class = "text-nowrap", "Ollama:"),
        shiny::uiOutput(ns("ollama_badge"), inline = TRUE),
        shiny::actionButton(ns("refresh_ollama"), "Refresh",
                            class = "btn-sm btn-outline-secondary"),
        shiny::tags$div(class = "vr mx-2"),
        shiny::tags$small(class = "text-nowrap", "GPU:"),
        shiny::uiOutput(ns("gpu_badge"), inline = TRUE),
        shiny::tags$div(class = "vr mx-2"),
        shiny::tags$small(class = "text-muted text-nowrap", "Pull model:"),
        shiny::tags$div(
          class = "flex-grow-1",
          shiny::selectizeInput(
            ns("pull_tag"), NULL, choices = NULL,
            options = list(
              # Let the user type any Ollama tag not in the catalog.
              create = TRUE,
              createOnBlur = TRUE,
              placeholder = "pick from list or type any Ollama tag",
              persist = FALSE
            ),
            width = "100%"
          )
        ),
        shiny::actionButton(ns("pull_btn"), "Pull",
                            class = "btn-sm btn-outline-primary")
      ),
      # Any active pull progress banner (rendered by pull_progress_ui)
      shiny::uiOutput(ns("pull_progress_ui")),
      # Main ensemble card fills all remaining vertical space.
      bslib::card(
        class = "flex-grow-1",
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
          shiny::uiOutput(ns("model_list")),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns("ensemble_mode")),
            shiny::numericInput(ns("replicates"), "Replicates per model:",
                                value = 3, min = 1, max = 5, width = "100%")
          ),
          shiny::tags$hr(class = "my-2"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::uiOutput(ns("pull_missing_ui"))
            ),
            shiny::column(
              6,
              shiny::actionButton(ns("save_ensemble"), "Save ensemble",
                                  class = "btn-success w-100")
            )
          )
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

    # Load every saved artefact for a project and restore the ensemble
    # radio/checkboxes so the UI reflects what's on disk.
    load_project_into_state <- function(proj, notify = TRUE) {
      if (is.null(proj) || !nzchar(proj)) return(invisible())
      state$project   <- proj
      state$records   <- load_artefact(proj, "records")
      state$criteria  <- load_artefact(proj, "criteria")
      state$ensemble  <- load_artefact(proj, "ensemble")
      state$ranked    <- load_artefact(proj, "ranked")
      state$plan      <- load_artefact(proj, "plan")
      state$decisions <- load_artefact(proj, "decisions")
      # Restore the ensemble UI to match what was saved.
      restore_ensemble_ui(state$ensemble)
      if (notify) {
        shiny::showNotification(
          sprintf("Loaded project: %s", proj), duration = 3
        )
      }
    }

    # Match a saved ensemble against the two presets by comparing model
    # sets. If no preset matches exactly, treat as custom.
    restore_ensemble_ui <- function(ens) {
      if (is.null(ens)) return(invisible())
      mode <- if (setequal(ens$models, .PINNED_DEFAULT_MODELS)) {
        "default"
      } else if (setequal(ens$models, .PINNED_LIGHT_MODELS)) {
        "light"
      } else {
        "custom"
      }
      shiny::updateRadioButtons(session, "ensemble_mode", selected = mode)
      if (identical(mode, "custom")) {
        # `model_list` is re-rendered on radio change, and it uses the
        # last input$custom_models as the initial selection. Push the
        # saved selection so the checkboxes populate correctly.
        shiny::updateCheckboxGroupInput(
          session, "custom_models",
          choices = shiny::isolate(ollama_state()$installed),
          selected = ens$models
        )
        shiny::updateNumericInput(session, "replicates",
                                   value = ens$replicates)
      }
    }

    # Auto-load when the user picks a project from the dropdown -
    # requiring a "Create" click was surprising.
    shiny::observeEvent(input$project_select, ignoreInit = TRUE, {
      sel <- input$project_select
      if (is.null(sel) || !nzchar(sel) || identical(sel, "(none)")) return()
      if (identical(sel, state$project)) return()  # no-op on initial sync
      load_project_into_state(sel)
    })

    # At session start, if the app was launched with an initial
    # project (either via launch_app(project = "...") or because the
    # state already has one set), load its artefacts and restore the
    # Setup-tab UI. This is a one-shot: it runs once at module init.
    shiny::isolate({
      init_proj <- state$project
      if (!is.null(init_proj) && init_proj %in% list_projects()) {
        load_project_into_state(init_proj, notify = FALSE)
      }
    })

    # Explicit Create button: creates the project directory if a name
    # was typed, or (re)loads the dropdown selection.
    shiny::observeEvent(input$create_project, {
      new <- trimws(input$new_project)
      if (nzchar(new)) {
        project_dir(new, create = TRUE)
        proj <- slugify_project_name(new)
        shiny::updateTextInput(session, "new_project", value = "")
        load_project_into_state(proj)
      } else {
        sel <- input$project_select
        if (!identical(sel, "(none)") && !is.null(sel) && nzchar(sel)) {
          load_project_into_state(sel)
        }
      }
    })

    # ---- Ollama panel --------------------------------------------------

    # `ollama_refresh` is bumped whenever we want `ollama_state` to
    # re-fetch: on button click, on session start, and on pull complete.
    ollama_refresh <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$refresh_ollama,
                        ignoreNULL = FALSE, ignoreInit = FALSE, {
      ollama_refresh(shiny::isolate(ollama_refresh()) + 1L)
    })

    ollama_state <- shiny::reactive({
      ollama_refresh()  # dependency; refetch when bumped
      up <- ollama_health(quiet = TRUE)
      installed <- if (up) ollama_installed_models() else character()
      list(up = up, installed = installed)
    })

    output$ollama_badge <- shiny::renderUI({
      s <- ollama_state()
      colour <- if (isTRUE(s$up)) "success" else "danger"
      msg <- if (isTRUE(s$up)) "reachable" else "not reachable"
      shiny::tags$span(class = sprintf("badge bg-%s", colour), msg)
    })

    # GPU detection is a system-level probe (nvidia-smi / rocm-smi /
    # Apple Silicon), independent of Ollama. Cached for the session --
    # the answer does not change while the app is open.
    gpu_info <- shiny::reactive(detect_gpu())

    # Populate the Pull selectize with the curated catalog. Already-
    # installed tags are dropped so the user is not encouraged to
    # re-pull. selectize is configured with create = TRUE so users can
    # still type any tag not in the catalog.
    shiny::observe({
      installed <- ollama_state()$installed
      cat <- ollama_catalog()
      cat <- cat[!(cat$tag %in% installed), , drop = FALSE]
      labels <- sprintf("%s - %s (~%d GB)",
                        cat$tag, cat$description, cat$size_gb)
      choices <- stats::setNames(cat$tag, labels)
      shiny::updateSelectizeInput(
        session, "pull_tag",
        choices = c("", choices),
        server = FALSE
      )
    })

    output$gpu_badge <- shiny::renderUI({
      g <- gpu_info()
      colour <- if (isTRUE(g$available)) "success" else "secondary"
      label <- if (isTRUE(g$available)) g$kind else "none"
      shiny::tags$span(
        class = sprintf("badge bg-%s", colour),
        title = g$detail, label
      )
    })

    # ---- Unified model list --------------------------------------------

    # Auto-refresh the installed-models list whenever the user opens
    # Custom mode, so a `ollama pull` / `ollama rm` from a terminal
    # is picked up without needing to click Refresh.
    shiny::observeEvent(input$ensemble_mode, {
      if (identical(input$ensemble_mode, "custom")) {
        ollama_refresh(shiny::isolate(ollama_refresh()) + 1L)
      }
    })

    output$model_list <- shiny::renderUI({
      s <- ollama_state()
      mode <- input$ensemble_mode %||% "default"
      installed <- s$installed
      if (identical(mode, "custom")) {
        if (!isTRUE(s$up) || length(installed) == 0L) {
          return(shiny::tags$div(
            class = "alert alert-warning py-1 my-1",
            shiny::tags$small(
              "No models installed yet. Pull one below, or switch to a preset."
            )
          ))
        }
        # Preserve any prior selection when re-rendering.
        selected <- shiny::isolate(input$custom_models) %||% character()
        selected <- intersect(selected, installed)
        # Enrich each label with the catalog's size hint when known;
        # unknown tags just show the bare tag.
        cat <- ollama_catalog()
        size_by_tag <- stats::setNames(cat$size_gb, cat$tag)
        labels <- vapply(installed, function(tag) {
          sz <- size_by_tag[tag]
          if (is.na(sz)) tag else sprintf("%s  (~%d GB)", tag, sz)
        }, character(1))
        choices <- stats::setNames(installed, labels)
        shiny::checkboxGroupInput(
          ns("custom_models"),
          label = shiny::tags$small(
            class = "text-muted",
            sprintf("Include (pick as many as you want; %d installed):",
                    length(installed))
          ),
          choices = choices, selected = selected
        )
      } else {
        wanted <- switch(mode,
                         default = .PINNED_DEFAULT_MODELS,
                         light   = .PINNED_LIGHT_MODELS,
                         character())
        rows <- lapply(wanted, function(m) {
          is_installed <- m %in% installed
          badge <- if (is_installed) {
            shiny::tags$span(class = "badge bg-success ms-2", "installed")
          } else {
            shiny::tags$span(class = "badge bg-warning text-dark ms-2", "missing")
          }
          shiny::tags$li(class = "list-group-item py-2 px-3 d-flex align-items-center",
                         shiny::tags$code(m), badge)
        })
        shiny::tags$div(
          shiny::tags$label(class = "form-label small text-muted mb-1",
                            "Models in this preset:"),
          shiny::tags$ul(class = "list-group mb-2", rows)
        )
      }
    })

    # ---- Pull-missing button + custom pull-any --------------------------

    # State for both pull flows.
    pulling_model <- shiny::reactiveVal(NULL)
    notified <- shiny::reactiveVal(character())
    pull_queue <- shiny::reactiveVal(character())  # for pull-missing

    # Pull-missing button (visible only when preset selected and models missing).
    output$pull_missing_ui <- shiny::renderUI({
      mode <- input$ensemble_mode %||% "default"
      if (identical(mode, "custom")) return(NULL)
      wanted <- switch(mode,
                       default = .PINNED_DEFAULT_MODELS,
                       light   = .PINNED_LIGHT_MODELS,
                       character())
      s <- ollama_state()
      if (!isTRUE(s$up)) return(NULL)
      missing <- setdiff(wanted, s$installed)
      if (length(missing) == 0L) {
        return(shiny::tags$small(class = "text-success",
                                 "All models installed."))
      }
      shiny::actionButton(
        ns("pull_missing_btn"),
        sprintf("Pull %d missing", length(missing)),
        class = "btn-outline-warning w-100"
      )
    })

    shiny::observeEvent(input$pull_missing_btn, {
      mode <- input$ensemble_mode %||% "default"
      wanted <- switch(mode,
                       default = .PINNED_DEFAULT_MODELS,
                       light   = .PINNED_LIGHT_MODELS,
                       character())
      s <- ollama_state()
      missing <- setdiff(wanted, s$installed)
      if (length(missing) == 0L) return(NULL)
      # Queue subsequent models; kick off the first now.
      pull_queue(missing[-1])
      start_and_track(missing[1])
    })

    # ---- Pull-any-model button + progress -------------------------------

    shiny::observeEvent(input$pull_btn, {
      tag <- trimws(input$pull_tag)
      if (!nzchar(tag)) return(NULL)
      start_and_track(tag)
    })

    # Shared launcher used by both pull-missing and pull-any.
    start_and_track <- function(tag) {
      handle <- try(start_pull_job(tag), silent = TRUE)
      if (inherits(handle, "try-error")) {
        shiny::showNotification(attr(handle, "condition")$message,
                                type = "error", duration = 6)
        return(NULL)
      }
      pulling_model(tag)
      shiny::showNotification(
        sprintf("Started background pull for %s. You can keep using the app.", tag),
        duration = 4
      )
    }

    # Poll the pull progress every 500 ms.
    pull_status <- shiny::reactivePoll(
      intervalMillis = 500,
      session = session,
      checkFunc = function() {
        m <- pulling_model()
        if (is.null(m)) return(0)
        path <- pull_progress_path(m)
        if (!fs::file_exists(path)) return(0)
        file.info(path)$mtime
      },
      valueFunc = function() {
        m <- pulling_model()
        if (is.null(m)) return(NULL)
        st <- pull_job_status(m)
        if (identical(st$status, "done") && !(m %in% notified())) {
          shiny::showNotification(sprintf("Pulled %s.", m),
                                  type = "message", duration = 6)
          notified(c(notified(), m))
          ollama_refresh(shiny::isolate(ollama_refresh()) + 1L)
          pulling_model(NULL)
          # If there are queued models (pull-missing flow), start next.
          q <- pull_queue()
          if (length(q) > 0L) {
            pull_queue(q[-1])
            start_and_track(q[1])
          }
        } else if (identical(st$status, "error") && !(m %in% notified())) {
          shiny::showNotification(
            sprintf("Pull %s failed: %s", m, st$error %||% "(no detail)"),
            type = "error", duration = 8
          )
          notified(c(notified(), m))
          pulling_model(NULL)
          # Abort the queue on error to avoid cascading failures.
          pull_queue(character())
        }
        st
      }
    )

    # Live progress banner shared across both pull entry points.
    output$pull_progress_ui <- shiny::renderUI({
      st <- pull_status()
      if (is.null(st) || identical(st$status, "idle")) return(NULL)
      pct <- st$percent %||% 0
      detail <- st$detail %||% st$status
      color <- switch(st$status,
                      done = "success", error = "danger", "info")
      shiny::tags$div(
        class = sprintf("alert alert-%s py-1 my-2", color),
        shiny::tags$small(
          shiny::tags$strong(st$model %||% ""),
          sprintf(" - %s (%.0f%%)", detail, pct)
        )
      )
    })

    # ---- Save ensemble --------------------------------------------------

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
              cli::cli_abort("Tick at least one model.")
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

      # Cross-check the ensemble's models against what Ollama has
      # installed. Saving an ensemble that references missing models
      # produces a "successful" ranking with all-NA scores, which is
      # far worse than a hard error.
      installed <- ollama_state()$installed
      missing <- setdiff(ens$models, installed)
      if (length(missing) > 0L) {
        shiny::showNotification(
          shiny::tags$div(
            shiny::tags$strong("Cannot save: models not installed."),
            shiny::tags$br(),
            shiny::tags$span(
              sprintf("Ollama does not have %d of %d model%s in this ensemble: ",
                      length(missing), length(ens$models),
                      if (length(ens$models) == 1L) "" else "s"),
              shiny::tags$code(paste(missing, collapse = ", ")),
              ". Ranking would return no scores. Pull them first (Pull button above, or the 'Pull missing' button)."
            )
          ),
          type = "error", duration = 12
        )
        return(NULL)
      }

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
