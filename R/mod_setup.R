# Setup tab: pick/create project, verify Ollama, choose ensemble.

#' @keywords internal
mod_setup_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(6, 6),
    # ---- LEFT: all the action (project, ollama+pull, ensemble) ----------
    shiny::tags$div(
      class = "d-flex flex-column gap-2",
      # Project card
      bslib::card(
        bslib::card_header("Project"),
        bslib::card_body(
          shiny::selectizeInput(
            ns("project_select"), NULL,
            choices = NULL,
            # dropdownParent = 'body' portals the popup to document.body
            # so it isn't clipped by the enclosing card's overflow:hidden
            # (which bslib::card sets to keep rounded corners clean).
            # Without this the tail of a long project list is invisible
            # behind the card's bottom edge.
            options = list(placeholder = "(select existing project)",
                            dropdownParent = "body")
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
      # Slim Ollama + GPU + pull strip. The action button toggles
      # between "Install Ollama" (when nothing responds at the API)
      # and "Refresh" (when Ollama is up).
      shiny::tags$div(
        class = "d-flex align-items-center gap-2 py-1 px-2 border rounded flex-wrap",
        shiny::tags$small(class = "text-nowrap", "Ollama:"),
        shiny::uiOutput(ns("ollama_badge"), inline = TRUE),
        shiny::uiOutput(ns("ollama_action_btn"), inline = TRUE),
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
              create = TRUE, createOnBlur = TRUE,
              placeholder = "pick from list or type any Ollama tag",
              persist = FALSE,
              dropdownParent = "body"
            ),
            width = "100%"
          )
        ),
        shiny::actionButton(ns("pull_btn"), "Pull",
                            class = "btn-sm btn-outline-primary")
      ),
      shiny::uiOutput(ns("pull_progress_ui")),
      # Ensemble card
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
          shiny::uiOutput(ns("model_list")),
          shiny::tags$hr(class = "my-2"),
          shiny::fluidRow(
            shiny::column(6, shiny::uiOutput(ns("pull_missing_ui"))),
            shiny::column(
              6,
              shiny::actionButton(ns("save_ensemble"), "Save ensemble",
                                  class = "btn-success w-100")
            )
          )
        )
      )
    ),
    # ---- RIGHT: about panel + workflow overview -------------------------
    bslib::card(
      bslib::card_header("About screenllm"),
      bslib::card_body(
        shiny::tags$p(
          class = "small mb-2",
          shiny::tags$strong("screenllm"),
          " runs a locally-hosted LLM ensemble (via Ollama) over a corpus ",
          "of paper titles + abstracts to rank them by relevance, and ",
          "applies the SAFE stopping rule to tell you how many the human ",
          "reviewer needs to look at. Everything stays on your machine; ",
          "no API keys, no cloud spend."
        ),
        shiny::tags$hr(class = "my-2"),
        shiny::tags$h6(class = "small text-muted mb-2", "The 7-tab workflow"),
        shiny::tags$ol(
          class = "small mb-2",
          shiny::tags$li(shiny::tags$strong("Setup"), " (this tab): pick a project, verify Ollama + GPU, choose an ensemble."),
          shiny::tags$li(shiny::tags$strong("Corpus"), ": upload records (CSV, XLSX, or RIS from Zotero / EndNote). Duplicates are flagged automatically."),
          shiny::tags$li(shiny::tags$strong("Criteria"), ": write the inclusion criteria. Point weights auto-scale."),
          shiny::tags$li(shiny::tags$strong("Rank"), ": kick off the ensemble scoring. Records stream in as each model completes. Use the sample-size input to preview with a small subset (\"pilot\")."),
          shiny::tags$li(shiny::tags$strong("Plan"), ": pick the SAFE stopping thresholds. Live plot + per-gate diagnostics show where the run would stop."),
          shiny::tags$li(shiny::tags$strong("Screen"), ": walk through the ranked records above the stop point, mark Accept / Reject. Per-model LLM justifications visible for context."),
          shiny::tags$li(shiny::tags$strong("Report"), ": summary, downloads, and a strong LLM-vs-human disagreement audit. Click a disagreement to revisit and flip your decision.")
        ),
        shiny::tags$hr(class = "my-2"),
        shiny::tags$h6(class = "small text-muted mb-2", "Where things live"),
        shiny::tags$ul(
          class = "small mb-2",
          shiny::tags$li("Projects (records, criteria, ensemble, ranking, decisions) persist to the data dir shown top-left; they survive R restarts."),
          shiny::tags$li("Per-call LLM scores are cached under each project so an interrupted run resumes without re-scoring."),
          shiny::tags$li("The Rank tab has a ",
                          shiny::tags$strong("Clear cached scores"),
                          " control if you need to redo a model.")
        ),
        shiny::tags$hr(class = "my-2"),
        shiny::tags$h6(class = "small text-muted mb-2", "Learn more"),
        shiny::tags$ul(
          class = "small mb-2",
          shiny::tags$li(
            shiny::tags$a(href = "https://github.com/s-spillias/screenllm",
                          target = "_blank", "GitHub repo")
          ),
          shiny::tags$li(
            shiny::tags$a(href = "https://github.com/s-spillias/screenllm/blob/main/docs/getting-started.md",
                          target = "_blank", "Getting-started guide")
          ),
          shiny::tags$li(
            shiny::tags$a(href = "https://github.com/s-spillias/screenllm/blob/main/docs/troubleshooting.md",
                          target = "_blank", "Troubleshooting")
          )
        ),
        shiny::tags$hr(class = "my-2"),
        shiny::tags$p(
          class = "small text-muted mb-0",
          shiny::tags$strong("Cite:"),
          " Spillias, S. et al. (2026). ",
          shiny::tags$em("An open-source LLM-assisted screening workflow for environmental systematic reviews."),
          " (in submission)."
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
      # Normalise the decisions shape at load time so a legacy file
      # (older schema, missing columns) can't crash downstream tabs.
      raw_decisions <- load_artefact(proj, "decisions")
      state$decisions <- normalise_decisions_shape(raw_decisions)
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
      # Never let a probe error grey out the whole Setup tab. If
      # either helper throws we treat Ollama as unreachable so the
      # UI shows "not reachable" + the Install button, and the rest
      # of the app stays responsive.
      up <- tryCatch(ollama_health(quiet = TRUE),
                     error = function(e) FALSE)
      installed <- if (isTRUE(up)) {
        tryCatch(ollama_installed_models(), error = function(e) character())
      } else character()
      list(up = isTRUE(up), installed = installed)
    })

    output$ollama_badge <- shiny::renderUI({
      s <- ollama_state()
      colour <- if (isTRUE(s$up)) "success" else "danger"
      msg <- if (isTRUE(s$up)) "reachable" else "not reachable"
      shiny::tags$span(class = sprintf("badge bg-%s", colour), msg)
    })

    # The action button next to the Ollama badge is either "Refresh"
    # (when Ollama is up) or "Install Ollama" (when the API is not
    # reachable AND the ollama binary isn't on PATH -- meaning it's
    # actually not installed rather than just not running).
    output$ollama_action_btn <- shiny::renderUI({
      s <- ollama_state()
      have_binary <- nzchar(find_ollama_binary())
      if (isTRUE(s$up) || have_binary) {
        shiny::actionButton(ns("refresh_ollama"), "Refresh",
                            class = "btn-sm btn-outline-secondary")
      } else {
        shiny::actionButton(ns("install_ollama_btn"), "Install Ollama",
                            class = "btn-sm btn-primary",
                            icon = shiny::icon("download"))
      }
    })

    # Clicking Install Ollama opens a modal with OS-specific
    # guidance. We show the copy-pasteable command every time (works
    # everywhere) and, on macOS/Windows where the command doesn't
    # need a TTY-attached sudo, also offer a "Run it now" button.
    shiny::observeEvent(input$install_ollama_btn, {
      candidate <- ollama_install_candidate()
      sysname <- Sys.info()[["sysname"]]
      # Fallback command: only ever show the Linux install script
      # AS A LINUX fallback. Showing a `curl | sh` pipeline to a
      # Windows-10-without-winget user was actively misleading -- it
      # can't run in cmd/PowerShell as-is and the modal claimed
      # "Detected: Windows (manual)" while displaying a Linux command.
      cmd <- if (!is.null(candidate)) candidate$command
             else if (identical(sysname, "Linux"))
               "curl -fsSL https://ollama.com/install.sh | sh"
             else NA_character_
      manager <- candidate$manager %||% "manual"
      # On Linux, the install script uses sudo internally and needs a
      # TTY password prompt, which a Shiny modal can't provide. Show
      # copy-paste only there. macOS brew and Windows winget usually
      # don't need password input in this flow, so offer the button
      # too.
      can_run_directly <- !is.null(candidate) && sysname != "Linux"
      shiny::showModal(shiny::modalDialog(
        title = "Install Ollama",
        shiny::tags$p(
          "screenllm runs its language models through ",
          shiny::tags$a(href = "https://ollama.com", target = "_blank", "Ollama"),
          ", a small local server. Once installed you'll come back here ",
          "to pull models and choose an ensemble."
        ),
        shiny::tags$hr(),
        shiny::tags$p(
          shiny::tags$strong(sprintf("Detected: %s (%s).", sysname, manager)),
          " Copy the command below into a terminal and run it, or click ",
          "\"Try direct install\" if available."
        ),
        if (!is.na(cmd)) shiny::tags$pre(
          class = "p-2 bg-body-tertiary small",
          style = "user-select: text;",
          cmd
        ) else shiny::tags$p(
          class = "text-muted small",
          "No supported package manager detected on this OS. Use the ",
          "manual download link below."
        ),
        if (sysname == "Linux") shiny::tags$small(
          class = "text-muted",
          "Linux install writes to /usr/local/bin and needs sudo; run it in a terminal so it can prompt for your password. When it finishes, come back here and click Refresh."
        ),
        shiny::tags$hr(),
        shiny::tags$p(
          shiny::tags$strong("Or install manually: "),
          shiny::tags$a(href = "https://ollama.com/download",
                        target = "_blank", "ollama.com/download")
        ),
        footer = shiny::tagList(
          if (can_run_directly)
            shiny::actionButton(ns("install_run_now"), "Try direct install",
                                class = "btn-primary",
                                icon = shiny::icon("play")),
          shiny::actionButton(ns("install_close"),
                              "Close (I'll install it in a terminal)",
                              class = "btn-outline-secondary")
        ),
        easyClose = TRUE, size = "l"
      ))
    })

    shiny::observeEvent(input$install_close, {
      shiny::removeModal()
      # Immediate refresh so if the user just installed it, we
      # pick it up.
      ollama_refresh(shiny::isolate(ollama_refresh()) + 1L)
    })

    # Direct-install path (macOS brew / Windows winget). Run the
    # command in a background process so the app stays responsive,
    # then trigger a Refresh once it exits.
    install_handle <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$install_run_now, {
      candidate <- ollama_install_candidate()
      if (is.null(candidate)) {
        shiny::showNotification(
          "No supported package manager detected on this OS.",
          type = "warning"
        )
        return()
      }
      shiny::removeModal()
      shiny::showNotification(
        paste0("Running: ", candidate$command,
               ".  This can take a few minutes; the app will refresh when it's done."),
        duration = 8
      )
      rlang::check_installed("callr", "to run the install in the background.")
      handle <- callr::r_bg(
        func = function(cmd) system(cmd),
        args = list(cmd = candidate$command),
        supervise = FALSE
      )
      install_handle(handle)
    })

    # Track when we launched, so we can bail on a hung install
    # (brew triggering an Xcode CLT install dialog behind the browser;
    # winget spawning a UAC elevation prompt the Shiny process can't
    # answer). 10 minutes is generous for a real install and short
    # enough that a hung one gets recovered without the user having
    # to force-quit the app.
    install_start_time <- shiny::reactiveVal(NULL)
    INSTALL_TIMEOUT_SECS <- 600
    shiny::observeEvent(input$install_run_now, {
      install_start_time(Sys.time())
    }, priority = 100)  # fires before the launch handler above

    # Poll the background install every 2s. When it finishes, bump
    # ollama_refresh and tell the user. Kill and warn if it blows the
    # timeout.
    shiny::observe({
      h <- install_handle()
      if (is.null(h)) return()
      shiny::invalidateLater(2000, session)
      started <- install_start_time()
      if (!is.null(started) &&
            as.numeric(Sys.time() - started, units = "secs") >
              INSTALL_TIMEOUT_SECS &&
            h$is_alive()) {
        tryCatch(h$kill(), error = function(e) NULL)
        install_handle(NULL); install_start_time(NULL)
        shiny::showNotification(
          paste0("Install did not finish within ",
                 round(INSTALL_TIMEOUT_SECS / 60),
                 " minutes and was cancelled. Common causes: brew is ",
                 "waiting on an Xcode Command Line Tools dialog (macOS), ",
                 "or winget is waiting on a UAC elevation prompt (Windows). ",
                 "Run the command in a terminal so it can prompt you."),
          type = "warning", duration = 15
        )
        return()
      }
      if (!h$is_alive()) {
        exit <- tryCatch(h$get_exit_status(), error = function(e) NA_integer_)
        install_start_time(NULL)
        if (identical(exit, 0L)) {
          shiny::showNotification("Ollama install finished.", duration = 5)
        } else {
          shiny::showNotification(
            sprintf("Install exited with status %s. Try running the command in a terminal.",
                    exit %||% "?"),
            type = "warning", duration = 10
          )
        }
        install_handle(NULL)
        ollama_refresh(shiny::isolate(ollama_refresh()) + 1L)
      }
    })

    # GPU detection: `detect_gpu()` is one-shot (kind of GPU doesn't
    # change during a session). `gpu_live()` polls nvidia-smi every
    # 3s so we can catch the "throttled" state where the dGPU is
    # loaded but running at idle clocks (typically because the
    # laptop is on battery or in a power-saver profile).
    gpu_info <- shiny::reactive(detect_gpu())
    gpu_live <- shiny::reactivePoll(
      intervalMillis = 3000,
      session = session,
      checkFunc = function() Sys.time(),
      valueFunc = function() gpu_status()
    )

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
      live <- gpu_live()
      if (!isTRUE(g$available)) {
        return(shiny::tags$span(class = "badge bg-secondary",
                                title = g$detail, "none"))
      }
      is_throttled <- isTRUE(live$throttled)
      colour <- if (is_throttled) "warning text-dark" else "success"
      # Rich tooltip: kind + live clock + power + hint if throttled.
      tooltip <- if (isTRUE(live$available)) {
        base <- sprintf(
          "%s | clock %.0f MHz | VRAM %.1f/%.1f GB | %.0f W",
          g$detail,
          live$graphics_clock_mhz,
          live$memory_used_mib / 1024,
          live$memory_total_mib / 1024,
          live$power_draw_w
        )
        if (is_throttled) paste0(base, " -- THROTTLED. ", live$hint) else base
      } else {
        g$detail
      }
      shiny::tags$span(
        class = sprintf("badge bg-%s", colour),
        title = tooltip,
        if (is_throttled) paste0(g$kind, " (throttled)") else g$kind
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
            # Replicates default to 3 here; the Rank tab lets the
            # user override at run time.
            custom_ensemble(models = models, replicates = 3L)
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
        sprintf("Ensemble config saved (%d models). Replicates set on the Rank tab.",
                length(ens$models)),
        duration = 3
      )
    })
  })
}
