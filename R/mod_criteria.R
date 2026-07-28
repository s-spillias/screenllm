# Criteria tab: dynamic form for scope + inclusion criteria.

#' @keywords internal
mod_criteria_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header("Edit criteria"),
      shiny::textAreaInput(
        ns("scope"), "Scope (one sentence):",
        value = "", rows = 3, width = "100%"
      ),
      shiny::uiOutput(ns("inclusions_ui")),
      shiny::fluidRow(
        shiny::column(6, shiny::actionButton(ns("add"), "+ Add criterion",
                                             class = "btn-outline-primary")),
        shiny::column(6, shiny::actionButton(ns("remove"), "- Remove last",
                                             class = "btn-outline-secondary"))
      ),
      shiny::hr(),
      shiny::div(
        class = "d-flex align-items-center gap-2 flex-wrap",
        shiny::actionButton(ns("save"), "Save criteria",
                            class = "btn-success"),
        shiny::actionButton(ns("load_cbfm"),
                            "Load CBFM example criteria",
                            class = "btn-outline-secondary btn-sm",
                            title = paste0("Populate the scope + inclusions with the ",
                                            "example criteria matching the toy CBFM ",
                                            "corpus, so you can run a real end-to-end ",
                                            "demo without inventing your own criteria."))
      ),
      shiny::tags$small(
        shiny::textOutput(ns("autosave_indicator"), inline = TRUE),
        class = "text-muted ms-2"
      )
    ),
    bslib::card(
      bslib::card_header("Rendered LLM prompt (first record preview)"),
      shiny::verbatimTextOutput(ns("prompt_preview"))
    )
  )
}

#' @keywords internal
mod_criteria_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # `n_criteria` drives how many textareas the UI shows.
    # `stored_values` seeds `renderUI` when it rebuilds after an Add/Remove
    # so the fresh textareas come back with the text the user had typed.
    # We update `stored_values` at Add/Remove time only, snapshotting from
    # the live inputs via `isolate()`. During normal typing we do NOT
    # write to `stored_values`; the live input values are the source of
    # truth for downstream reactives like `current_criteria`.
    n_criteria <- shiny::reactiveVal(4L)
    stored_values <- shiny::reactiveVal(rep("", 4L))

    # Read the current values of the criterion textareas.  Passing
    # `isolate = TRUE` blocks reactive dependencies on the individual
    # inputs; the default (FALSE) creates deps so the caller re-runs
    # whenever the user types.
    read_inputs <- function(n, isolate = FALSE) {
      f <- if (isolate) shiny::isolate else identity
      f(vapply(
        seq_len(n),
        function(i) input[[sprintf("inc_%d", i)]] %||% "",
        character(1)
      ))
    }

    # Rehydrate the form when the active project changes. Triggering
    # on `state$criteria` directly would fire every time autosave
    # writes back to the same object, calling updateTextAreaInput
    # and jumping the cursor mid-type. Guarding by project + a
    # "the form already matches" check breaks that feedback loop
    # while still restoring on project switch. When the new project
    # has NO saved criteria, we must reset the form to blank -- doing
    # nothing (the old behaviour) left the previous project's values
    # visible on a fresh new project.
    shiny::observeEvent(state$project, ignoreNULL = TRUE, {
      c <- state$criteria
      if (is.null(c)) {
        # Blank the form: scope empty, four empty criterion textareas.
        shiny::updateTextAreaInput(session, "scope", value = "")
        n_criteria(4L)
        stored_values(rep("", 4L))
        for (i in seq_len(4L)) {
          shiny::updateTextAreaInput(session, sprintf("inc_%d", i),
                                      value = "")
        }
        return(NULL)
      }
      current_scope <- shiny::isolate(input$scope %||% "")
      current_texts <- shiny::isolate(read_inputs(n_criteria(), isolate = TRUE))
      current_texts <- trimws(current_texts)
      current_texts <- current_texts[nzchar(current_texts)]
      # If the form already reflects the saved criteria (e.g. because
      # this trigger fired as a side effect of an autosave that came
      # from the form itself), don't push values back and jump the
      # cursor.
      if (identical(trimws(current_scope), c$scope) &&
          identical(current_texts, as.character(c$inclusions))) {
        return(NULL)
      }
      shiny::updateTextAreaInput(session, "scope", value = c$scope)
      n_criteria(length(c$inclusions))
      stored_values(as.character(c$inclusions))
      for (i in seq_along(c$inclusions)) {
        shiny::updateTextAreaInput(session, sprintf("inc_%d", i),
                                    value = c$inclusions[[i]])
      }
    })

    output$inclusions_ui <- shiny::renderUI({
      n <- n_criteria()
      vals <- stored_values()
      lapply(seq_len(n), function(i) {
        val <- if (i <= length(vals)) vals[i] else ""
        shiny::textAreaInput(
          ns(sprintf("inc_%d", i)),
          sprintf("Criterion %d", i),
          value = val,
          rows = 2, width = "100%"
        )
      })
    })

    shiny::observeEvent(input$add, {
      n <- n_criteria()
      current <- read_inputs(n, isolate = TRUE)
      stored_values(c(current, ""))
      n_criteria(n + 1L)
    })
    shiny::observeEvent(input$remove, {
      n <- n_criteria()
      if (n <= 1L) return(NULL)
      current <- read_inputs(n, isolate = TRUE)
      stored_values(current[seq_len(n - 1L)])
      n_criteria(n - 1L)
    })

    # Live view of the criteria. Reads the inputs reactively so the
    # prompt preview updates as the user types AND on Add/Remove.
    current_criteria <- shiny::reactive({
      scope <- trimws(input$scope %||% "")
      if (!nzchar(scope)) return(NULL)
      texts <- read_inputs(n_criteria(), isolate = FALSE)
      texts <- trimws(texts)
      texts <- texts[nzchar(texts)]
      if (length(texts) == 0L) return(NULL)
      tryCatch(
        define_criteria(scope = scope, inclusions = texts),
        error = function(e) NULL
      )
    })

    output$prompt_preview <- shiny::renderText({
      c <- current_criteria()
      if (is.null(c)) return("(criteria not yet valid)")
      # An empty (header-only) corpus makes state$records[1, ] a
      # 0-row frame, which build_prompt() rightly rejects with
      # cli::cli_abort("record must be a one-row data frame ..."),
      # and the abort message then shows up in the preview panel.
      # Fall back to the stub record whenever state$records has
      # no rows.
      rec <- if (!is.null(state$records) && nrow(state$records) > 0L) {
        state$records[1, ]
      } else {
        data.frame(id = "record_1",
                   title = "(no record loaded)",
                   abstract = "")
      }
      build_prompt(c, rec)
    })

    # Load the CBFM criteria from inst/extdata rather than hardcoding
    # here. That file is the source of truth (the exact criteria from
    # Anonymous et al. 2024, Cell Reports Sustainability) and any
    # edit to it flows through to the app.
    shiny::observeEvent(input$load_cbfm, {
      spec <- tryCatch(load_toy_cbfm_criteria(),
                       error = function(e) NULL)
      if (is.null(spec)) {
        shiny::showNotification(
          "Could not load the CBFM example criteria file.",
          type = "error", duration = 6
        )
        return()
      }
      shiny::updateTextAreaInput(session, "scope", value = spec$scope)
      n_criteria(length(spec$inclusions))
      stored_values(as.character(spec$inclusions))
      # The observer above rebuilds inclusions_ui when n_criteria
      # changes; also push each value in case the number of textareas
      # is unchanged (Shiny only re-renders when the count differs).
      for (i in seq_along(spec$inclusions)) {
        shiny::updateTextAreaInput(session, sprintf("inc_%d", i),
                                    value = spec$inclusions[[i]])
      }
      shiny::showNotification(
        paste0(
          "Loaded the CBFM example criteria from Anonymous et al. ",
          "(2024, Cell Reports Sustainability). Edit as needed, then Save."
        ),
        duration = 5, type = "message"
      )
    })

    shiny::observeEvent(input$save, {
      shiny::req(state$project)
      c <- current_criteria()
      if (is.null(c)) {
        shiny::showNotification("Criteria are not valid yet.", type = "warning")
        return(NULL)
      }
      state$criteria <- c
      save_artefact(state$project, "criteria", c)
      shiny::showNotification(
        sprintf("Saved %d inclusion criteria.", length(c$inclusions)),
        duration = 3
      )
      last_autosave(Sys.time())
    })

    # Autosave: debounce well past a normal typing pause (4 s) so the
    # save doesn't fire mid-thought, and only when the criteria are
    # valid and a project is selected. If the resulting criteria are
    # already identical to what's on disk (typical when autosave fires
    # after a save button click), do nothing.
    last_autosave <- shiny::reactiveVal(NULL)
    autosaved_criteria <- shiny::debounce(current_criteria, millis = 4000)
    shiny::observe({
      c <- autosaved_criteria()
      if (is.null(c) || is.null(state$project)) return(NULL)
      if (identical(c, state$criteria)) return(NULL)
      state$criteria <- c
      save_artefact(state$project, "criteria", c)
      last_autosave(Sys.time())
    })

    output$autosave_indicator <- shiny::renderText({
      t <- last_autosave()
      if (is.null(t)) return("")
      # Show only for the first 4 s after a save, then blank out.
      shiny::invalidateLater(4000)
      elapsed <- as.numeric(difftime(Sys.time(), t, units = "secs"))
      if (elapsed > 4) "" else sprintf("Auto-saved %.0fs ago.", elapsed)
    })
  })
}
