# Pilot tab: score a small sample record-by-record in the background,
# streaming results into the UI as each one lands so the user gets
# concrete feedback instead of a static "piloting..." message.

#' @keywords internal
mod_pilot_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Pilot on a sample"),
      shiny::uiOutput(ns("readiness")),
      shiny::numericInput(ns("n"), "Sample size (records):",
                          value = 20L, min = 5L, max = 200L, step = 5L),
      shiny::checkboxInput(ns("random"), "Sample at random", value = TRUE),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::actionButton(ns("run"), "Run pilot",
                              class = "btn-primary w-100")
        ),
        shiny::column(
          6,
          shiny::actionButton(ns("cancel"), "Cancel",
                              class = "btn-outline-danger w-100")
        )
      ),
      shiny::hr(),
      shiny::helpText(
        shiny::em(
          "A pilot runs the ensemble at one replicate per model on a small ",
          "sample so you can see what the LLM is doing before committing to ",
          "the full run. Results stream in one record at a time; the app ",
          "stays responsive while it runs."
        )
      )
    ),
    bslib::card(
      bslib::card_header("Pilot results"),
      shiny::uiOutput(ns("progress_banner")),
      DT::DTOutput(ns("results_table")),
      shiny::tags$hr(),
      shiny::tags$small(
        class = "text-muted",
        "Click a row above to see its per-model justifications."
      ),
      shiny::uiOutput(ns("details"))
    )
  )
}

#' @keywords internal
mod_pilot_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

    output$readiness <- shiny::renderUI({
      needs <- c(
        if (is.null(state$records)) "an uploaded corpus" else NULL,
        if (is.null(state$criteria)) "inclusion criteria" else NULL,
        if (is.null(state$ensemble)) "an ensemble config" else NULL
      )
      if (length(needs) == 0L) {
        shiny::tags$div(class = "alert alert-success py-1 my-1",
                       shiny::tags$small("Ready to pilot."))
      } else {
        shiny::tags$div(
          class = "alert alert-warning py-1 my-1",
          shiny::tags$small("Missing: ", paste(needs, collapse = ", "))
        )
      }
    })

    # Handle for the currently running pilot worker (if any), plus a
    # one-shot flag so the "complete" toast fires exactly once per run.
    pilot_handle <- shiny::reactiveVal(NULL)
    completion_fired <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$run, {
      shiny::req(state$records, state$criteria, state$ensemble)
      cur <- pilot_handle()
      if (!is.null(cur) && cur$handle$is_alive()) {
        shiny::showNotification("A pilot is already running. Cancel it first.",
                                type = "warning", duration = 4)
        return(NULL)
      }
      job <- try(
        start_pilot_job(
          records  = state$records,
          criteria = state$criteria,
          ensemble = state$ensemble,
          n        = as.integer(input$n),
          sample   = isTRUE(input$random)
        ),
        silent = TRUE
      )
      if (inherits(job, "try-error")) {
        shiny::showNotification(attr(job, "condition")$message,
                                type = "error", duration = 8)
        return(NULL)
      }
      pilot_handle(job)
      completion_fired(FALSE)
      shiny::showNotification(
        sprintf("Pilot started - scoring %d records in the background.",
                as.integer(input$n)),
        duration = 4
      )
    })

    shiny::observeEvent(input$cancel, {
      cur <- pilot_handle()
      if (!is.null(cur)) {
        pilot_job_cancel(cur$handle)
        pilot_handle(NULL)
        shiny::showNotification("Pilot cancelled.", duration = 3)
      }
    })

    # Poll the progress file every 500 ms. As soon as a record has been
    # scored, the reactivePoll fires and the DT below re-renders with
    # the current partial results.
    pilot_status <- shiny::reactivePoll(
      intervalMillis = 500,
      session = session,
      checkFunc = function() {
        path <- pilot_progress_path()
        if (!fs::file_exists(path)) return(0)
        file.info(path)$mtime
      },
      valueFunc = function() {
        st <- pilot_job_status()
        # Fire the completion toast exactly once when the worker
        # transitions to done/error, so long-lived polls don't spam.
        if (identical(st$status, "done") && !isTRUE(completion_fired())) {
          shiny::showNotification(
            sprintf("Pilot complete: %d records scored.",
                    length(st$results)),
            type = "message", duration = 5
          )
          completion_fired(TRUE)
          pilot_handle(NULL)
        } else if (identical(st$status, "error") &&
                     !isTRUE(completion_fired())) {
          shiny::showNotification(
            sprintf("Pilot failed: %s", st$error %||% "(no detail)"),
            type = "error", duration = 8
          )
          completion_fired(TRUE)
          pilot_handle(NULL)
        }
        st
      }
    )

    # Streaming progress banner: shows N of M scored and a spinner
    # while the worker is active.
    output$progress_banner <- shiny::renderUI({
      st <- pilot_status()
      if (is.null(st) || identical(st$status, "idle")) return(NULL)
      if (identical(st$status, "done")) {
        return(shiny::tags$div(
          class = "alert alert-success py-1 my-2",
          shiny::tags$small(sprintf(
            "Pilot complete - %d of %d records scored.",
            st$processed, st$total
          ))
        ))
      }
      if (identical(st$status, "error")) {
        return(shiny::tags$div(
          class = "alert alert-danger py-1 my-2",
          shiny::tags$small(sprintf(
            "Pilot failed: %s", st$error %||% "(no detail)"
          ))
        ))
      }
      shiny::tags$div(
        class = "alert alert-info d-flex align-items-center py-2 my-2",
        shiny::tags$span(class = "spinner-border spinner-border-sm me-2",
                         role = "status"),
        shiny::tags$span(
          shiny::tags$strong("Piloting..."),
          sprintf(" scored %d of %d records so far (%.0f%%).",
                  st$processed, st$total, st$percent)
        )
      )
    })

    # Build the current partial-results tibble from the polled status.
    # This is what the results DT renders; it grows one row at a time
    # as the worker appends to the progress file.
    partial <- shiny::reactive({
      st <- pilot_status()
      if (is.null(st) || length(st$results) == 0L) return(NULL)
      pilot_results_as_tibble(st$results)
    })

    output$results_table <- DT::renderDT({
      p <- partial()
      if (is.null(p) || nrow(p) == 0L) return(NULL)
      p <- p[order(-p$universal_best_score), , drop = FALSE]
      preview <- vapply(p$justifications, function(js) {
        if (is.null(js) || nrow(js) == 0L) return("")
        m <- js$explanation[nzchar(js$explanation) & !is.na(js$explanation)]
        if (length(m) == 0L) return("")
        substr(gsub("\\s+", " ", m[1]), 1, 220)
      }, character(1))
      tbl <- data.frame(
        id = p$id,
        score = round(p$universal_best_score),
        title = p$title,
        justification_preview = preview,
        stringsAsFactors = FALSE
      )
      DT::datatable(
        tbl,
        options = list(pageLength = 15, autoWidth = FALSE, scrollX = TRUE),
        rownames = FALSE, selection = "single",
        class = "compact"
      )
    })

    output$details <- shiny::renderUI({
      p <- partial()
      sel <- input$results_table_rows_selected
      if (is.null(p) || nrow(p) == 0L || is.null(sel) || length(sel) == 0L) {
        return(shiny::tags$p(class = "text-muted fst-italic small mt-2",
                             "No row selected."))
      }
      p <- p[order(-p$universal_best_score), , drop = FALSE]
      rec <- p[sel[1L], ]
      js <- rec$justifications[[1L]]
      if (is.null(js) || nrow(js) == 0L) {
        return(shiny::tags$p(class = "text-muted fst-italic small mt-2",
                             "(no justifications recorded for this record)"))
      }
      # Title row summarising the selected record.
      header <- shiny::tags$div(
        class = "mb-2",
        shiny::tags$span(class = "badge bg-primary me-2",
                         sprintf("score %.0f", rec$universal_best_score)),
        shiny::tags$span(class = "fw-semibold",
                         substr(rec$title %||% "", 1, 200))
      )
      # One panel per model/replicate. `text-break` + `text-wrap` on
      # the paragraph forces long words to wrap instead of pushing a
      # horizontal scroll bar.
      panels <- lapply(seq_len(nrow(js)), function(i) {
        explanation <- js$explanation[i] %||% ""
        explanation <- gsub("\\s+", " ", explanation)
        shiny::tags$div(
          class = "border rounded p-2 mb-2 bg-body-tertiary",
          shiny::tags$div(
            class = "d-flex justify-content-between align-items-baseline mb-1",
            shiny::tags$code(class = "small", js$model[i]),
            shiny::tags$span(class = "small text-muted",
                             sprintf("replicate %d", js$replicate[i]))
          ),
          shiny::tags$p(class = "mb-0 small text-break text-wrap",
                        if (nzchar(explanation)) explanation
                        else shiny::tags$em("(no explanation returned by the model)"))
        )
      })
      shiny::tagList(header, panels)
    })
  })
}
