# Pilot tab: run the ensemble on a small sample at r=1 and print
# per-record aggregate score plus per-criterion justifications so the
# user can see whether the criteria are behaving before triggering a
# hours-long full ranking.

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
      shiny::actionButton(ns("run"), "Run pilot", class = "btn-primary"),
      shiny::hr(),
      shiny::helpText(
        shiny::em(
          "A pilot runs the ensemble at one replicate per model on a small ",
          "sample so you can see what the LLM is doing before committing to ",
          "the full run. Use it to catch obvious mis-scoring (a criterion ",
          "the LLM is ignoring, or one that triggers on off-topic records) ",
          "before you invest hours of compute."
        )
      )
    ),
    bslib::card(
      bslib::card_header("Pilot results"),
      DT::DTOutput(ns("results_table")),
      shiny::tags$hr(),
      shiny::tags$small(
        class = "text-muted",
        "Click a row to expand its per-model justifications."
      ),
      shiny::verbatimTextOutput(ns("details"))
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

    pilot_out <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run, {
      shiny::req(state$records, state$criteria, state$ensemble)
      shiny::withProgress(message = "Piloting ensemble...", value = 0.5, {
        out <- try(
          pilot(state$records, state$criteria,
                ensemble = state$ensemble,
                n = as.integer(input$n),
                sample = isTRUE(input$random),
                verbose = FALSE),
          silent = TRUE
        )
      })
      if (inherits(out, "try-error")) {
        shiny::showNotification(attr(out, "condition")$message, type = "error")
        return(NULL)
      }
      pilot_out(out)
    })

    # Render the pilot output as a sortable DT (score-ordered) so the
    # user can visually skim the ensemble's decisions. Selecting a row
    # reveals the raw per-model justifications below the table.
    output$results_table <- DT::renderDT({
      p <- pilot_out()
      if (is.null(p)) return(NULL)
      p <- p[order(-p$universal_best_score), , drop = FALSE]
      # Preview of the first justification, truncated for the table.
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

    # When the user picks a row, print all justifications for that
    # record verbatim so they can read what each model said.
    output$details <- shiny::renderText({
      p <- pilot_out()
      sel <- input$results_table_rows_selected
      if (is.null(p) || is.null(sel) || length(sel) == 0L) {
        return("(select a row above to see per-model justifications)")
      }
      p <- p[order(-p$universal_best_score), , drop = FALSE]
      rec <- p[sel[1L], ]
      js <- rec$justifications[[1L]]
      if (is.null(js) || nrow(js) == 0L) return("(no justifications recorded)")
      lines <- vapply(seq_len(nrow(js)), function(i) {
        sprintf("[%s / rep %d]\n%s",
                js$model[i], js$replicate[i],
                gsub("\\s+", " ", js$explanation[i] %||% ""))
      }, character(1))
      paste(lines, collapse = "\n\n")
    })
  })
}
