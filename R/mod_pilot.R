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
      shiny::verbatimTextOutput(ns("results"))
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

    output$results <- shiny::renderPrint({
      p <- pilot_out()
      if (is.null(p)) {
        cat("(no pilot run yet)\n")
        return(invisible())
      }
      print(p)
    })
  })
}
