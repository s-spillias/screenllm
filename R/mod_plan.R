# Plan tab: SAFE parameter sliders, live stop-point plot.

#' @keywords internal
mod_plan_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("SAFE settings"),
      shiny::sliderInput(ns("target_recall"), "Target recall:",
                         min = 0.80, max = 0.99, value = 0.95, step = 0.01),
      shiny::sliderInput(ns("min_cover"), "Minimum coverage:",
                         min = 0.05, max = 0.90, value = 0.50, step = 0.05),
      shiny::sliderInput(ns("run_length"), "Consecutive-negative run length:",
                         min = 5L, max = 500L, value = 50L, step = 5L),
      shiny::numericInput(ns("spot_check_n"), "Spot-check size (n):",
                          value = 200, min = 20, max = 1000),
      shiny::actionButton(ns("save"), "Save plan", class = "btn-success")
    ),
    bslib::card(
      bslib::card_header("Score distribution + SAFE stop point"),
      shiny::plotOutput(ns("score_plot"), height = "260px"),
      shiny::hr(),
      shiny::uiOutput(ns("summary"))
    )
  )
}

#' @keywords internal
mod_plan_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    live_plan <- shiny::reactive({
      r <- state$ranked
      if (is.null(r)) return(NULL)
      plan_screening(
        r,
        target_recall = input$target_recall,
        safe_min_cover = input$min_cover,
        safe_run_length = as.integer(input$run_length),
        spot_check_n = as.integer(input$spot_check_n)
      )
    })

    output$score_plot <- shiny::renderPlot({
      r <- state$ranked; plan <- live_plan()
      if (is.null(r)) return(NULL)
      par(mar = c(4, 4, 1, 1))
      plot(r$rank, r$universal_best_score, type = "h", lwd = 1,
           col = "grey60",
           xlab = "Rank position", ylab = "LLM score (0-100)",
           ylim = c(0, 100))
      if (!is.null(plan)) {
        abline(v = plan$stop_at, col = "firebrick", lwd = 2, lty = 2)
        legend("topright", bty = "n",
               legend = sprintf("Stop at %d (%.1f%% workload)",
                                plan$stop_at, plan$expected_workload_pct),
               text.col = "firebrick")
      }
    })

    output$summary <- shiny::renderUI({
      plan <- live_plan()
      if (is.null(plan)) return(shiny::em("Waiting for a ranked corpus."))
      shiny::tags$ul(
        shiny::tags$li(sprintf("Stop at record %d of %d", plan$stop_at, plan$N)),
        shiny::tags$li(sprintf("Records to screen: %d", plan$stop_at)),
        shiny::tags$li(sprintf("Records excluded: %d", plan$N - plan$stop_at)),
        shiny::tags$li(sprintf("Expected workload: %.1f%%", plan$expected_workload_pct))
      )
    })

    shiny::observeEvent(input$save, {
      shiny::req(state$project)
      p <- live_plan()
      if (is.null(p)) {
        shiny::showNotification("No plan to save (rank the corpus first).",
                                 type = "warning")
        return(NULL)
      }
      state$plan <- p
      save_artefact(state$project, "plan", p)
      shiny::showNotification("Plan saved.", duration = 3)
    })
  })
}
