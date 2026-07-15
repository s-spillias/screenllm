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
    # Track the last plan_screening error so we can display it in the
    # summary panel instead of letting it propagate and grey out the
    # app. Cleared whenever a fresh computation succeeds.
    plan_error <- shiny::reactiveVal(NULL)

    live_plan <- shiny::reactive({
      r <- state$ranked
      if (is.null(r)) return(NULL)
      out <- tryCatch(
        plan_screening(
          r,
          target_recall = input$target_recall,
          safe_min_cover = input$min_cover,
          safe_run_length = as.integer(input$run_length),
          spot_check_n = as.integer(input$spot_check_n)
        ),
        error = function(e) e
      )
      if (inherits(out, "error")) {
        plan_error(conditionMessage(out))
        return(NULL)
      }
      plan_error(NULL)
      out
    })

    output$score_plot <- shiny::renderPlot({
      r <- state$ranked; plan <- live_plan()
      if (is.null(r)) return(NULL)
      par(mar = c(4, 4, 1, 1))
      plot(r$rank, r$universal_best_score, type = "h", lwd = 1,
           col = "grey60",
           xlab = "Rank position", ylab = "LLM score (0-100)",
           ylim = c(0, 100))
      abline(h = 50, col = "grey80", lty = 3)  # "probable accept" cutoff
      if (!is.null(plan)) {
        # Vertical markers for each gate that has a finite fire
        # position: min coverage in blue, run length in orange, spot
        # check (when evaluated) in purple. Stop position in red on
        # top, dashed.
        gp <- plan$gates
        marker <- function(pos, colour, label) {
          if (!is.finite(pos)) return()
          abline(v = pos, col = colour, lwd = 1, lty = 3)
        }
        marker(gp$min_coverage$position, "steelblue")
        marker(gp$run_length$position,   "darkorange")
        if (isTRUE(gp$spot_check$evaluated)) {
          marker(gp$spot_check$position, "purple")
        }
        abline(v = plan$stop_at, col = "firebrick", lwd = 2.5, lty = 2)
        legend("topright", bty = "n", cex = 0.9,
               legend = c(
                 sprintf("stop at %d (%.0f%% workload)",
                         plan$stop_at, plan$expected_workload_pct),
                 "min-coverage gate",
                 "run-length gate"
               ),
               col = c("firebrick", "steelblue", "darkorange"),
               lty = c(2, 3, 3), lwd = c(2.5, 1, 1))
      }
    })

    output$summary <- shiny::renderUI({
      plan <- live_plan()
      err <- plan_error()
      if (!is.null(err)) {
        return(shiny::tags$div(
          class = "alert alert-danger py-2 my-1",
          shiny::tags$strong("plan_screening() failed:"),
          shiny::tags$br(),
          shiny::tags$code(err)
        ))
      }
      if (is.null(plan)) return(shiny::em("Waiting for a ranked corpus."))

      gp <- plan$gates
      N  <- plan$N
      # Small helper: pretty per-gate row.
      gate_row <- function(name, pretty, gate, threshold_desc,
                            hint_when_missing = NULL) {
        pos <- gate$position
        binding <- identical(plan$binding_gate, name)
        if (is.finite(pos)) {
          badge <- if (binding) {
            shiny::tags$span(class = "badge bg-danger ms-1", "binding")
          } else {
            shiny::tags$span(class = "badge bg-success ms-1", "OK")
          }
          shiny::tags$li(
            shiny::tags$strong(pretty), ": ",
            sprintf("fires at record %d", as.integer(pos)),
            " (", threshold_desc, ")", badge
          )
        } else {
          shiny::tags$li(
            shiny::tags$strong(pretty), ": ",
            shiny::tags$span(class = "text-danger",
                              "never fires with current settings"),
            if (!is.null(hint_when_missing)) shiny::tagList(
              shiny::tags$br(),
              shiny::tags$small(class = "text-muted", hint_when_missing)
            )
          )
        }
      }

      # Hints tailored to each impossible-to-fire case.
      # NB: sprintf takes ONE format string then values. Splitting the
      # message across multiple strings makes each string a separate
      # format arg -- exactly the bug that landed here before.
      run_length_hint <- if (!gp$run_length$fires) {
        sprintf(
          paste0(
            "Longest actual negative streak in the ranking is %d. ",
            "Lower the run-length slider to at most %d for this gate ",
            "to fire."
          ),
          as.integer(plan$max_negative_streak),
          as.integer(plan$max_negative_streak)
        )
      } else NULL

      shiny::tagList(
        shiny::tags$div(
          class = if (plan$stop_at == N) "alert alert-warning py-2 my-2"
                  else "alert alert-success py-2 my-2",
          if (plan$stop_at == N) {
            shiny::tagList(
              shiny::tags$strong("SAFE cannot stop before the end. "),
              sprintf("You would screen all %d records (100%% workload). ",
                      N),
              "See which gate is holding things below."
            )
          } else {
            shiny::tagList(
              shiny::tags$strong(sprintf("Stop at record %d of %d",
                                          plan$stop_at, N)),
              sprintf(" - screen %d, exclude %d (%.0f%% workload).",
                      plan$stop_at, N - plan$stop_at,
                      plan$expected_workload_pct)
            )
          }
        ),
        shiny::tags$ul(
          class = "list-unstyled small",
          gate_row("min_coverage", "Gate 1 - min coverage",
                    gp$min_coverage,
                    sprintf(">= %d records seen", as.integer(gp$min_coverage$position %||% 1))),
          gate_row("run_length", "Gate 2 - run length",
                    gp$run_length,
                    sprintf(">= %d consecutive rejects",
                            plan$settings$safe_run_length),
                    hint_when_missing = run_length_hint),
          if (isTRUE(gp$spot_check$evaluated)) {
            gate_row("spot_check", "Gate 3 - spot check",
                      gp$spot_check, "spot-check accepts >= target")
          } else {
            shiny::tags$li(
              shiny::tags$strong("Gate 3 - spot check"), ": ",
              shiny::tags$span(class = "text-muted",
                                "not evaluated (no labelled spot-check records provided)")
            )
          }
        ),
        shiny::tags$small(
          class = "text-muted d-block mt-2",
          "Stop point is the first position where ALL gates fire. ",
          "The red dashed line on the plot marks the stop; the coloured ",
          "dotted lines mark each gate's earliest fire position."
        )
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
