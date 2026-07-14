# Rank tab: start async ranking job, poll progress, review outputs.

#' @keywords internal
mod_rank_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(5, 7),
    bslib::card(
      bslib::card_header("Ranking job"),
      shiny::uiOutput(ns("readiness")),
      shiny::uiOutput(ns("estimate_banner")),
      shiny::actionButton(ns("start"), "Start ranking",
                          class = "btn-primary"),
      shiny::actionButton(ns("cancel"), "Cancel",
                          class = "btn-outline-danger"),
      shiny::hr(),
      shiny::textOutput(ns("status_line")),
      shiny::plotOutput(ns("progress_plot"), height = "40px")
    ),
    bslib::card(
      bslib::card_header("Ranked corpus (top 25)"),
      DT::DTOutput(ns("ranked_table"))
    )
  )
}

#' @keywords internal
mod_rank_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$estimate_banner <- shiny::renderUI({
      if (is.null(state$records) || is.null(state$ensemble)) return(NULL)
      est <- estimate_runtime(nrow(state$records), state$ensemble)
      shiny::tags$div(
        class = "alert alert-info py-1 my-1",
        shiny::tags$small(
          shiny::tags$strong("Estimated runtime: "),
          est$human_readable,
          sprintf(" (%s LLM calls at ~%.0fs each). Rough heuristic; GPUs run several times faster.",
                  format(est$n_calls, big.mark = ","), est$seconds_per_call)
        )
      )
    })

    output$readiness <- shiny::renderUI({
      needs <- c(
        if (is.null(state$project)) "a project" else NULL,
        if (is.null(state$records)) "an uploaded corpus" else NULL,
        if (is.null(state$criteria)) "inclusion criteria" else NULL,
        if (is.null(state$ensemble)) "an ensemble config" else NULL
      )
      if (length(needs) == 0L) {
        shiny::tags$span(class = "badge bg-success",
                         "All inputs ready. Click Start ranking.")
      } else {
        shiny::tags$div(
          shiny::tags$span(class = "badge bg-warning",
                           "Missing:"),
          shiny::tags$ul(lapply(needs, shiny::tags$li))
        )
      }
    })

    shiny::observeEvent(input$start, {
      shiny::req(state$project, state$records, state$criteria, state$ensemble)
      handle <- try(
        start_rank_job(state$project, ensemble = state$ensemble),
        silent = TRUE
      )
      if (inherits(handle, "try-error")) {
        shiny::showNotification(attr(handle, "condition")$message, type = "error")
        return(NULL)
      }
      state$rank_handle <- handle$handle
      shiny::showNotification("Ranking job started in background.", duration = 4)
    })

    shiny::observeEvent(input$cancel, {
      if (!is.null(state$rank_handle)) {
        rank_job_cancel(state$rank_handle)
        state$rank_handle <- NULL
        shiny::showNotification("Job cancelled.", duration = 3)
      }
    })

    # Poll the progress file every 500 ms while a job is running.
    poll <- shiny::reactivePoll(
      intervalMillis = 500,
      session = session,
      checkFunc = function() {
        if (is.null(state$project)) return(0)
        file.info(fs::path(project_dir(state$project, create = FALSE),
                            list_project_artefacts()["progress"]))$mtime
      },
      valueFunc = function() {
        if (is.null(state$project)) return(NULL)
        st <- rank_job_status(state$project)
        # If the job just finished, load the ranked object.
        if (identical(st$status, "done") && is.null(state$ranked)) {
          state$ranked <- load_artefact(state$project, "ranked")
        }
        st
      }
    )

    output$status_line <- shiny::renderText({
      st <- poll()
      if (is.null(st) || identical(st$status, "idle")) return("Idle.")
      elapsed <- if (!is.null(st$elapsed_secs) && is.finite(st$elapsed_secs)) {
        sprintf(" (%.0f s elapsed)", st$elapsed_secs)
      } else ""
      err <- if (isTRUE(nzchar(st$error))) sprintf(" | ERROR: %s", st$error) else ""
      sprintf("%s - %d/%d (%.1f%%)%s%s",
              toupper(st$status), st$processed, st$total, st$percent, elapsed, err)
    })

    output$progress_plot <- shiny::renderPlot({
      st <- poll()
      pct <- if (!is.null(st$percent)) st$percent else 0
      par(mar = c(0, 0, 0, 0))
      plot.new()
      rect(0, 0.3, 1, 0.7, col = "grey85", border = NA)
      rect(0, 0.3, pct / 100, 0.7,
           col = if (identical(st$status, "error")) "firebrick" else "steelblue",
           border = NA)
    })

    output$ranked_table <- DT::renderDT({
      r <- state$ranked
      if (is.null(r)) return(NULL)
      cols <- intersect(c("id", "rank", "universal_best_score", "title"), names(r))
      DT::datatable(
        r[seq_len(min(25L, nrow(r))), cols, drop = FALSE],
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
  })
}
