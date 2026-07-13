# Shiny screening UI for `screenllm`.
# State (plan, ranking, decisions file path) is passed via the package's
# `.screenllm_shiny_state` environment, populated by `launch_screening_app()`.

library(shiny)
library(bslib)

state_env <- getFromNamespace(".screenllm_shiny_state", "screenllm")
plan <- state_env$plan
ranked <- state_env$ranked
out_file <- state_env$out_file %||% "screening_decisions.csv"

records <- plan$to_screen
n_records <- nrow(records)
prior_decisions <- if (file.exists(out_file)) {
  utils::read.csv(out_file, stringsAsFactors = FALSE)
} else {
  data.frame(
    id = character(0),
    human_decision = character(0),
    note = character(0),
    timestamp = character(0),
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

ui <- page_navbar(
  title = "screenllm",
  theme = bs_theme(bootswatch = "flatly"),
  nav_panel(
    "Screen",
    layout_sidebar(
      sidebar = sidebar(
        title = "Progress",
        uiOutput("progress_ui"),
        uiOutput("nav_buttons_ui"),
        hr(),
        helpText(
          "Decisions are auto-saved to ", tags$code(out_file),
          " after every click."
        )
      ),
      uiOutput("record_ui")
    )
  ),
  nav_panel(
    "All decisions",
    DT::DTOutput("decisions_table")
  )
)

server <- function(input, output, session) {
  # Reactive decisions log.
  decisions <- reactiveVal(prior_decisions)

  current_index <- reactiveVal({
    done_ids <- prior_decisions$id
    remaining <- which(!(records$id %in% done_ids))
    if (length(remaining) == 0L) length(records$id) else remaining[1L]
  })

  current_record <- reactive({
    idx <- current_index()
    if (idx > n_records) return(NULL)
    records[idx, , drop = FALSE]
  })

  record_justifications <- reactive({
    rec <- current_record()
    if (is.null(rec)) return(NULL)
    just <- ranked$justifications[ranked$id == rec$id][[1]]
    if (is.null(just) || nrow(just) == 0L) return(NULL)
    just
  })

  output$progress_ui <- renderUI({
    idx <- current_index()
    done <- min(idx - 1L, n_records)
    tagList(
      strong(sprintf("%d / %d screened", done, n_records)),
      br(),
      div(
        class = "progress",
        style = "height: 8px;",
        div(
          class = "progress-bar",
          role = "progressbar",
          style = sprintf("width: %.1f%%;", 100 * done / n_records)
        )
      )
    )
  })

  output$record_ui <- renderUI({
    rec <- current_record()
    if (is.null(rec)) {
      return(div(
        h3("All records above the stopping point have been screened."),
        p(sprintf(
          "Decisions saved to %s. You may close this window.",
          out_file
        ))
      ))
    }
    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        h4(sprintf("Record %d of %d", current_index(), n_records)),
        span(
          class = "badge bg-info",
          sprintf("LLM score: %.0f", rec$universal_best_score)
        )
      ),
      h5(rec$title),
      p(tags$em(rec$abstract %||% "(no abstract)")),
      hr(),
      div(
        style = "display: flex; gap: 8px;",
        actionButton("accept", "Accept", class = "btn-success", width = "160px"),
        actionButton("reject", "Reject", class = "btn-danger", width = "160px"),
        actionButton("skip", "Skip", class = "btn-secondary", width = "160px")
      ),
      textAreaInput(
        "note",
        label = "Note (optional)",
        width = "100%",
        rows = 2L
      ),
      hr(),
      h6("LLM per-model reasoning"),
      if (!is.null(record_justifications())) {
        DT::datatable(
          record_justifications(),
          options = list(dom = "t", pageLength = 20),
          rownames = FALSE
        )
      } else {
        p(em("No per-model justifications recorded."))
      }
    )
  })

  output$nav_buttons_ui <- renderUI({
    tagList(
      actionButton("prev_btn", "Previous", width = "100%", class = "btn-outline-secondary"),
      actionButton("next_btn", "Next (skip)", width = "100%", class = "btn-outline-secondary")
    )
  })

  save_decision <- function(decision) {
    rec <- current_record()
    if (is.null(rec)) return()
    row <- data.frame(
      id = rec$id,
      human_decision = decision,
      note = isolate(input$note %||% ""),
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      stringsAsFactors = FALSE
    )
    log <- decisions()
    # Overwrite any prior decision for this id.
    log <- log[log$id != rec$id, , drop = FALSE]
    log <- rbind(log, row)
    decisions(log)
    utils::write.csv(log, out_file, row.names = FALSE)
    updateTextAreaInput(session, "note", value = "")
    current_index(current_index() + 1L)
  }

  observeEvent(input$accept, save_decision("Accept"))
  observeEvent(input$reject, save_decision("Reject"))
  observeEvent(input$skip, {
    current_index(current_index() + 1L)
  })
  observeEvent(input$prev_btn, {
    current_index(max(1L, current_index() - 1L))
  })
  observeEvent(input$next_btn, {
    current_index(min(n_records + 1L, current_index() + 1L))
  })

  output$decisions_table <- DT::renderDT({
    DT::datatable(
      decisions(),
      options = list(pageLength = 25L, order = list(list(3L, "desc"))),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
