# Screen tab: browse ranked records above the SAFE stop point,
# record Accept / Reject decisions, persist incrementally.

#' @keywords internal
mod_screen_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Records to screen"),
      shiny::checkboxInput(ns("hide_done"),
                            "Hide records I've already decided", value = TRUE),
      DT::DTOutput(ns("records_table"))
    ),
    bslib::card(
      bslib::card_header(shiny::textOutput(ns("record_title"))),
      shiny::htmlOutput(ns("record_meta")),
      shiny::tags$hr(),
      shiny::tags$h5("Abstract"),
      shiny::htmlOutput(ns("record_abstract")),
      shiny::tags$hr(),
      shiny::tags$h5("LLM justification"),
      shiny::verbatimTextOutput(ns("record_justification")),
      shiny::tags$hr(),
      shiny::fluidRow(
        shiny::column(4, shiny::actionButton(ns("accept"), "Accept",
                                              class = "btn-success btn-lg",
                                              width = "100%")),
        shiny::column(4, shiny::actionButton(ns("reject"), "Reject",
                                              class = "btn-danger btn-lg",
                                              width = "100%")),
        shiny::column(4, shiny::actionButton(ns("skip"), "Skip",
                                              class = "btn-secondary btn-lg",
                                              width = "100%"))
      ),
      shiny::tags$br(),
      shiny::textAreaInput(ns("note"), "Note (optional)", "",
                            rows = 2, width = "100%")
    )
  )
}

#' @keywords internal
mod_screen_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    current <- shiny::reactiveVal(1L)

    to_screen_df <- shiny::reactive({
      p <- state$plan
      if (is.null(p)) return(NULL)
      df <- p$to_screen
      df$row_no <- seq_len(nrow(df))
      df
    })

    decisions_df <- shiny::reactive({
      d <- state$decisions
      if (is.null(d) || nrow(d) == 0L) {
        return(data.frame(id = character(), human_decision = character()))
      }
      d
    })

    visible <- shiny::reactive({
      df <- to_screen_df(); if (is.null(df)) return(NULL)
      if (isTRUE(input$hide_done)) {
        df <- df[!(df$id %in% decisions_df()$id), , drop = FALSE]
      }
      df
    })

    active <- shiny::reactive({
      v <- visible(); if (is.null(v) || nrow(v) == 0L) return(NULL)
      v[min(current(), nrow(v)), , drop = FALSE]
    })

    output$records_table <- DT::renderDT({
      v <- visible()
      if (is.null(v)) {
        return(DT::datatable(
          data.frame(Message = "Save a plan first."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      done_ids <- decisions_df()$id
      v$done <- ifelse(v$id %in% done_ids, "yes", "")
      DT::datatable(
        v[, c("row_no", "done", "universal_best_score", "title"), drop = FALSE],
        selection = "single",
        colnames = c("Rank", "Done", "Score", "Title"),
        options = list(pageLength = 20, order = list(list(0, "asc"))),
        rownames = FALSE
      )
    })

    shiny::observeEvent(input$records_table_rows_selected, {
      if (length(input$records_table_rows_selected)) {
        current(input$records_table_rows_selected)
      }
    })

    output$record_title <- shiny::renderText({
      r <- active()
      if (is.null(r)) "No records to screen" else r$title
    })

    output$record_meta <- shiny::renderUI({
      r <- active(); if (is.null(r)) return(NULL)
      shiny::HTML(sprintf(
        "<b>ID:</b> %s &nbsp; <b>Rank:</b> %d &nbsp; <b>LLM score:</b> %.1f",
        r$id, r$rank, r$universal_best_score
      ))
    })

    output$record_abstract <- shiny::renderUI({
      r <- active(); if (is.null(r)) return(NULL)
      shiny::HTML(gsub("\n", "<br>", r$abstract %||% "", fixed = TRUE))
    })

    output$record_justification <- shiny::renderText({
      r <- active(); if (is.null(r)) return("")
      just <- state$ranked$justifications[state$ranked$id == r$id][[1]]
      if (is.null(just) || nrow(just) == 0L) return("(no justification recorded)")
      paste(sprintf("[%s r%d] %s", just$model, just$replicate, just$explanation),
            collapse = "\n\n")
    })

    record_decision <- function(decision) {
      shiny::req(state$project)
      r <- active(); if (is.null(r)) return(invisible())
      new_row <- data.frame(
        id = r$id, human_decision = decision, note = input$note,
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        stringsAsFactors = FALSE
      )
      d <- decisions_df()
      d <- d[d$id != r$id, , drop = FALSE]
      d <- rbind(d, new_row)
      state$decisions <- d
      save_artefact(state$project, "decisions", d)
      shiny::updateTextAreaInput(session, "note", value = "")
      current(current() + 1L)
    }

    shiny::observeEvent(input$accept, record_decision("Accept"))
    shiny::observeEvent(input$reject, record_decision("Reject"))
    shiny::observeEvent(input$skip, current(current() + 1L))
  })
}
