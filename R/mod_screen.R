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
      # Rendered non-empty only when the user has actually screened
      # everything above the stop point AND "hide already decided"
      # is on -- so the table below is empty for a helpful reason,
      # not because the plan is empty or the user is confused.
      shiny::uiOutput(ns("done_banner")),
      DT::DTOutput(ns("records_table"))
    ),
    bslib::card(
      # Compact single-record header: title on top, metadata badges +
      # action buttons on one row underneath. No huge <h5>s, no
      # verbatim monospace blocks. Everything below fits in one screen
      # for a laptop viewport.
      bslib::card_header(
        shiny::div(
          shiny::uiOutput(ns("record_header"))
        )
      ),
      # Metadata + action row.
      shiny::div(
        class = "d-flex align-items-center gap-2 flex-wrap mb-2",
        shiny::uiOutput(ns("record_meta"), inline = TRUE),
        shiny::div(class = "ms-auto d-flex gap-1",
          shiny::actionButton(ns("accept"), "Accept",
                              class = "btn-success", icon = shiny::icon("check")),
          shiny::actionButton(ns("reject"), "Reject",
                              class = "btn-danger", icon = shiny::icon("xmark")),
          shiny::actionButton(ns("skip"), "Skip",
                              class = "btn-outline-secondary", icon = shiny::icon("forward"))
        )
      ),
      # Two independent scroll regions inside the card body so the
      # abstract and the justifications can be scanned separately.
      shiny::div(
        style = "max-height: 220px; overflow-y: auto; overflow-x: hidden;",
        shiny::tags$p(
          class = "small mb-0 text-break text-wrap",
          shiny::uiOutput(ns("record_abstract"))
        )
      ),
      shiny::tags$hr(class = "my-2"),
      shiny::tags$small(class = "text-muted",
                        "Per-model justifications:"),
      shiny::div(
        style = "max-height: 260px; overflow-y: auto; overflow-x: hidden;",
        shiny::uiOutput(ns("record_justification"))
      ),
      shiny::tags$hr(class = "my-2"),
      shiny::textAreaInput(ns("note"), NULL, "",
                           placeholder = "Optional note about this decision...",
                           rows = 1, width = "100%")
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
      # Older plan artefacts (pre-0.1.0 schema) may not have populated
      # $to_screen; nrow(NULL) is NULL and seq_len(NULL) crashes, which
      # greys the whole Screen tab on project load.
      if (!is.data.frame(df) || nrow(df) == 0L) return(NULL)
      df$row_no <- seq_len(nrow(df))
      df
    })

    # Empty-placeholder must match the schema of the rows we actually
    # append below (id, human_decision, note, timestamp); previously
    # it had only two columns, so the very first rbind() with a full
    # new_row failed and the observer silently swallowed the error,
    # discarding every decision the user made.
    decisions_df <- shiny::reactive({
      normalise_decisions_shape(state$decisions)
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

    # True when the user has decided every record in the plan (so
    # visible() with hide_done ON is empty, but the underlying plan
    # is not).
    all_done <- shiny::reactive({
      full <- to_screen_df()
      if (is.null(full) || nrow(full) == 0L) return(FALSE)
      dec_ids <- decisions_df()$id
      all(full$id %in% dec_ids)
    })

    output$done_banner <- shiny::renderUI({
      if (!isTRUE(input$hide_done)) return(NULL)
      if (!isTRUE(all_done())) return(NULL)
      full <- to_screen_df()
      n <- if (is.null(full)) 0L else nrow(full)
      shiny::div(
        class = "alert alert-success d-flex align-items-center gap-2 py-2 px-3 mb-2",
        role = "alert",
        shiny::icon("circle-check"),
        shiny::tags$div(
          shiny::tags$strong("All screened."),
          sprintf(" You've recorded a decision on all %d record%s ", n,
                  if (n == 1L) "" else "s"),
          "in the plan.",
          shiny::tags$br(),
          shiny::tags$small(class = "text-muted",
                            "Untick \"Hide records I've already decided\" to review or change any decision.")
        )
      )
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
      v$done <- ifelse(v$id %in% done_ids, "y", "")
      DT::datatable(
        v[, c("row_no", "done", "universal_best_score", "title"), drop = FALSE],
        selection = "single",
        colnames = c("#", "Done", "Score", "Title"),
        options = list(
          dom = "ti", paging = FALSE,
          scrollY = "560px", scrollCollapse = TRUE,
          autoWidth = FALSE, scrollX = TRUE,
          order = list(list(0, "asc"))
        ),
        rownames = FALSE, class = "compact"
      )
    })

    shiny::observeEvent(input$records_table_rows_selected, {
      if (length(input$records_table_rows_selected)) {
        current(input$records_table_rows_selected)
      }
    })

    output$record_header <- shiny::renderUI({
      r <- active()
      if (is.null(r)) {
        return(shiny::span(class = "text-muted", "No records to screen"))
      }
      shiny::tags$strong(r$title)
    })

    output$record_meta <- shiny::renderUI({
      r <- active(); if (is.null(r)) return(NULL)
      shiny::tagList(
        shiny::tags$span(class = "badge bg-primary",
                         sprintf("score %.0f", r$universal_best_score)),
        shiny::tags$span(class = "badge bg-secondary",
                         sprintf("rank %d", r$rank)),
        shiny::tags$span(class = "badge bg-light text-dark font-monospace",
                         r$id)
      )
    })

    output$record_abstract <- shiny::renderUI({
      r <- active(); if (is.null(r)) return(NULL)
      abstract <- r$abstract %||% ""
      if (!nzchar(abstract)) {
        return(shiny::em(class = "text-muted", "(no abstract)"))
      }
      # `\r?\n` catches CRLF line endings from Windows-authored
      # abstracts too; the old fixed = TRUE + "\n" left a stray \r
      # before every <br>.
      shiny::HTML(gsub("\r?\n", "<br>", abstract))
    })

    output$record_justification <- shiny::renderUI({
      r <- active(); if (is.null(r)) {
        return(shiny::em(class = "text-muted small",
                         "No record selected."))
      }
      # Guard three failure modes: (a) ranked artefact from an older
      # schema with no justifications column at all (state$ranked$justifications
      # is NULL -> [[1]] out of bounds); (b) record id not present in
      # ranked (empty subset -> [[1]] out of bounds); (c) justification
      # cell is a bare list (older schema) rather than a data frame,
      # making nrow() return NULL and the `if` cascade to NA.
      just <- tryCatch(
        state$ranked$justifications[state$ranked$id == r$id][[1]],
        error = function(e) NULL
      )
      if (is.null(just) || !is.data.frame(just) || nrow(just) == 0L) {
        return(shiny::em(class = "text-muted small",
                         "(no justification recorded)"))
      }
      # One bordered card per model/replicate, wrapping long text,
      # matching the Rank tab's per-model panel style.
      lapply(seq_len(nrow(just)), function(i) {
        explanation <- just$explanation[i] %||% ""
        explanation <- gsub("\\s+", " ", explanation)
        shiny::tags$div(
          class = "border rounded p-2 mb-2 bg-body-tertiary",
          shiny::tags$div(
            class = "d-flex justify-content-between align-items-baseline mb-1",
            shiny::tags$code(class = "small", just$model[i]),
            shiny::tags$span(class = "small text-muted",
                             sprintf("replicate %d", just$replicate[i]))
          ),
          shiny::tags$p(class = "mb-0 small text-break text-wrap",
                        if (nzchar(explanation)) explanation
                        else shiny::tags$em("(no explanation returned)"))
        )
      })
    })

    record_decision <- function(decision) {
      shiny::req(state$project)
      r <- active(); if (is.null(r)) return(invisible())
      new_row <- data.frame(
        id = r$id, human_decision = decision,
        note = input$note %||% "",
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        stringsAsFactors = FALSE
      )
      d <- decisions_df()
      d <- d[d$id != r$id, , drop = FALSE]
      # bind_rows tolerates a column-name mismatch (fills with NA)
      # so any legacy 2-column state$decisions loaded from an older
      # session doesn't wedge new decisions like base rbind did.
      d <- dplyr::bind_rows(d, new_row)
      d <- normalise_decisions_shape(d)  # canonical schema before save
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
