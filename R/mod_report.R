# Report tab: summarise the screen, audit strong disagreements, offer
# CSV downloads for decisions and the audit queue.

#' @keywords internal
mod_report_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(5, 7),
    bslib::card(
      bslib::card_header("Summary"),
      shiny::uiOutput(ns("summary")),
      shiny::hr(),
      shiny::downloadButton(ns("dl_decisions"), "Download decisions.csv"),
      shiny::downloadButton(ns("dl_ranked"), "Download ranked corpus (CSV)"),
      shiny::downloadButton(ns("dl_report"), "Download report (RDS)"),
      shiny::downloadButton(ns("dl_html"), "Download HTML report"),
      shiny::helpText(shiny::em(
        "The HTML report is a self-contained document; open it in a ",
        "browser and use Print > Save as PDF to archive."
      ))
    ),
    bslib::card(
      bslib::card_header("Strong LLM-human disagreements"),
      shiny::helpText(shiny::em(
        "Records where the ensemble was confident but the human decided the ",
        "opposite. Worth a manual look; on one of the paper's benchmark ",
        "reviews, a similar audit caught legitimate reviewer errors in 28% of these."
      )),
      # Wrap DT in a clearfix so its info row doesn't stack over
      # the review panel below.
      shiny::div(
        class = "clearfix",
        DT::DTOutput(ns("audit_table"))
      ),
      shiny::div(
        class = "d-flex align-items-center gap-2 mt-2",
        shiny::downloadButton(ns("dl_audit"),
                               "Download disagreements.csv",
                               class = "btn-sm btn-outline-secondary"),
        shiny::tags$small(class = "text-muted ms-2",
                          "Click a row to review the record and change your decision.")
      ),
      shiny::tags$hr(class = "my-2"),
      shiny::div(
        style = "max-height: 380px; overflow-y: auto; overflow-x: hidden;",
        shiny::uiOutput(ns("review_panel"))
      )
    )
  )
}

#' @keywords internal
mod_report_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    report <- shiny::reactive({
      if (is.null(state$ranked) || is.null(state$decisions)) return(NULL)
      summarise_screening(state$ranked, state$decisions, plan = state$plan)
    })

    output$summary <- shiny::renderUI({
      rep <- report()
      if (is.null(rep)) return(shiny::em("Screen some records to see the report."))
      shiny::tags$ul(
        shiny::tags$li(sprintf("Records total: %d", rep$n_records)),
        shiny::tags$li(sprintf("Records screened: %d (%.1f%% workload)",
                                rep$n_screened, rep$workload_pct)),
        shiny::tags$li(sprintf("Human accepts: %d", rep$n_accepts)),
        if (!is.na(rep$stop_at)) shiny::tags$li(sprintf(
          "Planned stop at: record %d", rep$stop_at
        ))
      )
    })

    # Same tryCatch pattern used on the Plan tab: never let an
    # audit_disagreements() error grey out the report or render as
    # "[object Object]" from Shiny's default error handler.
    audit_error <- shiny::reactiveVal(NULL)
    audit <- shiny::reactive({
      if (is.null(state$ranked) || is.null(state$decisions) ||
            nrow(state$decisions) == 0L) {
        audit_error(NULL)
        return(NULL)
      }
      out <- tryCatch(
        audit_disagreements(state$ranked, state$decisions),
        error = function(e) e
      )
      if (inherits(out, "error")) {
        audit_error(conditionMessage(out))
        return(NULL)
      }
      audit_error(NULL)
      out
    })

    output$audit_table <- DT::renderDT({
      err <- audit_error()
      if (!is.null(err)) {
        return(DT::datatable(
          data.frame(Error = err),
          options = list(dom = "t"), rownames = FALSE,
          colnames = "audit_disagreements() failed"
        ))
      }
      a <- audit()
      if (is.null(a) || nrow(a) == 0L) {
        return(DT::datatable(
          data.frame(Message = "No strong disagreements (or nothing screened yet)."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      # Coloured HTML badges for FP vs FN so the eye can pick them
      # apart at a glance. FP = LLM over-accepts (amber/warning);
      # FN = LLM under-accepts (red/danger).
      kind <- ifelse(startsWith(a$disagreement, "Strong FN"), "FN", "FP")
      badge <- ifelse(
        kind == "FN",
        '<span class="badge bg-danger">FN: LLM missed accept</span>',
        '<span class="badge bg-warning text-dark">FP: LLM over-accepts</span>'
      )
      score_badge <- sprintf(
        '<span class="badge bg-secondary">%.0f</span>',
        a$universal_best_score
      )
      human_badge <- ifelse(
        a$human_decision == "Accept",
        '<span class="badge bg-success">Accept</span>',
        '<span class="badge bg-secondary">Reject</span>'
      )
      tbl <- data.frame(
        kind = badge,
        score = score_badge,
        human = human_badge,
        id = a$id,
        title = a$title,
        stringsAsFactors = FALSE
      )
      DT::datatable(
        tbl,
        colnames = c("Type", "LLM score", "Human", "ID", "Title"),
        escape = FALSE,  # render badge HTML
        options = list(
          dom = "ti", paging = FALSE,
          scrollY = "460px", scrollCollapse = TRUE,
          autoWidth = FALSE, scrollX = TRUE
        ),
        rownames = FALSE,
        selection = "single",
        class = "compact"
      )
    })

    # ---- Revisit panel for the disagreements table ----------------
    # Track the currently-selected disagreement by record id (not by
    # DT row index) so re-renders don't lose the selection.
    selected_id <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$audit_table_rows_selected,
                        ignoreNULL = FALSE, {
      sel <- input$audit_table_rows_selected
      if (length(sel) == 0L) return()
      a <- shiny::isolate(audit())
      if (is.null(a) || nrow(a) == 0L) return()
      if (sel[1L] >= 1L && sel[1L] <= nrow(a)) {
        selected_id(as.character(a$id[sel[1L]]))
      }
    })

    output$review_panel <- shiny::renderUI({
      sid <- selected_id()
      if (is.null(sid)) {
        return(shiny::tags$p(
          class = "text-muted fst-italic small mt-2",
          "Select a row above to review this record and (if you want) ",
          "change your decision."
        ))
      }
      r <- state$ranked
      if (is.null(r)) return(NULL)
      idx <- which(r$id == sid)
      if (length(idx) == 0L) {
        return(shiny::tags$p(class = "text-muted fst-italic small mt-2",
                             "(record not found)"))
      }
      rec <- r[idx[1L], ]
      # Current human decision (from state$decisions, not the ranked
      # ground truth column).
      current_dec <- {
        d <- normalise_decisions_shape(state$decisions)
        val <- d$human_decision[d$id == sid]
        if (length(val) == 0L || is.na(val[1])) NA_character_ else val[1]
      }
      dec_badge <- if (is.na(current_dec)) {
        shiny::tags$span(class = "badge bg-secondary", "not screened")
      } else if (current_dec == "Accept") {
        shiny::tags$span(class = "badge bg-success", "your decision: Accept")
      } else {
        shiny::tags$span(class = "badge bg-secondary", "your decision: Reject")
      }
      # LLM justifications block, same style as Rank + Screen tabs.
      just <- rec$justifications[[1L]]
      panels <- if (is.null(just) || nrow(just) == 0L) {
        shiny::tags$em(class = "text-muted small",
                       "(no justifications recorded)")
      } else {
        lapply(seq_len(nrow(just)), function(i) {
          explanation <- gsub("\\s+", " ",
                              just$explanation[i] %||% "")
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
                          else shiny::tags$em("(no explanation)"))
          )
        })
      }
      # Only offer the "flip" button (the opposite of the current
      # decision); leave the current-choice button as a no-op
      # confirm.
      shiny::tagList(
        shiny::tags$div(
          class = "mb-2",
          shiny::tags$span(class = "badge bg-primary me-2",
                           sprintf("LLM score %.0f", rec$universal_best_score)),
          dec_badge,
          shiny::tags$span(class = "ms-2 fw-semibold small",
                           substr(rec$title %||% "", 1, 200))
        ),
        shiny::tags$p(
          class = "small text-break text-wrap mb-2",
          substr(rec$abstract %||% "", 1, 1500),
          if (nchar(rec$abstract %||% "") > 1500) "..."
        ),
        shiny::tags$small(class = "text-muted d-block mb-1",
                          "Per-model justifications:"),
        panels,
        shiny::tags$hr(class = "my-2"),
        shiny::tags$div(
          class = "d-flex gap-2 justify-content-end",
          shiny::actionButton(ns("flip_accept"), "Change to Accept",
                              class = "btn-success btn-sm",
                              icon = shiny::icon("check")),
          shiny::actionButton(ns("flip_reject"), "Change to Reject",
                              class = "btn-danger btn-sm",
                              icon = shiny::icon("xmark"))
        )
      )
    })

    # Shared handler: write the new decision to state$decisions and
    # persist. Reactive graph then re-runs audit; a fixed
    # disagreement drops out of the table.
    flip_to <- function(new_decision) {
      shiny::req(state$project)
      sid <- selected_id()
      if (is.null(sid)) return(invisible())
      d <- normalise_decisions_shape(state$decisions)
      d <- d[d$id != sid, , drop = FALSE]
      d <- dplyr::bind_rows(d, data.frame(
        id = sid, human_decision = new_decision,
        note = "revised on Report tab",
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        stringsAsFactors = FALSE
      ))
      d <- normalise_decisions_shape(d)
      state$decisions <- d
      save_artefact(state$project, "decisions", d)
      shiny::showNotification(
        sprintf("%s -> %s.", sid, new_decision),
        type = "message", duration = 4
      )
    }
    shiny::observeEvent(input$flip_accept, flip_to("Accept"))
    shiny::observeEvent(input$flip_reject, flip_to("Reject"))

    output$dl_decisions <- shiny::downloadHandler(
      filename = function() "decisions.csv",
      content = function(file) {
        utils::write.csv(state$decisions %||% data.frame(), file, row.names = FALSE)
      }
    )

    output$dl_ranked <- shiny::downloadHandler(
      filename = function() "ranked.csv",
      content = function(file) {
        r <- state$ranked; if (is.null(r)) return()
        # Drop list-columns before writing to CSV.
        drop <- intersect(c("per_model_scores", "justifications"), names(r))
        utils::write.csv(r[, setdiff(names(r), drop)], file, row.names = FALSE)
      }
    )

    output$dl_report <- shiny::downloadHandler(
      filename = function() "report.rds",
      content = function(file) saveRDS(report(), file)
    )

    output$dl_html <- shiny::downloadHandler(
      filename = function() {
        proj <- state$project %||% "screenllm"
        sprintf("%s-screening-report.html", proj)
      },
      content = function(file) {
        shiny::withProgress(message = "Rendering report...", value = 0.5, {
          export_report(
            output_file = file,
            project = state$project,
            ranked = state$ranked,
            plan = state$plan,
            decisions = state$decisions,
            criteria = state$criteria,
            ensemble = state$ensemble
          )
        })
      }
    )

    output$dl_audit <- shiny::downloadHandler(
      filename = function() "disagreements.csv",
      content = function(file) {
        a <- audit(); if (is.null(a)) a <- data.frame()
        utils::write.csv(a, file, row.names = FALSE)
      }
    )
  })
}
