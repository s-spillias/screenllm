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
        "opposite. Worth a manual look; on the paper's Habitat Effect benchmark ",
        "a similar audit caught legitimate reviewer errors in 28% of these."
      )),
      DT::DTOutput(ns("audit_table")),
      shiny::downloadButton(ns("dl_audit"), "Download disagreements.csv")
    )
  )
}

#' @keywords internal
mod_report_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
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

    audit <- shiny::reactive({
      if (is.null(state$ranked) || is.null(state$decisions)) return(NULL)
      audit_disagreements(state$ranked, state$decisions)
    })

    output$audit_table <- DT::renderDT({
      a <- audit()
      if (is.null(a) || nrow(a) == 0L) {
        return(DT::datatable(
          data.frame(Message = "No strong disagreements (or nothing screened yet)."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      DT::datatable(
        a[, c("id", "universal_best_score", "human_decision", "disagreement",
              "title"), drop = FALSE],
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })

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
