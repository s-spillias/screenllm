# Corpus tab: upload records, preview, save to project.

#' @keywords internal
mod_corpus_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Upload"),
      shiny::uiOutput(ns("project_hint")),
      shiny::fileInput(
        ns("file"), "Corpus (CSV, TSV, XLSX, or RIS):",
        accept = c(".csv", ".tsv", ".xlsx", ".xls", ".ris", ".txt")
      ),
      shiny::helpText(
        "Accepts CSV, TSV, Excel, and RIS exports from Zotero, EndNote, ",
        "Mendeley, or Web of Science. Column names from Scopus / WoS / ",
        "EndNote / Zotero are auto-normalised. At minimum the file needs ",
        shiny::tags$code("title"), " and ", shiny::tags$code("abstract"),
        " columns (or the RIS equivalents ", shiny::tags$code("TI"),
        " / ", shiny::tags$code("AB"), ")."
      ),
      shiny::hr(),
      shiny::tags$small(class = "text-muted d-block mb-1",
                        "Try it with the built-in toy corpus:"),
      shiny::div(
        class = "d-flex gap-2 flex-wrap",
        shiny::actionButton(ns("use_toy"), "Load toy CBFM corpus",
                            class = "btn-outline-secondary btn-sm"),
        shiny::downloadButton(ns("dl_toy"), "Download toy CBFM (CSV)",
                              class = "btn-outline-secondary btn-sm",
                              icon = shiny::icon("file-csv"))
      ),
      shiny::tags$small(
        class = "text-muted d-block mt-1",
        "The CSV shows the expected input format: an ", shiny::tags$code("id"),
        ", ", shiny::tags$code("title"), ", ", shiny::tags$code("abstract"),
        " column, plus (in the toy file only) a ",
        shiny::tags$code("human_decision"),
        " column with ground-truth screening labels for benchmarking. ",
        "Toy corpus + labels are from Spillias et al. (2024), ",
        shiny::tags$em("Human-AI collaboration to identify literature for evidence synthesis"),
        ", Cell Reports Sustainability, ",
        shiny::tags$a(href = "https://doi.org/10.1016/j.crsus.2024.100132",
                      target = "_blank", "doi.org/10.1016/j.crsus.2024.100132"),
        "."
      ),
      shiny::hr(),
      shiny::uiOutput(ns("summary")),
      shiny::uiOutput(ns("dup_banner"))
    ),
    bslib::card(
      bslib::card_header("Preview"),
      DT::DTOutput(ns("preview"))
    )
  )
}

#' @keywords internal
mod_corpus_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Ensure a project exists before we try to save anything to disk.
    # If none has been picked on Setup, create a "default" project so
    # the user is not left wondering why buttons appear to do nothing.
    ensure_project <- function() {
      if (is.null(state$project) || !nzchar(state$project)) {
        state$project <- slugify_project_name("default")
        project_dir(state$project, create = TRUE)
        shiny::showNotification(
          sprintf(
            "No project was selected, so I auto-created \"%s\". You can rename it on the Setup tab.",
            state$project
          ),
          type = "warning", duration = 6
        )
      }
      state$project
    }

    output$project_hint <- shiny::renderUI({
      if (is.null(state$project)) {
        shiny::tags$div(
          class = "alert alert-warning",
          shiny::tags$strong("No project selected."),
          " Loading a corpus below will auto-create a \"default\" project."
        )
      } else {
        shiny::tags$div(
          class = "alert alert-info",
          shiny::tags$strong("Saving to project: "),
          shiny::tags$code(state$project)
        )
      }
    })

    shiny::observeEvent(input$file, {
      proj <- ensure_project()
      f <- input$file
      records <- try(read_records(f$datapath), silent = TRUE)
      if (inherits(records, "try-error")) {
        shiny::showNotification(attr(records, "condition")$message, type = "error")
        return(NULL)
      }
      state$records <- records
      save_artefact(proj, "records", records)
      shiny::showNotification(
        sprintf("Loaded %d records from %s into project \"%s\".",
                nrow(records), f$name, proj),
        duration = 4
      )
    })

    shiny::observeEvent(input$use_toy, {
      proj <- ensure_project()
      toy_path <- system.file("extdata", "toy_cbfm.csv",
                              package = "screenllm")
      if (!nzchar(toy_path)) {
        shiny::showNotification(
          "Could not locate the toy corpus file inside the installed package.",
          type = "error", duration = 6
        )
        return(NULL)
      }
      toy <- try(read_records(toy_path), silent = TRUE)
      if (inherits(toy, "try-error")) {
        shiny::showNotification(
          sprintf("Failed to load toy corpus: %s",
                  attr(toy, "condition")$message),
          type = "error", duration = 6
        )
        return(NULL)
      }
      state$records <- toy
      save_artefact(proj, "records", toy)
      shiny::showNotification(
        sprintf("Loaded toy corpus (%d records) into project \"%s\".",
                nrow(toy), proj),
        duration = 4
      )
    })

    output$summary <- shiny::renderUI({
      r <- state$records
      if (is.null(r)) return(shiny::em("No records uploaded yet."))
      shiny::tags$ul(
        shiny::tags$li(shiny::strong("Records: "), nrow(r)),
        shiny::tags$li(shiny::strong("Columns: "), paste(names(r), collapse = ", ")),
        shiny::tags$li(
          shiny::strong("Missing abstracts: "),
          sum(!nzchar(r$abstract) | is.na(r$abstract))
        )
      )
    })

    # Detect duplicates whenever records change. Kept out of the
    # summary reactive so the (potentially slow) fuzzy match runs
    # once per load, not on every render.
    dup_flag <- shiny::reactiveVal(NULL)
    shiny::observeEvent(state$records, ignoreNULL = TRUE, {
      r <- state$records
      if (nrow(r) > 2000L) {
        dup_flag(list(skipped = TRUE, n_dup = NA_integer_))
      } else {
        flagged <- find_duplicates(r)
        n_dup <- sum(!is.na(flagged$duplicate_of))
        dup_flag(list(skipped = FALSE, n_dup = n_dup, flagged = flagged))
      }
    })

    output$dup_banner <- shiny::renderUI({
      d <- dup_flag()
      if (is.null(d)) return(NULL)
      if (isTRUE(d$skipped)) {
        return(shiny::tags$div(
          class = "alert alert-info py-1 my-1",
          shiny::tags$small(
            "Corpus is large (>2000 records); duplicate detection skipped in the app. ",
            shiny::tags$code("find_duplicates()"), " runs it from the R console."
          )
        ))
      }
      if (d$n_dup == 0L) return(shiny::tags$div(
        class = "alert alert-success py-1 my-1",
        shiny::tags$small("No duplicates detected (by DOI or title).")
      ))
      shiny::tags$div(
        class = "alert alert-warning py-1 my-1",
        shiny::tags$small(sprintf(
          "Detected %d likely duplicate record%s (DOI or normalised title match).",
          d$n_dup, if (d$n_dup == 1L) "" else "s"
        )),
        shiny::actionButton(ns("dedup"), "Remove duplicates",
                            class = "btn-sm btn-outline-warning mt-1")
      )
    })

    shiny::observeEvent(input$dedup, {
      d <- dup_flag()
      if (is.null(d) || isTRUE(d$skipped) || d$n_dup == 0L) return(NULL)
      keep <- is.na(d$flagged$duplicate_of)
      new <- state$records[keep, , drop = FALSE]
      state$records <- new
      if (!is.null(state$project)) save_artefact(state$project, "records", new)
      shiny::showNotification(
        sprintf("Removed %d duplicate record%s. %d remain.",
                d$n_dup, if (d$n_dup == 1L) "" else "s", nrow(new)),
        duration = 4
      )
      dup_flag(list(skipped = FALSE, n_dup = 0L))
    })

    output$dl_toy <- shiny::downloadHandler(
      filename = function() "toy_cbfm.csv",
      content = function(file) {
        toy_path <- system.file("extdata", "toy_cbfm.csv",
                                package = "screenllm")
        if (!nzchar(toy_path)) {
          # Rare: source-install without the installed data path.
          # Fail loudly rather than write an empty file.
          shiny::showNotification(
            "Could not locate the toy corpus inside the installed package.",
            type = "error", duration = 6
          )
          return()
        }
        file.copy(toy_path, file, overwrite = TRUE)
      }
    )

    output$preview <- DT::renderDT({
      r <- state$records
      if (is.null(r)) return(NULL)
      DT::datatable(
        r[, intersect(c("id", "title", "abstract"), names(r)), drop = FALSE],
        options = list(pageLength = 15, autoWidth = FALSE, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}
