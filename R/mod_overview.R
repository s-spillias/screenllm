# Overview tab: plain-language landing page. First thing new users
# see -- explains what screenllm does, how the workflow fits together,
# and where to go next. Kept module-shaped for symmetry with the
# other tabs, though the panel has no server-side reactivity to
# speak of.

#' @keywords internal
mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  # A short step description used in both the numbered checklist
  # (below) and, verbatim, in each downstream tab's own top-of-card
  # helper text.
  step <- function(num, title, blurb) {
    shiny::tags$li(
      class = "mb-2",
      shiny::tags$span(class = "badge bg-primary me-2", num),
      shiny::tags$strong(title),
      shiny::tags$span(class = "d-block small text-muted ms-4", blurb)
    )
  }
  bslib::layout_columns(
    col_widths = c(7, 5),
    # ---- Left: what it does + step-by-step ----
    bslib::card(
      bslib::card_header("What is screenllm?"),
      bslib::card_body(
        shiny::tags$p(
          class = "lead mb-2",
          "screenllm is a laptop-friendly tool for the ",
          shiny::tags$strong("title/abstract screening"),
          " step of a systematic review. It reads your corpus, asks ",
          "an ensemble of local (Ollama-served) language models to ",
          "rank each record for relevance, applies a statistically ",
          "justified stopping rule, and hands you a much smaller list ",
          "to screen by eye."
        ),
        shiny::tags$p(
          class = "small text-muted mb-3",
          "No API keys, no cloud spend, no HPC. Everything runs on ",
          "your machine. Typical time budget on a laptop: overnight ",
          "ranking of 2000-3000 records against a 4-model ensemble, ",
          "then an hour or two of human screening."
        ),
        shiny::tags$hr(class = "my-2"),
        shiny::tags$p(class = "mb-1", shiny::tags$strong("The workflow, one tab at a time:")),
        shiny::tags$ol(
          class = "list-unstyled ps-2 mb-0",
          step("1", "Setup",
                paste0("Pick or create a project (a persistent folder on disk ",
                       "for your review), verify Ollama is running, and choose ",
                       "an ensemble of models. Preset \"light\" fits on 8-16 GB RAM.")),
          step("2", "Corpus",
                paste0("Upload your search results (CSV, TSV, Excel, or RIS from ",
                       "Zotero / EndNote / Web of Science / Scopus). Or use the ",
                       "built-in toy CBFM corpus to walk through the workflow first.")),
          step("3", "Criteria",
                paste0("Write the scope + one inclusion criterion per line. Or ",
                       "click \"Load CBFM example criteria\" to see a worked example. ",
                       "The rendered LLM prompt updates live so you can see what ",
                       "the models will actually be asked.")),
          step("4", "Rank",
                paste0("Kick off the ranking run. Ollama serves one model at a ",
                       "time and each model scores each record R times; scores ",
                       "are averaged. Cached to disk so an interrupted run resumes. ",
                       "Progress and live per-model justifications update in the UI.")),
          step("5", "Plan",
                paste0("The SAFE stopping rule tells you the point in the ranked ",
                       "corpus below which further reading is unlikely to find ",
                       "more accepts. Three gates -- minimum coverage, run length, ",
                       "spot check -- keep it from stopping too early.")),
          step("6", "Screen",
                paste0("Browse each record above the stop point with LLM ",
                       "justifications visible; hit Accept / Reject. Decisions ",
                       "autosave after each click.")),
          step("7", "Report",
                paste0("Summary of what happened, per-record disagreements ",
                       "between LLM and human (real user-error catches!), and ",
                       "downloads of everything: decisions.csv, ranked.csv, and ",
                       "a self-contained HTML report."))
        )
      )
    ),
    # ---- Right: first-time checklist + links ----
    shiny::tagList(
      bslib::card(
        bslib::card_header("Getting started"),
        bslib::card_body(
          shiny::tags$p(
            class = "small mb-2",
            shiny::tags$strong("First time?"), " Try the workflow on the ",
            "toy corpus before pointing it at your real search results:"
          ),
          shiny::tags$ol(
            class = "small",
            shiny::tags$li("Go to ", shiny::tags$strong("Setup"),
                            ": create a project (e.g. \"cbfm-demo\") and pick a model ensemble."),
            shiny::tags$li("Go to ", shiny::tags$strong("Corpus"),
                            ": click \"Load toy CBFM corpus\" or download the ",
                            "CSV to inspect the format."),
            shiny::tags$li("Go to ", shiny::tags$strong("Criteria"),
                            ": click \"Load CBFM example criteria\" and Save."),
            shiny::tags$li("Go to ", shiny::tags$strong("Rank"),
                            ": click Start. Grab a coffee (2-15 minutes for 40 records)."),
            shiny::tags$li("Continue through Plan -> Screen -> Report.")
          )
        )
      ),
      bslib::card(
        bslib::card_header("Help + citation"),
        bslib::card_body(
          shiny::tags$ul(
            class = "small mb-2",
            shiny::tags$li(
              shiny::tags$a(href = "https://github.com/s-spillias/screenllm/blob/main/docs/getting-started.md",
                            target = "_blank", "Getting-started guide")),
            shiny::tags$li(
              shiny::tags$a(href = "https://github.com/s-spillias/screenllm/blob/main/docs/troubleshooting.md",
                            target = "_blank", "Troubleshooting")),
            shiny::tags$li(
              shiny::tags$a(href = "https://github.com/s-spillias/screenllm/issues/new/choose",
                            target = "_blank", "Report an issue"))
          ),
          shiny::tags$hr(class = "my-2"),
          shiny::tags$p(class = "small text-muted mb-1",
                        shiny::tags$strong("Cite screenllm:")),
          shiny::tags$p(
            class = "small text-muted mb-2",
            "Spillias, S. et al. (2026). ",
            shiny::tags$em("Operationalising LLM-assisted screening of literature to support systematic reviews."),
            " (in submission; preprint forthcoming)."
          ),
          shiny::tags$p(class = "small text-muted mb-1",
                        shiny::tags$strong("Toy CBFM corpus + criteria are from:")),
          shiny::tags$p(
            class = "small text-muted mb-0",
            "Spillias, S., Tuohy, P., Andreotta, M., Annand-Jones, R., ",
            "Boschetti, F., Cvitanovic, C., Duggan, J., Fulton, E. A., ",
            "Karcher, D. B., Paris, C., Shellock, R., & Trebilco, R. (2024). ",
            shiny::tags$em("Human-AI collaboration to identify literature for evidence synthesis."),
            " Cell Reports Sustainability, 1(7), 100132. ",
            shiny::tags$a(href = "https://doi.org/10.1016/j.crsus.2024.100132",
                          target = "_blank",
                          "doi.org/10.1016/j.crsus.2024.100132")
          )
        )
      )
    )
  )
}

#' @keywords internal
mod_overview_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    # No reactivity on the Overview tab. Kept as a moduleServer so
    # every tab in the app_server call site has the same shape.
    invisible()
  })
}
