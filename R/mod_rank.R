# Rank tab: start async ranking (full corpus or a sample), poll
# progress, and stream partial per-model results into a live table.
# The old dedicated Pilot tab is folded in here: a small "Sample size"
# input lets the user run against a subset, which is what a pilot
# fundamentally was.

# Format seconds as HH:MM:SS (or MM:SS if under an hour).
fmt_hms <- function(secs) {
  if (is.na(secs) || !is.finite(secs) || secs < 0) return("-")
  h <- floor(secs / 3600)
  m <- floor((secs %% 3600) / 60)
  s <- floor(secs %% 60)
  if (h > 0) sprintf("%d:%02d:%02d", h, m, s)
  else sprintf("%d:%02d", m, s)
}

#' @keywords internal
mod_rank_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Ranking job"),
      shiny::uiOutput(ns("readiness")),
      shiny::uiOutput(ns("estimate_banner")),
      shiny::numericInput(
        ns("sample_size"),
        "Records to score (0 = all)",
        value = 0L, min = 0L, max = 100000L, step = 5L
      ),
      shiny::conditionalPanel(
        # Only offer the random-sample toggle when a partial run is
        # actually happening. If the user leaves sample_size = 0
        # (full corpus), the toggle is meaningless.
        condition = sprintf("input['%s'] > 0", ns("sample_size")),
        shiny::checkboxInput(ns("random_sample"),
                              "Random sample (uncheck for first N in file order)",
                              value = TRUE)
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::actionButton(ns("start"), "Start ranking",
                                              class = "btn-primary w-100")),
        shiny::column(6, shiny::actionButton(ns("cancel"), "Cancel",
                                              class = "btn-outline-danger w-100"))
      ),
      shiny::hr(),
      shiny::textOutput(ns("status_line")),
      shiny::uiOutput(ns("model_progress")),
      shiny::hr(),
      shiny::tags$details(
        # Housekeeping controls tucked inside <details> so they don't
        # crowd the main flow; expanded only when needed.
        shiny::tags$summary(
          class = "text-muted small",
          "Clear cached scores (re-run needed)"
        ),
        shiny::tags$div(
          class = "mt-2",
          shiny::selectizeInput(
            ns("clear_model"), NULL, choices = NULL,
            options = list(placeholder = "select a model or 'all'",
                            create = FALSE)
          ),
          shiny::actionButton(ns("clear_cache_btn"),
                               "Clear + delete ranking",
                               class = "btn-outline-danger w-100 btn-sm"),
          shiny::tags$small(
            class = "text-muted d-block mt-1",
            "Removes cached LLM scores for the chosen model (or all) ",
            "AND removes the current ranked artefact so the next ",
            "Start ranking gets a fresh run."
          )
        )
      )
    ),
    bslib::card(
      bslib::card_header("Ensemble scores (live)"),
      # Two independent scroll regions so scrolling a long
      # justifications panel doesn't move the ranked table above,
      # and vice versa. DT's own scrollY handles the top region;
      # the details panel has an overflow-y wrapper.
      shiny::tags$div(
        class = "clearfix",
        DT::DTOutput(ns("scores_table"))
      ),
      shiny::tags$hr(class = "my-2"),
      shiny::tags$small(
        class = "text-muted",
        "Click a row above to see each model's justification for that record."
      ),
      shiny::tags$div(
        style = "max-height: 340px; overflow-y: auto; overflow-x: hidden;",
        shiny::uiOutput(ns("details"))
      )
    )
  )
}

#' @keywords internal
mod_rank_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

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
          shiny::tags$span(class = "badge bg-warning", "Missing:"),
          shiny::tags$ul(lapply(needs, shiny::tags$li))
        )
      }
    })

    output$estimate_banner <- shiny::renderUI({
      if (is.null(state$records) || is.null(state$ensemble)) return(NULL)
      n_full <- nrow(state$records)
      n_target <- if (isTRUE(as.integer(input$sample_size) > 0L)) {
        min(as.integer(input$sample_size), n_full)
      } else n_full
      est <- estimate_runtime(n_target, state$ensemble)
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

    # Same completion-toast bookkeeping as before, keyed by project.
    rank_notified <- shiny::reactiveVal(character())

    shiny::observeEvent(input$start, {
      shiny::req(state$project, state$records, state$criteria, state$ensemble)
      handle <- try(
        start_rank_job(
          state$project,
          ensemble = state$ensemble,
          sample_size = as.integer(input$sample_size),
          random_sample = isTRUE(input$random_sample)
        ),
        silent = TRUE
      )
      if (inherits(handle, "try-error")) {
        shiny::showNotification(attr(handle, "condition")$message, type = "error")
        return(NULL)
      }
      state$rank_handle <- handle$handle
      # Reset the notification tracking for this project so a re-run
      # can fire its own completion toast.
      rank_notified(setdiff(rank_notified(), state$project))
      # Fresh state$ranked so the streaming table starts empty (the
      # prior run's rankings would confuse the display).
      state$ranked <- NULL
      shiny::showNotification("Ranking job started in background.", duration = 4)
    })

    shiny::observeEvent(input$cancel, {
      if (!is.null(state$rank_handle)) {
        rank_job_cancel(state$rank_handle)
        state$rank_handle <- NULL
        shiny::showNotification("Job cancelled.", duration = 3)
      }
    })

    # Populate the "Clear cache" dropdown with the ensemble's models
    # plus an "all" option. Refreshed whenever the ensemble changes.
    shiny::observe({
      ens <- state$ensemble
      choices <- if (is.null(ens)) c("all" = "__all__") else
        c("all models" = "__all__",
          stats::setNames(ens$models, ens$models))
      shiny::updateSelectizeInput(session, "clear_model",
                                    choices = choices, server = FALSE)
    })

    shiny::observeEvent(input$clear_cache_btn, {
      shiny::req(state$project)
      sel <- input$clear_model
      if (is.null(sel) || !nzchar(sel)) {
        shiny::showNotification("Pick a model (or 'all models') first.",
                                type = "warning")
        return(NULL)
      }
      model_arg <- if (identical(sel, "__all__")) NULL else sel
      removed <- clear_cache(state$project, model = model_arg,
                              delete_ranked = TRUE)
      state$ranked <- NULL  # drop the in-memory ranking too
      # Force the completion-toast bookkeeping to fire again on the
      # next completed run.
      rank_notified(setdiff(rank_notified(), state$project))
      shiny::showNotification(
        sprintf("Cleared %d cached score%s%s. Click Start ranking to re-run.",
                removed, if (removed == 1L) "" else "s",
                if (is.null(model_arg)) "" else sprintf(" for %s", model_arg)),
        duration = 6, type = "message"
      )
    })

    # Poll the progress file every 500 ms while a job is running, and
    # keep polling briefly after completion so the completion toast
    # + final-scores render actually get a chance to fire.
    poll <- shiny::reactivePoll(
      intervalMillis = 500,
      session = session,
      checkFunc = function() {
        if (is.null(state$project)) return(0)
        file.info(fs::path(project_dir(state$project, create = FALSE),
                            .project_artefacts$progress))$mtime
      },
      valueFunc = function() {
        if (is.null(state$project)) return(NULL)
        st <- rank_job_status(state$project)
        # Load the final ranked artefact once the worker signals done.
        if (identical(st$status, "done") && is.null(state$ranked)) {
          state$ranked <- load_artefact(state$project, "ranked")
        }
        proj <- state$project
        # One-shot completion toast, warning if fail-rate is high.
        if (identical(st$status, "done") && !(proj %in% rank_notified())) {
          r <- state$ranked
          fail_rate <- if (!is.null(r) && "universal_best_score" %in% names(r) &&
                             nrow(r) > 0) {
            mean(is.na(r$universal_best_score))
          } else 0
          if (fail_rate > 0.1) {
            shiny::showNotification(
              sprintf(
                "Ranking finished but %.0f%% of records got no score. Check that the ensemble's models are installed (Setup tab), then re-run.",
                100 * fail_rate
              ),
              type = "warning", duration = 15
            )
          } else {
            shiny::showNotification(
              sprintf("Ranking complete for project \"%s\" (%d records).",
                      proj, nrow(r %||% data.frame())),
              type = "message", duration = 8
            )
          }
          rank_notified(c(rank_notified(), proj))
        } else if (identical(st$status, "error") &&
                     !(proj %in% rank_notified())) {
          shiny::showNotification(
            sprintf("Ranking failed for \"%s\": %s",
                    proj, st$error %||% "(no detail)"),
            type = "error", duration = 10
          )
          rank_notified(c(rank_notified(), proj))
        }
        st
      }
    )

    output$status_line <- shiny::renderText({
      st <- poll()
      if (is.null(st) || identical(st$status, "idle")) return("Idle.")
      elapsed <- if (!is.null(st$elapsed_secs) && is.finite(st$elapsed_secs)) {
        sprintf(" | elapsed %s", fmt_hms(st$elapsed_secs))
      } else ""
      eta <- if (!is.null(st$eta_secs) && is.finite(st$eta_secs) &&
                   identical(st$status, "running")) {
        sprintf(" | ETA %s", fmt_hms(st$eta_secs))
      } else ""
      model <- if (!is.null(st$current_model) && !is.na(st$current_model) &&
                     nzchar(st$current_model) &&
                     identical(st$status, "running")) {
        sprintf(" | scoring %s", st$current_model)
      } else ""
      err <- if (isTRUE(nzchar(st$error))) sprintf(" | ERROR: %s", st$error) else ""
      sprintf("%s - %d/%d (%.1f%%)%s%s%s%s",
              toupper(st$status), st$processed, st$total, st$percent,
              elapsed, eta, model, err)
    })

    # A tiny per-model progress bar so the user can see model-major
    # ordering in action: model A ticks through all N records first,
    # then model B, and so on.
    output$model_progress <- shiny::renderUI({
      st <- poll()
      if (is.null(st) || is.null(st$ensemble_models) ||
            length(st$ensemble_models) == 0L) {
        return(NULL)
      }
      models <- st$ensemble_models
      per_model_target <- st$n_records * (st$ensemble_replicates %||% 1L)
      # Count how many calls each model has completed.
      scores <- st$scores %||% data.frame(model = character())
      done_by_model <- if (nrow(scores) > 0L) {
        table(factor(scores$model, levels = models))
      } else stats::setNames(integer(length(models)), models)
      bars <- lapply(models, function(m) {
        n_done <- as.integer(done_by_model[m] %||% 0L)
        pct <- if (per_model_target > 0L) round(100 * n_done / per_model_target) else 0
        colour <- if (pct >= 100) "success" else "info"
        shiny::tags$div(
          class = "mb-1",
          shiny::tags$div(
            class = "d-flex justify-content-between",
            shiny::tags$small(class = "text-nowrap font-monospace", m),
            shiny::tags$small(class = "text-muted",
                              sprintf("%d/%d (%d%%)", n_done, per_model_target, pct))
          ),
          shiny::tags$div(
            class = "progress", style = "height: 4px;",
            shiny::tags$div(
              class = sprintf("progress-bar bg-%s", colour),
              role = "progressbar",
              style = sprintf("width: %d%%;", pct)
            )
          )
        )
      })
      shiny::tags$div(class = "mt-2", bars)
    })

    # ------ Live scores table ------

    # Build the streaming per-record view from the polled per-call
    # scores. Aggregates provisionally (mean of scored replicates so
    # far); once every model x replicate has landed for a record the
    # score matches the final ranked artefact.
    partial <- shiny::reactive({
      # Prefer the persisted ranked artefact when it's already there
      # (a completed prior run) - it's a full tibble with justifications
      # attached, which is what the details panel needs.
      if (!is.null(state$ranked) && nrow(state$ranked) > 0L) {
        return(list(
          scores = state$ranked,
          n_reps = attr(state$ranked, "ensemble")$replicates %||% 1L,
          models = attr(state$ranked, "ensemble")$models %||% character(),
          source = "artefact"
        ))
      }
      st <- poll()
      if (is.null(st) || is.null(st$scores) || nrow(st$scores) == 0L) {
        return(NULL)
      }
      # Aggregate provisional scores per record.
      long <- st$scores
      long <- long[!is.na(long$score), , drop = FALSE]
      if (nrow(long) == 0L) return(NULL)
      agg <- do.call(rbind, lapply(split(long, long$id), function(sub) {
        data.frame(
          id = sub$id[1L],
          universal_best_score = mean(sub$score, na.rm = TRUE),
          n_scored = nrow(sub),
          stringsAsFactors = FALSE
        )
      }))
      # Attach titles and abstracts from record_meta.
      meta <- st$record_meta
      out <- if (!is.null(meta)) {
        merge(meta, agg, by = "id", all.y = TRUE)
      } else {
        agg$title <- NA_character_
        agg$abstract <- NA_character_
        agg
      }
      # Attach the per-model score list-column so the details panel
      # can still work off partial state.
      per_model <- split(long, long$id)
      pms <- lapply(rownames(out), function(i) {
        entry <- per_model[[out$id[as.integer(i)]]]
        data.frame(
          model = entry$model, replicate = entry$replicate,
          score = entry$score,
          stringsAsFactors = FALSE
        )
      })
      jus <- lapply(rownames(out), function(i) {
        entry <- per_model[[out$id[as.integer(i)]]]
        data.frame(
          model = entry$model, replicate = entry$replicate,
          explanation = entry$explanation,
          stringsAsFactors = FALSE
        )
      })
      out$per_model_scores <- pms
      out$justifications <- jus
      list(
        scores = out,
        n_reps = st$ensemble_replicates %||% 1L,
        models = st$ensemble_models %||% character(),
        source = "partial"
      )
    })

    # ---- Selection persistence across streaming re-renders ----
    selected_id <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$scores_table_rows_selected,
                        ignoreNULL = FALSE, {
      sel <- input$scores_table_rows_selected
      if (length(sel) == 0L) return()
      p <- shiny::isolate(partial())
      if (is.null(p) || nrow(p$scores) == 0L) return()
      sc <- p$scores[order(-p$scores$universal_best_score), , drop = FALSE]
      if (sel[1L] >= 1L && sel[1L] <= nrow(sc)) {
        selected_id(as.character(sc$id[sel[1L]]))
      }
    })

    output$scores_table <- DT::renderDT({
      p <- partial()
      if (is.null(p) || nrow(p$scores) == 0L) return(NULL)
      sc <- p$scores[order(-p$scores$universal_best_score), , drop = FALSE]
      target_scores <- max(1L, length(p$models) * as.integer(p$n_reps))
      # Truncate title for the table; full text is in the details panel.
      short_title <- if ("title" %in% names(sc)) {
        substr(sc$title, 1, 100)
      } else rep("", nrow(sc))
      # Show provisional status via n/target when partial.
      progress_col <- if ("n_scored" %in% names(sc)) {
        sprintf("%d/%d", sc$n_scored, target_scores)
      } else rep(sprintf("%d/%d", target_scores, target_scores), nrow(sc))
      tbl <- data.frame(
        rank = seq_len(nrow(sc)),
        id = sc$id,
        score = round(sc$universal_best_score),
        scored = progress_col,
        title = short_title,
        stringsAsFactors = FALSE
      )
      sid <- shiny::isolate(selected_id())
      preselect <- if (!is.null(sid)) which(sc$id == sid) else integer(0)
      DT::datatable(
        tbl,
        options = list(
          # Vertical scroll instead of pagination -- easier to scan
          # a live-updating ranked list than clicking through pages.
          # `dom = "ti"` = table + "Showing X of Y" footer, no
          # pagination controls, no length menu, no search.
          dom = "ti",
          paging = FALSE,
          scrollY = "420px",
          scrollCollapse = TRUE,
          autoWidth = FALSE, scrollX = TRUE
        ),
        rownames = FALSE,
        selection = list(mode = "single", selected = preselect),
        class = "compact"
      )
    })

    output$details <- shiny::renderUI({
      p <- partial()
      sid <- selected_id()
      if (is.null(p) || nrow(p$scores) == 0L || is.null(sid)) {
        return(shiny::tags$p(class = "text-muted fst-italic small mt-2",
                             "No row selected."))
      }
      sc <- p$scores
      idx <- which(sc$id == sid)
      if (length(idx) == 0L) {
        return(shiny::tags$p(class = "text-muted fst-italic small mt-2",
                             "(record not found)"))
      }
      rec <- sc[idx[1L], ]
      js <- rec$justifications[[1L]]
      if (is.null(js) || nrow(js) == 0L) {
        return(shiny::tags$p(class = "text-muted fst-italic small mt-2",
                             "(no justifications yet)"))
      }
      header <- shiny::tags$div(
        class = "mb-2",
        shiny::tags$span(class = "badge bg-primary me-2",
                         sprintf("score %.0f", rec$universal_best_score)),
        shiny::tags$span(class = "fw-semibold",
                         substr(rec$title %||% "", 1, 200))
      )
      panels <- lapply(seq_len(nrow(js)), function(i) {
        explanation <- js$explanation[i] %||% ""
        explanation <- gsub("\\s+", " ", explanation)
        shiny::tags$div(
          class = "border rounded p-2 mb-2 bg-body-tertiary",
          shiny::tags$div(
            class = "d-flex justify-content-between align-items-baseline mb-1",
            shiny::tags$code(class = "small", js$model[i]),
            shiny::tags$span(class = "small text-muted",
                             sprintf("replicate %d", js$replicate[i]))
          ),
          shiny::tags$p(class = "mb-0 small text-break text-wrap",
                        if (nzchar(explanation)) explanation
                        else shiny::tags$em("(no explanation returned by the model)"))
        )
      })
      shiny::tagList(header, panels)
    })
  })
}
