test_that("export_report renders a self-contained HTML document", {
  # Small end-to-end fixture: rank three records, plan, invent
  # decisions, and render the report.
  records <- data.frame(
    id = paste0("r", 1:6),
    title = paste("Title", 1:6),
    abstract = paste("Abstract", 1:6)
  )
  criteria <- define_criteria("Scope",
                              c("Criterion A.", "Criterion B."))
  ens <- default_ensemble(backend = backend_mock())
  ranked <- rank_records(records, criteria, ensemble = ens, verbose = FALSE)
  plan <- plan_screening(ranked, safe_run_length = 5, spot_check_n = 3)
  decisions <- data.frame(
    id = ranked$id[1:3],
    human_decision = c("Accept", "Accept", "Reject"),
    stringsAsFactors = FALSE
  )

  out <- tempfile(fileext = ".html")
  export_report(
    output_file = out,
    project = "test-project",
    ranked = ranked, plan = plan, decisions = decisions,
    criteria = criteria, ensemble = ens
  )
  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "test-project")
  expect_match(html, "SAFE plan|Screening summary")
})

test_that("export_report tolerates missing artefacts", {
  out <- tempfile(fileext = ".html")
  # All params NULL should still render (the template writes "(not
  # recorded)" placeholders).
  export_report(output_file = out, project = "minimal")
  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "minimal")
})
