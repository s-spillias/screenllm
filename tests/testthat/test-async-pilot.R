test_that("pilot_results_as_tibble converts empty list to empty tibble", {
  out <- pilot_results_as_tibble(list())
  expect_s3_class(out, "screenllm_pilot")
  expect_equal(nrow(out), 0L)
})

test_that("pilot_results_as_tibble converts a list of scored records", {
  fake <- list(
    list(id = "r1", title = "T1", abstract = "A1",
         universal_best_score = 70,
         per_model_scores = data.frame(model = "m", replicate = 1L, score = 70),
         justifications = data.frame(model = "m", replicate = 1L,
                                      explanation = "why")),
    list(id = "r2", title = "T2", abstract = "A2",
         universal_best_score = 30,
         per_model_scores = data.frame(model = "m", replicate = 1L, score = 30),
         justifications = data.frame(model = "m", replicate = 1L,
                                      explanation = "why not"))
  )
  out <- pilot_results_as_tibble(fake)
  expect_s3_class(out, "screenllm_pilot")
  expect_equal(nrow(out), 2L)
  expect_equal(out$id, c("r1", "r2"))
  expect_equal(out$universal_best_score, c(70, 30))
})

test_that("pilot_job_status is idle when the progress file doesn't exist", {
  path <- tempfile("nonexistent-pilot-", fileext = ".rds")
  st <- pilot_job_status(path)
  expect_identical(st$status, "idle")
})

test_that("pilot_job_status reads a fake progress file", {
  path <- tempfile("test-pilot-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(list(
    status = "running",
    processed = 3L,
    total = 10L,
    results = list(),
    error = NULL,
    started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), path)
  st <- pilot_job_status(path)
  expect_identical(st$status, "running")
  expect_equal(st$percent, 30)
  expect_equal(st$processed, 3L)
})

test_that("start_pilot_job spawns a worker that scores against the mock backend", {
  skip_if_not_installed("callr")

  # Isolate this test's progress artefacts under tempdir() so we can
  # NEVER overwrite a user's real Shiny session's pilot progress
  # file (which lives under R_user_dir).
  tmpdir <- tempfile("screenllm-pilot-test-")
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  records <- data.frame(
    id = paste0("r", 1:5),
    title = paste("Title", 1:5),
    abstract = paste("Abstract", 1:5),
    stringsAsFactors = FALSE
  )
  criteria <- define_criteria("Scope", c("Criterion A.", "Criterion B."))
  ens <- default_ensemble(backend = backend_mock())
  job <- start_pilot_job(records, criteria, ens, n = 3,
                          sample = FALSE, progress_dir = tmpdir)
  # Wait deterministically for the worker to finish.
  job$handle$wait(timeout = 60 * 1000)
  expect_equal(job$handle$get_exit_status(), 0L)
  # The handle includes the per-job path; use it to read status.
  st <- pilot_job_status(job$progress_path)
  expect_identical(st$status, "done")
  expect_equal(st$processed, 3L)
  expect_equal(length(st$results), 3L)
  tbl <- pilot_results_as_tibble(st$results)
  expect_equal(nrow(tbl), 3L)
  expect_true(all(!is.na(tbl$universal_best_score)))
})

test_that("start_pilot_job produces a unique progress path per invocation", {
  skip_if_not_installed("callr")

  tmpdir <- tempfile("screenllm-pilot-uniq-")
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  records <- data.frame(
    id = "r1", title = "T", abstract = "A", stringsAsFactors = FALSE
  )
  criteria <- define_criteria("Scope", "x")
  ens <- default_ensemble(backend = backend_mock())
  j1 <- start_pilot_job(records, criteria, ens, n = 1,
                         sample = FALSE, progress_dir = tmpdir)
  j2 <- start_pilot_job(records, criteria, ens, n = 1,
                         sample = FALSE, progress_dir = tmpdir)
  # Different job ids -> different paths, so the two runs can never
  # overwrite each other's progress.
  expect_false(identical(j1$progress_path, j2$progress_path))
  expect_false(identical(j1$job_id, j2$job_id))
  j1$handle$wait(30 * 1000)
  j2$handle$wait(30 * 1000)
})
