test_that("pilot_progress_path is deterministic and under R_user_dir", {
  p <- pilot_progress_path()
  expect_true(grepl("pilots/progress\\.rds$", as.character(p)))
})

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

test_that("pilot_job_status is idle when no progress file exists", {
  # Clear the progress file first for a clean state.
  path <- pilot_progress_path()
  if (fs::file_exists(path)) unlink(path)
  st <- pilot_job_status()
  expect_identical(st$status, "idle")
})

test_that("pilot_job_status reads a fake progress file", {
  path <- pilot_progress_path()
  fs::dir_create(fs::path_dir(path), recurse = TRUE)
  on.exit(unlink(path), add = TRUE)
  saveRDS(list(
    status = "running",
    processed = 3L,
    total = 10L,
    results = list(),
    error = NULL,
    started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), path)
  st <- pilot_job_status()
  expect_identical(st$status, "running")
  expect_equal(st$percent, 30)
  expect_equal(st$processed, 3L)
})

test_that("start_pilot_job spawns a worker that scores against the mock backend", {
  skip_if_not_installed("callr")

  records <- data.frame(
    id = paste0("r", 1:5),
    title = paste("Title", 1:5),
    abstract = paste("Abstract", 1:5),
    stringsAsFactors = FALSE
  )
  criteria <- define_criteria("Scope", c("Criterion A.", "Criterion B."))
  ens <- default_ensemble(backend = backend_mock())
  job <- start_pilot_job(records, criteria, ens, n = 3, sample = FALSE)
  # Wait deterministically for the worker to finish; polling for the
  # progress file races the throttled writer.
  job$handle$wait(timeout = 60 * 1000)
  expect_equal(job$handle$get_exit_status(), 0L)
  st <- pilot_job_status()
  expect_identical(st$status, "done")
  expect_equal(st$processed, 3L)
  expect_equal(length(st$results), 3L)
  # Sanity-check the tibble conversion round-trips
  tbl <- pilot_results_as_tibble(st$results)
  expect_equal(nrow(tbl), 3L)
  expect_true(all(!is.na(tbl$universal_best_score)))
})
