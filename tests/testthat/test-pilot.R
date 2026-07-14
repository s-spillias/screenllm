test_that("pilot() returns a tibble with per-record justifications", {
  records <- data.frame(
    id = paste0("r", 1:8),
    title = paste("Title", 1:8),
    abstract = paste("Abstract", 1:8)
  )
  criteria <- define_criteria(
    scope = "Demo scope",
    inclusions = c("Criterion A.", "Criterion B.")
  )
  ens <- default_ensemble(backend = backend_mock())
  out <- pilot(records, criteria, ensemble = ens, n = 5, verbose = FALSE)
  expect_s3_class(out, "screenllm_pilot")
  expect_equal(nrow(out), 5L)
  expect_true("universal_best_score" %in% names(out))
  expect_true("justifications" %in% names(out))
  expect_true(all(!is.na(out$universal_best_score)))
})

test_that("pilot() forces replicates = 1 regardless of ensemble config", {
  records <- data.frame(
    id = paste0("r", 1:5),
    title = paste("Title", 1:5),
    abstract = paste("Abstract", 1:5)
  )
  criteria <- define_criteria("Scope", "Only criterion.")
  ens <- custom_ensemble(models = c("m1", "m2"), replicates = 5L,
                         backend = backend_mock())
  out <- pilot(records, criteria, ensemble = ens, n = 3, verbose = FALSE)
  # Each per_model_scores row should have only one replicate per model.
  reps <- out$per_model_scores[[1]]
  expect_true(all(reps$replicate == 1L))
})

test_that("pilot() clamps n to nrow(records)", {
  records <- data.frame(
    id = paste0("r", 1:3),
    title = paste("Title", 1:3),
    abstract = paste("Abstract", 1:3)
  )
  criteria <- define_criteria("Scope", "Only criterion.")
  ens <- default_ensemble(backend = backend_mock())
  out <- pilot(records, criteria, ensemble = ens, n = 999, verbose = FALSE)
  expect_equal(nrow(out), 3L)
})
