test_that("rank_records runs end-to-end against the mock backend", {
  toy_path <- system.file("extdata", "toy_habitat_effect.csv", package = "screenllm")
  # In devtools::test() the extdata is at inst/extdata; fall back if needed.
  if (!nzchar(toy_path)) {
    toy_path <- testthat::test_path("../../inst/extdata/toy_habitat_effect.csv")
  }
  skip_if_not(file.exists(toy_path), "toy dataset not found in this test environment")

  records <- read_records(toy_path)
  expect_true(all(c("id", "title", "abstract") %in% names(records)))
  expect_gte(nrow(records), 10L)

  criteria <- define_criteria(
    scope = "toy test scope",
    inclusions = c("The record is a real study.", "The record is empirical.")
  )
  ensemble <- custom_ensemble(
    models = c("model_a", "model_b"),
    replicates = 2L,
    backend = backend_mock()
  )
  cache_dir <- tempfile("screenllm-cache-")
  ranked <- rank_records(
    records, criteria, ensemble = ensemble,
    cache_dir = cache_dir, verbose = FALSE
  )

  expect_true(all(c("universal_best_score", "rank") %in% names(ranked)))
  expect_equal(nrow(ranked), nrow(records))
  expect_true(all(!is.na(ranked$universal_best_score)))
  expect_true(all(ranked$universal_best_score >= 0 & ranked$universal_best_score <= 100))
  expect_setequal(ranked$rank, seq_len(nrow(ranked)))
})

test_that("plan_screening returns a stop point and a to-screen subset", {
  toy_path <- system.file("extdata", "toy_habitat_effect.csv", package = "screenllm")
  skip_if_not(nzchar(toy_path) && file.exists(toy_path))
  records <- read_records(toy_path)
  criteria <- define_criteria(
    scope = "toy",
    inclusions = c("x", "y")
  )
  ranked <- rank_records(
    records, criteria,
    ensemble = custom_ensemble(c("m"), 1L, backend = backend_mock()),
    cache_dir = tempfile("screenllm-cache-"),
    verbose = FALSE
  )
  plan <- plan_screening(ranked, safe_run_length = 5L, safe_min_cover = 0.2)
  expect_s3_class(plan, "screenllm_plan")
  expect_true(plan$stop_at >= 1L && plan$stop_at <= nrow(ranked))
  expect_equal(nrow(plan$to_screen), plan$stop_at)
})
