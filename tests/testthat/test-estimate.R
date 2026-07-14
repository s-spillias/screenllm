test_that("estimate_runtime scales with corpus, models, replicates", {
  ens1 <- custom_ensemble(models = "gemma3:27b", replicates = 1L,
                          backend = backend_mock())
  ens3 <- custom_ensemble(models = c("gemma3:27b", "gpt-oss:20b"),
                          replicates = 3L, backend = backend_mock())
  e1 <- estimate_runtime(100L, ens1)
  e3 <- estimate_runtime(100L, ens3)
  expect_lt(e1$seconds_total, e3$seconds_total)
  expect_equal(e1$n_calls, 100L)
  expect_equal(e3$n_calls, 600L)
})

test_that("estimate_runtime uses a smaller per-call cost for small models", {
  small <- custom_ensemble(models = "gemma3:4b", replicates = 1L,
                           backend = backend_mock())
  big <- custom_ensemble(models = "gemma3:27b", replicates = 1L,
                         backend = backend_mock())
  e_small <- estimate_runtime(50L, small)
  e_big <- estimate_runtime(50L, big)
  expect_lt(e_small$seconds_per_call, e_big$seconds_per_call)
})

test_that("estimate_runtime accepts an override", {
  ens <- default_ensemble(backend = backend_mock())
  e <- estimate_runtime(100L, ens, seconds_per_call = 2)
  expect_equal(e$seconds_per_call, 2)
})
