test_that("detect_gpu returns the expected shape", {
  info <- detect_gpu()
  expect_type(info, "list")
  expect_setequal(names(info), c("available", "kind", "detail"))
  expect_true(is.logical(info$available) && length(info$available) == 1L)
  expect_true(info$kind %in% c("apple", "nvidia", "amd", "none"))
  expect_true(is.character(info$detail) && length(info$detail) == 1L)
})

test_that("estimate_runtime honours the gpu argument", {
  ens <- default_ensemble(backend = backend_mock())
  cpu_est <- estimate_runtime(100L, ens, gpu = FALSE)
  gpu_est <- estimate_runtime(100L, ens, gpu = TRUE)
  expect_false(cpu_est$gpu)
  expect_true(gpu_est$gpu)
  # GPU should be strictly faster than CPU under the heuristic.
  expect_lt(gpu_est$seconds_per_call, cpu_est$seconds_per_call)
  expect_lt(gpu_est$seconds_total, cpu_est$seconds_total)
})

test_that("estimate_runtime NULL gpu auto-detects and matches detect_gpu()", {
  ens <- default_ensemble(backend = backend_mock())
  est <- estimate_runtime(100L, ens)
  info <- detect_gpu()
  expect_identical(est$gpu, info$available)
})
