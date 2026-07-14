test_that("pull_progress_path is deterministic and slugs unsafe chars", {
  p1 <- pull_progress_path("mistral:7b")
  p2 <- pull_progress_path("mistral:7b")
  p3 <- pull_progress_path("qwen3:30b-a3b-instruct-2507")
  expect_identical(p1, p2)
  expect_true(grepl("mistral_7b\\.rds$", p1))
  expect_true(grepl("qwen3_30b-a3b-instruct-2507\\.rds$", p3))
})

test_that("pull_job_status returns idle when no progress file exists", {
  # Use a model tag no test will collide with.
  st <- pull_job_status("no-such-model:9999b")
  expect_identical(st$status, "idle")
  expect_equal(st$percent, 0)
})

test_that("pull_job_status parses an on-disk progress file", {
  # Write a fake progress file so we test the reader in isolation
  # (without spawning a callr subprocess that would need real Ollama).
  path <- pull_progress_path("fake-model:1b")
  fs::dir_create(fs::path_dir(path), recurse = TRUE)
  on.exit(unlink(path), add = TRUE)
  saveRDS(list(
    model = "fake-model:1b",
    status = "running",
    completed = 2500000,
    total = 5000000,
    detail = "downloading digestsha256:xxxx",
    error = NULL,
    started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), path)
  st <- pull_job_status("fake-model:1b")
  expect_identical(st$status, "running")
  expect_equal(st$percent, 50)
  expect_match(st$detail, "downloading")
})
