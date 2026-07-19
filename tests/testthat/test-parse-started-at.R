# parse_started_at (in R/async.R) must handle every timestamp shape
# the workers write, plus the shape base R's as.POSIXct fails to
# parse on some Windows locales (%z-formatted string), which
# previously left elapsed/ETA blank throughout a rank run.

test_that("parse_started_at parses timestamps with a numeric %z offset", {
  ts <- "2026-07-20T14:30:00+1000"
  t <- screenllm:::parse_started_at(ts)
  expect_s3_class(t, "POSIXct")
  expect_false(is.na(t))
})

test_that("parse_started_at parses timestamps without an offset", {
  ts <- "2026-07-20T14:30:00"
  t <- screenllm:::parse_started_at(ts)
  expect_s3_class(t, "POSIXct")
  expect_false(is.na(t))
})

test_that("parse_started_at handles NULL / empty / garbage safely", {
  expect_true(is.na(screenllm:::parse_started_at(NULL)))
  expect_true(is.na(screenllm:::parse_started_at("")))
  expect_true(inherits(screenllm:::parse_started_at("not a date"),
                       "POSIXct"))
})
