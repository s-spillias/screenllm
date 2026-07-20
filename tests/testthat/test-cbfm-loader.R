# Regression: load_toy_cbfm_criteria() must return the exact scope
# and inclusions shipped in inst/extdata/toy_cbfm_criteria.R, so the
# Criteria tab's "Load CBFM example criteria" button lines up with
# what a user reading the file would see.

test_that("load_toy_cbfm_criteria pulls the criteria from extdata", {
  c <- load_toy_cbfm_criteria()
  expect_s3_class(c, "screenllm_criteria")
  expect_true(nzchar(c$scope))
  expect_true(grepl("community-based fisheries management",
                    c$scope, ignore.case = TRUE))
  expect_length(c$inclusions, 3L)
  # The country-list inclusion enumerates Pacific Island countries
  # by name (Fiji, Vanuatu, ...) rather than saying "Pacific".
  expect_true(any(grepl("Fiji", c$inclusions)))
  expect_true(any(grepl("community-based", c$inclusions,
                        ignore.case = TRUE)))
})

test_that("load_toy_cbfm_criteria stays in sync with the extdata file", {
  # Read the raw file directly and confirm the loader produces the
  # same scope + inclusions. Guards against silent drift if someone
  # edits the file's variable name or shape.
  path <- system.file("extdata", "toy_cbfm_criteria.R",
                      package = "screenllm")
  skip_if(!nzchar(path) || !file.exists(path),
          "toy_cbfm_criteria.R not on disk (dev install?)")
  env <- new.env(parent = baseenv())
  sys.source(path, envir = env, keep.source = FALSE)
  expect_identical(load_toy_cbfm_criteria()$scope,
                   env$toy_cbfm_criteria$scope)
  expect_identical(as.character(load_toy_cbfm_criteria()$inclusions),
                   as.character(env$toy_cbfm_criteria$inclusions))
})
