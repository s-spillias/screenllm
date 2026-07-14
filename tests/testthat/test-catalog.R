test_that("ollama_catalog returns a well-shaped tibble", {
  cat <- ollama_catalog()
  expect_s3_class(cat, "tbl_df")
  expect_setequal(names(cat), c("tag", "family", "size_gb", "description"))
  expect_gt(nrow(cat), 10L)
  expect_true(all(nzchar(cat$tag)))
  expect_true(all(cat$size_gb > 0))
  # No duplicates - each tag should appear once.
  expect_equal(anyDuplicated(cat$tag), 0L)
  # Every family should be non-empty.
  expect_true(all(nzchar(cat$family)))
})

test_that("catalog covers both paper preset and light preset models", {
  cat <- ollama_catalog()
  expect_true(all(.PINNED_DEFAULT_MODELS %in% cat$tag))
  expect_true(all(.PINNED_LIGHT_MODELS %in% cat$tag))
})
