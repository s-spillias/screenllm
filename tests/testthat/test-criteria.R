test_that("define_criteria validates its inputs", {
  expect_error(define_criteria("", c("x")), "nzchar")
  expect_error(define_criteria("scope", character(0)))
  expect_error(
    define_criteria("scope", c("a", "b"), exclusion_notes = list("x")),
    "same length"
  )
})

test_that("build_prompt substitutes all placeholders", {
  crit <- define_criteria(
    scope = "test scope",
    inclusions = c("first criterion", "second criterion")
  )
  rec <- list(id = "record_1", title = "T", abstract = "A")
  prompt <- build_prompt(crit, rec)
  expect_type(prompt, "character")
  expect_true(!grepl("{{", prompt, fixed = TRUE))
  expect_true(grepl("test scope", prompt))
  expect_true(grepl("first criterion", prompt))
  expect_true(grepl("record_1", prompt))
})
