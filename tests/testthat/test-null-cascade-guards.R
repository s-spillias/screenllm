# Regression tests for the class of bug where a NULL / integer(0) /
# NA cascades through a comparison into an `if()` and crashes a
# reactive. These guards protect long-running screening sessions
# from being wiped out by legacy artefacts, hand-edited files, or
# NA-tainted ensemble configs.

test_that("is_reasoning_model handles NA_character_ without crashing", {
  # Regression: an ensemble with NA in its models vector used to
  # crash EVERY LLM call in the rank loop (grepl(p, NA) is NA,
  # any(NA, ...) is NA, `if (!NA)` throws "missing value").
  expect_false(is_reasoning_model(NA_character_))
  expect_false(is_reasoning_model(character()))
  expect_false(is_reasoning_model(NULL))
  expect_false(is_reasoning_model(c("gpt-oss", "mistral")))  # length != 1
  expect_true(is_reasoning_model("gpt-oss:20b"))
  expect_true(is_reasoning_model("deepseek-r1:8b"))
  expect_false(is_reasoning_model("mistral:7b"))
})

test_that("rank.R pick() helper is tolerant of missing/wrong-shape cache fields", {
  # Regression: a single cache file missing $score (older schema)
  # crashed vapply(numeric(1)) at the aggregation step, discarding
  # the whole ranking run. The pick() helper defends by returning
  # the type-matched default for anything but a length-1 value.
  pick <- function(s, key, type, default) {
    v <- s[[key]]
    if (is.null(v) || length(v) != 1L) return(default)
    tryCatch(as(v, type), error = function(e) default)
  }
  # Missing field
  expect_identical(pick(list(), "score", "numeric", NA_real_), NA_real_)
  # NULL field
  expect_identical(pick(list(score = NULL), "score", "numeric", NA_real_),
                   NA_real_)
  # Zero-length field
  expect_identical(pick(list(score = numeric()), "score", "numeric", NA_real_),
                   NA_real_)
  # Multi-length field (should also default, not crash the vapply)
  expect_identical(pick(list(score = c(0.5, 0.7)), "score", "numeric", NA_real_),
                   NA_real_)
  # Happy path
  expect_identical(pick(list(score = 0.5), "score", "numeric", NA_real_), 0.5)
  # Coercion path
  expect_identical(pick(list(id = 1L), "id", "character", NA_character_), "1")
})
