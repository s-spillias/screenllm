test_that("backend_mock returns deterministic score+explanation", {
  b <- backend_mock()
  r1 <- b$score_record("model_a", "some prompt", 0.7)
  r2 <- b$score_record("model_a", "some prompt", 0.7)
  expect_equal(r1$score, r2$score)
  expect_true(r1$score >= 0 && r1$score <= 100)
  expect_type(r1$explanation, "character")
})

test_that("different prompts give different mock scores", {
  b <- backend_mock()
  r1 <- b$score_record("model_a", "prompt one", 0.7)
  r2 <- b$score_record("model_a", "prompt two", 0.7)
  expect_false(identical(r1$score, r2$score))
})
