test_that("ollama_score returns an error message on transport failure", {
  # Point at a port that is essentially guaranteed to be closed so we
  # hit the transport-error branch without needing mock HTTP infra.
  # This exercises the same failure surface a 404 goes through: NA
  # score, NA explanation, and a non-NA error message the caller can
  # display.
  out <- screenllm:::ollama_score(
    model = "any-model",
    prompt = "test",
    temperature = 0,
    ollama_url = "http://127.0.0.1:1",  # closed port
    timeout = 1,
    max_retries = 1
  )
  expect_true(is.na(out$score))
  expect_true(is.na(out$explanation))
  expect_true(!is.na(out$error))
  expect_true(nzchar(out$error))
})
