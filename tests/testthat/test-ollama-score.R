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

test_that("qwen tags get explicit stop tokens; other models do not", {
  # qwen3 Ollama tags run past the JSON without a stop token, corrupting
  # a large fraction of records; ollama_request_body() registers the
  # ChatML end markers as stops so generation halts after the JSON.
  qwen <- screenllm:::ollama_request_body(
    "qwen3:30b-a3b-instruct-2507", "p", temperature = 0.7)
  expect_identical(qwen$options$stop, c("<|endoftext|>", "<|im_end|>"))
  # thinking-variant qwen (skips format="json") still gets the stops
  qwen_think <- screenllm:::ollama_request_body(
    "qwen3:30b-a3b-thinking-2507", "p", temperature = 0.7)
  expect_identical(qwen_think$options$stop, c("<|endoftext|>", "<|im_end|>"))
  # non-qwen models are untouched (no stop key)
  for (m in c("gemma3:27b", "gpt-oss:20b", "mistral-small3.2:24b",
              "deepseek-r1:14b")) {
    expect_null(screenllm:::ollama_request_body(m, "p", 0.7)$options$stop)
  }
})
