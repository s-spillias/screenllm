test_that("is_reasoning_model detects known reasoning families", {
  expect_true(screenllm:::is_reasoning_model("gpt-oss:20b"))
  expect_true(screenllm:::is_reasoning_model("gpt-oss:latest"))
  expect_true(screenllm:::is_reasoning_model("deepseek-r1:14b"))
  expect_true(screenllm:::is_reasoning_model("deepseek-r1:32b-qwen-distill-fp16"))
  expect_true(screenllm:::is_reasoning_model("phi4-reasoning:14b"))
  expect_true(screenllm:::is_reasoning_model("qwen3:30b-a3b-thinking-2507"))
  expect_true(screenllm:::is_reasoning_model("something-reasoning"))

  expect_false(screenllm:::is_reasoning_model("gemma3:4b"))
  expect_false(screenllm:::is_reasoning_model("mistral:7b"))
  expect_false(screenllm:::is_reasoning_model("qwen3:30b-a3b-instruct-2507"))
  expect_false(screenllm:::is_reasoning_model("llama3.2:3b"))
  expect_false(screenllm:::is_reasoning_model(""))
})

test_that("extract_first_json pulls JSON from mixed text", {
  msg <- 'We need to score each criterion. {"id": "r1", "relevance": "42"} done.'
  extracted <- screenllm:::extract_first_json(msg)
  expect_equal(extracted, '{"id": "r1", "relevance": "42"}')
  parsed <- jsonlite::fromJSON(extracted)
  expect_equal(parsed$relevance, "42")
})

test_that("extract_first_json handles nested braces", {
  msg <- 'text {"outer": {"inner": 1}, "x": 2} trailing'
  extracted <- screenllm:::extract_first_json(msg)
  expect_equal(extracted, '{"outer": {"inner": 1}, "x": 2}')
})

test_that("extract_first_json returns NULL on no complete object", {
  expect_null(screenllm:::extract_first_json("no braces here"))
  expect_null(screenllm:::extract_first_json("{ unclosed"))
  expect_null(screenllm:::extract_first_json(""))
  expect_null(screenllm:::extract_first_json(NA_character_))
})
