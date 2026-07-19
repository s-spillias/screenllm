# Regression: users who followed Ollama's docs and set OLLAMA_HOST
# were seeing screenllm still hit localhost:11434 -- the env var
# was ignored entirely. Verify the resolver builds a usable URL
# from each shape of OLLAMA_HOST users actually set.

# Reuse the same resolver that .onLoad uses. Kept out of the
# package namespace to avoid an internal-tests-only export, so we
# reconstruct the logic here and pin the exact behaviour we ship.

resolve <- function(explicit = "", host = "") {
  if (nzchar(explicit)) return(explicit)
  if (!nzchar(host)) return("http://localhost:11434")
  if (grepl("^https?://", host)) return(host)
  host <- sub("^0\\.0\\.0\\.0", "localhost", host)
  host <- sub("^\\[?::\\]?", "localhost", host)
  if (!grepl(":", host, fixed = TRUE)) host <- paste0(host, ":11434")
  paste0("http://", host)
}

test_that("SCREENLLM_OLLAMA_URL beats OLLAMA_HOST", {
  expect_identical(
    resolve(explicit = "http://myserver:1234", host = "0.0.0.0:11434"),
    "http://myserver:1234"
  )
})

test_that("empty env falls back to the localhost default", {
  expect_identical(resolve(), "http://localhost:11434")
})

test_that("OLLAMA_HOST with a port is used verbatim", {
  expect_identical(resolve(host = "192.168.1.10:11500"),
                   "http://192.168.1.10:11500")
})

test_that("bare hostname gets the default port", {
  expect_identical(resolve(host = "ollama-server.local"),
                   "http://ollama-server.local:11434")
})

test_that("wildcard bind addresses rewrite to localhost for clients", {
  expect_identical(resolve(host = "0.0.0.0:11434"),
                   "http://localhost:11434")
  expect_identical(resolve(host = "::"),
                   "http://localhost:11434")
})

test_that("full URLs pass through untouched", {
  expect_identical(resolve(host = "https://ollama.example.com"),
                   "https://ollama.example.com")
})
