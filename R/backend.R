# Backend abstraction. A backend is a list with:
#   $name         - character
#   $score_record - function(model, prompt, temperature, ...) -> numeric score
#                   in [0, 100] and an explanation string
#   $health       - function() -> logical

#' Ollama backend
#'
#' Talks to a locally-running Ollama server over HTTP. Each call sends one
#' record's prompt to one model at the configured temperature, parses the
#' JSON response, and returns the numeric relevance score plus the LLM's
#' free-text explanation.
#'
#' @param ollama_url Base URL of the Ollama server.
#' @param timeout Per-call timeout in seconds.
#' @param max_retries Retries on transient errors.
#' @return A backend object.
#' @export
backend_ollama <- function(ollama_url = getOption("screenllm.ollama_url"),
                           timeout = 300,
                           max_retries = 3L) {
  structure(
    list(
      name = "ollama",
      url = ollama_url,
      timeout = timeout,
      max_retries = max_retries,
      score_record = function(model, prompt, temperature) {
        ollama_score(model, prompt, temperature, ollama_url, timeout, max_retries)
      },
      health = function() ollama_health(ollama_url, quiet = TRUE)
    ),
    class = c("screenllm_backend", "list")
  )
}

#' Mock backend for tests
#'
#' Returns deterministic scores derived from a `digest::digest` hash of the
#' prompt. Never hits the network. Useful for unit tests and for
#' package-development end-to-end runs without needing Ollama.
#'
#' @param base Baseline score (integer 0-100). Added to a small
#'   prompt-derived variation.
#' @return A backend object.
#' @export
#' @examples
#' b <- backend_mock()
#' b$score_record("any-model", "any prompt", temperature = 0)
backend_mock <- function(base = 50L) {
  structure(
    list(
      name = "mock",
      score_record = function(model, prompt, temperature) {
        digest_bytes <- as.integer(charToRaw(digest::digest(
          paste(model, prompt), algo = "md5", serialize = FALSE
        )))
        # Deterministic pseudo-random score in [0, 100]
        val <- (base + sum(digest_bytes[1:4]) %% 51 - 25) %% 101
        list(
          score = as.numeric(val),
          explanation = paste0("Mock score from ", model)
        )
      },
      health = function() TRUE
    ),
    class = c("screenllm_backend", "list")
  )
}

#' @keywords internal
ollama_score <- function(model, prompt, temperature,
                         ollama_url, timeout, max_retries) {
  body <- list(
    model = model,
    prompt = prompt,
    stream = FALSE,
    options = list(temperature = temperature),
    format = "json"
  )
  attempt <- 0L
  while (attempt < max_retries) {
    attempt <- attempt + 1L
    resp <- try(
      httr2::request(paste0(ollama_url, "/api/generate")) |>
        httr2::req_body_json(body) |>
        httr2::req_timeout(timeout) |>
        httr2::req_error(is_error = function(...) FALSE) |>
        httr2::req_perform(),
      silent = TRUE
    )
    if (inherits(resp, "try-error") || httr2::resp_status(resp) != 200L) {
      if (attempt >= max_retries) {
        return(list(score = NA_real_, explanation = NA_character_))
      }
      Sys.sleep(min(2^attempt, 30))
      next
    }
    body_json <- try(
      httr2::resp_body_json(resp, simplifyVector = TRUE),
      silent = TRUE
    )
    if (inherits(body_json, "try-error")) next
    parsed <- try(jsonlite::fromJSON(body_json$response), silent = TRUE)
    if (inherits(parsed, "try-error")) next
    score <- suppressWarnings(as.numeric(parsed$relevance %||% NA))
    if (length(score) != 1L || is.na(score) || score < 0 || score > 100) next
    return(list(
      score = score,
      explanation = as.character(parsed$explanation %||% "")
    ))
  }
  list(score = NA_real_, explanation = NA_character_)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
