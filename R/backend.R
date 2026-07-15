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
    # `think = FALSE` asks reasoning models (gpt-oss, deepseek-r1,
    # qwen3-thinking) to suppress their chain-of-thought so
    # `response` contains only the final JSON. Ignored by models
    # that don't support it -- harmless flag.
    think = FALSE,
    options = list(temperature = temperature)
  )
  # Grammar-constrained JSON (`format = "json"`) makes small
  # instruction-tuned models produce clean output, but breaks
  # reasoning-first models: gpt-oss:20b, deepseek-r1's newer
  # variants, and qwen3 *-thinking* just emit chain-of-thought until
  # they hit stop tokens, never reaching the JSON. Detect those and
  # skip the constraint -- we rely on the prompt's schema block +
  # extract_first_json() to salvage what they return.
  if (!is_reasoning_model(model)) {
    body$format <- "json"
  }
  # `error` is filled in by any failure path (HTTP error, JSON parse
  # error, out-of-range score). Callers can distinguish
  # missing-model / auth failures from an all-timeouts run.
  na_result <- function(error) {
    list(score = NA_real_, explanation = NA_character_, error = error)
  }
  attempt <- 0L
  last_error <- NA_character_
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
    if (inherits(resp, "try-error")) {
      last_error <- sprintf("transport error: %s",
                             conditionMessage(attr(resp, "condition")))
      if (attempt >= max_retries) return(na_result(last_error))
      Sys.sleep(min(2^attempt, 30))
      next
    }
    status <- httr2::resp_status(resp)
    if (status != 200L) {
      # Try to read the JSON error body so the user sees a useful
      # message. Ollama returns {"error": "model 'X' not found"}.
      msg <- tryCatch({
        b <- httr2::resp_body_json(resp)
        b$error %||% b$message %||% sprintf("HTTP %d", status)
      }, error = function(e) sprintf("HTTP %d", status))
      last_error <- sprintf("HTTP %d: %s", status, msg)
      # 4xx errors (bad model name, unauthorised, bad request) are
      # permanent; retrying them just wastes time. Give up
      # immediately. 5xx and unknown errors get the exponential-
      # backoff treatment.
      if (status >= 400L && status < 500L) return(na_result(last_error))
      if (attempt >= max_retries) return(na_result(last_error))
      Sys.sleep(min(2^attempt, 30))
      next
    }
    body_json <- try(
      httr2::resp_body_json(resp, simplifyVector = TRUE),
      silent = TRUE
    )
    if (inherits(body_json, "try-error")) {
      last_error <- "response body was not valid JSON"
      if (attempt >= max_retries) return(na_result(last_error))
      next
    }
    parsed <- try(jsonlite::fromJSON(body_json$response), silent = TRUE)
    # Reasoning models sometimes emit chain-of-thought around the
    # JSON even when format = "json" and think = FALSE are set.
    # Fall back to grabbing the first balanced-braces JSON object
    # from the raw text before giving up.
    if (inherits(parsed, "try-error")) {
      extracted <- extract_first_json(body_json$response)
      if (!is.null(extracted)) {
        parsed <- try(jsonlite::fromJSON(extracted), silent = TRUE)
      }
    }
    if (inherits(parsed, "try-error") || is.null(parsed)) {
      last_error <- sprintf(
        "model response was not valid JSON: %s",
        substr(gsub("\\s+", " ", body_json$response %||% ""), 1, 200)
      )
      if (attempt >= max_retries) return(na_result(last_error))
      next
    }
    score <- suppressWarnings(as.numeric(parsed$relevance %||% NA))
    if (length(score) != 1L || is.na(score) || score < 0 || score > 100) {
      last_error <- sprintf("relevance field missing or out of range: %s",
                             substr(as.character(parsed$relevance %||% "(none)"),
                                    1, 100))
      if (attempt >= max_retries) return(na_result(last_error))
      next
    }
    return(list(
      score = score,
      explanation = as.character(parsed$explanation %||% ""),
      error = NA_character_
    ))
  }
  na_result(last_error)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Reasoning-first models that misbehave under Ollama's grammar-
# constrained JSON mode (they emit chain-of-thought until stop tokens
# and never reach the JSON). Match by prefix so tag suffixes
# (`:20b`, `:latest`, `:q4_K_M`) don't need enumerating.
#' @keywords internal
is_reasoning_model <- function(tag) {
  if (!is.character(tag) || length(tag) != 1L) return(FALSE)
  patterns <- c("^gpt-oss", "^deepseek-r1", "^phi4-reasoning",
                "-thinking", "-reasoning")
  any(vapply(patterns, function(p) grepl(p, tag), logical(1)))
}

# Extract the first balanced-braces JSON object substring from `s`.
# Used as a fallback when a reasoning model returns chain-of-thought
# text with a JSON blob buried inside, instead of clean JSON only.
# Returns the substring (character(1)) or NULL if no complete
# top-level object is found.
#' @keywords internal
extract_first_json <- function(s) {
  if (!is.character(s) || length(s) != 1L || is.na(s) || !nzchar(s)) {
    return(NULL)
  }
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  start <- NA_integer_
  depth <- 0L
  in_string <- FALSE
  escape <- FALSE
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (in_string) {
      if (escape) { escape <- FALSE; next }
      if (ch == "\\") { escape <- TRUE; next }
      if (ch == '"')  { in_string <- FALSE; next }
      next
    }
    if (ch == '"') { in_string <- TRUE; next }
    if (ch == "{") {
      if (depth == 0L) start <- i
      depth <- depth + 1L
      next
    }
    if (ch == "}") {
      depth <- depth - 1L
      if (depth == 0L && !is.na(start)) {
        return(substr(s, start, i))
      }
    }
  }
  NULL
}
