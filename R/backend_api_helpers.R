# Shared helpers for HTTP-API backends (OpenAI / Anthropic / Gemini).
#
# Every proprietary backend follows the same recipe:
#   1. Build a request body from (prompt, temperature).
#   2. POST it to the provider endpoint with an API-key header.
#   3. Retry on 5xx / transport errors; fail fast on 4xx.
#   4. Extract the response text from provider-specific JSON.
#   5. Parse that text as JSON to get {id, explanation, relevance}.
#   6. Return list(score, explanation, error) shaped like backend_ollama.
#
# Only steps 1, 2 and 4 differ per provider; the rest (retry, JSON
# extraction, score validation) is shared here so the individual
# backend files stay small.

# Package-scope null-coalescing operator; defined once in backend.R.
# Referenced here to keep helpers self-contained under lintr.

#' @keywords internal
parse_relevance_from_text <- function(text) {
  na_result <- function(error) {
    list(score = NA_real_, explanation = NA_character_, error = error)
  }
  parsed <- tryCatch(jsonlite::fromJSON(text), error = function(e) NULL)
  if (is.null(parsed)) {
    extracted <- extract_first_json(text)
    if (!is.null(extracted)) {
      parsed <- tryCatch(jsonlite::fromJSON(extracted),
                          error = function(e) NULL)
    }
  }
  if (is.null(parsed)) {
    return(na_result(sprintf(
      "model response was not valid JSON: %s",
      substr(gsub("\\s+", " ", text), 1, 200)
    )))
  }
  score <- suppressWarnings(as.numeric(parsed$relevance %||% NA))
  if (length(score) != 1L || is.na(score) || score < 0 || score > 100) {
    return(na_result(sprintf(
      "relevance field missing or out of range: %s",
      substr(as.character(parsed$relevance %||% "(none)"), 1, 100)
    )))
  }
  list(score = score,
       explanation = as.character(parsed$explanation %||% ""),
       error = NA_character_)
}

#' @keywords internal
perform_api_call <- function(req_builder, extract_text,
                             max_retries = 3L, backoff_cap = 30) {
  # req_builder: function(no args) -> httr2::request already fully
  #   populated with method / body / headers / timeout.
  # extract_text: function(response_body_list) -> character(1) or NULL.
  na_result <- function(error) {
    list(score = NA_real_, explanation = NA_character_, error = error)
  }
  attempt <- 0L
  last_error <- NA_character_
  while (attempt < max_retries) {
    attempt <- attempt + 1L
    resp <- tryCatch(
      req_builder() |>
        httr2::req_error(is_error = function(...) FALSE) |>
        httr2::req_perform(),
      error = function(e) e
    )
    if (inherits(resp, "error")) {
      last_error <- sprintf("transport error: %s", conditionMessage(resp))
      if (attempt >= max_retries) return(na_result(last_error))
      Sys.sleep(min(2^attempt, backoff_cap))
      next
    }
    status <- httr2::resp_status(resp)
    if (status != 200L) {
      msg <- tryCatch({
        b <- httr2::resp_body_json(resp)
        # Providers put the message in different places.
        b$error$message %||% b$error %||% b$message %||%
          sprintf("HTTP %d", status)
      }, error = function(e) sprintf("HTTP %d", status))
      last_error <- sprintf("HTTP %d: %s", status, msg)
      # 4xx = permanent (bad model / auth / body). No retry.
      if (status >= 400L && status < 500L) return(na_result(last_error))
      if (attempt >= max_retries) return(na_result(last_error))
      Sys.sleep(min(2^attempt, backoff_cap))
      next
    }
    body_json <- tryCatch(
      httr2::resp_body_json(resp, simplifyVector = FALSE),
      error = function(e) e
    )
    if (inherits(body_json, "error")) {
      last_error <- "response body was not valid JSON"
      if (attempt >= max_retries) return(na_result(last_error))
      next
    }
    text <- tryCatch(extract_text(body_json), error = function(e) NULL)
    if (is.null(text) || !nzchar(text)) {
      last_error <- "provider returned an empty completion"
      if (attempt >= max_retries) return(na_result(last_error))
      next
    }
    return(parse_relevance_from_text(text))
  }
  na_result(last_error)
}

#' @keywords internal
require_api_key <- function(value, env_name, provider) {
  if (is.null(value) || !nzchar(value)) {
    cli::cli_abort(paste0(
      "No API key for {.field ", provider, "}. Set the environment ",
      "variable {.envvar ", env_name, "} (e.g. in ~/.Renviron), or ",
      "pass {.arg api_key} explicitly."
    ))
  }
  invisible(value)
}
