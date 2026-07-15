#' Google Gemini generateContent backend
#'
#' Talks to Google's `generativelanguage.googleapis.com/v1beta`
#' endpoint. Uses `responseMimeType = "application/json"` in the
#' generationConfig so Gemini returns clean JSON.
#'
#' The `api_key` is read from `GOOGLE_API_KEY` by default (also
#' accepts `GEMINI_API_KEY` if that env var is set instead).
#'
#' Model tags follow Gemini's IDs (e.g. `"gemini-2.5-pro"`,
#' `"gemini-2.5-flash"`). Pass whatever tag your account has access
#' to.
#'
#' @param api_key Google API key. Defaults to
#'   `Sys.getenv("GOOGLE_API_KEY")`, then `GEMINI_API_KEY`.
#' @param base_url Base URL for the Gemini API.
#' @param timeout Per-call timeout in seconds.
#' @param max_retries Retries on transport or 5xx errors.
#' @return A backend object usable with `custom_ensemble()`.
#' @export
#' @examples
#' \dontrun{
#' # ~/.Renviron: GOOGLE_API_KEY=AIzaSy...
#' ens <- custom_ensemble(
#'   models = "gemini-2.5-flash",
#'   replicates = 1L,
#'   backend = backend_gemini()
#' )
#' }
backend_gemini <- function(
    api_key = {
      k <- Sys.getenv("GOOGLE_API_KEY")
      if (!nzchar(k)) k <- Sys.getenv("GEMINI_API_KEY")
      k
    },
    base_url = "https://generativelanguage.googleapis.com/v1beta",
    timeout = 300,
    max_retries = 3L) {
  require_api_key(api_key, "GOOGLE_API_KEY", "Google Gemini")
  structure(
    list(
      name = "gemini",
      base_url = base_url,
      timeout = timeout,
      max_retries = max_retries,
      score_record = function(model, prompt, temperature) {
        url <- sprintf("%s/models/%s:generateContent", base_url, model)
        req_builder <- function() {
          httr2::request(url) |>
            httr2::req_headers(
              `x-goog-api-key` = api_key,
              `Content-Type` = "application/json"
            ) |>
            httr2::req_body_json(list(
              contents = list(list(parts = list(list(text = prompt)))),
              generationConfig = list(
                temperature = temperature,
                responseMimeType = "application/json"
              )
            )) |>
            httr2::req_timeout(timeout)
        }
        extract <- function(body) {
          parts <- body$candidates[[1]]$content$parts
          if (is.null(parts) || length(parts) == 0L) return(NULL)
          paste(vapply(parts, function(p) p$text %||% "",
                       character(1)), collapse = "")
        }
        perform_api_call(req_builder, extract, max_retries = max_retries)
      },
      health = function() {
        resp <- tryCatch(
          httr2::request(paste0(base_url, "/models")) |>
            httr2::req_headers(`x-goog-api-key` = api_key) |>
            httr2::req_timeout(10) |>
            httr2::req_error(is_error = function(...) FALSE) |>
            httr2::req_perform(),
          error = function(e) NULL
        )
        !is.null(resp) && httr2::resp_status(resp) == 200L
      }
    ),
    class = c("screenllm_backend", "list")
  )
}
