#' OpenAI Chat Completions backend
#'
#' Talks to OpenAI's `/v1/chat/completions` endpoint. Uses the
#' response_format = "json_object" JSON mode so the model returns
#' valid JSON matching the screening prompt's schema.
#'
#' The `api_key` is read from `OPENAI_API_KEY` by default. Store the
#' key in your user-level `~/.Renviron` so it isn't committed to code
#' or exposed in shell history:
#'
#' \preformatted{
#' OPENAI_API_KEY=sk-...
#' }
#'
#' Model tags follow OpenAI's naming (e.g. `"gpt-5"`, `"gpt-4o"`,
#' `"gpt-4o-mini"`). Pass whatever tag your account has access to.
#'
#' @param api_key OpenAI API key. Defaults to `Sys.getenv("OPENAI_API_KEY")`.
#' @param base_url Base URL for the OpenAI API. Defaults to the
#'   official endpoint; override for Azure-OpenAI proxies or a
#'   self-hosted OpenAI-compatible gateway.
#' @param timeout Per-call timeout in seconds.
#' @param max_retries Retries on transport or 5xx errors.
#' @return A backend object usable with `custom_ensemble()`.
#' @export
#' @examples
#' \dontrun{
#' # Set your key first in ~/.Renviron: OPENAI_API_KEY=sk-...
#' ens <- custom_ensemble(
#'   models = c("gpt-5", "gpt-4o"),
#'   replicates = 1L,
#'   backend = backend_openai()
#' )
#' }
backend_openai <- function(api_key = Sys.getenv("OPENAI_API_KEY"),
                           base_url = "https://api.openai.com/v1",
                           timeout = 300,
                           max_retries = 3L) {
  require_api_key(api_key, "OPENAI_API_KEY", "OpenAI")
  structure(
    list(
      name = "openai",
      base_url = base_url,
      timeout = timeout,
      max_retries = max_retries,
      score_record = function(model, prompt, temperature) {
        req_builder <- function() {
          httr2::request(paste0(base_url, "/chat/completions")) |>
            httr2::req_headers(
              Authorization = paste("Bearer", api_key),
              `Content-Type` = "application/json"
            ) |>
            httr2::req_body_json(list(
              model = model,
              messages = list(list(role = "user", content = prompt)),
              temperature = temperature,
              response_format = list(type = "json_object")
            )) |>
            httr2::req_timeout(timeout)
        }
        extract <- function(body) body$choices[[1]]$message$content
        perform_api_call(req_builder, extract, max_retries = max_retries)
      },
      health = function() {
        # Cheap check: /v1/models responds 200 to a valid key.
        resp <- tryCatch(
          httr2::request(paste0(base_url, "/models")) |>
            httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
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
