#' OpenRouter backend
#'
#' OpenRouter exposes hundreds of open- and closed-source models
#' behind an OpenAI-compatible `/chat/completions` API. Model tags
#' are namespaced by provider, e.g. `"openai/gpt-5"`,
#' `"anthropic/claude-sonnet-5"`, `"meta-llama/llama-3.1-70b-instruct"`,
#' `"deepseek/deepseek-r1"`, `"google/gemini-2.5-flash"`.
#'
#' Two attribution headers (`HTTP-Referer`, `X-Title`) are optional but
#' recommended -- they get your traffic shown on your OpenRouter
#' account rankings without affecting behaviour.
#'
#' The `api_key` is read from `OPENROUTER_API_KEY` by default.
#'
#' @param api_key OpenRouter API key. Defaults to
#'   `Sys.getenv("OPENROUTER_API_KEY")`.
#' @param base_url Base URL for OpenRouter's API.
#' @param http_referer Optional attribution header. Set to the URL of
#'   your project.
#' @param x_title Optional attribution header. Human-readable app name.
#' @param timeout Per-call timeout in seconds.
#' @param max_retries Retries on transport or 5xx errors.
#' @return A backend object usable with `custom_ensemble()`.
#' @export
#' @examples
#' \dontrun{
#' # ~/.Renviron: OPENROUTER_API_KEY=sk-or-...
#' ens <- custom_ensemble(
#'   models = c("openai/gpt-5", "anthropic/claude-sonnet-5",
#'              "google/gemini-2.5-flash"),
#'   replicates = 1L,
#'   backend = backend_openrouter(x_title = "screenllm")
#' )
#' }
backend_openrouter <- function(api_key = Sys.getenv("OPENROUTER_API_KEY"),
                               base_url = "https://openrouter.ai/api/v1",
                               http_referer = NULL,
                               x_title = NULL,
                               timeout = 300,
                               max_retries = 3L) {
  require_api_key(api_key, "OPENROUTER_API_KEY", "OpenRouter")
  extra_headers <- list()
  if (!is.null(http_referer) && nzchar(http_referer)) {
    extra_headers$`HTTP-Referer` <- http_referer
  }
  if (!is.null(x_title) && nzchar(x_title)) {
    extra_headers$`X-Title` <- x_title
  }
  structure(
    list(
      name = "openrouter",
      base_url = base_url,
      timeout = timeout,
      max_retries = max_retries,
      score_record = function(model, prompt, temperature) {
        req_builder <- function() {
          headers <- c(
            list(Authorization = paste("Bearer", api_key),
                 `Content-Type` = "application/json"),
            extra_headers
          )
          req <- httr2::request(paste0(base_url, "/chat/completions")) |>
            httr2::req_body_json(list(
              model = model,
              messages = list(list(role = "user", content = prompt)),
              temperature = temperature,
              response_format = list(type = "json_object")
            )) |>
            httr2::req_timeout(timeout)
          do.call(httr2::req_headers, c(list(req), headers))
        }
        extract <- function(body) body$choices[[1]]$message$content
        perform_api_call(req_builder, extract, max_retries = max_retries)
      },
      health = function() {
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
