#' Anthropic Claude Messages backend
#'
#' Talks to Anthropic's `/v1/messages` endpoint. Anthropic doesn't
#' have OpenAI's `response_format = json_object` toggle, so we rely
#' on the prompt schema (screening prompt says "respond ONLY with a
#' JSON object") and the balanced-braces JSON extraction fallback to
#' handle occasional prose-wrapping. In practice Claude follows the
#' schema.
#'
#' The `api_key` is read from `ANTHROPIC_API_KEY` by default.
#'
#' Model tags follow Anthropic's IDs (e.g. `"claude-opus-4-5"`,
#' `"claude-sonnet-5"`, `"claude-haiku-4-5"`).
#'
#' @param api_key Anthropic API key. Defaults to
#'   `Sys.getenv("ANTHROPIC_API_KEY")`.
#' @param base_url Base URL for the Anthropic API.
#' @param anthropic_version API version header. `"2023-06-01"` is
#'   the long-lived stable version.
#' @param max_tokens Maximum completion tokens. 1024 is comfortable
#'   for a per-record score + explanation; raise if you're truncating.
#' @param timeout Per-call timeout in seconds.
#' @param max_retries Retries on transport or 5xx errors.
#' @return A backend object usable with `custom_ensemble()`.
#' @export
#' @examples
#' \dontrun{
#' # Set your key first in ~/.Renviron: ANTHROPIC_API_KEY=sk-ant-...
#' ens <- custom_ensemble(
#'   models = "claude-sonnet-5",
#'   replicates = 1L,
#'   backend = backend_anthropic()
#' )
#' }
backend_anthropic <- function(api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                              base_url = "https://api.anthropic.com/v1",
                              anthropic_version = "2023-06-01",
                              max_tokens = 1024L,
                              timeout = 300,
                              max_retries = 3L) {
  require_api_key(api_key, "ANTHROPIC_API_KEY", "Anthropic")
  structure(
    list(
      name = "anthropic",
      base_url = base_url,
      timeout = timeout,
      max_retries = max_retries,
      score_record = function(model, prompt, temperature) {
        req_builder <- function() {
          httr2::request(paste0(base_url, "/messages")) |>
            httr2::req_headers(
              `x-api-key` = api_key,
              `anthropic-version` = anthropic_version,
              `Content-Type` = "application/json"
            ) |>
            httr2::req_body_json(list(
              model = model,
              max_tokens = as.integer(max_tokens),
              temperature = temperature,
              messages = list(list(role = "user", content = prompt))
            )) |>
            httr2::req_timeout(timeout)
        }
        extract <- function(body) {
          # content is a list of blocks; the text block(s) carry the answer.
          text_blocks <- Filter(function(b) identical(b$type, "text"),
                                 body$content)
          if (length(text_blocks) == 0L) return(NULL)
          paste(vapply(text_blocks, function(b) b$text %||% "",
                       character(1)), collapse = "")
        }
        perform_api_call(req_builder, extract, max_retries = max_retries)
      },
      health = function() {
        # Anthropic doesn't publish an obvious cheap ping endpoint;
        # a bad-key check on /messages costs a token or two but
        # tells us the key is valid. Skip health-probing at boot,
        # let the first real call surface auth errors.
        TRUE
      }
    ),
    class = c("screenllm_backend", "list")
  )
}
