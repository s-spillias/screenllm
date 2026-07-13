#' @keywords internal
#' @importFrom graphics par abline legend plot.new rect
NULL

# Package options and internal constants.

# The four LLMs pinned as the default ensemble in Spillias et al. (2026).
# Users override with `custom_ensemble()`; these tags are Ollama identifiers.
.PINNED_DEFAULT_MODELS <- c(
  "gemma3:27b",
  "gpt-oss:20b",
  "mistral-small3.2:24b",
  "qwen3:30b-a3b-instruct-2507"
)

# A "light" ensemble of 3-4 B-class models. Fits in ~10 GB on disk and
# ~8 GB of RAM, so it runs on a laptop. Slightly less accurate than
# `.PINNED_DEFAULT_MODELS` (~5-10 percentage-point drop in AP on the
# paper benchmarks), but sufficient for exploration and small reviews.
# Same four vendors as the paper default so the ensemble stays diverse.
.PINNED_LIGHT_MODELS <- c(
  "gemma3:4b",
  "llama3.2:3b",
  "qwen3:4b",
  "mistral:7b"
)

.DEFAULT_REPLICATES <- 3L
.DEFAULT_TEMPERATURE <- 0.7
.DEFAULT_TARGET_RECALL <- 0.95
.DEFAULT_SAFE_MIN_COVER <- 0.50
.DEFAULT_SAFE_RUN_LENGTH <- 50L
.DEFAULT_SPOT_CHECK_N <- 200L
.DEFAULT_OLLAMA_URL <- "http://localhost:11434"

.onLoad <- function(libname, pkgname) {
  op <- options()
  defaults <- list(
    screenllm.ollama_url = Sys.getenv(
      "SCREENLLM_OLLAMA_URL",
      unset = .DEFAULT_OLLAMA_URL
    ),
    screenllm.cache_dir = NULL,
    screenllm.verbose = TRUE,
    screenllm.max_workers = 1L
  )
  to_set <- !(names(defaults) %in% names(op))
  if (any(to_set)) options(defaults[to_set])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "screenllm loaded. Run `check_setup()` to verify Ollama and required models."
  )
}
