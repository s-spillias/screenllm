#' @keywords internal
#' @importFrom graphics par abline legend plot.new rect
NULL

# Package options and internal constants.

# The four LLMs pinned as the default ensemble in Anonymous et al. (2026).
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

# The paper ran r = 3 replicates at temperature 0.1. Its replicate cost-benefit
# analysis shows a single replicate recovers almost all ranking and stopping
# performance (1 -> 3 replicates adds only ~0.008 AP, within replicate noise);
# the extra replicates mainly buy a variance estimate and guard against a
# degenerate run. The package therefore defaults to a single replicate, and to
# the paper's temperature of 0.1. custom_ensemble() warns when temperature is
# changed, because the reported accuracy and stopping guarantees are conditional
# on it.
.DEFAULT_REPLICATES <- 1L
.DEFAULT_TEMPERATURE <- 0.1
.DEFAULT_TARGET_RECALL <- 0.95
.DEFAULT_SAFE_MIN_COVER <- 0.50
.DEFAULT_SAFE_RUN_LENGTH <- 50L
.DEFAULT_SPOT_CHECK_N <- 200L
.DEFAULT_OLLAMA_URL <- "http://localhost:11434"

.onLoad <- function(libname, pkgname) {
  op <- options()
  # Ollama's own CLI honours OLLAMA_HOST, and every Ollama tutorial
  # tells users to set it when running on a non-default port or a
  # remote host. Read it as a fallback so `screenllm` doesn't refuse
  # to connect to a daemon the ollama CLI can see happily. The value
  # is a host[:port] (no scheme); prepend http:// if bare.
  resolve_ollama_url <- function() {
    explicit <- Sys.getenv("SCREENLLM_OLLAMA_URL", unset = "")
    if (nzchar(explicit)) return(explicit)
    host <- Sys.getenv("OLLAMA_HOST", unset = "")
    if (!nzchar(host)) return(.DEFAULT_OLLAMA_URL)
    if (grepl("^https?://", host)) return(host)
    # Ollama's "listen everywhere" bind values (0.0.0.0, ::, [::])
    # should be rewritten to localhost for the client's benefit
    # BEFORE port normalisation, so a bare "::" becomes
    # "localhost:11434" not "localhost:" (empty port).
    host <- sub("^0\\.0\\.0\\.0", "localhost", host)
    host <- sub("^\\[?::\\]?", "localhost", host)
    if (!grepl(":", host, fixed = TRUE)) host <- paste0(host, ":11434")
    paste0("http://", host)
  }
  defaults <- list(
    screenllm.ollama_url = resolve_ollama_url(),
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
    "screenllm loaded.\n",
    "  * First time?          install_prereqs(preset = \"light\")   # installs Ollama + 4 small models\n",
    "  * Ready to go:         launch_app()                         # open the browser workflow\n",
    "  * Diagnose Ollama:     check_setup()"
  )
}
