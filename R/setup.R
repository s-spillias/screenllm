#' Verify local setup for `screenllm`
#'
#' Confirms that Ollama is reachable at the configured URL, that R can
#' talk to it, and (optionally) that the four default models are available.
#' Returns `TRUE` invisibly on success and prints a diagnostic panel;
#' returns `FALSE` on failure with actionable messages.
#'
#' @param models Character vector of model tags to require. Defaults to the
#'   four LLMs pinned by `default_ensemble()`. Pass `NULL` to skip the
#'   model-availability check.
#' @param ollama_url Base URL of the local Ollama server. Defaults to the
#'   `screenllm.ollama_url` option (usually `http://localhost:11434`).
#' @return Invisible logical.
#' @export
#' @examples
#' \dontrun{
#' check_setup()
#' check_setup(models = NULL) # just check server, not models
#' }
check_setup <- function(models = .PINNED_DEFAULT_MODELS,
                        ollama_url = getOption("screenllm.ollama_url")) {
  cli::cli_h1("screenllm setup check")

  server_ok <- ollama_health(ollama_url = ollama_url, quiet = TRUE)
  if (server_ok) {
    cli::cli_alert_success("Ollama reachable at {.url {ollama_url}}")
  } else {
    cli::cli_alert_danger(
      "Ollama not reachable at {.url {ollama_url}}. Start it with `ollama serve` \\
      (macOS: it also runs from the tray app)."
    )
    return(invisible(FALSE))
  }

  if (is.null(models)) {
    cli::cli_alert_info("Skipping model check (models = NULL).")
    return(invisible(TRUE))
  }

  installed <- ollama_installed_models(ollama_url = ollama_url)
  missing <- setdiff(models, installed)
  if (length(missing) == 0) {
    cli::cli_alert_success(
      "All requested models installed ({length(models)}): {.val {models}}"
    )
    invisible(TRUE)
  } else {
    cli::cli_alert_warning(
      "{length(missing)} model{?s} missing: {.val {missing}}. \\
      Pull with `pull_model(<name>)` or `ollama pull <name>` in a shell."
    )
    invisible(FALSE)
  }
}

#' Ping the Ollama server
#'
#' Returns `TRUE` if the Ollama HTTP API responds within a short timeout.
#' Used by `check_setup()` and callable directly by backends.
#'
#' @param ollama_url Base URL of the Ollama server.
#' @param quiet If `FALSE` (the default when interactive), print status.
#' @return Logical.
#' @export
ollama_health <- function(ollama_url = getOption("screenllm.ollama_url"),
                          quiet = !interactive()) {
  req <- try(
    httr2::request(paste0(ollama_url, "/api/tags")) |>
      httr2::req_timeout(3) |>
      httr2::req_error(is_error = function(...) FALSE) |>
      httr2::req_perform(),
    silent = TRUE
  )
  ok <- !inherits(req, "try-error") && httr2::resp_status(req) == 200L
  if (!quiet) {
    if (ok) {
      cli::cli_alert_success("Ollama OK at {.url {ollama_url}}")
    } else {
      cli::cli_alert_danger("Ollama not reachable at {.url {ollama_url}}")
    }
  }
  ok
}

#' List Ollama models installed on the local server
#'
#' @param ollama_url Base URL of the Ollama server.
#' @param include_embedding Logical. Include known embedding-only
#'   models (e.g. `mxbai-embed-large`, `nomic-embed-text`,
#'   `snowflake-arctic-embed`, `all-minilm`) in the returned list.
#'   Defaults to `FALSE`; these models do not respond to Ollama's
#'   `/api/generate` in the shape `screenllm` needs, so exposing them
#'   in a chat-model picker would silently produce all-NA rankings.
#' @return Character vector of model tags. Empty character vector if the
#'   server is not reachable.
#' @keywords internal
ollama_installed_models <- function(
    ollama_url = getOption("screenllm.ollama_url"),
    include_embedding = FALSE) {
  resp <- try(
    httr2::request(paste0(ollama_url, "/api/tags")) |>
      httr2::req_timeout(5) |>
      httr2::req_perform(),
    silent = TRUE
  )
  if (inherits(resp, "try-error")) return(character())
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  mods <- body$models
  # A fresh Ollama install with zero models pulled returns
  # {"models": []}, which simplifyVector parses to an empty list().
  # nrow(list()) is NULL, so the previous guard cascaded to NA in `if`
  # and crashed the whole Setup tab's reactive graph. Handle each
  # shape (NULL / empty list / empty data.frame / populated
  # data.frame) explicitly.
  if (is.null(mods)) return(character())
  n_mods <- if (is.data.frame(mods)) nrow(mods) else length(mods)
  if (n_mods == 0L) return(character())
  tags <- if (is.data.frame(mods)) {
    as.character(mods$name)
  } else {
    vapply(mods, function(m) as.character(m$name %||% ""), character(1))
  }
  if (isTRUE(include_embedding)) return(tags)
  # Drop known embedding-model families. The naming isn't strictly
  # enforced by Ollama; this covers the popular ones.
  embed_pattern <- paste0(
    "^(mxbai-embed|nomic-embed|snowflake-arctic-embed|all-minilm|",
    "bge-|granite-embedding|embeddinggemma)"
  )
  tags[!grepl(embed_pattern, tags, ignore.case = TRUE)]
}

#' One-stop setup for a fresh machine
#'
#' Walks a non-technical user through everything needed to run `screenllm`:
#'
#' 1. Detects the OS and checks whether Ollama is installed.
#' 2. If Ollama is missing, offers to install it (`brew` on macOS,
#'    `winget` on Windows, the official install script on Linux). If the
#'    user declines or the package manager is unavailable, opens
#'    <https://ollama.com/download> in the browser.
#' 3. Waits for the Ollama daemon to come up (up to `wait_seconds`).
#' 4. Pulls each model in `models` that is not already installed.
#' 5. Verifies with `check_setup()`.
#'
#' Safe to re-run: it is a no-op if everything is already in place.
#'
#' @param preset One of `"paper"` (the four ~20-30B paper models, ~65
#'   GB), `"light"` (four ~3-7B models, ~10 GB) or `"none"` (skip the
#'   model pull). Overrides `models` when set. Defaults to `NULL`,
#'   which means "use `models`".
#' @param models Character vector of Ollama model tags to ensure are
#'   installed. Ignored if `preset` is supplied. Defaults to the
#'   four-LLM paper ensemble.
#' @param interactive Whether to prompt the user before running install
#'   commands or pulling large models. Defaults to `base::interactive()`.
#'   When `FALSE`, the function reports missing components but never
#'   installs or downloads anything.
#' @param wait_seconds Seconds to wait for the Ollama daemon to come up
#'   after (re)starting it. Defaults to 60.
#' @param ollama_url Base URL of the Ollama server.
#' @return Invisible logical: `TRUE` if all prerequisites are ready
#'   after the call, `FALSE` otherwise.
#' @export
#' @examples
#' \dontrun{
#' # Laptop-friendly preset (~10 GB; runs on 8-16 GB RAM):
#' install_prereqs(preset = "light")
#'
#' # Paper ensemble (~65 GB; needs a workstation):
#' install_prereqs(preset = "paper")
#'
#' # Check Ollama is installed without pulling any models:
#' install_prereqs(preset = "none")
#' }
install_prereqs <- function(preset = NULL,
                            models = .PINNED_DEFAULT_MODELS,
                            interactive = base::interactive(),
                            wait_seconds = 60L,
                            ollama_url = getOption("screenllm.ollama_url")) {
  if (!is.null(preset)) {
    preset <- match.arg(preset, c("paper", "light", "none"))
    models <- switch(preset,
                     paper = .PINNED_DEFAULT_MODELS,
                     light = .PINNED_LIGHT_MODELS,
                     none  = NULL)
  }
  cli::cli_h1("screenllm: install prerequisites")

  # Step 1: is Ollama on PATH?
  have_binary <- nzchar(Sys.which("ollama"))
  if (have_binary) {
    cli::cli_alert_success("Ollama binary found at {.path {unname(Sys.which('ollama'))}}")
  } else {
    cli::cli_alert_warning("Ollama binary not found on PATH.")
    if (!interactive) {
      cli::cli_alert_info(
        "Non-interactive mode: skipping install. \\
        Visit {.url https://ollama.com/download} to install manually."
      )
      return(invisible(FALSE))
    }
    installed <- try_install_ollama()
    if (!isTRUE(installed)) {
      cli::cli_alert_info(
        "Please finish installing Ollama, then re-run `install_prereqs()`."
      )
      return(invisible(FALSE))
    }
  }

  # Step 2: is the daemon reachable? If not, try to start it and wait.
  if (!ollama_health(ollama_url = ollama_url, quiet = TRUE)) {
    cli::cli_alert_info("Ollama daemon not responding; attempting to start it.")
    try_start_ollama_daemon()
    ok <- wait_for_ollama(seconds = wait_seconds, ollama_url = ollama_url)
    if (!ok) {
      cli::cli_alert_danger(
        "Ollama daemon did not come up within {wait_seconds}s. \\
        Try running {.code ollama serve} in a separate terminal, then re-run \\
        `install_prereqs()`."
      )
      return(invisible(FALSE))
    }
  }
  cli::cli_alert_success("Ollama daemon reachable at {.url {ollama_url}}")

  # Step 3: pull any missing models.
  if (is.null(models) || length(models) == 0L) {
    cli::cli_alert_info("Skipping model pull ({.code models = NULL}).")
    return(invisible(TRUE))
  }
  installed <- ollama_installed_models(ollama_url = ollama_url)
  missing <- setdiff(models, installed)
  if (length(missing) == 0L) {
    cli::cli_alert_success("All requested models already installed.")
    return(invisible(TRUE))
  }
  cli::cli_alert_info(
    "{length(missing)} model{?s} to pull: {.val {missing}}"
  )
  if (interactive) {
    ans <- utils::menu(
      c("Yes, pull them now.", "No, skip the model pull."),
      title = "Pull these models? (Large download; can take an hour on the paper ensemble.)"
    )
    if (ans != 1L) {
      cli::cli_alert_info(
        "Skipping model pull. Rerun `install_prereqs()` when you are ready."
      )
      return(invisible(FALSE))
    }
  }
  for (m in missing) {
    ok <- try(pull_model(m, ollama_url = ollama_url, verbose = TRUE),
              silent = TRUE)
    if (inherits(ok, "try-error") || !isTRUE(ok)) {
      cli::cli_alert_danger("Failed to pull {.val {m}}.")
      return(invisible(FALSE))
    }
  }
  cli::cli_alert_success("All requested models installed.")
  cli::cli_alert_info(
    "Setup complete. Launch the app with {.code screenllm::launch_app()}."
  )
  invisible(TRUE)
}

# ---- helpers for install_prereqs() -------------------------------------

# Attempt to install Ollama via a package manager. Only runs after user
# confirms. Returns TRUE if we can confirm the binary is present after
# the attempt, FALSE otherwise.
try_install_ollama <- function() {
  sysname <- Sys.info()[["sysname"]]
  candidate <- ollama_install_candidate(sysname)
  if (is.null(candidate)) {
    cli::cli_alert_info(
      "No supported package manager detected on {.val {sysname}}."
    )
    open_download_page()
    return(FALSE)
  }
  cli::cli_alert_info(
    "Detected {.val {candidate$manager}}. Proposed install command:"
  )
  cli::cli_code(candidate$command)
  ans <- utils::menu(
    c(sprintf("Yes, run `%s`.", candidate$command),
      "No, open the download page in my browser instead.",
      "Cancel."),
    title = "Install Ollama?"
  )
  if (ans == 1L) {
    status <- system(candidate$command)
    if (!identical(status, 0L)) {
      cli::cli_alert_danger("Install command exited with status {status}.")
      return(FALSE)
    }
    return(nzchar(Sys.which("ollama")))
  } else if (ans == 2L) {
    open_download_page()
    return(FALSE)
  } else {
    return(FALSE)
  }
}

# For a given OS, return a list(manager, command) that would install
# Ollama, or NULL if no supported manager is available.
ollama_install_candidate <- function(sysname = Sys.info()[["sysname"]]) {
  has <- function(cmd) nzchar(Sys.which(cmd))
  switch(
    sysname,
    "Darwin" = if (has("brew")) list(manager = "Homebrew",
                                     command = "brew install ollama") else NULL,
    "Windows" = if (has("winget"))
      list(manager = "winget",
           command = "winget install -e --id Ollama.Ollama") else NULL,
    "Linux" = list(manager = "official install script",
                   command = "curl -fsSL https://ollama.com/install.sh | sh"),
    NULL
  )
}

# Open ollama.com/download in the user's browser (best-effort).
open_download_page <- function() {
  url <- "https://ollama.com/download"
  cli::cli_alert_info(
    "Please install Ollama from {.url {url}} and then re-run \\
    `install_prereqs()`."
  )
  try(utils::browseURL(url), silent = TRUE)
  invisible(NULL)
}

# Try to launch `ollama serve` in the background. Best-effort; on macOS
# and Windows the tray app usually handles this itself once installed.
try_start_ollama_daemon <- function() {
  ollama <- Sys.which("ollama")
  if (!nzchar(ollama)) return(invisible(FALSE))
  # Fire-and-forget; ignore output.
  suppressWarnings(system2(ollama, "serve",
                           stdout = FALSE, stderr = FALSE, wait = FALSE))
  invisible(TRUE)
}

# Poll `ollama_health()` up to `seconds` seconds. Returns TRUE if it
# comes up, FALSE otherwise.
wait_for_ollama <- function(seconds = 60L,
                            ollama_url = getOption("screenllm.ollama_url"),
                            poll_interval = 1) {
  deadline <- Sys.time() + as.numeric(seconds)
  repeat {
    if (ollama_health(ollama_url = ollama_url, quiet = TRUE)) return(TRUE)
    if (Sys.time() >= deadline) return(FALSE)
    Sys.sleep(poll_interval)
  }
}

#' Pull an Ollama model to the local server
#'
#' Wrapper around Ollama's HTTP pull endpoint. Streams progress messages
#' if `verbose = TRUE`. Blocks until the pull completes.
#'
#' @param model The model tag (e.g. `"gemma3:27b"`).
#' @param ollama_url Base URL of the Ollama server.
#' @param verbose Logical.
#' @return Invisible `TRUE` on success.
#' @export
pull_model <- function(model,
                       ollama_url = getOption("screenllm.ollama_url"),
                       verbose = getOption("screenllm.verbose", TRUE)) {
  stopifnot(is.character(model), length(model) == 1L, nzchar(model))
  if (verbose) cli::cli_alert_info("Pulling {.val {model}} - may take several minutes.")
  req <- httr2::request(paste0(ollama_url, "/api/pull")) |>
    httr2::req_body_json(list(name = model, stream = FALSE)) |>
    httr2::req_timeout(60 * 60) |>
    httr2::req_perform()
  ok <- httr2::resp_status(req) == 200L
  if (ok && verbose) cli::cli_alert_success("Pulled {.val {model}}")
  invisible(ok)
}
