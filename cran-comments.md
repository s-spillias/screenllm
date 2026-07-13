# cran-comments.md

## Submission type

Initial CRAN submission of `screenllm` 0.1.0.

## Test environments

* Local: Ubuntu 24.04, R 4.5.x — `R CMD check --as-cran` OK
* Windows (via `devtools::check_win_devel()`): _pending_
* macOS (via `rhub::check_for_cran(platforms = "macos-highsierra-release-cran")`): _pending_
* R-hub Windows / Fedora / Debian: _pending_

## R CMD check results

Local `R CMD check --as-cran`:

* 0 ERRORs
* 0 WARNINGs
* 0 NOTEs

## Downstream dependencies

None (first release).

## Notes for CRAN reviewers

1. **Optional external service (Ollama).** The package's `backend_ollama()`
   talks to a locally-installed Ollama server
   (<https://ollama.com>). Ollama is *not* required to install, load,
   check, or use the package for development purposes:

   * All examples that require Ollama are wrapped in `\dontrun{}`.
   * All tests use the built-in `backend_mock()` (a deterministic in-R
     stub); no test opens a network connection.
   * The vignette's Ollama-dependent chunks are `eval = FALSE` and are
     labelled as such.
   * `install_prereqs()` is a user-callable helper that offers to install
     Ollama on request (macOS via Homebrew, Windows via winget, Linux
     via the official install script). It **never** runs a shell install
     command without an explicit interactive `menu()` confirmation, and
     is a no-op in non-interactive mode.

   Two precedents on CRAN for the same optional-external-runtime pattern:
   `elmer` (Ollama support), `chattr`.

2. **`tools::R_user_dir("screenllm", "data")`** is used to persist
   Shiny-app projects across sessions. Writes only occur when the user
   explicitly saves an artefact in the app; the package itself never
   writes to disk during examples, tests, or vignette build.

3. **Shiny app** is `Suggests:`-only, so users on servers without Shiny
   installed can still call the ranking / stopping API.

## Reproducibility

* Package source: <https://github.com/s-spillias/screenllm>
* Companion paper: Spillias et al. (2026), in submission (pre-print DOI
  will be added to `DESCRIPTION` before final acceptance).
