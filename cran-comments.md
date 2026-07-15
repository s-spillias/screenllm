# cran-comments.md

## Submission type

Initial CRAN submission of `screenllm` 0.1.0.

## Test environments

* Local: Ubuntu 24.04, R 4.5.x — `R CMD check --as-cran` OK (0 errors,
  0 warnings, 0 notes).
* Windows (via `devtools::check_win_devel()`): __PASTE RESULTS HERE__.
  The result URL is emailed from win-builder within 30 minutes of
  upload and looks like <https://win-builder.r-project.org/xxxxxxxx/>.
* R-hub (via GitHub Actions R-hub workflow): __PASTE RESULTS HERE__.
  See <https://github.com/s-spillias/screenllm/actions/workflows/rhub.yaml>.
* GitHub Actions R-CMD-check matrix (see `.github/workflows/R-CMD-check.yaml`):
  ubuntu-latest R-release + R-devel, macos-latest R-release,
  windows-latest R-release. Latest green: __PASTE COMMIT SHA HERE__.

## R CMD check results

Local `R CMD check --as-cran` on commit `d4b7496`:

* 0 ERRORs
* 0 WARNINGs
* 0 NOTEs

## Downstream dependencies

None (first release).

## Alpha testing

Package went through internal alpha testing with a small group of
colleagues before submission; feedback surfaced several UX bugs
(silent decision-drop from a base-`rbind` column mismatch; a
`cli_alert_info` argument-list misuse on the "no Ollama installed"
path; a "records with baked-in `human_decision`" collision in the
report; numeric-id vapply crash at aggregation). All are fixed and
regression-tested.

## Notes for CRAN reviewers

1. **Optional external service (Ollama).** The package's `backend_ollama()`
   talks to a locally-installed Ollama server
   (<https://ollama.com>). Ollama is *not* required to install, load,
   check, or use the package for development purposes:

   * All examples that require Ollama are wrapped in `\dontrun{}`.
   * All tests use the built-in `backend_mock()` (a deterministic in-R
     stub); no test opens a network connection.
   * The vignette's Ollama-dependent chunks are `eval = FALSE`.
   * `install_prereqs()` is a user-callable helper that offers to
     install Ollama on request (macOS via Homebrew, Windows via
     winget, Linux via the official install script). It **never**
     runs a shell install command without an explicit interactive
     `menu()` confirmation, and is a no-op in non-interactive mode.

   Two precedents on CRAN for the same optional-external-runtime
   pattern: `elmer` (Ollama support), `chattr`.

2. **`tools::R_user_dir("screenllm", "data")`** is used to persist
   Shiny-app projects across sessions. Writes only occur when the
   user explicitly saves an artefact in the app; the package itself
   never writes to disk during examples, tests, or vignette build.

3. **Shiny app** is `Suggests:`-only, so users on servers without
   Shiny installed can still call the ranking / stopping API.

4. **Companion paper.** Spillias et al. (2026), *An open-source
   LLM-assisted screening workflow for environmental systematic
   reviews.* (in submission — DOI to be added on acceptance and
   reflected in a 0.1.1 update).

## Reproducibility

* Source: <https://github.com/s-spillias/screenllm>
* Public before submission: yes (private during alpha).
