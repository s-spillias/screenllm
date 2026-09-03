# cran-comments.md

## Test environments

* Local: Ubuntu 24.04, R 4.6.1
* win-builder: R-devel
* R-hub: macOS R-devel, Windows R-devel
* GitHub Actions: Ubuntu R-release + R-devel, Windows R-release

## R CMD check results

0 errors | 0 warnings | 1 NOTE.

* New submission.
* "Possibly misspelled words" in DESCRIPTION — Ollama (the software),
  Spillias and Vembye (author surnames), choosable, and et/al (from
  "et al."); all correct as written.

## Downstream dependencies

None (first release).

## Note for reviewers

`backend_ollama()` talks to a locally-installed Ollama server
(<https://ollama.com>), which is optional. Examples that need a live
server use `\dontrun{}`; the others run against a built-in mock backend.
All tests use that mock backend and open no network connection, and the
vignette's Ollama-dependent chunks are `eval = FALSE`. `install_prereqs()`
offers to install Ollama only after an interactive confirmation, and is a
no-op in non-interactive sessions.
