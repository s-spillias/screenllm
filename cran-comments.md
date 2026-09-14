# cran-comments.md

## Resubmission

This is a resubmission. In response to the reviewer's comments I have:

* Put software, package, and API names in single quotes in the
  Description ('Ollama', 'Shiny', 'OpenAI').
* Reformatted the reference as authors (year) <doi:...> with the year in
  parentheses: Vembye et al. (2025) <doi:10.1037/met0000769>.
* Replaced \dontrun{} with \donttest{} everywhere the example can run.
  The only remaining \dontrun{} is on install_prereqs(), which installs
  external software (Ollama) and pulls multi-GB models.
* Removed the default write to the home filespace: launch_screening_app()
  now defaults out_file to tempdir() instead of the working directory, and
  every example/test/vignette writes only to tempdir(). No function writes
  to the user's home filespace by default.
* Reset graphical parameters with an immediate on.exit(par(oldpar)) before
  changing par() in the plot code.

## Test environments

* Local: Ubuntu 24.04, R 4.6.1
* win-builder: R-devel
* R-hub: macOS R-devel, Windows R-devel
* GitHub Actions: Ubuntu R-release + R-devel, Windows R-release

## R CMD check results

0 errors | 0 warnings | 1 NOTE.

* New submission.
* "Possibly misspelled words" in DESCRIPTION — Ollama (the software),
  Spillias and Vembye (author surnames), and et/al (from "et al.");
  all correct as written.

## Downstream dependencies

None (first release).

## Note for reviewers

`backend_ollama()` talks to a locally-installed Ollama server
(<https://ollama.com>), which is optional. All tests use a built-in mock
backend and open no network connection, the vignette's Ollama-dependent
chunks are `eval = FALSE`, and examples either use the mock backend or
degrade gracefully when no server is present. The one remaining
`\dontrun{}` is on `install_prereqs()`, which installs Ollama and pulls
multi-GB models; it offers to install only after an interactive
confirmation and is a no-op in non-interactive sessions.
