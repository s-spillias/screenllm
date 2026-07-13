test_that("default_ensemble_light returns a 4-model mean ensemble", {
  ens <- default_ensemble_light(backend = backend_mock())
  expect_s3_class(ens, "screenllm_ensemble")
  expect_length(ens$models, 4L)
  expect_identical(ens$aggregator, "mean")
  # The pinned tags are meant to fit on a laptop; verify none of them are
  # accidentally the paper's 20-30 B variants.
  expect_false(any(grepl(":2[0-9]b|:30b|:27b", ens$models)))
})

test_that("ollama_install_candidate matches the platform", {
  # Force each branch and confirm the returned command mentions a
  # recognisable installer for that OS.
  darwin <- ollama_install_candidate("Darwin")
  win    <- ollama_install_candidate("Windows")
  linux  <- ollama_install_candidate("Linux")
  # On CI/dev machines, brew/winget may or may not be on PATH, so we only
  # assert the shape when the manager IS available.
  if (!is.null(darwin)) expect_match(darwin$command, "brew")
  if (!is.null(win))    expect_match(win$command, "winget")
  expect_false(is.null(linux))  # curl script is always the fallback
  expect_match(linux$command, "ollama.com/install.sh")
})

test_that("wait_for_ollama returns FALSE when the server is unreachable", {
  # Point at a port that is essentially guaranteed to be closed.
  bogus <- "http://127.0.0.1:1"
  t0 <- Sys.time()
  ok <- wait_for_ollama(seconds = 2, ollama_url = bogus, poll_interval = 0.5)
  expect_false(ok)
  # Should have honored the 2-second budget (allow slop).
  expect_lt(as.numeric(Sys.time() - t0, units = "secs"), 6)
})

test_that("install_prereqs is a safe no-op in non-interactive mode without Ollama", {
  # If Ollama isn't installed on the test machine, non-interactive mode
  # should just report and return FALSE without prompting.
  skip_if(nzchar(Sys.which("ollama")),
          "Ollama is installed on this machine; skipping the missing-binary branch.")
  ok <- suppressMessages(
    install_prereqs(models = NULL, interactive = FALSE,
                    ollama_url = "http://127.0.0.1:1")
  )
  expect_false(ok)
})

test_that("install_prereqs(models = NULL) short-circuits after Ollama is confirmed", {
  # If Ollama is reachable, `install_prereqs(models = NULL)` should
  # confirm the daemon and return TRUE.
  skip_if_not(ollama_health(quiet = TRUE), "Ollama not available")
  ok <- suppressMessages(install_prereqs(models = NULL, interactive = FALSE))
  expect_true(ok)
})
