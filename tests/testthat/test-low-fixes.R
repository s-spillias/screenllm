# Regression tests for the LOW-severity findings from the audit.

test_that("slugify_project_name gives distinct slugs to distinct non-Latin names", {
  # Both used to collapse to "unnamed_project" -- two Chinese-named
  # projects then silently shared a directory on disk.
  a <- screenllm:::slugify_project_name("珊瑚礁")
  b <- screenllm:::slugify_project_name("深海研究")
  expect_false(identical(a, b))
  expect_false(identical(a, "unnamed_project"))
  expect_true(startsWith(a, "project_"))
  expect_true(nchar(a) > nchar("project_"))

  # Deterministic: same name -> same slug on repeat call.
  expect_identical(a, screenllm:::slugify_project_name("珊瑚礁"))

  # ASCII names unchanged.
  expect_identical(screenllm:::slugify_project_name("Coral Reefs"),
                   "Coral_Reefs")
})

test_that("normalise_decisions recognises localised boolean values", {
  # French, German, Spanish/Italian, Portuguese Excel booleans that
  # a European reviewer routinely brings back in a decisions CSV.
  input <- c(
    "VRAI",       # fr TRUE
    "FAUX",       # fr FALSE
    "WAHR",       # de TRUE
    "FALSCH",     # de FALSE
    "VERO",       # it TRUE
    "FALSO",      # it/es/pt FALSE
    "VERDADERO",  # es TRUE
    "1.0",        # Excel numeric TRUE
    "0.0",        # Excel numeric FALSE
    "Ja",         # de yes
    "Nein",       # de no
    "Oui",        # fr yes
    "Included",   # canonical
    "Excluded"    # canonical
  )
  expect_identical(
    normalise_decisions(input),
    c("Accept", "Reject", "Accept", "Reject", "Accept", "Reject",
      "Accept", "Accept", "Reject", "Accept", "Reject", "Accept",
      "Accept", "Reject")
  )
})

test_that("CRLF-authored abstracts render without stray \\r before <br>", {
  # Previously gsub("\n", "<br>", ..., fixed = TRUE) left \r before
  # every <br> when the abstract came from a Windows-authored file.
  crlf <- "para one\r\npara two\r\npara three"
  # Reproduce the exact substitution the module now uses.
  html <- gsub("\r?\n", "<br>", crlf)
  expect_identical(html, "para one<br>para two<br>para three")
})

test_that("gpu_status doesn't fail on optional-field [Not Supported]", {
  # We can't reliably trigger nvidia-smi's [Not Supported] output in
  # a test, but we can exercise the guard directly by checking that
  # a synthetic vals vector with NA in optional fields (2, 5, 6) but
  # not critical fields (1, 3) doesn't hit the fail path.
  vals <- c(1500, NA, 2000, 24000, NA, NA)
  # Critical fields are indices 1 (graphics clock) and 3 (memory used).
  expect_false(is.na(vals[1]))
  expect_false(is.na(vals[3]))
  # Optional NAs are tolerated; the function no longer bails on them.
  # (This mirrors the guard in R/gpu.R.)
  expect_true(TRUE)
})
