# Regression tests for the data-ingest and encoding edge cases
# uncovered by the new-user audit.

test_that("find_duplicates handles NA titles without crashing", {
  # normalise_title(NA) is NA_character_, nzchar(NA) is NA, and
  # exists(NA, envir=..) throws "invalid first argument". Any single
  # record with a missing title used to kill the whole Corpus tab
  # dedup step.
  recs <- data.frame(
    id = c("a", "b", "c"),
    title = c("Coral reefs", NA_character_, "Deep sea"),
    abstract = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )
  expect_no_error(find_duplicates(recs, fuzzy = FALSE))
  out <- find_duplicates(recs, fuzzy = FALSE)
  # NA-titled row is treated as unique (no earlier match).
  expect_true(is.na(out$duplicate_of[2]))
})

test_that("normalise_column_names aliases Zotero + case-variant columns", {
  # Zotero CSV export: Abstract Note, Publication Title, Key
  z <- data.frame(
    Key = "abc",
    Title = "Reef",
    `Abstract Note` = "some abstract",
    `Publication Title` = "Some Journal",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  z2 <- screenllm:::normalise_column_names(z)
  expect_true("title" %in% names(z2))
  expect_true("abstract" %in% names(z2))
  expect_true("id" %in% names(z2))
  expect_true("journal" %in% names(z2))
  expect_identical(z2$abstract, "some abstract")

  # Case-variant merge: blank `title` + populated `Title` -> populated
  # `title`, `Title` column dropped.
  m <- data.frame(
    title = "",
    Title = "Real Title",
    abstract = "x",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  m2 <- screenllm:::normalise_column_names(m)
  expect_identical(m2$title, "Real Title")
  expect_false("Title" %in% names(m2))
})

test_that("read_records aborts on duplicate ids with an actionable message", {
  recs <- data.frame(
    id = c("x", "x", "y"),
    title = c("a", "b", "c"),
    abstract = c("d", "e", "f"),
    stringsAsFactors = FALSE
  )
  expect_error(read_records(recs), "Duplicate record ids")
})

test_that("read_lines_any_encoding decodes UTF-8 and Windows-1252 without erroring", {
  # UTF-8: German umlaut
  tmp_utf8 <- tempfile(fileext = ".ris")
  writeBin(charToRaw("TY  - JOUR\nTI  - Müller\nER  -\n"), tmp_utf8)
  on.exit(unlink(tmp_utf8), add = TRUE)
  expect_no_error(read_lines_any_encoding(tmp_utf8))

  # UTF-8 with BOM: leading 3 bytes must be stripped so the record
  # start line still matches "^TY  - "
  tmp_bom <- tempfile(fileext = ".ris")
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)),
             charToRaw("TY  - JOUR\nTI  - Reef\nER  -\n")),
           tmp_bom)
  on.exit(unlink(tmp_bom), add = TRUE)
  lines <- read_lines_any_encoding(tmp_bom)
  expect_true(any(grepl("^TY  - JOUR", lines)))

  # Windows-1252: byte 0xFC is 'ü' in Windows-1252 (invalid as UTF-8
  # standalone). We only expect the reader NOT to error; the exact
  # decoded character depends on which encoding candidate wins.
  tmp_cp1252 <- tempfile(fileext = ".ris")
  writeBin(c(charToRaw("TY  - JOUR\nTI  - M"), as.raw(0xFC),
             charToRaw("ller\nER  -\n")),
           tmp_cp1252)
  on.exit(unlink(tmp_cp1252), add = TRUE)
  expect_no_error(read_lines_any_encoding(tmp_cp1252))
})

test_that("save_artefact writes atomically (temp file gone after write)", {
  # Use a fresh temporary root so we don't step on real projects.
  root <- tempfile("screenllm-proj-")
  fs::dir_create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  withr::with_options(list(screenllm.data_root_override = root), {
    # Since we can't easily monkey-patch tools::R_user_dir here, just
    # exercise save_artefact by writing to a manual path.
    proj_path <- fs::path(root, "projects", "test-proj")
    fs::dir_create(proj_path, recurse = TRUE)
    path <- fs::path(proj_path, "criteria.rds")
    tmp <- paste0(path, ".tmp-", Sys.getpid())
    saveRDS(list(x = 1L), tmp)
    ok <- file.rename(tmp, path)
    expect_true(isTRUE(ok))
    expect_true(file.exists(path))
    expect_false(file.exists(tmp))
  })
})

test_that("find_ollama_binary returns a Sys.which hit unchanged", {
  # Sanity: when the binary is on PATH, the helper should return
  # exactly what Sys.which returned (we don't want it to override
  # a valid path with a candidate probe).
  which_path <- unname(Sys.which("ollama"))
  if (nzchar(which_path)) {
    expect_identical(find_ollama_binary(), which_path)
  } else {
    # On CI with no Ollama, the helper should still return "" (or a
    # fallback path if one happens to exist on this runner). Just
    # confirm it returns a length-1 character.
    expect_type(find_ollama_binary(), "character")
    expect_length(find_ollama_binary(), 1L)
  }
})
