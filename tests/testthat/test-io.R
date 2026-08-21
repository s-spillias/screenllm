test_that("read_records reads a minimal RIS file", {
  ris <- c(
    "TY  - JOUR",
    "TI  - Coral reef restoration outcomes",
    "AB  - We monitored transplanted corals for 12 months.",
    "PY  - 2024",
    "DO  - 10.1234/example",
    "JO  - Marine Ecology",
    "ER  - ",
    "",
    "TY  - JOUR",
    "T1  - Deep sea mining impacts",
    "N2  - Modelling study of sediment plumes.",
    "Y1  - 2023",
    "ER  - "
  )
  tmp <- tempfile(fileext = ".ris")
  writeLines(ris, tmp)
  recs <- read_records(tmp)
  expect_equal(nrow(recs), 2L)
  expect_equal(recs$title[1], "Coral reef restoration outcomes")
  expect_equal(recs$title[2], "Deep sea mining impacts")
  expect_match(recs$abstract[1], "transplanted")
  expect_match(recs$abstract[2], "sediment plumes")
})

test_that("find_duplicates catches exact DOI matches", {
  recs <- data.frame(
    id = c("a", "b", "c"),
    title = c("Coral reefs", "Something else", "Deep sea"),
    abstract = c("x", "y", "z"),
    doi = c("10.1/x", "10.1/x", "10.2/y"),
    stringsAsFactors = FALSE
  )
  out <- find_duplicates(recs)
  expect_equal(out$duplicate_of, c(NA, "a", NA))
})

test_that("find_duplicates catches normalised title matches", {
  recs <- data.frame(
    id = c("a", "b", "c"),
    title = c("Coral reefs!", "coral   reefs.", "Deep sea"),
    abstract = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )
  out <- find_duplicates(recs, fuzzy = FALSE)
  expect_equal(out$duplicate_of, c(NA, "a", NA))
})

test_that("find_duplicates fuzzy match catches near-duplicates", {
  recs <- data.frame(
    id = c("a", "b", "c"),
    title = c("A study of coral reef restoration outcomes",
              "A study of coral reef restoration outcome",  # missing s
              "Deep sea mining"),
    abstract = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )
  out <- find_duplicates(recs, fuzzy = TRUE)
  expect_equal(out$duplicate_of[2], "a")
  expect_true(is.na(out$duplicate_of[3]))
})
