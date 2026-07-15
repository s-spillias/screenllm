test_that("summarise_screening counts decisions attached via bind_rows over a legacy df", {
  # Regression for the Screen-tab bug: an empty 2-column
  # placeholder + a 4-column new_row used to blow up in base
  # rbind(), and the Shiny observer silently swallowed the error,
  # so no decisions ever landed. If we switch to dplyr::bind_rows
  # the merged data has NA-filled columns but still contains the
  # decision.
  ranked <- data.frame(
    id = paste0("r", 1:5),
    universal_best_score = c(90, 80, 70, 40, 10),
    rank = 1:5
  )
  # Simulate the legacy state$decisions layout (2 columns) plus a
  # new_row (4 columns). This is exactly what the module used to
  # produce before the fix.
  legacy <- data.frame(id = character(), human_decision = character(),
                        stringsAsFactors = FALSE)
  new_row <- data.frame(
    id = "r1", human_decision = "Accept",
    note = "", timestamp = "2026-07-15T10:00:00+1000",
    stringsAsFactors = FALSE
  )
  merged <- dplyr::bind_rows(legacy, new_row)
  expect_equal(nrow(merged), 1L)
  expect_equal(merged$human_decision, "Accept")

  rep <- summarise_screening(ranked, merged)
  expect_equal(rep$n_screened, 1L)
  expect_equal(rep$n_accepts, 1L)
})

test_that("normalise_decisions_shape adds missing columns without dropping data", {
  # NULL -> empty 4-col tibble
  z <- screenllm:::normalise_decisions_shape(NULL)
  expect_equal(nrow(z), 0L)
  expect_setequal(names(z), c("id", "human_decision", "note", "timestamp"))

  # 1-column legacy df -> 4-col df with NA fills
  legacy <- data.frame(id = c("r1", "r2"), stringsAsFactors = FALSE)
  fixed <- screenllm:::normalise_decisions_shape(legacy)
  expect_equal(nrow(fixed), 2L)
  expect_setequal(names(fixed), c("id", "human_decision", "note", "timestamp"))
  expect_equal(fixed$id, c("r1", "r2"))
  expect_true(all(is.na(fixed$human_decision)))
})

test_that("summarise_screening + audit_disagreements survive a legacy decisions df", {
  # This is the case that broke in production: a decisions df with
  # only the `id` column. The Report tab used to render Shiny's
  # default "Error: [object Object]" toast and stop working.
  ranked <- data.frame(
    id = paste0("r", 1:5),
    universal_best_score = c(90, 80, 70, 40, 10),
    rank = 1:5
  )
  broken <- data.frame(id = c("r1", "r3"), stringsAsFactors = FALSE)
  # Should return zero screened / zero accepts, not error.
  rep <- summarise_screening(ranked, broken)
  expect_equal(rep$n_screened, 0L)
  expect_equal(rep$n_accepts, 0L)
  # audit likewise returns an empty tibble, not an error.
  aud <- audit_disagreements(ranked, broken)
  expect_s3_class(aud, "data.frame")
  expect_equal(nrow(aud), 0L)
})
