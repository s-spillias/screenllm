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

test_that("report functions ignore any pre-existing human_decision on ranked", {
  # The toy CBFM corpus ships with ground-truth `human_decision`
  # baked in; a user's uploaded CSV might too. dplyr::left_join
  # then produces `human_decision.x` / `human_decision.y` and both
  # summarise_screening and audit_disagreements used to crash on
  # the missing plain-named column. Regression: strip decision
  # columns from the ranked side before the join and use only the
  # values from the human's real Screen-tab decisions.
  ranked <- data.frame(
    id = paste0("r", 1:4),
    universal_best_score = c(90, 80, 20, 10),
    rank = 1:4,
    # ground-truth baked in
    human_decision = c("Accept", "Reject", "Accept", "Reject"),
    stringsAsFactors = FALSE
  )
  decisions <- data.frame(
    id = c("r1", "r2"),
    human_decision = c("Reject", "Accept"),  # user's real decisions
    note = c("", ""),
    timestamp = c("t1", "t2"),
    stringsAsFactors = FALSE
  )
  rep <- summarise_screening(ranked, decisions)
  # Real user decisions have 2 rows: 1 Accept (r2), 1 Reject (r1)
  expect_equal(rep$n_screened, 2L)
  expect_equal(rep$n_accepts, 1L)

  aud <- audit_disagreements(ranked, decisions)
  # r1: LLM score 90 (>=70) + human Reject -> Strong FP
  # r2: LLM score 80 (>=70) + human Accept -> not a disagreement
  expect_equal(nrow(aud), 1L)
  expect_equal(aud$id, "r1")
  expect_match(aud$disagreement, "Strong FP")
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
