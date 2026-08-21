# screenllm quickstart

This guide walks through the six-call workflow `screenllm` implements.
Everything below runs on a laptop against a locally-served Ollama
backend. If you don't yet have Ollama installed and the four default
models pulled, `check_setup()` will tell you what's missing.

## 1. Verify setup

```r
library(screenllm)
check_setup()
```

## 2. Load a corpus

Any tibble with `title` and `abstract` columns works; `read_records()`
also accepts CSV, XLSX, or RIS paths and normalises common column-name
variations (Scopus, Web of Science, EndNote). The package ships with a
40-record toy dataset drawn from the *Community-Based Fisheries
Management* (CBFM) review used in the manuscript, which we use here
for demonstration.

```r
library(screenllm)
toy_path <- system.file("extdata", "toy_cbfm.csv", package = "screenllm")
records <- read_records(toy_path)
head(records[, c("id", "title")])
```

## 3. Define the inclusion criteria

```r
criteria <- define_criteria(
  scope = "Articles potentially relevant to community-based fisheries management (CBFM) in Pacific Island contexts.",
  inclusions = c(
    "It is possible that the study includes a case study from a Pacific Island country (e.g. Fiji, Solomon Islands, Vanuatu, Papua New Guinea, Samoa, Tonga, or similar).",
    "It is possible that the study discusses fisheries and/or marine resource management.",
    "It is possible that the study discusses a community-based approach."
  )
)
print(criteria)
```

## 4. Rank the corpus

Real screening uses `default_ensemble()`, which talks to Ollama. For this
guide we use `backend_mock()` so the code runs without Ollama.

```r
mock_ensemble <- custom_ensemble(
  models = c("gemma3:27b", "gpt-oss:20b"),
  backend = backend_mock()
)
ranked <- rank_records(records, criteria, ensemble = mock_ensemble, verbose = FALSE)
head(ranked[, c("id", "title", "universal_best_score", "rank")])
```

For a real run, swap the mock for the default:

```r
ranked <- rank_records(records, criteria, ensemble = default_ensemble())
```

## 5. Plan the human screening set

```r
plan <- plan_screening(ranked)
plan
```

The `to_screen` element is the tibble of records the reviewer should
inspect. Everything below the stopping point is treated as excluded.

## 6. Screen and report

Interactive screening via the Shiny app:

```r
launch_screening_app(plan, ranked, out_file = "screening_decisions.csv")
```

Offline screening (spreadsheet round-trip):

```r
export_worksheet(plan, path = "to_screen.xlsx")
# Reviewer fills in the human_decision column and saves as
# 'to_screen_completed.xlsx'.
decisions <- read_decisions("to_screen_completed.xlsx")
```

Summarise the run and surface any strong LLM-human disagreements as a
manual audit queue:

```r
report <- summarise_screening(ranked, decisions, plan = plan)
print(report)

disagreements <- audit_disagreements(ranked, decisions)
disagreements
```

## 7. Do's and don'ts for deployment

A short checklist distilled from the ten-review benchmark. The paper's
Methods and Supplement give the evidence behind each point.

**Do**

- **Ensemble, then average.** Run three or four comparable open-weight
  models and take the *mean* of their relevance scores. The specific
  four-model set is less important than using more than one model;
  returns saturate by three or four, so a fifth rarely helps.
- **One run per model is usually enough.** At the default temperature of
  0.1 a single replicate recovers almost all of the ranking and stopping
  performance. Add replicates only when you want a variance estimate or
  insurance against an occasional degenerate run. Raise the sampling
  temperature only if you understand the trade-off; the package warns you,
  because the reported guarantees are conditional on temperature 0.1.
- **Keep the per-criterion prompt.** The default prompt scores each
  inclusion criterion and sums them. This partial-credit gradient is
  what makes the ranking work.
- **Stop with the advance-fixed SAFE default** (`min_coverage = 0.50`,
  `run_length = 50`). It is the only setting that behaves in real
  practice, where you must commit before seeing labels; it reached
  ≥ 95% recall on all ten benchmark reviews.
- **Read miss-rate, not just work saved.** A stopping rule that saves
  effort but misses relevant records is not a good trade. Report both.
- **Use the ensemble as triage.** A human still screens everything above
  the stopping point; the ranking decides reading *order* and *where to
  stop*, not the final include/exclude decision.

**Don't**

- **Don't tighten SAFE blind.** Choosing a lower `min_coverage` or a
  shorter `run_length` without labels is the main mis-specification
  failure mode: the rule can stop early and fall short of 95% recall on
  harder reviews. If you want to tune, label a small subset first;
  otherwise keep the default. (See the SAFE-robustness analysis in the
  paper's Supplement.)
- **Don't reuse a fixed score cutoff across reviews.** The score
  threshold that hits 95% recall differs several-fold between reviews, so
  a cutoff tuned on one review will over-screen or miss the target on the
  next. Stop on rank position via SAFE instead.
- **Don't treat the ensemble as a standalone classifier.** At a single
  score cutoff its agreement with human decisions is well below what a
  second human reviewer gives; it is a ranker, not a replacement screener.
- **Don't collapse the prompt to a yes/no.** A binary include/exclude
  prompt measurably lowered ranking quality on every review tested.
- **Don't assume bigger is better.** Adding models past three or four
  gave no reliable gain in the benchmark.
