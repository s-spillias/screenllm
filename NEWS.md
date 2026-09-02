# screenllm 0.1.0

Initial CRAN release.

## Ranking

* `rank_records()` scores a corpus with an ensemble of open-source LLMs
  served locally by Ollama. Every per-call score is cached under the
  project's cache directory, keyed by
  `digest(list(criteria_hash, model, replicate, id, temperature))`. An
  interrupted run resumes from the cache on the next call. Cache writes are
  atomic (temp file + rename) so a crash mid-write can't leave a
  truncated file behind.
* Optional `on_score` callback for streaming partial-run progress into
  a UI without editing `rank_records()` itself.
* `default_ensemble()` returns the four-LLM mean ensemble reported as
  the universal ranker in Spillias et al. (2026) (`gemma3:27b`,
  `gpt-oss:20b`, `mistral-small3.2:24b`, `qwen3:30b-a3b-instruct-2507`;
  three replicates each at temperature 0.7).
* `default_ensemble_light()` returns a laptop-friendly alternative
  (`gemma3:4b`, `llama3.2:3b`, `qwen3:4b`, `mistral:7b`; ~10 GB total).
  Slightly less accurate than the paper ensemble but runs on 8 GB of
  RAM.
* `custom_ensemble()` accepts any Ollama-served model, replicate count,
  and aggregator (`mean`, `median`, `max`, `topk_mean`).
* `clear_cache()` invalidates cached scores by model or wholesale so a
  bad run can be re-done without discarding the good models' work.
* `estimate_runtime()` gives an order-of-magnitude wall-clock estimate
  scaled by model size and hardware profile (GPU / CPU / throttled).

## Screening plan

* `plan_screening()` applies the SAFE stopping rule at the paper's
  advance-choosable default (target recall 0.95, minimum coverage
  0.50, run length 50, spot-check n = 200) and returns per-gate
  diagnostics (which gate binds, where each gate would fire).

## Corpus

* `read_records()` accepts data frames, CSV, TSV, XLSX, and RIS
  exports from Zotero / EndNote / Mendeley / Web of Science.
* `find_duplicates()` flags DOI matches, normalised-title matches,
  and (optionally, when `stringdist` is installed) fuzzy-title
  near-duplicates.

## Criteria

* `define_criteria()` builds a scope-plus-inclusions object.
* `build_prompt()` renders the per-criterion partial-credit prompt
  used in the paper. Point weights and the three scale bands
  auto-scale with the number of criteria, so three-, four-, or
  five-criterion reviews all sum to 100.

## Shiny app

* `launch_app()` opens a seven-tab workflow (Setup / Corpus /
  Criteria / Rank / Plan / Screen / Report). All user artefacts
  persist under `tools::R_user_dir("screenllm", "data")` so
  projects survive across R sessions. Refuses to run as root and
  falls back to a URL-only launch on headless (no-DISPLAY)
  systems.
* Streaming Rank tab: per-model progress bars and a live scores
  table that grows as each model completes.
* Report tab: click a strong LLM-vs-human disagreement to review
  it and flip your decision.
* Setup tab includes an in-app **Install Ollama** button that
  detects the OS package manager and offers a copy-pasteable
  install command (macOS + Windows can also run it directly in a
  background process).
* `launch_screening_app()` provides the standalone incremental
  screening UI for teams working from an already-ranked list.

## Backends

* `backend_ollama()` (default): local HTTP client, no API keys, no
  network egress. Fails fast on HTTP 4xx (bad model tag / auth) and
  surfaces the actual error message. Handles reasoning-model
  families (`gpt-oss`, `deepseek-r1`, `qwen3-thinking`,
  `phi4-reasoning`) that misbehave under Ollama's grammar-
  constrained JSON mode. Suppresses chain-of-thought via
  `think = FALSE`; recovers JSON from mixed text via a balanced-
  braces fallback parser.
* `backend_mock()`: deterministic mock backend used in examples,
  tests, and vignettes so nothing depends on Ollama being
  installed.

## Setup helpers

* `install_prereqs(preset = "light" | "paper" | "none")` walks a
  fresh machine through Ollama installation, daemon startup, and
  model pulls. Detects the OS and proposes an OS-appropriate
  install command (`brew` / `winget` / official install script)
  which the user must confirm before it runs. Safe to re-run; a
  no-op if everything is already in place.
* `check_setup()`, `ollama_health()`, `pull_model()`,
  `ollama_catalog()` exposed for finer-grained control.
* `detect_gpu()` + `gpu_status()`: hardware detection (Apple
  Silicon / NVIDIA / AMD) plus live clock / VRAM / power query,
  including a "throttled" flag that catches the laptop-on-battery
  case where a dGPU reports 99 % utilisation but runs at idle
  clocks.

## Async workers

* `start_rank_job()`, `rank_job_status()`, `rank_job_cancel()`
  spawn ranking jobs in a background R process (via `callr`) so
  Shiny stays responsive; the worker writes throttled progress
  and per-call scores back through a per-project file.
* `start_pull_job()` / `pull_job_status()` / `pull_job_cancel()`
  do the same for Ollama model downloads.

## Reporting

* `summarise_screening()` summarises ranked-vs-screened outcomes
  and reports SAFE-derived recall bounds.
* `audit_disagreements()` highlights strong LLM-vs-human
  disagreements for reviewer follow-up. Tolerates records that
  carry a pre-existing `human_decision` column (baked-in ground
  truth) by stripping decision columns before the join.
* `export_worksheet()` writes an Excel workbook for offline
  screening.
* `export_report()` renders a self-contained HTML report (open in
  a browser and print-to-PDF to archive).