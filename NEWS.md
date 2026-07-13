# screenllm 0.1.0

Initial CRAN release.

## Ranking

* `rank_records()` scores a corpus with an ensemble of open-source LLMs
  served locally by Ollama. Every call is cached and keyed by
  `digest(list(criteria_hash, model, replicate, id, temperature))`, so an
  interrupted run resumes seamlessly.
* `default_ensemble()` returns the four-LLM mean ensemble reported as the
  universal ranker in Spillias et al. (2026) (`gemma3:27b`, `gpt-oss:20b`,
  `mistral-small3.2:24b`, `qwen3:30b-a3b-instruct-2507`; three replicates
  each at temperature 0.7).
* `default_ensemble_light()` returns a laptop-friendly alternative
  (`gemma3:4b`, `llama3.2:3b`, `qwen3:4b`, `mistral:7b`; ~10 GB total).
  Slightly less accurate than the paper ensemble but runs on 8 GB of RAM.
* `custom_ensemble()` accepts any Ollama-served model, replicate count,
  and aggregator (`mean`, `median`, `max`, `topk_mean`).

## Screening plan

* `plan_screening()` applies the SAFE stopping rule at the paper's
  advance-choosable default (target recall 0.95, minimum coverage 0.50,
  run length 50, spot-check n = 200).

## Criteria

* `define_criteria()` builds a scope-plus-inclusions object.
* `build_prompt()` renders the per-criterion partial-credit prompt used
  in the paper. Point weights (`W_POINTS`) and the three scale bands
  (`P20`, `P40`, `P80`) auto-scale with the number of criteria, so
  three-, four-, or five-criterion reviews all sum to 100.

## Shiny app

* `launch_app()` opens a seven-tab workflow (Setup / Corpus / Criteria /
  Rank / Plan / Screen / Report). All user artefacts persist under
  `tools::R_user_dir("screenllm", "data")` so projects survive across
  R sessions.
* `launch_screening_app()` provides the standalone incremental screening
  UI for teams working from an already-ranked list.

## Backends

* `backend_ollama()` (default): local HTTP client, no API keys, no
  network egress.
* `backend_mock()`: deterministic mock backend used in examples, tests,
  and vignettes so nothing depends on Ollama being installed.

## Setup helpers

* `install_prereqs()` walks a fresh machine through Ollama installation,
  daemon startup, and default-model pulls. Detects the OS and proposes
  an OS-appropriate install command (`brew` / `winget` / official
  install script), which the user must confirm before it runs. Safe to
  re-run; a no-op if everything is already in place.
* `check_setup()`, `ollama_health()`, and `pull_model()` are exposed for
  finer-grained control.

## Reporting

* `summarise_screening()` summarises ranked-vs-screened outcomes and
  reports SAFE-derived recall bounds.
* `audit_disagreements()` highlights strong LLM-vs-human disagreements
  for reviewer follow-up.
* `export_worksheet()` writes an Excel workbook for offline screening.
