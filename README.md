# screenllm

Turn-key LLM-assisted title/abstract screening for systematic reviews, on a
laptop.

`screenllm` operationalises the two workflow choices systematically
evaluated in **Spillias et al. (2026)**:

1. **Ranking** — score every record with a locally-served ensemble of
   open-source LLMs (default: four models × three replicates, averaged) and
   sort the corpus by aggregated score.
2. **Stopping** — apply the SAFE stopping rule at its advance-choosable
   default (minimum coverage 50 %, run length 50, spot-check n = 200) to
   identify the subset of records a human should screen.

The reviewer then screens the records above the stop point using either a
Shiny mini-app (interactive), an exported Excel worksheet (offline / team),
or a plain CSV. Everything below the stop point is treated as excluded.

## Setup

Two options depending on how much you have to install.

**Turn-key (recommended for a fresh machine):**

```r
install.packages("screenllm")           # once on CRAN
# or: remotes::install_github("s-spillias/screenllm")
library(screenllm)
install_prereqs()                        # detects OS, offers to install
                                         # Ollama, pulls the four paper models
launch_app()                             # opens the Shiny workflow in browser
```

`install_prereqs()` walks through:

1. Ollama binary check (`brew` on macOS, `winget` on Windows, official
   install script on Linux — asks first).
2. Daemon startup and a health check.
3. Pull each of the four default models (asks first — it's a big download).

**Laptop-friendly alternative (~10 GB total):**

```r
install_prereqs(models = c("gemma3:4b", "llama3.2:3b",
                           "qwen3:4b", "mistral:7b"))
```

Then choose the "Light (4 small models)" preset in the Setup tab of the
app, or from R use `default_ensemble_light()`. Slightly lower accuracy
than the paper ensemble, but runs comfortably on 8-16 GB of RAM.

**Manual install** (if you prefer to install Ollama yourself):

1. Install Ollama: <https://ollama.com/download>
2. Pull the four default models (~65 GB total):

   ```sh
   ollama pull gemma3:27b
   ollama pull gpt-oss:20b
   ollama pull mistral-small3.2:24b
   ollama pull qwen3:30b-a3b-instruct-2507
   ```

3. `check_setup()` in R to confirm all models are visible.

## The six-call workflow

```r
library(screenllm)

records  <- read_records("my_search_results.csv")
criteria <- define_criteria(
  scope = "Field-based coral reef restoration and performance",
  inclusions = c(
    "The study is conducted at a field-based coral reef restoration site.",
    "The study describes a project with an explicit restoration goal.",
    "The study describes an active restoration intervention.",
    "The study monitors at least one restoration-performance metric."
  )
)

ranked <- rank_records(records, criteria)      # overnight on a laptop
plan   <- plan_screening(ranked)                # SAFE at recommended default
launch_screening_app(plan, ranked, out_file = "decisions.csv")

decisions <- read_decisions("decisions.csv")
summarise_screening(ranked, decisions, plan = plan)
audit_disagreements(ranked, decisions)          # LLM–human disagreement audit
```

## Design choices

- **Caching is on by default.** Every LLM call is keyed by
  `digest(list(model, replicate, criteria_hash, record_id, temperature))`.
  An interrupted run resumes seamlessly.
- **Sequential model service.** Ollama serves one model at a time on a
  laptop. `rank_records()` iterates models × replicates × records; the
  progress bar shows where you are.
- **The four default models are pinned** in package data, but any model
  Ollama can serve is usable via `custom_ensemble(models = c("...", ...))`.
- **The prompt template is exactly the one from the paper**
  (`inst/prompts/standard.txt`) and can be inspected with `build_prompt()`.
- **No API keys, no cloud spend, no HPC** in the recommended path.
  Optional hosted-API backends (OpenAI, Anthropic, Google) are planned for
  v0.3.

## What it does *not* do

Following the paper's own scoping:

- No pilot workload prediction (the paper's RQ5 rule was not reliable
  enough to ship as a default).
- No criteria-revision assistant (a confounded intervention in the paper).
- No custom prompt-tuning (the tested prompt is the default; power users
  can supply their own).

## Status

**0.1.0 — first release.** Under active development alongside the paper
revision. API surface is small (six calls in the manual path, one call
via the Shiny app) and stable; internals may change.

## Related packages

`screenllm` targets locally-served open-weights ensembles with an
integrated stopping rule. If your use case is different, you may want:

- **[AIscreenR](https://cran.r-project.org/package=AIscreenR)** — cloud-hosted
  OpenAI GPT models, single-model screening, quality-assessment tooling.
  Recommended if you have an OpenAI API key and no data-privacy constraint
  ([Vembye et al. 2025, *Psychol. Methods*](https://doi.org/10.1037/met0000769)).
- **[revtools](https://cran.r-project.org/package=revtools)** — deduplication
  and interactive text-mining visualisations of the corpus (no
  classification).
- **[metagear](https://cran.r-project.org/package=metagear)** — GUI for
  fully-manual human screening; also handles PRISMA diagrams.
- **[ollamar](https://cran.r-project.org/package=ollamar)** /
  **[rollama](https://cran.r-project.org/package=rollama)** — generic
  Ollama HTTP wrappers (no screening-specific tooling).

## Cite

If you use `screenllm` in a review, please cite:

> Spillias, S. et al. (2026). *An open-source LLM-assisted screening
> workflow for environmental systematic reviews.* (in prep.)
