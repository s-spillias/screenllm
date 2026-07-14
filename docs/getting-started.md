# Getting started with `screenllm` (alpha)

**Time commitment:** about 30 minutes for install, 20 minutes to try. You can
walk away during the model download (~10 GB, ~15-30 minutes on typical
broadband).

**What you need on your computer:**
- Windows 10+, macOS 12+, or Ubuntu 22.04+
- At least 16 GB of RAM (8 GB will work but is slow)
- At least 15 GB of free disk space
- Internet connection for the initial download

**What you don't need:** any programming experience.

---

## 1. Install R

R is a free statistical programming language. Download the installer for
your operating system from <https://cloud.r-project.org>, run it, and
click through with the defaults.

Windows: pick the "base" download and run the `.exe`.
macOS: pick the `.pkg` for your chip (Apple Silicon or Intel).
Linux: your package manager will have it — `sudo apt install r-base` on
Ubuntu/Debian.

## 2. Install RStudio Desktop

RStudio is a friendly window around R. Download the free "RStudio
Desktop" from <https://posit.co/download/rstudio-desktop/> and install.

Open RStudio. You'll see a `>` prompt in the bottom-left panel — that's
where you type R commands.

## 3. Install `screenllm`

Copy-paste this line into the R console (bottom-left panel) and press
Enter:

```r
install.packages("remotes"); remotes::install_github("s-spillias/screenllm")
```

It'll print a lot of "installing" messages for a couple of minutes. If it
asks whether to update other packages, answer `n` (no updates) unless
you know what you're doing.

When it finishes, load the package:

```r
library(screenllm)
```

## 4. Install Ollama and download the models

`screenllm` runs its language models locally through a small program
called Ollama. From R:

```r
install_prereqs(preset = "light")
```

This will:

1. Detect your operating system.
2. Ask before running any install command (say `1` = yes).
3. On macOS/Windows, offer to install Ollama via Homebrew/winget. On
   Linux, offer to run the official Ollama install script.
4. Wait for Ollama to start.
5. Ask before pulling four small language models (~10 GB total).
6. Show a progress bar for each model.

**If you have a workstation with 32 GB+ of RAM and 70 GB of free disk,**
use `preset = "paper"` instead — this gives you the exact four models
we tested in the paper (~65 GB).

**If you'd rather install Ollama yourself,** grab it from
<https://ollama.com/download>, then re-run `install_prereqs(preset = "light")`
in R.

## 5. Launch the app

```r
launch_app()
```

A browser window opens with an eight-tab workflow.

## 6. Try the toy dataset

Go through the tabs in order. There's a demo corpus baked in so you
don't need to bring anything.

**Tab 1 - Setup**
- Type a project name (e.g. `test-drive`) in the "Create a new project"
  field, click "Create / select".
- On the right, confirm "Ollama reachable" (green badge).
- Under "Choose ensemble", pick "Light (4 small models)".
- Click "Save ensemble config".

**Tab 2 - Corpus**
- Click "Or load the toy CBFM corpus". You'll see 40 records appear on
  the right. These are drawn from the *Community-Based Fisheries
  Management* review used in the paper.

**Tab 3 - Criteria**
- The tab starts with four blank criteria boxes. Fill them in with (or
  paste all four at once):
  1. `It is possible that the study includes a case study from a Pacific Island country (e.g. Fiji, Solomon Islands, Vanuatu, Papua New Guinea, Samoa, Tonga, or similar).`
  2. `It is possible that the study discusses fisheries and/or marine resource management.`
  3. `It is possible that the study discusses a community-based approach.`
- Remove the fourth (blank) criterion with the "- Remove last" button.
- Watch the right-hand "Rendered LLM prompt" panel update as you type.
- Notice the small "Auto-saved..." indicator appears near the button.
- Click "Save criteria" for a hard commit.

**Tab 4 - Pilot**
- Click "Run pilot". This runs a small sample (default 20 records at
  one replicate) so you can see what the LLM is saying before
  committing to the full run.
- Read a few of the printed justifications. Do they match how you'd
  interpret the criteria?

**Tab 5 - Rank**
- You'll see an estimated wall-clock time near the top.
- Click "Start ranking". The progress bar updates as records are
  scored. On the "light" preset with 40 records this takes ~10-20
  minutes.
- When it finishes, the top 25 records appear on the right, sorted by
  score.

**Tab 6 - Plan**
- Slide the SAFE parameters. Notice how the plotted stopping point
  moves.
- Leave the defaults for now (target recall 0.95, minimum coverage
  0.50, run length 50).
- Click "Save plan".

**Tab 7 - Screen**
- Screen a few records: read the title and abstract, click Accept or
  Reject. The next record loads automatically.
- You don't have to finish — screen 5-10 to get the feel.

**Tab 8 - Report**
- Click "Download HTML report" to get a self-contained report of your
  test session. Open it in a browser. Use "Print > Save as PDF" if you
  want an archive.

## 7. Send us your feedback

You're done. Please spend three minutes filling out the feedback form:

<https://github.com/s-spillias/screenllm/issues/new/choose>

Pick "Alpha tester feedback." If something broke, pick "Bug report"
instead.

If GitHub is blocked at your institution, email Scott
(scott.spillias@csiro.au) with:

- Your operating system and RAM
- Which step (from the numbered list above) failed
- What the error message was, verbatim
- How long you spent on the whole exercise
- Would you use this on a real review? Why or why not?

## Common questions

**Q: Do I need a GPU?** No. It's faster with one, but the "light"
preset runs at usable speeds on any modern laptop CPU.

**Q: Does anything leave my computer?** No. Ollama runs the LLMs
locally. `screenllm` never phones home. Even the report is a plain
HTML file on your disk.

**Q: What if I have a corpus I want to try?** Upload it on Tab 2. CSV,
Excel, and RIS (Zotero/EndNote) are all supported. It needs at least a
`title` and `abstract` column.

**Q: What if the models are too big for my machine?** Try even
smaller: `install_prereqs(models = c("gemma3:1b", "llama3.2:1b"))`.
Accuracy drops but it'll run on 4 GB of RAM.

**Q: Can I use this for a real review?** Yes, on the understanding
that it's alpha software. The methodology is peer-reviewed (Spillias et
al. 2026); the package around it is still being refined based on
feedback like yours.

**Q: How do I close everything down?** Close the browser tab, then in R
run `q()`. Ollama keeps running in the background — quit it via the
tray icon (macOS/Windows) or `pkill ollama` (Linux) if you want to
reclaim RAM.
