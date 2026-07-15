# Troubleshooting

Common problems and their fixes. If none of these help, open an issue or
email Scott.

## Installation

### `install.packages("remotes")` fails

You may be behind a corporate firewall that blocks CRAN. Try:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
```

If that also fails, contact your IT team to whitelist `cloud.r-project.org`
and `github.com`.

### `remotes::install_github(...)` asks for a GitHub token

The repo is public, so no token is needed. This prompt appears if
`remotes` cached an outdated auth setting. Skip it (press Enter with
no input); the install continues without.

### `install_prereqs()` says "Ollama not found"

On macOS: check that Homebrew is installed (`brew --version` in a
terminal). If not, install it from <https://brew.sh> first, or install
Ollama from <https://ollama.com/download>.

On Windows: winget comes with recent Windows 10/11. If it's missing,
install Ollama from <https://ollama.com/download>.

On Linux: `curl` needs to be installed (`sudo apt install curl`).

Once Ollama is installed by any means, re-run
`install_prereqs(preset = "light")`.

### `install_prereqs()` says "Ollama daemon did not come up within 60s"

Open a fresh terminal window and run `ollama serve`. Leave it open.
Then, in R, run `install_prereqs(preset = "light")` again.

### Model pull is stuck / very slow

Downloads can take 15-30 minutes on typical broadband, sometimes an
hour on hotel wifi. The progress bar updates in chunks so it may look
frozen for a minute or two between steps.

If it truly hangs (no progress for 10+ minutes), interrupt with Escape
in the R console and re-run. Ollama resumes partial downloads.

## Using the app

### `launch_app()` says a package is missing

Install the missing suggests:

```r
install.packages(c("shiny", "bslib", "DT", "callr", "writexl"))
```

Then try `launch_app()` again.

### The Ranking tab estimate says "14 hours"

That's the light-model, no-GPU estimate for a corpus of your size.
Check that:

- The "Choose ensemble" radio on Setup is on **Light**, not Paper.
- Your corpus has as many records as you expect (Corpus tab).
- Your machine has a GPU. Ollama uses it automatically if present, which
  cuts the estimate 5-10x.

If you're on the Paper preset and the corpus is small (< 100 records),
even the paper models finish in an hour or two.

### The pilot output looks nonsensical

Pilot uses one replicate per model instead of three. That extra noise
is normal — the goal is to sanity-check that the LLM understands your
criteria at all, not to reproduce the full-run accuracy.

If the LLM is scoring records opposite to your intuition:

- Are your criteria phrased as *what should be true of an included
  study*? If any criterion reads like an exclusion ("The study is not
  in a marine environment"), invert it.
- Are the criteria too broad? "The study is about marine ecology" will
  flag almost everything.
- Are the criteria too narrow? "The study uses a randomised crossover
  design on North Atlantic cod at three-year intervals" will flag
  almost nothing.

Edit and re-run pilot. It's fast.

### The ranking job stops halfway

If the Rank tab progress bar freezes:

- The status line says the last-updated timestamp. If it's more than
  60 seconds old, the worker probably crashed.
- Look in your project directory: `data_root()` in R prints the path.
  Inside the project folder there's a `rank_stderr.log` — the last few
  lines usually explain what happened.
- Common causes: Ollama ran out of RAM (close other applications),
  Ollama daemon was killed, disk filled up.

You can safely re-run — everything already scored is cached and won't
be redone.

### The Screen tab shows "No records to screen"

Two possibilities:

1. You skipped the Rank or Plan tab. Go back through them.
2. Your SAFE plan set the stop point at or before the highest-scored
   record. Try lower "minimum coverage" or shorter "run length" on the
   Plan tab.

### The Report tab can't render HTML

You need `rmarkdown`:

```r
install.packages("rmarkdown")
```

Restart R (`Ctrl+Shift+F10` in RStudio) and try again.

## GPU is not being used

Ollama automatically uses a GPU if one is available and the drivers
are installed. If the Setup tab's GPU badge shows "none" or your
runtime estimate looks like a CPU-only number (many hours instead of
tens of minutes), check the following.

### macOS

- Apple Silicon (M1/M2/M3/M4) is used automatically via Metal. No
  setup needed. Intel Macs run on CPU.
- If you're on Apple Silicon and the badge still shows "none":
  update Ollama (`brew upgrade ollama`) and restart it.

### Linux / Windows with NVIDIA

- Install a recent NVIDIA driver (515+ recommended).
- Confirm the driver is loaded by running `nvidia-smi` in a
  terminal. If that command isn't found or errors, the driver isn't
  installed correctly.
- Confirm Ollama sees the GPU: `ollama ps` after loading a model
  should show a non-zero `SIZE (GPU)` column.
- If `nvidia-smi` works but `ollama ps` shows 0 GPU usage, restart
  the Ollama server (`ollama serve` in a fresh terminal, or restart
  the tray app).

### Linux with AMD

- Install ROCm (see <https://rocm.docs.amd.com>). Not all AMD GPUs
  are supported.
- Confirm `rocm-smi` works.
- Set the `HSA_OVERRIDE_GFX_VERSION` env var if your GPU model
  needs it (check Ollama's ROCm docs).

### GPU is throttled (99 % utilisation but the run is slow)

Symptom: the Setup tab GPU badge turns amber and reads
"nvidia (throttled)", or a warning banner appears on the Rank tab
during a running job. Effective throughput drops ~10x (each LLM
call takes 20-30 s instead of 2-3 s) even though `nvidia-smi`
reports 99 % utilisation.

Cause: the dGPU is running its cores at idle clock speeds (a few
hundred MHz instead of ~2 GHz) while it should be under load. On
laptops this almost always means:

- **On battery power.** Discrete GPUs like the RTX 3500 Ada aggressively throttle when the AC adapter is unplugged.
- **OS is in a power-saver profile.** Windows "Battery Saver" or "Balanced", macOS "Low Power Mode", Linux `powersave` governor.
- **Persistence mode is off.** The driver unloads between calls and reloads in a low-power P-state.

Fixes, in order:

1. **Plug in the AC adapter.** Wait ~30 s and re-check the badge.
2. **Set the OS power profile to Performance** (Windows Settings → Power, macOS Settings → Battery, Linux `cpupower frequency-set -g performance` or your DE's power menu).
3. **On Linux, enable NVIDIA persistence:**
   ```sh
   sudo nvidia-persistenced --user root
   sudo nvidia-smi -pm 1
   ```
4. **Verify with a live query while a call is running:**
   ```sh
   nvidia-smi --query-gpu=clocks.current.graphics,power.draw --format=csv
   ```
   Under real load a modern dGPU should report `1500-2500 MHz` and `40-100 W`. If it still reads ~200 MHz and ~10 W after the fixes above, check your vendor's power management app (Dell Power Manager, Lenovo Vantage, ThinkPad Power Manager, etc.) for a discrete-GPU cap.
5. **Some laptops have an Optimus / Prime mode** that keeps the dGPU off entirely. Toggle to "dGPU on" or "Discrete Graphics" via the vendor app or BIOS.

### Verifying at runtime

While a `screenllm` pilot or ranking is in progress, open a terminal
and run:

```sh
ollama ps
```

The row for each running model has a `SIZE (GPU)` column showing how
many bytes are loaded to VRAM. If it's the same as the total model
size, everything is on GPU; if it's a fraction, Ollama split it
between GPU and CPU because the model was too big for available
VRAM; if it's zero, Ollama is running on CPU.

### Not enough VRAM

Ollama gracefully falls back to CPU if a model doesn't fit. Options:
- Switch to the light preset (~10 GB total, models fit in 8 GB VRAM).
- Pull smaller variants: `mistral:7b-instruct-q4_K_S` uses ~4 GB
  VRAM vs `mistral:7b`'s ~5 GB, at a small accuracy cost.
- Close other GPU-using applications (browsers with hardware
  acceleration, video calls, other ML processes).

## "cannot open display" / "no authorization" when I launch_app()

You're almost certainly running R with `sudo` (either `sudo R`,
`sudo Rscript`, or invoked from a root shell). Don't do this. `sudo`
was only ever meant for the one-off Ollama install command inside
`install_prereqs()`; the R session itself should run as your normal
user.

Running R as root causes:

- **X11 authorisation failure.** The X server's auth cookie belongs
  to your normal user; the root session can't attach to your
  display, so `browseURL()` errors with "cannot open display" or
  "no authorization is specified".
- **Wrong project directory.** `tools::R_user_dir()` returns
  `/root/.local/share/R/screenllm/...` in the sudo session. Any
  projects you save there won't be findable when you go back to
  your normal account.

**Fix:**

```
exit                              # leave the root shell
R                                 # start R as your normal user
> library(screenllm)
> install_prereqs(preset = "light")   # prompts sudo only for the install line
> launch_app()
```

`screenllm` now refuses to `launch_app()` from a root session and
prints this explanation, so you shouldn't hit the raw X11 error
again.

## When you're truly stuck

Open a GitHub issue at
<https://github.com/s-spillias/screenllm/issues/new/choose> and pick
"Bug report." Include:

- Your operating system
- The output of `sessionInfo()` (paste it into the issue)
- The exact command you ran and the error message

Or email Scott directly (scott.spillias@csiro.au) with the same info.
