#' Detect a usable GPU for Ollama inference
#'
#' Probes the system for a GPU that Ollama can offload models to. Does
#' not require Ollama itself; the checks are OS-level so users can
#' verify their hardware is set up before pulling any models.
#'
#' Checks in order:
#' 1. macOS with Apple Silicon (Metal is used automatically).
#' 2. `nvidia-smi` returning success -- an NVIDIA GPU with a working
#'    CUDA driver.
#' 3. `rocm-smi` returning success -- an AMD GPU with a working ROCm
#'    driver.
#'
#' Ollama itself decides how much of a model to offload to the GPU
#' (based on free VRAM); this function only reports whether the
#' underlying hardware is available. To verify Ollama is actually
#' using the GPU during a run, look at `ollama ps` in a terminal: the
#' `SIZE (GPU)` column shows how many bytes are on the GPU.
#'
#' @return A list with:
#'   * `available`: logical
#'   * `kind`: one of `"apple"`, `"nvidia"`, `"amd"`, or `"none"`
#'   * `detail`: a short human-readable string (e.g. the GPU model
#'     name, or the reason detection failed)
#' @export
#' @examples
#' info <- detect_gpu()
#' info$available
#' info$kind
detect_gpu <- function() {
  sysname <- Sys.info()[["sysname"]]

  # macOS: Apple Silicon (M1/M2/M3) has a unified-memory GPU that
  # Ollama uses via Metal automatically.
  if (identical(sysname, "Darwin")) {
    cpu <- try(suppressWarnings(system2("sysctl",
                                        "-n machdep.cpu.brand_string",
                                        stdout = TRUE, stderr = FALSE)),
                silent = TRUE)
    if (!inherits(cpu, "try-error") && length(cpu) == 1L &&
          grepl("Apple", cpu, fixed = TRUE)) {
      return(list(available = TRUE, kind = "apple",
                  detail = paste("Apple Silicon (", cpu, ")", sep = "")))
    }
    return(list(available = FALSE, kind = "none",
                detail = "Intel macOS; Ollama runs on CPU."))
  }

  # NVIDIA (Linux or Windows): nvidia-smi is installed with the CUDA
  # driver stack. Its exit code is 0 if a GPU is present and usable.
  if (nzchar(Sys.which("nvidia-smi"))) {
    out <- try(
      suppressWarnings(system2("nvidia-smi",
                                "--query-gpu=name --format=csv,noheader",
                                stdout = TRUE, stderr = FALSE)),
      silent = TRUE
    )
    if (!inherits(out, "try-error") && length(out) >= 1L && nzchar(out[1])) {
      return(list(available = TRUE, kind = "nvidia",
                  detail = paste("NVIDIA:", out[1])))
    }
  }

  # AMD: rocm-smi ships with ROCm.
  if (nzchar(Sys.which("rocm-smi"))) {
    status <- suppressWarnings(system2("rocm-smi",
                                        stdout = FALSE, stderr = FALSE))
    if (identical(status, 0L)) {
      return(list(available = TRUE, kind = "amd",
                  detail = "AMD GPU (ROCm)."))
    }
  }

  list(available = FALSE, kind = "none",
       detail = "No GPU detected; Ollama will run on CPU.")
}

#' Live NVIDIA GPU status snapshot
#'
#' Queries `nvidia-smi` for current graphics clock, memory clock,
#' memory used, power draw and utilisation. Used to catch the
#' "throttled" state where a laptop dGPU reports 99% utilisation but
#' is running its cores at idle clock speeds (typically because the
#' laptop is on battery, in a power-saver profile, or persistence
#' mode is off). In that state throughput drops ~10x and each LLM
#' call takes 20-30s instead of 2-3s.
#'
#' Returns `available = FALSE` on non-NVIDIA systems; the Metal /
#' ROCm equivalents don't expose live clock data through a portable
#' CLI.
#'
#' @param throttled_clock_mhz Threshold below which the graphics
#'   clock is considered throttled. Defaults to 800 MHz (a modern
#'   dGPU under real inference load should be 1500-2500 MHz).
#' @param loaded_mib Threshold at which VRAM is considered "in use
#'   by a model" (avoids false-positive throttling at rest, when
#'   idle clocks are expected and normal). Defaults to 500 MiB.
#' @return A list with `available`, `graphics_clock_mhz`,
#'   `memory_clock_mhz`, `memory_used_mib`, `memory_total_mib`,
#'   `power_draw_w`, `utilisation_pct`, `throttled` (logical), and
#'   `hint` (short human-readable action if throttled).
#' @export
#' @examples
#' \dontrun{
#' s <- gpu_status()
#' if (isTRUE(s$throttled)) message(s$hint)
#' }
gpu_status <- function(throttled_clock_mhz = 800,
                       loaded_mib = 500) {
  fail <- list(available = FALSE, throttled = NA,
               graphics_clock_mhz = NA_real_,
               memory_clock_mhz = NA_real_,
               memory_used_mib = NA_real_,
               memory_total_mib = NA_real_,
               power_draw_w = NA_real_,
               utilisation_pct = NA_real_,
               hint = "")
  if (!nzchar(Sys.which("nvidia-smi"))) return(fail)
  fields <- c("clocks.current.graphics", "clocks.current.memory",
              "memory.used", "memory.total",
              "power.draw", "utilization.gpu")
  out <- tryCatch(
    suppressWarnings(system2(
      "nvidia-smi",
      c(sprintf("--query-gpu=%s", paste(fields, collapse = ",")),
        "--format=csv,noheader,nounits"),
      stdout = TRUE, stderr = FALSE
    )),
    error = function(e) NULL
  )
  if (is.null(out) || length(out) == 0L || !nzchar(out[1])) return(fail)
  parts <- strsplit(out[1], ",", fixed = TRUE)[[1]]
  vals <- suppressWarnings(as.numeric(trimws(parts)))
  if (length(vals) < 6L || any(is.na(vals[1:4]))) return(fail)
  loaded <- vals[3] >= loaded_mib
  throttled <- isTRUE(loaded && vals[1] < throttled_clock_mhz)
  hint <- if (throttled) {
    paste(
      "GPU clock is stuck at low speed while a model is loaded.",
      "Usually caused by AC/battery state or a power-saver profile.",
      "Try: (1) plug in AC power, (2) set OS power profile to Performance,",
      "(3) on Linux, sudo nvidia-persistenced && sudo nvidia-smi -pm 1."
    )
  } else ""
  list(
    available = TRUE,
    graphics_clock_mhz = vals[1],
    memory_clock_mhz = vals[2],
    memory_used_mib = vals[3],
    memory_total_mib = vals[4],
    power_draw_w = vals[5],
    utilisation_pct = vals[6],
    throttled = throttled,
    hint = hint
  )
}
