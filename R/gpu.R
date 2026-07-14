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
