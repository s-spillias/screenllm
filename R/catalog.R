# Curated Ollama model catalog. Intentionally not the full library
# (~200 models on ollama.com/library) -- just the popular
# instruction-tuned models people are most likely to pick for
# screening. Sizes are approximate Q4_K_M quantised file sizes; treat
# them as VRAM guidelines, not exact figures.
#
# When Ollama publishes new families this list will drift; that's the
# expected lifecycle. Users who need a tag not in this list can still
# type it into the pull box - selectize is configured with create =
# TRUE. Update this list periodically (or refresh from
# ollama.com/library) as the ecosystem changes.

.OLLAMA_CATALOG <- tibble::tribble(
  ~tag,                                ~family,          ~size_gb, ~description,
  # Paper-default ensemble
  "gemma3:27b",                        "gemma3",              16, "Google Gemma 3 27B (paper default)",
  "gpt-oss:20b",                       "gpt-oss",             12, "OpenAI gpt-oss 20B (paper default)",
  "mistral-small3.2:24b",              "mistral",             14, "Mistral Small 3.2 24B (paper default)",
  "qwen3:30b-a3b-instruct-2507",       "qwen3",               18, "Qwen3 30B A3B instruct (paper default)",
  # Light preset
  "gemma3:4b",                         "gemma3",               3, "Google Gemma 3 4B (light preset)",
  "llama3.2:3b",                       "llama",                2, "Meta Llama 3.2 3B (light preset)",
  "qwen3:4b",                          "qwen3",                3, "Alibaba Qwen3 4B (light preset)",
  "mistral:7b",                        "mistral",              5, "Mistral 7B (light preset)",
  # Other common variants (small to large)
  "gemma3:1b",                         "gemma3",               1, "Google Gemma 3 1B - tiny, fits in <4 GB RAM",
  "gemma3:12b",                        "gemma3",               8, "Google Gemma 3 12B",
  "llama3.2:1b",                       "llama",                1, "Meta Llama 3.2 1B - tiny",
  "llama3.1:8b",                       "llama",                5, "Meta Llama 3.1 8B",
  "llama3.1:70b",                      "llama",               40, "Meta Llama 3.1 70B - needs a lot of VRAM",
  "qwen3:1.5b",                        "qwen3",                1, "Alibaba Qwen3 1.5B",
  "qwen3:8b",                          "qwen3",                5, "Alibaba Qwen3 8B",
  "qwen3:14b",                         "qwen3",                8, "Alibaba Qwen3 14B",
  "phi3:3.8b",                         "phi",                  2, "Microsoft Phi-3 3.8B",
  "phi3:14b",                          "phi",                  8, "Microsoft Phi-3 14B",
  "deepseek-r1:1.5b",                  "deepseek",             1, "DeepSeek R1 1.5B (reasoning)",
  "deepseek-r1:7b",                    "deepseek",             4, "DeepSeek R1 7B (reasoning)",
  "deepseek-r1:14b",                   "deepseek",             9, "DeepSeek R1 14B (reasoning)",
  "deepseek-r1:32b",                   "deepseek",            20, "DeepSeek R1 32B (reasoning)",
  # Vision-capable (relevant if abstracts include figures / OCR text)
  "llava:7b",                          "llava",                5, "LLaVA 7B (vision-capable)",
  "llava:13b",                         "llava",                8, "LLaVA 13B (vision-capable)"
)

#' Curated catalog of Ollama models useful for screening
#'
#' Returns a small tibble of popular instruction-tuned models with
#' approximate Q4_K_M-quantised disk sizes and a one-line
#' description. Not exhaustive; the full Ollama library is at
#' <https://ollama.com/library>. Users can still pull any tag with
#' `pull_model()` regardless of whether it's in this catalog.
#'
#' The list is intentionally short and opinionated; it favours models
#' that behave well on screening prompts. Update this in the package
#' as the Ollama ecosystem evolves.
#'
#' @return A tibble with columns `tag`, `family`, `size_gb`,
#'   `description`.
#' @export
#' @examples
#' catalog <- ollama_catalog()
#' head(catalog)
#' # Filter to models that fit in 8 GB of VRAM.
#' catalog[catalog$size_gb <= 8, ]
ollama_catalog <- function() {
  .OLLAMA_CATALOG
}
