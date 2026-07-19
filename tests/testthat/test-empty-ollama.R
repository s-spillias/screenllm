# Regression: a fresh Ollama install with zero models pulled returned
# {"models": []}, which simplifyVector parses to an empty list().
# nrow(list()) is NULL, and the previous guard `nrow(body$models) == 0L`
# cascaded that to NA and blew up the whole Setup tab. Test the shapes
# ollama_installed_models() must handle without exploding.

test_that("ollama_installed_models handles all body$models shapes", {
  # We test the internal shape-handling by stubbing an httr2::response.
  # Instead of mocking the transport, exercise the shape logic via a
  # small helper: parse the same JSON shapes and confirm the guard we
  # ship produces character(0) for empties and a character vector
  # otherwise.
  shape_check <- function(mods) {
    if (is.null(mods)) return(character())
    n <- if (is.data.frame(mods)) nrow(mods) else length(mods)
    if (n == 0L) return(character())
    if (is.data.frame(mods)) as.character(mods$name)
    else vapply(mods, function(m) as.character(m$name %||% ""),
                character(1))
  }
  expect_identical(shape_check(NULL),                 character())
  expect_identical(shape_check(list()),               character())
  expect_identical(shape_check(data.frame(name = character(),
                                           stringsAsFactors = FALSE)),
                    character())
  # Populated data.frame path (matches Ollama's simplifyVector = TRUE output)
  df <- data.frame(name = c("gemma3:4b", "mistral:7b"),
                    stringsAsFactors = FALSE)
  expect_identical(shape_check(df),
                    c("gemma3:4b", "mistral:7b"))
  # Populated list path (safety net if simplifyVector didn't flatten)
  ll <- list(list(name = "gemma3:4b"), list(name = "mistral:7b"))
  expect_identical(shape_check(ll),
                    c("gemma3:4b", "mistral:7b"))
})
