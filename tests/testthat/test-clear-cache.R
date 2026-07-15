test_that("clear_cache removes only the requested model's cached scores", {
  # Isolate to a scratch data dir so this test can never touch the
  # user's real project cache.
  tmpdir <- tempfile("screenllm-clearcache-")
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  withr::with_envvar(c(R_USER_DATA_DIR = tmpdir), {
    proj <- "clear-cache-test"
    project_dir(proj, create = TRUE)
    cache_dir <- project_cache_dir(proj)
    fs::dir_create(cache_dir, recurse = TRUE)

    # Write a handful of fake cache files: 3 for mistral, 2 for gemma.
    writer <- function(model, id, replicate) {
      key <- paste(model, id, replicate, sep = "-")
      saveRDS(list(model = model, id = id, replicate = replicate,
                    score = 42, explanation = "why"),
              fs::path(cache_dir, paste0(key, ".rds")))
    }
    for (i in 1:3) writer("mistral:7b", paste0("r", i), 1L)
    for (i in 1:2) writer("gemma3:4b", paste0("r", i), 1L)

    # Sanity: 5 cache files present.
    expect_equal(length(fs::dir_ls(cache_dir, glob = "*.rds")), 5L)

    # Clearing mistral leaves gemma alone.
    removed <- clear_cache(proj, model = "mistral:7b", delete_ranked = FALSE)
    expect_equal(removed, 3L)
    remaining <- fs::dir_ls(cache_dir, glob = "*.rds")
    expect_equal(length(remaining), 2L)
    for (f in remaining) {
      expect_identical(readRDS(f)$model, "gemma3:4b")
    }

    # Clearing all removes the rest.
    removed_all <- clear_cache(proj, model = NULL, delete_ranked = FALSE)
    expect_equal(removed_all, 2L)
    expect_equal(length(fs::dir_ls(cache_dir, glob = "*.rds")), 0L)
  })
})

test_that("clear_cache(delete_ranked=TRUE) removes ranked.rds", {
  tmpdir <- tempfile("screenllm-clearcache-ranked-")
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  withr::with_envvar(c(R_USER_DATA_DIR = tmpdir), {
    proj <- "clear-cache-ranked-test"
    save_artefact(proj, "ranked", data.frame(id = "r1",
                                              universal_best_score = 50))
    ranked_path <- fs::path(project_dir(proj), .project_artefacts$ranked)
    expect_true(fs::file_exists(ranked_path))
    clear_cache(proj, delete_ranked = TRUE)
    expect_false(fs::file_exists(ranked_path))
  })
})
