# Redirect all per-user storage to the session's temporary directory so the
# test suite never writes to the real user home filespace (CRAN policy).
# tools::R_user_dir() honours these environment variables for the "data"
# and "cache" locations, so every data_root()/pull_progress_path() call
# during the tests resolves under tempdir().
local({
  data_dir <- tempfile("screenllm-data-")
  cache_dir <- tempfile("screenllm-cache-")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(R_USER_DATA_DIR = data_dir, R_USER_CACHE_DIR = cache_dir)
  withr::defer(
    {
      Sys.unsetenv(c("R_USER_DATA_DIR", "R_USER_CACHE_DIR"))
      unlink(c(data_dir, cache_dir), recursive = TRUE)
    },
    teardown_env()
  )
})
