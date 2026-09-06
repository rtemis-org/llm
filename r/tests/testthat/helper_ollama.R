ollama_test_url <- "http://localhost:11434"

skip_if_ollama_unavailable <- function(base_url = ollama_test_url) {
  # The `tryCatch` below already skips when nothing answers, and on a check
  # machine that costs ~30ms -- the loopback connection is refused, not timed
  # out. This states the intent up front instead: a CRAN check should not be
  # opening the connection at all, so the skip is declared rather than inferred
  # from a probe that failed as expected.
  testthat::skip_on_cran()
  tryCatch(
    ollama_list_models(base_url = base_url),
    error = function(e) {
      testthat::skip(paste0("Ollama server not available at ", base_url))
    }
  )
}

skip_if_ollama_model_missing <- function(
  model_name,
  base_url = ollama_test_url
) {
  models <- skip_if_ollama_unavailable(base_url = base_url)
  if (!model_name %in% models) {
    testthat::skip(paste0("Ollama model '", model_name, "' not available"))
  }
  invisible(models)
}
