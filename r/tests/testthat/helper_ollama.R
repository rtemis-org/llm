ollama_test_url <- "http://localhost:11434"

skip_if_ollama_unavailable <- function(base_url = ollama_test_url) {
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
