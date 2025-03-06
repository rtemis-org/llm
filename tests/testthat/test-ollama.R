# test-ollama.R
# ::kaimana::
# 2025 SDG rtemis.org

# List Ollama Models
test_that("list_ollama_models", {
  expect_type(list_ollama_models(), "character")
}) # /list_ollama_models

# Message Ollama
test_that("msg_ollama", {
  expect_type(
    msg_ollama(
      model = "phi3:latest",
      system = "You are a helpful assistant.",
      user = "Hello.",
      output_type = "text"
    ), "character"
  )
}) # /msg_ollama
