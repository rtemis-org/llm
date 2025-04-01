# test-ollama.R
# ::kaimana::
# 2025 SDG rtemis.org

test_model <- "gemma2:9b"
test_prompt <- "Pick a color."

# List Ollama Models ----
test_that("list_ollama_models works", {
  expect_type(list_ollama_models(), "character")
}) # /list_ollama_models

# Message Ollama ----
msg_res <- msg_ollama(
  model = test_model,
  system = "You are a helpful assistant.",
  user = "Hello.",
  output_type = "text"
)
test_that("msg_ollama works", {
  expect_type(msg_res, "character")
}) # /msg_ollama

#' Generate Ollama Response ----
gen_res <- gen_ollama(
  model = test_model,
  prompt = "Tell me a 50-word story.",
  output = "text"
)
test_that("gen_ollama works", {
}