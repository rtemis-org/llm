# test-ollama.R
# ::kaimana::
# 2025 EDG rtemis.org

# library(testthat)

model_name <- "qwen3:8b"

# List Ollama Models ----
test_that("ollama_list_models works", {
  expect_type(ollama_list_models(), "character")
}) # /ollama_list_models

# Message Ollama ----
msg_res <- msg_ollama(
  model = model_name,
  system = "You are a helpful assistant.",
  user = "Hello.",
  output_type = "text"
)
test_that("msg_ollama works", {
  expect_type(msg_res, "character")
}) # /msg_ollama

#' Generate Ollama Response ----
gen_res <- gen_ollama(
  model = model_name,
  prompt = "Pick a color.",
  output_type = "text"
)
test_that("gen_ollama works", {
  expect_type(gen_res, "character")
}) # /gen_ollama
