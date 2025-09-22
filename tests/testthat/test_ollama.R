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
msg_res <- ollama_chat(
  model = model_name,
  system = "You are a helpful assistant.",
  user = "Hello.",
  output_type = "text"
)
test_that("ollama_chat works", {
  expect_type(msg_res, "character")
}) # /ollama_chat

#' Generate Ollama Response ----
gen_res <- ollama_generate(
  model = model_name,
  prompt = "Pick a color.",
  output_type = "text"
)
test_that("ollama_generate works", {
  expect_type(gen_res, "character")
}) # /ollama_generate
