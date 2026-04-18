# test_LLM.R
# ::rtemis.llm::
# 2025 EDG rtemis.org

# %% Settings ----
model_name <- "qwen3.5:0.8b"


# %% LLMConfig Class ----
test_that("LLMConfig class works", {
  config <- LLMConfig(
    model_name = model_name,
    temperature = 0.7,
    backend = "ollama",
    base_url = "http://localhost:11434"
  )
  testthat::expect_true(S7_inherits(config, LLMConfig))
}) # /LLMConfig


# %% OllamaConfig Class ----
test_that("OllamaConfig class works", {
  config <- OllamaConfig(
    model_name = model_name,
    temperature = 0.7,
    base_url = "http://localhost:11434"
  )
  testthat::expect_true(S7_inherits(config, OllamaConfig))
}) # /OllamaConfig


# %% LLM Class ----
test_that("LLM class works", {
  llm <- LLM(
    name = "SuperLLM",
    system_prompt = "You are a meticulous research assistant."
  )
  testthat::expect_true(S7_inherits(llm, LLM))
}) # /LLM


# %% Ollama Class ----
test_that("Ollama class works", {
  llm <- Ollama(
    config = OllamaConfig(
      model_name = "gemma4:e4b",
      temperature = 0.3,
      base_url = "http://localhost:11434"
    ),
    system_prompt = "You are a meticulous research assistant."
  )
  testthat::expect_true(S7_inherits(llm, Ollama))
}) # /Ollama


# %% create_Ollama() ----
llm <- create_Ollama(
  model_name = model_name,
  system_prompt = "You are a meticulous research assistant.",
  temperature = 0.4,
  output_schema = NULL,
  base_url = "http://localhost:11434"
)
test_that("create_Ollama works", {
  testthat::expect_true(S7_inherits(llm, Ollama))
}) # /create_Ollama

# %% generate.Ollama ----
# Slow test, uncomment to run
# res <- llm |>
#   generate("What is your name?", verbosity = 2)
# test_that("generate.Ollama works", {
#   testthat::expect_true(S7_inherits(res, OllamaMessage))
# }) # /generate.Ollama
