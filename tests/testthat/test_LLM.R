# tes_LLM.R
# ::kaimana::
# 2025 EDG rtemis.org

# library(testthat)

# %% LLMConfig Class ----
test_that("LLMConfig class works", {
  config <- LLMConfig(
    model_name = "qwen3:8b",
    temperature = 0.7,
    backend = "ollama",
    base_url = "http://localhost:11434"
  )
  testthat::expect_true(S7_inherits(config, LLMConfig))
}) # /LLMConfig


# %% OllamaConfig Class ----
test_that("OllamaConfig class works", {
  config <- OllamaConfig(
    model_name = "qwen3:8b",
    temperature = 0.7,
    base_url = "http://localhost:11434"
  )
  testthat::expect_true(S7_inherits(config, OllamaConfig))
}) # /OllamaConfig


# %% LLM Class ----
test_that("Ollama class works", {
  llm <- Ollama(
    config = OllamaConfig(
      model_name = "qwen3:8b",
      temperature = 0.7,
      base_url = "http://localhost:11434"
    ),
    system_prompt = "You are a meticulous research assistant."
  )
  testthat::expect_true(S7_inherits(llm, Ollama))
}) # /Ollama


# %% Ollama Class ----
test_that("Ollama class works", {
  llm <- Ollama(
    config = OllamaConfig(
      model_name = "qwen3:8b",
      temperature = 0.7,
      base_url = "http://localhost:11434"
    ),
    system_prompt = "You are a meticulous research assistant."
  )
  testthat::expect_true(S7_inherits(llm, Ollama))
}) # /Ollama


# %% create_Ollama() ----
llm <- create_Ollama(
  model_name = "qwen3:8b",
  system_prompt = "You are a meticulous research assistant.",
  temperature = 0.4,
  output_schema = NULL,
  base_url = "http://localhost:11434"
)
test_that("create_Ollama works", {
  testthat::expect_true(S7_inherits(llm, Ollama))
}) # /create_Ollama


# %% Message Class ----
test_that("Message class works", {
  msg <- Message(
    role = "llm",
    name = "Preprocessor",
    content = "Hello.",
    metadata = list(project = "kmn")
  )
  testthat::expect_true(S7_inherits(msg, Message))
}) # /Message


# %% LLMMessage Class ----
test_that("LLMMessage class works", {
  msg <- LLMMessage(
    name = "Preprocessor",
    content = "Hello.",
    metadata = list(project = "kmn"),
    model_name = "qwen3:8b",
    reasoning = "I think therefore I am."
  )
  testthat::expect_true(S7_inherits(msg, LLMMessage))
}) # /LLMMessage


# %% OllamaMessage Class ----
test_that("OllamaMessage class works", {
  msg <- OllamaMessage(
    name = "Preprocessor",
    content = "Hello.",
    metadata = list(project = "kmn"),
    model_name = "qwen3:8b",
    reasoning = "I think therefore I am."
  )
  testthat::expect_true(S7_inherits(msg, OllamaMessage))
  testthat::expect_equal(msg@metadata[["provider"]], "Ollama")
}) # /OllamaMessage


# %% generate.Ollama ----
res <- llm |>
  generate("What is your name?", verbosity = 2)
test_that("generate.Ollama works", {
  testthat::expect_true(S7_inherits(res, OllamaMessage))
}) # /generate.Ollama
