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
  skip_if_ollama_model_missing(model_name)
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
  skip_if_ollama_model_missing(model_name)
  llm <- Ollama(
    config = OllamaConfig(
      model_name = model_name,
      temperature = 0.3,
      base_url = "http://localhost:11434"
    ),
    system_prompt = "You are a meticulous research assistant."
  )
  testthat::expect_true(S7_inherits(llm, Ollama))
}) # /Ollama


# %% create_Ollama() ----
test_that("create_Ollama works", {
  skip_if_ollama_model_missing(model_name)
  llm <- create_Ollama(
    model_name = model_name,
    system_prompt = "You are a meticulous research assistant.",
    temperature = 0.4,
    output_schema = NULL,
    base_url = "http://localhost:11434"
  )
  testthat::expect_true(S7_inherits(llm, Ollama))
}) # /create_Ollama

# %% OllamaConfig think property ----
test_that("OllamaConfig accepts valid think values", {
  skip_if_ollama_model_missing(model_name)
  # logical
  cfg_true <- OllamaConfig(
    model_name = model_name,
    temperature = 0.3,
    base_url = "http://localhost:11434",
    think = TRUE
  )
  testthat::expect_identical(cfg_true@think, TRUE)
  # character levels
  cfg_high <- OllamaConfig(
    model_name = model_name,
    temperature = 0.3,
    base_url = "http://localhost:11434",
    think = "high"
  )
  testthat::expect_identical(cfg_high@think, "high")
  # NULL default
  cfg_null <- OllamaConfig(
    model_name = model_name,
    temperature = 0.3,
    base_url = "http://localhost:11434"
  )
  testthat::expect_null(cfg_null@think)
}) # /OllamaConfig think


test_that("OllamaConfig rejects invalid think values", {
  skip_if_ollama_model_missing(model_name)
  testthat::expect_error(
    OllamaConfig(
      model_name = model_name,
      temperature = 0.3,
      base_url = "http://localhost:11434",
      think = "extreme"
    ),
    "think"
  )
  testthat::expect_error(
    OllamaConfig(
      model_name = model_name,
      temperature = 0.3,
      base_url = "http://localhost:11434",
      think = c(TRUE, FALSE)
    ),
    "think"
  )
}) # /OllamaConfig think invalid

# %% generate.Ollama ----
# Slow test, uncomment to run
# res <- llm |>
#   generate("What is your name?", verbosity = 2)
# test_that("generate.Ollama works", {
#   testthat::expect_true(S7_inherits(res, OllamaMessage))
# }) # /generate.Ollama
