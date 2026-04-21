# test-ollama.R
# ::rtemis.llm::
# 2025 EDG rtemis.org

# library(testthat)

model_name <- "qwen3.5:0.8b"

# %% ollama_list_models() ----
test_that("ollama_list_models works", {
  skip_if_ollama_unavailable()
  expect_type(ollama_list_models(), "character")
}) # /ollama_list_models


# %% ollama_get_model_info() ----
test_that("ollama_get_model_info works", {
  skip_if_ollama_unavailable()
  all_models <- ollama_get_model_info()
  expect_s3_class(all_models, "data.table")
}) # /ollama_get_model_info


# %% ollama_check_model() ----
test_that("ollama_check_model works", {
  skip_if_ollama_model_missing(model_name)
  expect_null(ollama_check_model(model_name))
  expect_error(ollama_check_model("non_existent_model_12345"))
}) # /ollama_check_model


# %% build_chat_request_body.OllamaConfig per-call overrides ----
test_that("Ollama request body honors per-call overrides", {
  skip_if_ollama_model_missing(model_name)
  config <- config_Ollama(
    model_name = model_name,
    temperature = 0.2
  )
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "Hi"),
    echo = FALSE,
    verbosity = 0L
  )
  body <- build_chat_request_body(
    config,
    state = state,
    temperature = 0.9,
    top_p = 0.5,
    max_tokens = 128L,
    stop = c("\n\n"),
    top_k = 40L,
    seed = 42L
  )
  expect_equal(body[["options"]][["temperature"]], 0.9)
  expect_equal(body[["options"]][["top_p"]], 0.5)
  expect_equal(body[["options"]][["top_k"]], 40L)
  expect_equal(body[["options"]][["seed"]], 42L)
  expect_equal(body[["options"]][["num_predict"]], 128L)
  expect_equal(body[["options"]][["stop"]], "\n\n")
})


# %% build_chat_request_body.OllamaConfig defaults fall through ----
test_that("Ollama request body falls back to config when overrides are NULL", {
  skip_if_ollama_model_missing(model_name)
  config <- config_Ollama(
    model_name = model_name,
    temperature = 0.2
  )
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "Hi"),
    echo = FALSE,
    verbosity = 0L
  )
  body <- build_chat_request_body(config, state = state)
  expect_equal(body[["options"]][["temperature"]], 0.2)
  expect_false("top_p" %in% names(body[["options"]]))
  expect_false("top_k" %in% names(body[["options"]]))
  expect_false("seed" %in% names(body[["options"]]))
  expect_false("num_predict" %in% names(body[["options"]]))
  expect_false("stop" %in% names(body[["options"]]))
})
