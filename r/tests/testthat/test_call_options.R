# test_call_options.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# Per-call options reach the request builders through `generate(..., ...)`, so
# the `prop_*()` bounds on the config classes never see them. These check that
# the builders reject at call time what the config would have rejected at
# construction, rather than sending it to the server.

model_name <- "qwen3.5:0.8b"

test_state <- function() {
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "Hi"),
    echo = FALSE,
    verbosity = 0L
  )
  state
}

openai_test_config <- function() {
  config_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    validate_model = FALSE
  )
}


# %% shared numeric bounds ----
test_that("out-of-range sampling options are rejected", {
  config <- openai_test_config()
  state <- test_state()
  expect_error(
    build_chat_request_body(config, state = state, temperature = 5),
    "temperature"
  )
  expect_error(
    build_chat_request_body(config, state = state, top_p = 1.5),
    "top_p"
  )
  expect_error(
    build_chat_request_body(config, state = state, max_tokens = 0),
    "max_tokens"
  )
})


test_that("wrong types are rejected rather than coerced", {
  # `as.integer("x")` is NA and `as.integer(100.5)` is 100: both would reach
  # the server looking like a deliberate value.
  config <- openai_test_config()
  state <- test_state()
  expect_error(
    build_chat_request_body(config, state = state, temperature = "hot"),
    "numeric"
  )
  expect_error(
    build_chat_request_body(config, state = state, max_tokens = 100.5),
    "whole number"
  )
  expect_error(
    build_chat_request_body(config, state = state, seed = 1.5),
    "whole number"
  )
  expect_error(
    build_chat_request_body(config, state = state, logprobs = "yes"),
    "logprobs"
  )
})


test_that("stop sequences must be a non-empty character vector", {
  config <- openai_test_config()
  state <- test_state()
  expect_error(
    build_chat_request_body(config, state = state, stop = 1),
    "character"
  )
  expect_error(
    build_chat_request_body(config, state = state, stop = c("a", NA)),
    "NA"
  )
  expect_error(
    build_chat_request_body(config, state = state, stop = character(0)),
    "at least one"
  )
  body <- build_chat_request_body(config, state = state, stop = c("a", "b"))
  expect_identical(body[["stop"]], c("a", "b"))
})


test_that("valid options are passed through untouched", {
  config <- openai_test_config()
  state <- test_state()
  body <- build_chat_request_body(
    config,
    state = state,
    temperature = 0.7,
    top_p = 0.9,
    max_tokens = 100,
    seed = 42
  )
  expect_equal(body[["temperature"]], 0.7)
  expect_equal(body[["top_p"]], 0.9)
  expect_equal(body[["max_tokens"]], 100L)
  expect_equal(body[["seed"]], 42L)
})


# %% per-backend bounds ----
test_that("temperature bounds follow the backend, not a shared default", {
  # Anthropic caps temperature at 1; Ollama and OpenAI at 2. A value that is
  # valid on one backend must not be waved through on another.
  state <- test_state()
  anthropic <- config_Anthropic(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
    validate_model = FALSE
  )
  expect_error(
    build_chat_request_body(anthropic, state = state, temperature = 1.5),
    "\\[0, 1\\]"
  )
  expect_no_error(
    build_chat_request_body(anthropic, state = state, temperature = 0.9)
  )
  expect_no_error(
    build_chat_request_body(
      openai_test_config(),
      state = state,
      temperature = 1.5
    )
  )
})


test_that("top_logprobs bounds follow the backend", {
  # OpenAI documents [0, 20]; Ollama sets no upper bound.
  state <- test_state()
  expect_error(
    build_chat_request_body(
      openai_test_config(),
      state = state,
      logprobs = TRUE,
      top_logprobs = 25
    ),
    "\\[0, 20\\]"
  )
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
  ollama <- config_Ollama(model_name = model_name)
  expect_no_error(
    build_chat_request_body(
      ollama,
      state = state,
      logprobs = TRUE,
      top_logprobs = 25
    )
  )
})


# %% Ollama-only options ----
test_that("Ollama num_ctx and keep_alive are validated", {
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
  config <- config_Ollama(model_name = model_name)
  state <- test_state()
  expect_error(
    build_chat_request_body(config, state = state, num_ctx = 0),
    "num_ctx"
  )
  expect_error(
    build_chat_request_body(config, state = state, keep_alive = list(1)),
    "keep_alive"
  )
  # A duration string, and seconds -- including -1 for "load indefinitely".
  expect_no_error(
    build_chat_request_body(config, state = state, keep_alive = "10m")
  )
  expect_no_error(
    build_chat_request_body(config, state = state, keep_alive = -1)
  )
})
