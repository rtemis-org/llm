# test-ollama.R
# ::rtemis.llm::
# 2025- EDG rtemis.org

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


# %% build_chat_request_body.OllamaConfig num_ctx / keep_alive ----
test_that("Ollama request body carries num_ctx and keep_alive", {
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
  config <- config_Ollama(model_name = model_name)
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
    num_ctx = 2048L,
    keep_alive = "10m"
  )
  # num_ctx is a sampling option; keep_alive is a top-level request field.
  expect_equal(body[["options"]][["num_ctx"]], 2048L)
  expect_equal(body[["keep_alive"]], "10m")
})


# %% build_chat_request_body.OllamaConfig defaults fall through ----
test_that("Ollama request body falls back to config when overrides are NULL", {
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
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
  expect_false("num_ctx" %in% names(body[["options"]]))
  expect_false("think" %in% names(body))
  expect_false("keep_alive" %in% names(body))
})


# %% build_chat_request_body.OllamaConfig thinking toggle ----
test_that("Ollama request body preserves an explicit disabled thinking toggle", {
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
  config <- config_Ollama(
    model_name = model_name,
    temperature = 0.2,
    think = FALSE
  )
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "Hi"),
    echo = FALSE,
    verbosity = 0L
  )

  body <- build_chat_request_body(config, state = state)

  expect_true("think" %in% names(body))
  expect_identical(body[["think"]], FALSE)
})


# %% build_chat_request_body.OllamaConfig logprobs ----
test_that("Ollama request body carries logprobs at the top level", {
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
  config <- config_Ollama(model_name = model_name)
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
    logprobs = TRUE,
    top_logprobs = 5L
  )
  # Top level, not under `options`: nested there the server ignores them.
  expect_true(body[["logprobs"]])
  expect_equal(body[["top_logprobs"]], 5L)
  expect_false("logprobs" %in% names(body[["options"]]))
  expect_false("top_logprobs" %in% names(body[["options"]]))
})

test_that("Ollama rejects top_logprobs without logprobs", {
  # Ollama drops a lone `top_logprobs` and returns no logprobs at all, so the
  # request would succeed and `token_probs()` would come back empty.
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
  config <- config_Ollama(model_name = model_name)
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "Hi"),
    echo = FALSE,
    verbosity = 0L
  )
  expect_error(
    build_chat_request_body(config, state = state, top_logprobs = 5L),
    "logprobs"
  )
  expect_error(
    build_chat_request_body(
      config,
      state = state,
      logprobs = FALSE,
      top_logprobs = 5L
    ),
    "logprobs"
  )
})


# %% parse_chat_response.OllamaConfig logprobs ----
test_that("Ollama response parsing carries logprobs onto metadata", {
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
  config <- config_Ollama(model_name = model_name)
  # Ollama returns logprobs as a top-level sibling of `message`, so the
  # existing metadata sweep already carries it.
  body <- jsonlite::toJSON(
    list(
      model = model_name,
      message = list(role = "assistant", content = "Yes"),
      done = TRUE,
      logprobs = list(
        list(
          token = "Yes",
          logprob = -0.25,
          top_logprobs = list(
            list(token = "Yes", logprob = -0.25),
            list(token = "No", logprob = -2.5)
          )
        )
      )
    ),
    auto_unbox = TRUE
  )
  resp <- httr2::response(
    status_code = 200,
    headers = list(`content-type` = "application/json"),
    body = charToRaw(body)
  )
  parsed <- parse_chat_response(config, resp)
  expect_equal(parsed[["metadata"]][["logprobs"]][[1L]][["token"]], "Yes")
})


# %% build_chat_request_body.OllamaConfig schema_in_prompt ----
# spec: llm/ollama-schema-instructions#request
ollama_schema_state <- function(system_prompt = "Extract.") {
  state <- InProcessAgentMemory()
  if (!is.null(system_prompt)) {
    append_message(
      state,
      SystemMessage(content = system_prompt),
      echo = FALSE,
      verbosity = 0L
    )
  }
  append_message(
    state,
    InputMessage(content = "The sky is blue."),
    echo = FALSE,
    verbosity = 0L
  )
  state
}

ollama_fruit_schema <- function() {
  schema(
    "answer",
    field("fruit", "Always the single word pineapple."),
    field("number", "Always the number 42.", type = "integer")
  )
}

test_that("Ollama appends the output schema to the system prompt", {
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
  config <- config_Ollama(model_name = model_name)
  expect_true(config@schema_in_prompt)
  state <- ollama_schema_state()
  body <- build_chat_request_body(
    config,
    state = state,
    output_schema = ollama_fruit_schema()
  )
  # The prompt carries the same JSON Schema as `format`, serialized as httr2
  # serializes the request body.
  format_json <- jsonlite::toJSON(
    body[["format"]],
    auto_unbox = TRUE,
    digits = 22,
    null = "null"
  )
  expect_identical(
    body[["messages"]][[1L]][["content"]],
    paste0(
      "Extract.\n\nRespond with JSON that conforms to this JSON Schema:\n",
      format_json
    )
  )
  expect_match(
    body[["messages"]][[1L]][["content"]],
    "Always the single word pineapple.",
    fixed = TRUE
  )
  expect_identical(body[["messages"]][[2L]][["content"]], "The sky is blue.")
  # Agent memory keeps the system prompt as written.
  expect_identical(get_message_list(state)[[1L]][["content"]], "Extract.")
})

test_that("Ollama sends the schema as a system message when there is none", {
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
  body <- build_chat_request_body(
    config_Ollama(model_name = model_name),
    state = ollama_schema_state(system_prompt = NULL),
    output_schema = ollama_fruit_schema()
  )
  expect_length(body[["messages"]], 2L)
  expect_identical(body[["messages"]][[1L]][["role"]], "system")
  expect_match(
    body[["messages"]][[1L]][["content"]],
    "^Respond with JSON that conforms to this JSON Schema:\n"
  )
})

test_that("schema_in_prompt = FALSE sends the system prompt as written", {
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
  config <- config_Ollama(model_name = model_name, schema_in_prompt = FALSE)
  body <- build_chat_request_body(
    config,
    state = ollama_schema_state(),
    output_schema = ollama_fruit_schema()
  )
  expect_identical(body[["messages"]][[1L]][["content"]], "Extract.")
  expect_false(is.null(body[["format"]]))
})

test_that("a request without an output schema leaves the system prompt as written", {
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
  body <- build_chat_request_body(
    config_Ollama(model_name = model_name),
    state = ollama_schema_state()
  )
  expect_identical(body[["messages"]][[1L]][["content"]], "Extract.")
  expect_null(body[["format"]])
})

test_that("create_Ollama forwards schema_in_prompt and repr shows it", {
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
  llm <- create_Ollama(model_name, schema_in_prompt = FALSE)
  expect_false(llm@config@schema_in_prompt)
  expect_false(as_list(llm@config)[["schema_in_prompt"]])
  expect_match(
    repr(llm@config, output_type = "plain"),
    "schema_in_prompt",
    fixed = TRUE
  )
  expect_error(config_Ollama(model_name, schema_in_prompt = NA))
  expect_error(config_Ollama(model_name, schema_in_prompt = "yes"))
})
