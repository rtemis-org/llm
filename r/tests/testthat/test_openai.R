# test_openai.R
# ::rtemis.llm::
# 2026 EDG rtemis.org

# %% OpenAIConfig ----
test_that("OpenAIConfig class works for local compatible servers", {
  config <- config_OpenAI(
    model_name = "local-model",
    temperature = 0.4,
    base_url = "http://localhost:1234/v1/",
    validate_model = FALSE
  )
  expect_true(S7_inherits(config, OpenAIConfig))
  expect_equal(config@base_url, "http://localhost:1234/v1")
  expect_null(resolve_api_key(config))
})


# %% OpenAIConfig API key handling ----
test_that("OpenAIConfig requires API key only for official OpenAI URL", {
  config <- config_OpenAI(
    model_name = "gpt-test",
    api_key_env = "RTEMIS_LLM_EMPTY_TEST_KEY",
    validate_model = FALSE
  )
  expect_error(resolve_api_key(config), "No OpenAI API key")
})


# %% repr.OpenAIConfig ----
test_that("OpenAIConfig repr redacts API keys", {
  config <- config_OpenAI(
    model_name = "gpt-test",
    api_key = "secret-key",
    validate_model = FALSE
  )
  out <- repr(config, output_type = "plain")
  expect_true(grepl("<redacted>", out, fixed = TRUE))
  expect_false(grepl("secret-key", out, fixed = TRUE))
})


# %% create_OpenAI() ----
test_that("create_OpenAI works", {
  llm <- create_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    system_prompt = "You are a meticulous research assistant.",
    validate_model = FALSE
  )
  expect_true(S7_inherits(llm, OpenAI))
}) # /create_OpenAI


# %% build_chat_request_body.OpenAIConfig ----
test_that("OpenAI-compatible request body uses chat completions shape", {
  config <- config_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    temperature = 0.2,
    extra_body = list(top_p = 0.9),
    validate_model = FALSE
  )
  state <- InProcessAgentMemory()
  append_message(
    state,
    SystemMessage(content = "System prompt."),
    echo = FALSE,
    verbosity = 0L
  )
  append_message(
    state,
    InputMessage(content = "Hello."),
    echo = FALSE,
    verbosity = 0L
  )
  body <- build_chat_request_body(config, state = state)
  expect_equal(body[["model"]], "local-model")
  expect_equal(body[["temperature"]], 0.2)
  expect_equal(body[["top_p"]], 0.9)
  expect_false("options" %in% names(body))
  expect_false("format" %in% names(body))
  expect_equal(body[["messages"]][[1]][["role"]], "system")
  expect_equal(body[["messages"]][[2]][["role"]], "user")
})


# %% build_chat_request_body.OpenAIConfig thinking ----
test_that("OpenAI-compatible request body enables local model thinking", {
  config <- config_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    temperature = 0.2,
    enable_thinking = TRUE,
    validate_model = FALSE
  )
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "Hello."),
    echo = FALSE,
    verbosity = 0L
  )
  body <- build_chat_request_body(config, state = state)
  expect_true(body[["enable_thinking"]])
  expect_true(body[["chat_template_kwargs"]][["enable_thinking"]])
})


# %% build_chat_request_body.OpenAIConfig thinking override ----
test_that("OpenAI-compatible generate think argument overrides config thinking", {
  config <- config_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    temperature = 0.2,
    enable_thinking = FALSE,
    validate_model = FALSE
  )
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "Hello."),
    echo = FALSE,
    verbosity = 0L
  )
  body <- build_chat_request_body(config, state = state, think = TRUE)
  expect_true(body[["enable_thinking"]])
  expect_true(body[["chat_template_kwargs"]][["enable_thinking"]])
})


# %% build_chat_request_body.OpenAIConfig official thinking ----
test_that("OpenAI-compatible thinking flag is local-only", {
  config <- config_OpenAI(
    model_name = "gpt-test",
    api_key = "test-key",
    enable_thinking = TRUE,
    validate_model = FALSE
  )
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "Hello."),
    echo = FALSE,
    verbosity = 0L
  )
  expect_error(
    build_chat_request_body(config, state = state),
    "local OpenAI-compatible servers"
  )
})


# %% build_response_format.OpenAIConfig ----
test_that("OpenAI-compatible response format wraps schemas", {
  config <- config_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    validate_model = FALSE
  )
  output_schema <- schema(
    answer = field("string", "Answer text.")
  )
  response_format <- build_response_format(config, output_schema)
  expect_equal(response_format[["type"]], "json_schema")
  expect_true(response_format[["json_schema"]][["strict"]])
  expect_false(response_format[["json_schema"]][["schema"]][["additionalProperties"]])
})


# %% decode_tool_arguments.OpenAIConfig ----
test_that("OpenAI-compatible tool arguments decode from JSON strings", {
  config <- config_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    validate_model = FALSE
  )
  tool_call <- list(
    id = "call_123",
    type = "function",
    `function` = list(
      name = "query_wikipedia",
      arguments = "{\"query\":\"rtemis\",\"limit\":1}"
    )
  )
  args <- decode_tool_arguments(config, tool_call)
  expect_equal(args[["query"]], "rtemis")
  expect_equal(args[["limit"]], 1)
})


# %% ToolMessage tool_call_id ----
test_that("OpenAI-compatible tool messages include tool_call_id", {
  config <- config_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    validate_model = FALSE
  )
  tool_call <- list(
    id = "call_123",
    type = "function",
    `function` = list(name = "query_wikipedia")
  )
  msg <- build_tool_message(
    config,
    tool_call = tool_call,
    tool_name = "query_wikipedia",
    tool_response = "Tool response."
  )
  expect_true(S7_inherits(msg, ToolMessage))
  expect_equal(msg@tool_call_id, "call_123")
})


# %% parse_chat_response.OpenAIConfig ----
test_that("OpenAI-compatible response parsing extracts message fields", {
  config <- config_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    validate_model = FALSE
  )
  body <- jsonlite::toJSON(
    list(
      id = "chatcmpl_test",
      model = "local-model",
      choices = list(list(
        index = 0,
        message = list(
          role = "assistant",
          content = "Hello",
          tool_calls = list()
        ),
        finish_reason = "stop"
      )),
      usage = list(total_tokens = 3)
    ),
    auto_unbox = TRUE
  )
  resp <- httr2::response(
    status_code = 200,
    headers = list(`content-type` = "application/json", `x-request-id` = "req_123"),
    body = charToRaw(body)
  )
  parsed <- parse_chat_response(config, resp)
  expect_equal(parsed[["content"]], "Hello")
  expect_equal(parsed[["metadata"]][["id"]], "chatcmpl_test")
  expect_equal(parsed[["metadata"]][["finish_reason"]], "stop")
  expect_equal(parsed[["metadata"]][["request_id"]], "req_123")
})


# %% parse_chat_response.OpenAIConfig reasoning_content ----
test_that("OpenAI-compatible response parsing extracts reasoning_content", {
  config <- config_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    validate_model = FALSE
  )
  body <- jsonlite::toJSON(
    list(
      id = "chatcmpl_reasoning_content",
      model = "local-model",
      choices = list(list(
        index = 0,
        message = list(
          role = "assistant",
          reasoning_content = "I should answer directly.",
          content = "Direct answer."
        ),
        finish_reason = "stop"
      ))
    ),
    auto_unbox = TRUE
  )
  resp <- httr2::response(
    status_code = 200,
    headers = list(`content-type` = "application/json"),
    body = charToRaw(body)
  )
  parsed <- parse_chat_response(config, resp)
  expect_equal(parsed[["content"]], "Direct answer.")
  expect_equal(parsed[["reasoning"]], "I should answer directly.")
})


# %% parse_chat_response.OpenAIConfig thinking ----
test_that("OpenAI-compatible response parsing extracts thinking fields", {
  config <- config_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    validate_model = FALSE
  )
  body <- jsonlite::toJSON(
    list(
      id = "chatcmpl_thinking",
      model = "local-model",
      choices = list(list(
        index = 0,
        message = list(
          role = "assistant",
          thinking = "The server exposed thinking explicitly.",
          content = "The visible answer."
        ),
        finish_reason = "stop"
      ))
    ),
    auto_unbox = TRUE
  )
  resp <- httr2::response(
    status_code = 200,
    headers = list(`content-type` = "application/json"),
    body = charToRaw(body)
  )
  parsed <- parse_chat_response(config, resp)
  expect_equal(parsed[["content"]], "The visible answer.")
  expect_equal(parsed[["reasoning"]], "The server exposed thinking explicitly.")
})


# %% parse_chat_response.OpenAIConfig think tags ----
test_that("OpenAI-compatible response parsing extracts embedded think tags", {
  config <- config_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    validate_model = FALSE
  )
  body <- jsonlite::toJSON(
    list(
      id = "chatcmpl_tags",
      model = "local-model",
      choices = list(list(
        index = 0,
        message = list(
          role = "assistant",
          content = "<think>I should keep this separate.</think>\n\nVisible answer."
        ),
        finish_reason = "stop"
      ))
    ),
    auto_unbox = TRUE
  )
  resp <- httr2::response(
    status_code = 200,
    headers = list(`content-type` = "application/json"),
    body = charToRaw(body)
  )
  parsed <- parse_chat_response(config, resp)
  expect_equal(parsed[["content"]], "Visible answer.")
  expect_equal(parsed[["reasoning"]], "I should keep this separate.")
})


# %% parse_chat_response.OpenAIConfig content array reasoning ----
test_that("OpenAI-compatible response parsing extracts reasoning content parts", {
  config <- config_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    validate_model = FALSE
  )
  body <- jsonlite::toJSON(
    list(
      id = "chatcmpl_content_parts",
      model = "local-model",
      choices = list(list(
        index = 0,
        message = list(
          role = "assistant",
          content = list(
            list(type = "reasoning", text = "Reasoning content part."),
            list(type = "text", text = "Visible answer part.")
          )
        ),
        finish_reason = "stop"
      ))
    ),
    auto_unbox = TRUE
  )
  resp <- httr2::response(
    status_code = 200,
    headers = list(`content-type` = "application/json"),
    body = charToRaw(body)
  )
  parsed <- parse_chat_response(config, resp)
  expect_equal(parsed[["content"]], "Visible answer part.")
  expect_equal(parsed[["reasoning"]], "Reasoning content part.")
})
