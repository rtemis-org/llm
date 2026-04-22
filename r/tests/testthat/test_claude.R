# test_claude.R
# ::rtemis.llm::
# 2026 EDG rtemis.org

# %% ClaudeConfig ----
test_that("ClaudeConfig class and config_Claude work", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    temperature = 0.4,
    api_key = "test-key",
    max_tokens = 1024L,
    validate_model = FALSE
  )
  expect_true(S7_inherits(config, ClaudeConfig))
  expect_equal(config@model_name, "claude-sonnet-4-6")
  expect_equal(config@max_tokens, 1024L)
  expect_equal(config@backend, "anthropic")
  expect_equal(config@base_url, "https://api.anthropic.com/v1")
})


# %% config_Claude API key handling ----
test_that("config_Claude aborts when no API key is resolvable", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key_env = "RTEMIS_LLM_CLAUDE_EMPTY_TEST_KEY",
    validate_model = FALSE
  )
  expect_error(resolve_claude_api_key(config), "No Anthropic API key")
})


# %% config_Claude max_tokens validation ----
test_that("config_Claude rejects non-positive max_tokens", {
  expect_error(
    config_Claude(
      model_name = "claude-sonnet-4-6",
      api_key = "test-key",
      max_tokens = 0,
      validate_model = FALSE
    ),
    "max_tokens"
  )
})


# %% config_Claude thinking budget validation ----
test_that("config_Claude rejects thinking budgets below the minimum", {
  expect_error(
    config_Claude(
      model_name = "claude-sonnet-4-6",
      api_key = "test-key",
      thinking_budget_tokens = 256L,
      validate_model = FALSE
    ),
    "thinking_budget_tokens"
  )
})


# %% repr.ClaudeConfig ----
test_that("ClaudeConfig repr redacts API keys", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "anthropic-secret",
    validate_model = FALSE
  )
  out <- repr(config, output_type = "plain")
  expect_true(grepl("<redacted>", out, fixed = TRUE))
  expect_false(grepl("anthropic-secret", out, fixed = TRUE))
})


# %% create_Claude() ----
test_that("create_Claude works", {
  llm <- create_Claude(
    model_name = "claude-sonnet-4-6",
    system_prompt = "You are a meticulous research assistant.",
    api_key = "test-key",
    validate_model = FALSE
  )
  expect_true(S7_inherits(llm, Claude))
  expect_equal(llm@config@model_name, "claude-sonnet-4-6")
})


# %% build_chat_request_body.ClaudeConfig ----
test_that("Claude request body uses Messages API shape", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
    temperature = 0.2,
    max_tokens = 2048L,
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
  expect_equal(body[["model"]], "claude-sonnet-4-6")
  expect_equal(body[["max_tokens"]], 2048L)
  expect_equal(body[["temperature"]], 0.2)
  expect_equal(body[["system"]], "System prompt.")
  expect_equal(body[["top_p"]], 0.9)
  expect_false("stream" %in% names(body))
  expect_false("response_format" %in% names(body))
  # System message must not appear in messages array
  roles <- vapply(body[["messages"]], function(m) m[["role"]], character(1L))
  expect_false("system" %in% roles)
  expect_equal(roles[1L], "user")
  expect_equal(
    body[["messages"]][[1L]][["content"]][[1L]][["type"]],
    "text"
  )
})


# %% build_chat_request_body.ClaudeConfig thinking ----
test_that("Claude request body includes thinking when configured", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
    thinking_budget_tokens = 2048L,
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
  expect_equal(body[["thinking"]][["type"]], "enabled")
  expect_equal(body[["thinking"]][["budget_tokens"]], 2048L)
})


# %% build_chat_request_body.ClaudeConfig think requires budget ----
test_that("Claude think=TRUE with no configured budget aborts", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
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
    build_chat_request_body(config, state = state, think = TRUE),
    "thinking budget"
  )
})


# %% build_chat_messages.ClaudeConfig tool merging ----
test_that("Claude merges consecutive tool messages into one user block", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
    validate_model = FALSE
  )
  state <- InProcessAgentMemory()
  append_message(
    state,
    SystemMessage(content = "Sys."),
    echo = FALSE,
    verbosity = 0L
  )
  append_message(
    state,
    InputMessage(content = "Question."),
    echo = FALSE,
    verbosity = 0L
  )
  append_message(
    state,
    ToolMessage(
      name = "query_wikipedia",
      tool_call_id = "toolu_1",
      content = "Result A"
    ),
    echo = FALSE,
    verbosity = 0L
  )
  append_message(
    state,
    ToolMessage(
      name = "query_arxiv",
      tool_call_id = "toolu_2",
      content = "Result B"
    ),
    echo = FALSE,
    verbosity = 0L
  )
  messages <- build_chat_messages(config, state)
  # First should be user input (system is stripped)
  roles <- vapply(messages, function(m) m[["role"]], character(1L))
  expect_equal(roles, c("user", "user"))
  # Second user message must contain two tool_result blocks
  tool_results <- messages[[2L]][["content"]]
  expect_length(tool_results, 2L)
  expect_equal(tool_results[[1L]][["type"]], "tool_result")
  expect_equal(tool_results[[1L]][["tool_use_id"]], "toolu_1")
  expect_equal(tool_results[[2L]][["tool_use_id"]], "toolu_2")
})


# %% build_chat_messages.ClaudeConfig tool_call_id required ----
test_that("Claude tool messages require a tool_call_id", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
    validate_model = FALSE
  )
  state <- InProcessAgentMemory()
  append_message(
    state,
    ToolMessage(name = "x", content = "Body"),
    echo = FALSE,
    verbosity = 0L
  )
  expect_error(build_chat_messages(config, state), "tool_call_id")
})


# %% Tool schema shape for Claude ----
test_that("Claude tools use flat {name, description, input_schema} shape", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
    validate_model = FALSE
  )
  tool <- create_tool(
    name = "Search",
    function_name = "query_wikipedia",
    description = "Search Wikipedia",
    parameters = list(
      tool_param("query", "string", "Search query.", required = TRUE),
      tool_param("limit", "integer", "Max pages.", required = FALSE)
    )
  )
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "Hi"),
    echo = FALSE,
    verbosity = 0L
  )
  body <- build_chat_request_body(config, state = state, tools = list(tool))
  expect_length(body[["tools"]], 1L)
  t <- body[["tools"]][[1L]]
  expect_equal(t[["name"]], "query_wikipedia")
  expect_equal(t[["description"]], "Search Wikipedia")
  expect_equal(t[["input_schema"]][["type"]], "object")
  expect_true("query" %in% names(t[["input_schema"]][["properties"]]))
  expect_false("function" %in% names(t))
  expect_false("type" %in% names(t))
})


# %% build_response_format.ClaudeConfig structured output ----
test_that("Claude structured output injects forced synthetic tool", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
    validate_model = FALSE
  )
  output_schema <- schema(
    answer = field("string", "Answer text.")
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
    output_schema = output_schema
  )
  tool_names <- vapply(body[["tools"]], function(t) t[["name"]], character(1L))
  expect_true("respond_with_structured_output" %in% tool_names)
  expect_equal(body[["tool_choice"]][["type"]], "tool")
  expect_equal(
    body[["tool_choice"]][["name"]],
    "respond_with_structured_output"
  )
})


# %% decode_tool_arguments.ClaudeConfig ----
test_that("Claude tool arguments are returned as named list", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
    validate_model = FALSE
  )
  tool_call <- list(
    id = "toolu_1",
    type = "function",
    `function` = list(
      name = "query_wikipedia",
      arguments = list(query = "rtemis", limit = 1L)
    )
  )
  args <- decode_tool_arguments(config, tool_call)
  expect_equal(args[["query"]], "rtemis")
  expect_equal(args[["limit"]], 1L)
})


# %% build_tool_message.ClaudeConfig ----
test_that("Claude tool messages include tool_call_id", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
    validate_model = FALSE
  )
  tool_call <- list(
    id = "toolu_abc",
    type = "function",
    `function` = list(name = "query_wikipedia")
  )
  m <- build_tool_message(
    config,
    tool_call = tool_call,
    tool_name = "query_wikipedia",
    tool_response = "Tool response."
  )
  expect_true(S7_inherits(m, ToolMessage))
  expect_equal(m@tool_call_id, "toolu_abc")
})


# %% parse_chat_response.ClaudeConfig ----
test_that("Claude response parsing extracts text, thinking and tool_use blocks", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
    validate_model = FALSE
  )
  body <- jsonlite::toJSON(
    list(
      id = "msg_123",
      type = "message",
      role = "assistant",
      model = "claude-sonnet-4-6",
      content = list(
        list(type = "thinking", thinking = "Let me think."),
        list(type = "text", text = "Visible answer."),
        list(
          type = "tool_use",
          id = "toolu_1",
          name = "query_wikipedia",
          input = list(query = "rtemis", limit = 1L)
        )
      ),
      stop_reason = "tool_use",
      stop_sequence = NA,
      usage = list(input_tokens = 10L, output_tokens = 5L)
    ),
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )
  resp <- httr2::response(
    status_code = 200,
    headers = list(
      `content-type` = "application/json",
      `request-id` = "req_abc"
    ),
    body = charToRaw(body)
  )
  parsed <- parse_chat_response(config, resp)
  expect_equal(parsed[["content"]], "Visible answer.")
  expect_equal(parsed[["reasoning"]], "Let me think.")
  expect_length(parsed[["tool_calls"]], 1L)
  expect_equal(parsed[["tool_calls"]][[1L]][["id"]], "toolu_1")
  expect_equal(
    parsed[["tool_calls"]][[1L]][["function"]][["name"]],
    "query_wikipedia"
  )
  expect_equal(
    parsed[["tool_calls"]][[1L]][["function"]][["arguments"]][["query"]],
    "rtemis"
  )
  expect_equal(parsed[["metadata"]][["stop_reason"]], "tool_use")
  expect_equal(parsed[["metadata"]][["request_id"]], "req_abc")
  expect_equal(parsed[["metadata"]][["id"]], "msg_123")
  expect_true(!is.null(parsed[["metadata"]][["raw_content"]]))
})


# %% parse_chat_response.ClaudeConfig refusal ----
test_that("Claude refusal stop_reason is surfaced", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
    validate_model = FALSE
  )
  body <- jsonlite::toJSON(
    list(
      id = "msg_refusal",
      type = "message",
      role = "assistant",
      model = "claude-sonnet-4-6",
      content = list(list(type = "text", text = "I cannot help with that.")),
      stop_reason = "refusal"
    ),
    auto_unbox = TRUE
  )
  resp <- httr2::response(
    status_code = 200,
    headers = list(`content-type` = "application/json"),
    body = charToRaw(body)
  )
  parsed <- parse_chat_response(config, resp)
  expect_equal(parsed[["refusal"]], "I cannot help with that.")
})


# %% parse_chat_response.ClaudeConfig inline <think> tags ----
test_that("Claude response parsing extracts reasoning from inline <think> tags", {
  config <- config_Claude(
    model_name = "gemma-4-26b-a4b-it-4bit",
    api_key = "test-key",
    validate_model = FALSE
  )
  body <- jsonlite::toJSON(
    list(
      id = "msg_gemma",
      type = "message",
      role = "assistant",
      model = "gemma-4-26b-a4b-it-4bit",
      content = list(
        list(
          type = "text",
          text = "<think>Weighing options.</think>Final answer."
        )
      ),
      stop_reason = "end_turn"
    ),
    auto_unbox = TRUE
  )
  resp <- httr2::response(
    status_code = 200,
    headers = list(`content-type` = "application/json"),
    body = charToRaw(body)
  )
  parsed <- parse_chat_response(config, resp)
  expect_equal(parsed[["content"]], "Final answer.")
  expect_equal(parsed[["reasoning"]], "Weighing options.")
})


# %% ClaudeMessage preserves raw content ----
test_that("ClaudeMessage preserves raw_content via metadata", {
  raw <- list(list(type = "text", text = "Hi"))
  m <- ClaudeMessage(
    content = "Hi",
    metadata = list(raw_content = raw),
    model_name = "claude-sonnet-4-6"
  )
  expect_equal(m@metadata[["raw_content"]], raw)
  expect_equal(m@metadata[["provider"]], "Anthropic")
})


# %% build_chat_request_body.ClaudeConfig per-call overrides ----
test_that("Claude request body honors per-call overrides", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
    temperature = 0.2,
    max_tokens = 1024L,
    validate_model = FALSE
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
    max_tokens = 256L,
    stop = c("\n\n", "END"),
    top_k = 40L
  )
  expect_equal(body[["temperature"]], 0.9)
  expect_equal(body[["top_p"]], 0.5)
  expect_equal(body[["max_tokens"]], 256L)
  expect_equal(body[["stop_sequences"]], c("\n\n", "END"))
  expect_equal(body[["top_k"]], 40L)
})


# %% build_chat_request_body.ClaudeConfig defaults fall through ----
test_that("Claude request body falls back to config when overrides are NULL", {
  config <- config_Claude(
    model_name = "claude-sonnet-4-6",
    api_key = "test-key",
    temperature = 0.2,
    max_tokens = 1024L,
    validate_model = FALSE
  )
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "Hi"),
    echo = FALSE,
    verbosity = 0L
  )
  body <- build_chat_request_body(config, state = state)
  expect_equal(body[["temperature"]], 0.2)
  expect_equal(body[["max_tokens"]], 1024L)
  expect_false("top_p" %in% names(body))
  expect_false("top_k" %in% names(body))
  expect_false("stop_sequences" %in% names(body))
})
