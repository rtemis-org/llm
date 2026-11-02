# test_anthropic.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# A 200 response with a JSON body, as the Messages API returns it.
anthropic_response <- function(body) {
  httr2::response(
    status_code = 200,
    headers = list(
      `content-type` = "application/json",
      `request-id` = "req_test"
    ),
    body = charToRaw(jsonlite::toJSON(
      body,
      auto_unbox = TRUE,
      null = "null",
      na = "null"
    ))
  )
}


# %% AnthropicConfig ----
test_that("AnthropicConfig class and config_Anthropic work", {
  config <- config_Anthropic(
    model_name = "claude-sonnet-4-6",
    temperature = 0.4,
    api_key = "test-key",
    max_tokens = 1024L,
    validate_model = FALSE
  )
  expect_true(S7_inherits(config, AnthropicConfig))
  expect_equal(config@model_name, "claude-sonnet-4-6")
  expect_equal(config@max_tokens, 1024L)
  expect_equal(config@backend, "anthropic")
  expect_equal(config@base_url, "https://api.anthropic.com/v1")
})


# %% config_Anthropic API key handling ----
test_that("config_Anthropic aborts when no API key is resolvable", {
  with_env(c(RTEMIS_LLM_ANTHROPIC_EMPTY_TEST_KEY = NA), {
    config <- config_Anthropic(
      model_name = "claude-sonnet-4-6",
      api_key_env = "RTEMIS_LLM_ANTHROPIC_EMPTY_TEST_KEY",
      validate_model = FALSE
    )
    expect_error(resolve_anthropic_api_key(config), "No Anthropic API key")
  })
})


# %% config_Anthropic max_tokens validation ----
test_that("config_Anthropic rejects non-positive max_tokens", {
  expect_error(
    config_Anthropic(
      model_name = "claude-sonnet-4-6",
      api_key = "test-key",
      max_tokens = 0,
      validate_model = FALSE
    ),
    "max_tokens"
  )
})


# %% config_Anthropic thinking budget validation ----
test_that("config_Anthropic rejects thinking budgets below the minimum", {
  expect_error(
    config_Anthropic(
      model_name = "claude-sonnet-4-6",
      api_key = "test-key",
      thinking_budget_tokens = 256L,
      validate_model = FALSE
    ),
    "thinking_budget_tokens"
  )
})


# %% repr.AnthropicConfig ----
test_that("AnthropicConfig repr redacts API keys", {
  config <- config_Anthropic(
    model_name = "claude-sonnet-4-6",
    api_key = "anthropic-secret",
    validate_model = FALSE
  )
  out <- repr(config, output_type = "plain")
  expect_true(grepl("<redacted>", out, fixed = TRUE))
  expect_false(grepl("anthropic-secret", out, fixed = TRUE))
})


# %% create_Anthropic() ----
test_that("create_Anthropic works", {
  llm <- create_Anthropic(
    model_name = "claude-sonnet-4-6",
    system_prompt = "You are a meticulous research assistant.",
    api_key = "test-key",
    validate_model = FALSE
  )
  expect_true(S7_inherits(llm, Anthropic))
  expect_equal(llm@config@model_name, "claude-sonnet-4-6")
})


# %% build_chat_request_body.AnthropicConfig ----
test_that("Anthropic request body uses Messages API shape", {
  config <- config_Anthropic(
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


# %% build_chat_request_body.AnthropicConfig thinking ----
test_that("Anthropic request body includes thinking when configured", {
  config <- config_Anthropic(
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


# %% build_chat_request_body.AnthropicConfig think requires budget ----
test_that("Anthropic think=TRUE with no budget aborts on budget-only models", {
  config <- config_Anthropic(
    model_name = "claude-haiku-4-5",
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
    "thinking_budget_tokens",
    class = "rtemis_llm_unsupported_parameter"
  )
})


# %% build_chat_messages.AnthropicConfig tool merging ----
test_that("Anthropic merges consecutive tool messages into one user block", {
  config <- config_Anthropic(
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


# %% build_chat_messages.AnthropicConfig tool_call_id required ----
test_that("Anthropic tool messages require a tool_call_id", {
  config <- config_Anthropic(
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


# %% Tool schema shape for Anthropic ----
test_that("Anthropic tools use flat {name, description, input_schema} shape", {
  config <- config_Anthropic(
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


# %% build_response_format.AnthropicConfig structured output ----
test_that("Anthropic structured output injects forced synthetic tool", {
  config <- config_Anthropic(
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


# %% decode_tool_arguments.AnthropicConfig ----
test_that("Anthropic tool arguments are returned as named list", {
  config <- config_Anthropic(
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


# %% build_tool_message.AnthropicConfig ----
test_that("Anthropic tool messages include tool_call_id", {
  config <- config_Anthropic(
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


# %% parse_chat_response.AnthropicConfig ----
test_that("Anthropic response parsing extracts text, thinking and tool_use blocks", {
  config <- config_Anthropic(
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


# %% parse_chat_response.AnthropicConfig refusal ----
test_that("Anthropic refusal is a failed call carrying stop_details", {
  config <- config_Anthropic(
    model_name = "claude-opus-5-5",
    api_key = "test-key",
    validate_model = FALSE
  )
  resp <- anthropic_response(list(
    id = "msg_refusal",
    type = "message",
    role = "assistant",
    model = "claude-opus-5-5",
    content = list(),
    stop_reason = "refusal",
    stop_details = list(
      type = "refusal",
      category = "bio",
      explanation = "Declined by a safety classifier."
    )
  ))
  cnd <- expect_error(
    parse_chat_response(config, resp),
    "declined the request",
    class = "rtemis_llm_refusal"
  )
  expect_equal(cnd[["category"]], "bio")
  expect_equal(cnd[["explanation"]], "Declined by a safety classifier.")
  expect_equal(cnd[["model"]], "claude-opus-5-5")
  expect_equal(cnd[["request_id"]], "req_test")
})


# %% parse_chat_response.AnthropicConfig inline <think> tags ----
test_that("Anthropic response parsing extracts reasoning from inline <think> tags", {
  config <- config_Anthropic(
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


# %% AnthropicMessage preserves raw content ----
test_that("AnthropicMessage preserves raw_content via metadata", {
  raw <- list(list(type = "text", text = "Hi"))
  m <- AnthropicMessage(
    content = "Hi",
    metadata = list(raw_content = raw),
    model_name = "claude-sonnet-4-6"
  )
  expect_equal(m@metadata[["raw_content"]], raw)
  expect_equal(m@metadata[["provider"]], "Anthropic")
})


# %% build_chat_request_body.AnthropicConfig per-call overrides ----
test_that("Anthropic request body honors per-call overrides", {
  config <- config_Anthropic(
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
    max_tokens = 256L,
    stop = c("\n\n", "END"),
    top_k = 40L
  )
  expect_equal(body[["temperature"]], 0.9)
  expect_false("top_p" %in% names(body))
  expect_equal(body[["max_tokens"]], 256L)
  expect_equal(body[["stop_sequences"]], c("\n\n", "END"))
  expect_equal(body[["top_k"]], 40L)
})


# %% build_chat_request_body.AnthropicConfig defaults fall through ----
test_that("Anthropic request body falls back to config when overrides are NULL", {
  config <- config_Anthropic(
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


# --- Model request rules ---------------------------------------------------------------------------
# spec: llm/anthropic-adapter#request-shape

# Sends one generate() call through the real builder, with the HTTP call
# replaced; returns the request body as it goes on the wire, the request
# headers, and the result.
anthropic_dry_run <- function(
  llm,
  prompt = "Score the pair.",
  response = NULL,
  ...
) {
  captured <- NULL
  response <- response %||%
    list(
      id = "msg_dry",
      type = "message",
      role = "assistant",
      model = llm@config@model_name,
      content = list(list(type = "text", text = "{\"score\":1}")),
      stop_reason = "end_turn"
    )
  testthat::local_mocked_bindings(
    .req_perform_status = function(req, verbosity = 1L) {
      captured <<- req
      anthropic_response(response)
    }
  )
  result <- generate(llm, prompt, verbosity = 0L, ...)
  wire <- do.call(
    jsonlite::toJSON,
    c(list(captured[["body"]][["data"]]), captured[["body"]][["params"]])
  )
  list(
    body = jsonlite::fromJSON(wire, simplifyVector = FALSE),
    headers = captured[["headers"]],
    result = result
  )
}

dry_schema <- function() {
  schema(
    score = field(name = "score", type = "integer", description = "Score."),
    label = field(
      name = "label",
      type = "string",
      description = "Label.",
      enum = c("low", "high")
    )
  )
}

dry_llm <- function(model_name, ...) {
  create_Anthropic(
    model_name = model_name,
    system_prompt = "Score.",
    output_schema = dry_schema(),
    api_key = "test-key",
    validate_model = FALSE,
    ...
  )
}

SAMPLING_FIELDS <- c("temperature", "top_p", "top_k")

# Models with structured outputs (claude-api, Structured Outputs).
OUTPUT_CONFIG_MODELS <- c(
  "claude-fable-5-1",
  "claude-mythos-5-1",
  "claude-fable-5",
  "claude-opus-5-5",
  "claude-opus-5",
  "claude-opus-4-8",
  "claude-sonnet-5-5",
  "claude-sonnet-5",
  "claude-haiku-5-5",
  "claude-opus-4-5",
  "claude-opus-4-1",
  "claude-haiku-4-5"
)
FORCED_TOOL_MODELS <- c(
  "claude-opus-4-7",
  "claude-opus-4-6",
  "claude-sonnet-4-6",
  "claude-sonnet-4-5",
  "claude-sonnet-4-0",
  "claude-opus-4-0"
)


# %% .anthropic_model_id() ----
test_that("Anthropic model names normalize to their family id", {
  expect_equal(
    .anthropic_model_id("claude-haiku-4-5-20251001"),
    "claude-haiku-4-5"
  )
  expect_equal(
    .anthropic_model_id("claude-opus-4-5@20251101"),
    "claude-opus-4-5"
  )
  expect_equal(
    .anthropic_model_id("anthropic.claude-opus-5-5"),
    "claude-opus-5-5"
  )
  expect_equal(
    .anthropic_model_id("us.anthropic.claude-sonnet-4-5-20250929-v1:0"),
    "claude-sonnet-4-5"
  )
  expect_equal(
    .anthropic_model_id("claude-sonnet-4-20250514"),
    "claude-sonnet-4"
  )
  expect_false(
    .anthropic_model_profile("claude-opus-5")[["effort_default"]] == "medium"
  )
  expect_equal(
    .anthropic_model_profile("claude-opus-5-5")[["effort_default"]],
    "medium"
  )
  expect_true(.anthropic_model_profile("claude-sonnet-4-20250514")[["known"]])
  expect_false(.anthropic_model_profile("gemma-4-26b")[["known"]])
})


# %% default request per model family ----
test_that("default structured requests carry no field the model rejects", {
  for (model in c(OUTPUT_CONFIG_MODELS, FORCED_TOOL_MODELS)) {
    run <- anthropic_dry_run(dry_llm(model))
    body <- run[["body"]]
    expect_false(any(SAMPLING_FIELDS %in% names(body)), info = model)
    expect_false("thinking" %in% names(body), info = model)
    expect_equal(body[["model"]], model, info = model)
    if (model %in% OUTPUT_CONFIG_MODELS) {
      expect_false("tool_choice" %in% names(body), info = model)
      expect_false("tools" %in% names(body), info = model)
      format <- body[["output_config"]][["format"]]
      expect_equal(format[["type"]], "json_schema", info = model)
      expect_false(format[["schema"]][["additionalProperties"]], info = model)
      expect_equal(
        unlist(format[["schema"]][["required"]]),
        c("score", "label"),
        info = model
      )
      expect_equal(
        unlist(format[["schema"]][["properties"]][["label"]][["enum"]]),
        c("low", "high"),
        info = model
      )
    } else {
      expect_equal(body[["tool_choice"]][["type"]], "tool", info = model)
      expect_equal(
        body[["tools"]][[1L]][["name"]],
        "respond_with_structured_output",
        info = model
      )
      expect_false("output_config" %in% names(body), info = model)
    }
    expect_equal(run[["result"]]@content, "{\"score\":1}", info = model)
  }
})


# %% Claude Opus 5.5 ----
test_that("Claude Opus 5.5 requests follow its rules", {
  run <- anthropic_dry_run(dry_llm("claude-opus-5-5", effort = "low"))
  expect_equal(run[["body"]][["output_config"]][["effort"]], "low")
  expect_equal(
    names(run[["body"]][["output_config"]]),
    c("format", "effort")
  )
  run <- anthropic_dry_run(dry_llm("claude-opus-5-5"), think = TRUE)
  expect_equal(run[["body"]][["thinking"]], list(type = "adaptive"))
  run <- anthropic_dry_run(dry_llm("claude-opus-5-5"), effort = "xhigh")
  expect_equal(run[["body"]][["output_config"]][["effort"]], "xhigh")
  expect_error(
    config_Anthropic("claude-opus-5-5", temperature = 0, api_key = "k"),
    "does not accept `temperature`",
    class = "rtemis_llm_unsupported_parameter"
  )
  expect_error(
    anthropic_dry_run(dry_llm("claude-opus-5-5"), top_k = 5L),
    "top_k",
    class = "rtemis_llm_unsupported_parameter"
  )
  expect_error(
    config_Anthropic("claude-opus-5-5", think = FALSE, api_key = "k"),
    "always thinks",
    class = "rtemis_llm_unsupported_parameter"
  )
  expect_error(
    config_Anthropic(
      "claude-opus-5-5",
      thinking_budget_tokens = 2048L,
      api_key = "k"
    ),
    "thinking_budget_tokens",
    class = "rtemis_llm_unsupported_parameter"
  )
})


# %% Claude Opus 5.5 with tools ----
test_that("user tools combine with output_config through automatic tool choice", {
  config <- config_Anthropic("claude-opus-5-5", api_key = "test-key")
  tool <- create_tool(
    name = "Search",
    function_name = "query_wikipedia",
    description = "Search Wikipedia",
    parameters = list(
      tool_param("query", "string", "Search query.", required = TRUE)
    )
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
    tools = list(tool),
    output_schema = dry_schema()
  )
  expect_equal(
    vapply(body[["tools"]], function(t) t[["name"]], character(1L)),
    "query_wikipedia"
  )
  expect_false("tool_choice" %in% names(body))
  expect_equal(body[["output_config"]][["format"]][["type"]], "json_schema")
})


# %% thinking off ----
test_that("think = FALSE maps to each model's off form", {
  off <- function(model, ...) {
    anthropic_dry_run(dry_llm(model, think = FALSE, ...))[["body"]][[
      "thinking"
    ]]
  }
  expect_equal(off("claude-opus-5"), list(type = "disabled"))
  expect_equal(off("claude-haiku-5-5"), list(type = "disabled"))
  expect_equal(off("claude-sonnet-5"), list(type = "disabled"))
  expect_equal(off("claude-sonnet-5-5"), list(type = "between_tools"))
  expect_null(off("claude-opus-4-8"))
  expect_null(off("claude-opus-4-6"))
  expect_null(off("claude-haiku-4-5"))
  expect_equal(off("claude-opus-5", effort = "high"), list(type = "disabled"))
  for (model in c("claude-opus-5", "claude-sonnet-5-5", "claude-haiku-5-5")) {
    expect_error(
      config_Anthropic(model, think = FALSE, effort = "xhigh", api_key = "k"),
      "only at effort \"high\" or below",
      class = "rtemis_llm_unsupported_parameter",
      info = model
    )
  }
  for (model in c("claude-fable-5-1", "claude-fable-5", "claude-opus-5-5")) {
    expect_error(
      config_Anthropic(model, think = FALSE, api_key = "k"),
      "always thinks",
      info = model
    )
  }
})


# %% thinking on ----
test_that("think = TRUE sends adaptive thinking or the configured budget", {
  on <- function(model, ...) {
    anthropic_dry_run(
      create_Anthropic(model, api_key = "k", ...),
      think = TRUE
    )[["body"]][["thinking"]]
  }
  expect_equal(on("claude-opus-4-8"), list(type = "adaptive"))
  expect_equal(on("claude-sonnet-4-6"), list(type = "adaptive"))
  expect_equal(
    on("claude-sonnet-4-6", thinking_budget_tokens = 2048L),
    list(type = "enabled", budget_tokens = 2048L)
  )
  expect_equal(
    on("claude-haiku-4-5", thinking_budget_tokens = 2048L),
    list(type = "enabled", budget_tokens = 2048L)
  )
  expect_error(
    config_Anthropic(
      "claude-haiku-4-5",
      thinking_budget_tokens = 4096L,
      max_tokens = 4096L,
      api_key = "k"
    ),
    "less than `max_tokens`"
  )
  expect_error(
    anthropic_dry_run(dry_llm("claude-sonnet-4-6"), think = TRUE),
    "forced tool call",
    class = "rtemis_llm_unsupported_parameter"
  )
})


# %% effort ----
test_that("effort is checked against the model's levels", {
  expect_error(
    config_Anthropic("claude-haiku-4-5", effort = "low", api_key = "k"),
    "does not accept `effort`"
  )
  expect_error(
    config_Anthropic("claude-sonnet-4-6", effort = "xhigh", api_key = "k"),
    "\"low\", \"medium\", \"high\", \"max\""
  )
  expect_error(
    config_Anthropic("claude-opus-4-5", effort = "max", api_key = "k"),
    "accepts `effort`"
  )
  expect_error(
    config_Anthropic("claude-opus-5-5", effort = "minimal", api_key = "k"),
    "`effort` must be one of"
  )
  run <- anthropic_dry_run(
    create_Anthropic("claude-sonnet-4-6", api_key = "k", effort = "max")
  )
  expect_equal(run[["body"]][["output_config"]], list(effort = "max"))
})


# %% sampling ----
test_that("sampling parameters follow the model's rules", {
  expect_error(
    config_Anthropic("claude-sonnet-5-5", temperature = 0, api_key = "k"),
    "only the default value of `temperature`"
  )
  run <- anthropic_dry_run(
    create_Anthropic("claude-haiku-5-5", temperature = 1, api_key = "k")
  )
  expect_false("temperature" %in% names(run[["body"]]))
  expect_error(
    anthropic_dry_run(
      create_Anthropic("claude-sonnet-4-5", temperature = 0.2, api_key = "k"),
      top_p = 0.9
    ),
    "`temperature` or `top_p`, not both"
  )
  run <- anthropic_dry_run(
    create_Anthropic("claude-sonnet-4-5", temperature = 0.2, api_key = "k")
  )
  expect_equal(run[["body"]][["temperature"]], 0.2)
})


# %% fallbacks ----
test_that("server-side fallbacks are opt-in and carry their beta header", {
  run <- anthropic_dry_run(dry_llm("claude-opus-5-5"))
  expect_false("fallbacks" %in% names(run[["body"]]))
  expect_null(run[["headers"]][["anthropic-beta"]])
  expect_false("fallback_served" %in% names(run[["result"]]@metadata))
  run <- anthropic_dry_run(dry_llm("claude-opus-5-5", fallbacks = "default"))
  expect_equal(run[["body"]][["fallbacks"]], "default")
  expect_equal(
    run[["headers"]][["anthropic-beta"]],
    "server-side-fallback-2026-07-01"
  )
  expect_false(run[["result"]]@metadata[["fallback_served"]])
  run <- anthropic_dry_run(
    dry_llm(
      "claude-opus-5-5",
      fallbacks = "claude-opus-5",
      anthropic_beta = "other-beta"
    )
  )
  expect_equal(
    run[["body"]][["fallbacks"]],
    list(list(model = "claude-opus-5"))
  )
  expect_equal(
    run[["headers"]][["anthropic-beta"]],
    "other-beta,server-side-fallback-2026-06-01"
  )
  expect_error(
    config_Anthropic(
      "claude-sonnet-5-5",
      fallbacks = "claude-sonnet-5",
      api_key = "k"
    ),
    "only `fallbacks = \"default\"`"
  )
  expect_error(
    config_Anthropic("claude-haiku-5-5", fallbacks = "default", api_key = "k"),
    "no server-side fallback"
  )
  expect_error(
    config_Anthropic(
      "claude-opus-5-5",
      fallbacks = c("default", "x"),
      api_key = "k"
    ),
    "`fallbacks` must be"
  )
})


# %% prefill ----
test_that("a trailing assistant message aborts on models that reject prefill", {
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "Hi"),
    echo = FALSE,
    verbosity = 0L
  )
  append_message(
    state,
    AnthropicMessage(content = "{", model_name = "claude-opus-5-5"),
    echo = FALSE,
    verbosity = 0L
  )
  expect_error(
    build_chat_request_body(
      config_Anthropic("claude-opus-5-5", api_key = "k"),
      state = state
    ),
    "prefill",
    class = "rtemis_llm_unsupported_parameter"
  )
  body <- build_chat_request_body(
    config_Anthropic("claude-haiku-4-5", api_key = "k"),
    state = state
  )
  expect_equal(body[["messages"]][[2L]][["role"]], "assistant")
})


# %% unknown model names ----
test_that("unknown model names pass configured fields through", {
  expect_no_warning(
    config <- config_Anthropic(
      "gemma-4-26b-a4b-it-4bit",
      temperature = 0.2,
      think = TRUE,
      effort = "high",
      api_key = "k"
    )
  )
  run <- anthropic_dry_run(
    Anthropic(
      config = config,
      system_prompt = "Score.",
      output_schema = dry_schema()
    ),
    top_p = 0.5
  )
  body <- run[["body"]]
  expect_equal(body[["temperature"]], 0.2)
  expect_equal(body[["top_p"]], 0.5)
  expect_equal(body[["thinking"]], list(type = "adaptive"))
  expect_equal(body[["output_config"]], list(effort = "high"))
  expect_equal(body[["tool_choice"]][["type"]], "tool")
  expect_warning(
    config_Anthropic("claude-opus-9", api_key = "k"),
    "not in rtemis.llm's Anthropic model table"
  )
})


# %% Models API capabilities ----
test_that("validate_model reads capabilities and overlays the table", {
  caps <- list(
    structured_outputs = list(supported = TRUE),
    thinking = list(
      supported = TRUE,
      types = list(
        enabled = list(supported = FALSE),
        adaptive = list(supported = TRUE)
      )
    ),
    effort = list(
      supported = TRUE,
      low = list(supported = TRUE),
      medium = list(supported = TRUE),
      high = list(supported = TRUE),
      xhigh = list(supported = TRUE),
      max = list(supported = TRUE)
    )
  )
  requested <- NULL
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      requested <<- req[["url"]]
      anthropic_response(list(
        id = "claude-opus-4-7",
        type = "model",
        capabilities = caps
      ))
    },
    .package = "httr2"
  )
  expect_warning(
    config <- config_Anthropic(
      "claude-opus-4-7",
      api_key = "k",
      validate_model = TRUE
    ),
    "structured_outputs"
  )
  expect_equal(requested, "https://api.anthropic.com/v1/models/claude-opus-4-7")
  expect_equal(config@capabilities, caps)
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
    output_schema = dry_schema()
  )
  expect_equal(body[["output_config"]][["format"]][["type"]], "json_schema")
  expect_false("tool_choice" %in% names(body))
  # An unknown name takes the reported capabilities without a warning.
  expect_no_warning(
    config <- config_Anthropic(
      "claude-opus-4-7-preview",
      api_key = "k",
      validate_model = TRUE
    )
  )
  expect_true(.anthropic_model_profile(
    config@model_name,
    config@capabilities
  )[["structured_outputs"]])
})


# %% response parsing ----
test_that("the answer is the text block after thinking blocks", {
  run <- anthropic_dry_run(
    dry_llm("claude-opus-5-5"),
    response = list(
      id = "msg_think",
      type = "message",
      role = "assistant",
      model = "claude-opus-5-5",
      content = list(
        list(type = "thinking", thinking = "", signature = "sig"),
        list(type = "text", text = "{\"score\":2,\"label\":\"high\"}")
      ),
      stop_reason = "end_turn"
    )
  )
  message <- run[["result"]]
  expect_equal(message@content, "{\"score\":2,\"label\":\"high\"}")
  expect_null(message@reasoning)
  expect_equal(validation_results(message)@status, "valid")
  expect_equal(
    message@metadata[["raw_content"]][[1L]][["signature"]],
    "sig"
  )
})

test_that("a max_tokens stop warns and keeps the truncated text", {
  expect_warning(
    run <- anthropic_dry_run(
      create_Anthropic("claude-opus-5-5", api_key = "k"),
      response = list(
        id = "msg_cut",
        type = "message",
        role = "assistant",
        model = "claude-opus-5-5",
        content = list(
          list(type = "thinking", thinking = "", signature = "sig"),
          list(type = "text", text = "The answer is")
        ),
        stop_reason = "max_tokens"
      )
    ),
    "stopped at `max_tokens`"
  )
  expect_equal(run[["result"]]@content, "The answer is")
  expect_equal(run[["result"]]@metadata[["stop_reason"]], "max_tokens")
})

test_that("a refusal fails generate() with the stop_details category", {
  expect_error(
    anthropic_dry_run(
      dry_llm("claude-opus-5-5"),
      response = list(
        id = "msg_ref",
        type = "message",
        role = "assistant",
        model = "claude-opus-5-5",
        content = list(),
        stop_reason = "refusal",
        stop_details = list(type = "refusal", category = "cyber")
      )
    ),
    "category \"cyber\"",
    class = "rtemis_llm_refusal"
  )
})


# %% live: Claude Opus 5.5 structured output ----
test_that("Claude Opus 5.5 returns a valid structured answer (live)", {
  skip_on_cran()
  skip_if(
    !nzchar(Sys.getenv("ANTHROPIC_API_KEY")),
    "ANTHROPIC_API_KEY is not set"
  )
  llm <- create_Anthropic(
    model_name = "claude-opus-5-5",
    system_prompt = "Rate how well the course objective matches the session.",
    output_schema = schema(
      score = field(
        name = "score",
        type = "integer",
        description = "Match from 0 (none) to 2 (full)."
      ),
      label = field(
        name = "label",
        type = "string",
        description = "Match category.",
        enum = c("none", "partial", "full")
      )
    ),
    effort = "low"
  )
  message <- generate(
    llm,
    paste(
      "Objective: Identify sources of bias in observational studies.",
      "Session: Review common threats to validity (confounding, measurement error bias)."
    ),
    verbosity = 0L
  )
  expect_equal(validation_results(message)@status, "valid")
  answer <- jsonlite::fromJSON(message@content)
  expect_true(answer[["score"]] %in% 0:2)
})
