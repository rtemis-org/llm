# test_apple.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# %% AppleConfig ----
test_that("AppleConfig is an OpenAIConfig fixed to the bridge", {
  config <- config_Apple(validate_model = FALSE)
  expect_true(S7_inherits(config, AppleConfig))
  expect_true(S7_inherits(config, OpenAIConfig))
  expect_identical(config@backend, "apple")
  expect_identical(config@model_name, "afm")
  expect_identical(config@base_url, "http://127.0.0.1:1977/v1")
  expect_false(config@validate_model)
})


test_that("AppleConfig accepts the arguments the bridge honors", {
  config <- config_Apple(
    temperature = 0.7,
    base_url = "http://localhost:2977/v1/",
    timeout = 120,
    extra_headers = list(`X-Test` = "1"),
    extra_body = list(top_k = 40L),
    validate_model = FALSE
  )
  expect_identical(config@temperature, 0.7)
  expect_identical(config@base_url, "http://localhost:2977/v1")
  expect_identical(config@timeout, 120)
  expect_identical(config@extra_headers, list(`X-Test` = "1"))
  expect_identical(config@extra_body, list(top_k = 40L))
})


test_that("AppleConfig rejects invalid arguments", {
  expect_error(config_Apple(temperature = -1, validate_model = FALSE))
  expect_error(config_Apple(base_url = 1977, validate_model = FALSE))
  expect_error(config_Apple(timeout = 0, validate_model = FALSE), "timeout")
  expect_error(config_Apple(validate_model = NA), "validate_model")
  # A non-logical `validate_model` is refused before the bridge is contacted.
  calls <- 0L
  local_mocked_bindings(
    apple_check_available = function(base_url) {
      calls <<- calls + 1L
      invisible(apple_health_body())
    },
    .package = "rtemis.llm"
  )
  expect_error(config_Apple(validate_model = 1), "validate_model")
  expect_identical(calls, 0L)
})


# %% AppleConfig API key ----
test_that("AppleConfig never resolves or sends an API key", {
  # An ambient key is the case that matters: it must not reach the loopback
  # server as a bearer token.
  with_env(c(OPENAI_API_KEY = "sk-must-not-leak"), {
    config <- config_Apple(validate_model = FALSE)
    expect_null(resolve_api_key(config))
    req <- .add_openai_headers(
      httr2::request("http://127.0.0.1:1977/v1/chat/completions"),
      config
    )
    expect_false("Authorization" %in% names(req[["headers"]]))
    control <- config_OpenAI(
      model_name = "local-model",
      base_url = "http://localhost:1234/v1",
      validate_model = FALSE
    )
    req <- .add_openai_headers(
      httr2::request("http://localhost:1234/v1/chat/completions"),
      control
    )
    expect_true("Authorization" %in% names(req[["headers"]]))
  })
})


# %% .openai_provider_name.AppleConfig ----
test_that("AppleConfig names its provider", {
  config <- config_Apple(validate_model = FALSE)
  expect_identical(.openai_provider_name(config), "Apple Foundation Models")
  message <- create_llm_message(config, content = "hi")
  expect_identical(message@metadata[["provider"]], "Apple Foundation Models")
})


# %% as_list / repr.AppleConfig ----
test_that("AppleConfig as_list and repr carry only the bridge's fields", {
  config <- config_Apple(validate_model = FALSE)
  fields <- as_list(config)
  expect_named(
    fields,
    c(
      "model_name",
      "temperature",
      "backend",
      "base_url",
      "timeout",
      "extra_headers",
      "extra_body",
      "validate_model"
    )
  )
  out <- repr(config, output_type = "plain")
  expect_true(grepl("<AppleConfig>", out, fixed = TRUE))
  expect_false(grepl("api_key", out, fixed = TRUE))
})


# %% AppleConfig validate_model ----
test_that("AppleConfig validate_model reads the bridge's health", {
  calls <- character()
  local_mocked_bindings(
    apple_check_available = function(base_url) {
      calls <<- c(calls, base_url)
      invisible(apple_health_body())
    },
    .package = "rtemis.llm"
  )
  config <- config_Apple(base_url = "http://localhost:2977/v1")
  expect_true(config@validate_model)
  expect_identical(calls, "http://localhost:2977/v1")
  config_Apple(validate_model = FALSE)
  expect_length(calls, 1L)
})


# %% create_Apple() ----
test_that("create_Apple works", {
  llm <- create_Apple(
    system_prompt = "You are a meticulous research assistant.",
    name = "afm-test",
    validate_model = FALSE
  )
  expect_true(S7_inherits(llm, Apple))
  expect_true(S7_inherits(llm, OpenAI))
  expect_true(S7_inherits(llm@config, AppleConfig))
  expect_identical(llm@name, "afm-test")
  out <- repr(llm, output_type = "plain")
  expect_true(grepl("<Apple>", out, fixed = TRUE))
  expect_false(grepl("<OpenAI>", out, fixed = TRUE))
}) # /create_Apple


test_that("Apple rejects a plain OpenAIConfig", {
  expect_error(
    Apple(
      config = config_OpenAI(
        model_name = "local-model",
        base_url = "http://localhost:1234/v1",
        validate_model = FALSE
      ),
      system_prompt = "hi"
    )
  )
})


# %% build_chat_request_body.AppleConfig ----
test_that("Apple request body uses the OpenAI chat completions shape", {
  config <- config_Apple(temperature = 0.2, validate_model = FALSE)
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "Hi"),
    echo = FALSE,
    verbosity = 0L
  )
  output_schema <- schema(
    "answer",
    field("city", "Capital city", type = "string")
  )
  body <- build_chat_request_body(
    config,
    state = state,
    output_schema = output_schema,
    use_tools = FALSE
  )
  expect_identical(body[["model"]], "afm")
  expect_false(body[["stream"]])
  expect_identical(body[["temperature"]], 0.2)
  expect_identical(body[["response_format"]][["type"]], "json_schema")
})


# %% .apple_health_url() ----
test_that("health URL sits beside /v1", {
  expect_identical(
    .apple_health_url("http://127.0.0.1:1977/v1"),
    "http://127.0.0.1:1977/health"
  )
  expect_identical(
    .apple_health_url("http://127.0.0.1:1977/v1/"),
    "http://127.0.0.1:1977/health"
  )
  expect_identical(
    .apple_health_url("http://localhost:1977"),
    "http://localhost:1977/health"
  )
})


# %% apple_check_available() ----
test_that("apple_check_available returns the health when available", {
  local_mocked_bindings(
    apple_health = function(base_url) apple_health_body(),
    .package = "rtemis.llm"
  )
  health <- apple_check_available()
  expect_identical(health[["model"]][["availability"]], "available")
  expect_identical(health[["model"]][["context_window"]], 8192L)
})


test_that("apple_check_available says what to do for each reason", {
  reasons <- c(
    appleIntelligenceNotEnabled = "System Settings",
    deviceNotEligible = "Apple silicon",
    modelNotReady = "downloaded",
    somethingNew = "rtemis-afm status"
  )
  for (reason in names(reasons)) {
    local_mocked_bindings(
      apple_health = function(base_url) {
        apple_health_body(availability = "unavailable", reason = reason)
      },
      .package = "rtemis.llm"
    )
    expect_error(apple_check_available(), reason, fixed = TRUE)
    expect_error(apple_check_available(), reasons[[reason]], fixed = TRUE)
  }
})


test_that("apple_check_available says how to start a bridge that is not running", {
  local_mocked_bindings(
    apple_health = function(base_url) {
      stop(errorCondition("Failed to connect", class = "httr2_failure"))
    },
    .package = "rtemis.llm"
  )
  expect_error(
    apple_check_available(),
    "Start it with `rtemis-afm`",
    fixed = TRUE
  )
})


test_that("apple_health rejects a server that is not rtemis-afm", {
  httr2::local_mocked_responses(
    function(req) httr2::response(status_code = 404L)
  )
  expect_error(
    apple_health("http://127.0.0.1:1976/v1"),
    "did not answer /health as rtemis-afm does (HTTP 404)",
    fixed = TRUE
  )
  httr2::local_mocked_responses(
    function(req) {
      httr2::response_json(status_code = 200L, body = list(status = "ok"))
    }
  )
  expect_error(apple_health(), "did not answer /health", fixed = TRUE)
})


test_that("apple_health parses the bridge's health body", {
  httr2::local_mocked_responses(
    function(req) {
      expect_identical(req[["url"]], "http://127.0.0.1:1977/health")
      httr2::response_json(status_code = 200L, body = apple_health_body())
    }
  )
  health <- apple_health()
  expect_identical(health[["status"]], "ok")
  expect_identical(health[["model"]][["id"]], "afm")
  expect_identical(health[["model"]][["name"]], "AFM 3 Core Advanced")
})


# %% Live: generate ----
test_that("Apple generate completes a chat against the bridge", {
  skip_if_apple_unavailable()
  llm <- create_Apple(system_prompt = "You answer with one word.")
  res <- generate(llm, "Reply with exactly one word: pong", verbosity = 0L)
  expect_true(S7_inherits(res, OpenAIMessage))
  expect_true(nzchar(res@content))
  expect_identical(res@metadata[["provider"]], "Apple Foundation Models")
  expect_identical(res@model_name, "afm")
})


# %% Live: structured output ----
test_that("Apple generate returns valid structured output against the bridge", {
  skip_if_apple_unavailable()
  output_schema <- schema(
    "answer",
    field("city", "Capital city", type = "string"),
    field("country", "Country", type = "string")
  )
  llm <- create_Apple(output_schema = output_schema)
  res <- generate(llm, "What is the capital of France?", verbosity = 0L)
  expect_identical(validation_results(res)@status, "valid")
  parsed <- jsonlite::fromJSON(res@content)
  expect_identical(parsed[["city"]], "Paris")
})


# %% Live: agent tool round trip ----
test_that("Apple agent completes a tool round trip against the bridge", {
  skip_if_apple_unavailable()
  agent <- create_agent(
    config_Apple(),
    tools = list(tool_datetime),
    system_prompt = "You are a helpful assistant. Use tools when needed.",
    verbosity = 0L
  )
  generate(agent, "What is the current date? Use the tool.", verbosity = 0L)
  messages <- get_messages(agent)
  roles <- vapply(messages, function(m) class(m)[1], character(1))
  expect_true(any(grepl("ToolMessage", roles, fixed = TRUE)))
  expect_true(nzchar(messages[[length(messages)]]@content))
})
