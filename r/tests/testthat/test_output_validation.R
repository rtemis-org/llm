# Offline response fixtures exercise the real provider parsers and generate methods.
validation_fixture <- function(content, backend = "openai", tool_calls = NULL) {
  body <- switch(
    backend,
    openai = list(
      choices = list(list(
        message = list(
          role = "assistant",
          content = content,
          tool_calls = tool_calls
        ),
        finish_reason = "stop"
      ))
    ),
    ollama = list(
      message = list(
        role = "assistant",
        content = content,
        tool_calls = tool_calls
      ),
      done = TRUE
    ),
    anthropic = list(
      content = list(list(type = "text", text = content)),
      stop_reason = "end_turn",
      usage = list(input_tokens = 1L, output_tokens = 1L)
    )
  )
  httr2::response(
    200L,
    headers = list(`content-type` = "application/json"),
    body = charToRaw(as.character(jsonlite::toJSON(
      body,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )))
  )
}

validation_model <- function(backend = "openai", sch = NULL) {
  switch(
    backend,
    openai = OpenAI(
      config = config_OpenAI("test", validate_model = FALSE),
      system_prompt = "test",
      output_schema = sch
    ),
    ollama = Ollama(
      config = config_Ollama("test"),
      system_prompt = "test",
      output_schema = sch
    ),
    anthropic = Anthropic(
      config = config_Anthropic(
        "test",
        api_key = "test",
        validate_model = FALSE
      ),
      system_prompt = "test",
      output_schema = sch
    )
  )
}

test_that("validation preserves raw JSON and reports type, required, enum and parse failures", {
  sch <- schema(
    "Count",
    field("n", type = "integer"),
    field("label", enum = c("yes", "no"))
  )
  input <- c(
    good = '{"n":10,"label":"yes"}',
    quoted = '{"n":"10","label":"maybe"}',
    missing = '{"label":"yes"}',
    fenced = '```json\n{"n":10,"label":"yes"}\n```',
    prose = 'Here is {"n":10,"label":"yes"}',
    truncated = '{"n":',
    empty = '',
    missing_response = NA_character_
  )
  report <- validate_output(input, sch)
  expect_identical(report@output, input)
  expect_identical(report@status, c("valid", rep("invalid", 6L), "unavailable"))
  expect_setequal(
    report@issues[["keyword"]],
    c("type", "enum", "required", "parse")
  )
  expect_true(all(c("/n", "/label") %in% report@issues[["path"]]))
  expect_equal(sum(report@issues[["index"]] == 2L), 2L)
  expect_output(print(report), "invalid 6")
  expect_length(validate_output(character(), sch)@status, 0L)
  expect_null(validation_results(input))
  expect_error(validate_output(1L, sch))
})

test_that("validation follows JSON types and the user schema, without implicit constraints", {
  sch <- schema(
    "Types",
    field("n", type = "integer"),
    field("o", type = "object"),
    field("a", type = "array", items = "string"),
    field("b", type = "boolean"),
    field("optional", required = FALSE)
  )
  input <- c(
    '{"n":10.0,"o":{},"a":[],"b":true,"extra":1}',
    '{"n":1e1,"o":{"arbitrary":1},"a":["anything"],"b":false}',
    '{"n":10.5,"o":{},"a":[],"b":true}',
    '{"n":10,"o":[],"a":{},"b":"true"}',
    '{"n":10,"o":{},"a":[],"b":true,"optional":null}',
    '[]',
    'null',
    '10',
    '"text"'
  )
  expect_identical(
    validate_output(input, sch)@status,
    c("valid", "valid", rep("invalid", 7L))
  )
  numeric_enum <- schema(
    "Precision",
    field("n", type = "number", enum = "0.123456789")
  )
  expect_identical(
    validate_output('{"n":0.123456789}', numeric_enum)@status,
    "valid"
  )
})

test_that("untrusted expressions and filenames never reach the schema engine", {
  sch <- schema("X", field("n"))
  validator <- function(...) stop("must not run")
  report <- .validate_output_text(
    sch,
    c(
      '(()=>{throw Error("executed")})()',
      'undefined',
      'NaN',
      '{n:1}',
      '/tmp/x'
    ),
    validator
  )
  expect_true(all(report@status == "invalid"))
  expect_true(all(report@issues[["keyword"]] == "parse"))
  # Even valid scalar JSON must be treated as data, never as a filename.
  seen <- NULL
  .validate_output_text(sch, '10', function(json, ...) {
    seen <<- json
    TRUE
  })
  expect_s3_class(seen, "json")
})

test_that("validators are reused by contents and engine failures throw", {
  sch <- schema("Cache", field("cache_field"))
  a <- .prepare_output_validation(sch, TRUE, "collect")
  b <- .prepare_output_validation(
    schema("Other name", field("cache_field")),
    TRUE,
    "warn"
  )
  expect_identical(a, b)
  expect_false(identical(
    a,
    .prepare_output_validation(
      schema("Cache", field("cache_field", type = "integer")),
      TRUE,
      "collect"
    )
  ))
  expect_null(.prepare_output_validation(sch, FALSE, "warn"))
  expect_null(.prepare_output_validation(NULL, TRUE, "warn"))
  expect_error(.prepare_output_validation(sch, NA, "warn"), "TRUE or FALSE")
  expect_error(
    .prepare_output_validation(sch, TRUE, "bad"),
    "on_validation_failure"
  )
  expect_error(
    .validate_output_text(sch, '{}', function(...) stop("engine broken")),
    class = "llm_validation_engine_error"
  )
  local_mocked_bindings(
    json_validator = function(...) stop("compile broken"),
    .package = "jsonvalidate"
  )
  expect_error(
    .prepare_output_validation(
      schema("Broken", field("unique_compile_failure")),
      TRUE,
      "warn"
    ),
    class = "llm_validation_engine_error"
  )
})

test_that("standalone validation handles messages, batches and missing final answers", {
  sch <- schema("X", field("n", type = "integer"))
  good <- LLMMessage(content = '{"n":1}', model_name = "m")
  bad <- LLMMessage(content = '{"n":"1"}', model_name = "m")
  tool <- LLMMessage(
    content = "",
    model_name = "m",
    tool_calls = list(list(id = "x"))
  )
  expect_identical(
    validate_output(list(a = good, b = bad, c = NULL), sch)@status,
    c("valid", "invalid", "unavailable")
  )
  conversations <- list(
    first = list(InputMessage(content = "prompt"), good),
    second = list(good, bad),
    third = NULL,
    fourth = list(good, tool)
  )
  report <- validate_output(conversations, sch)
  expect_identical(
    report@status,
    c("valid", "invalid", "unavailable", "unavailable")
  )
  expect_identical(names(report@output), names(conversations))
  expect_identical(
    validate_output(SystemMessage(content = "sys"), sch)@status,
    "unavailable"
  )
  expect_error(validate_output(list(42), sch), "Pass JSON text")
})

test_that("all LLM backends preserve invalid content and support opt out and overrides", {
  local_mocked_bindings(ollama_check_model = function(...) invisible(NULL))
  sch <- schema("X", field("n", type = "integer"))
  for (backend in c("ollama", "openai", "anthropic")) {
    local_mocked_bindings(perform_chat_request = function(...) {
      validation_fixture('{"n":"10"}', backend)
    })
    model <- validation_model(backend, sch)
    expect_warning(out <- generate(model, "prompt", verbosity = 0L), NA)
    expect_identical(out@content, '{"n":"10"}')
    expect_identical(validation_results(out)@status, "invalid")
    expect_identical(
      validation_results(responses(out)),
      validation_results(out)
    )
    off <- generate(model, "prompt", validate_output = FALSE, verbosity = 0L)
    expect_identical(validation_results(off)@status, "not_validated")
    expect_identical(validate_output(off, sch)@status, "invalid")
    override <- generate(
      model,
      "prompt",
      output_schema = schema("String", field("n")),
      verbosity = 0L
    )
    expect_identical(validation_results(override)@status, "valid")
    err <- tryCatch(
      generate(
        model,
        "prompt",
        on_validation_failure = "abort",
        verbosity = 0L
      ),
      llm_output_validation_error = identity
    )
    expect_s3_class(err, "llm_output_validation_error")
    expect_identical(err[["output"]], '{"n":"10"}')
    expect_identical(err[["validation"]]@status, "invalid")
    expect_null(validation_results(generate(
      validation_model(backend),
      "prompt",
      verbosity = 0L
    )))
  }
})

test_that("warn is a styled message; collect is silent and batches validate per item", {
  sch <- schema("X", field("n", type = "integer"))
  model <- validation_model(sch = sch)
  logs <- list()
  events <- character()
  original_validate <- .validate_output_text
  local_mocked_bindings(
    warn = function(..., use_warning = FALSE, verbosity = NULL) {
      expect_false(use_warning)
      logs[[length(logs) + 1L]] <<- paste0(..., collapse = "")
    },
    .validate_output_text = function(...) {
      events <<- c(events, "validate")
      original_validate(...)
    },
    perform_chat_request = function(x, request_body, ...) {
      events <<- c(events, "generate")
      messages <- request_body[["messages"]]
      prompt <- messages[[length(messages)]][["content"]]
      validation_fixture(prompt)
    }
  )
  generate(model, '{"n":"10"}', verbosity = 0L)
  expect_length(logs, 1L)
  generate(
    model,
    '{"n":"10"}',
    on_validation_failure = "collect",
    verbosity = 0L
  )
  expect_length(logs, 1L)
  logs <- list()
  events <- character()
  input <- c(a = '{"n":10}', b = '{"n":"10"}', c = 'not JSON')
  expect_warning(out <- llmapply(input, model, verbosity = 0L), NA)
  expect_identical(events, rep(c("generate", "validate"), 3L))
  expect_length(logs, 1L)
  expect_match(logs[[1L]], "1/3 valid; 2 invalid")
  expect_identical(as.vector(out), as.vector(input))
  expect_identical(names(out), names(input))
  expect_equal(nrow(attr(out, "errors")), 0L)
  report <- validation_results(out)
  expect_identical(report@status, c("valid", "invalid", "invalid"))
  expect_identical(report@output, input)
  expect_identical(report@issues[["index"]], c(2L, 3L))
  raw <- map(
    as.list(input),
    model,
    on_validation_failure = "collect",
    verbosity = 0L
  )
  expect_identical(validation_results(responses(raw)), validation_results(raw))
  expect_identical(validation_results(raw[[2L]])@status, "invalid")
  off <- llmapply(input, model, validate_output = FALSE, verbosity = 0L)
  expect_true(all(validation_results(off)@status == "not_validated"))
  expect_identical(validate_output(off, sch)@status, report@status)
  empty <- llmapply(character(), model, verbosity = 0L)
  expect_length(validation_results(empty)@status, 0L)
})

test_that("batch validation aborts preserve rejected output and distinguish request failures", {
  sch <- schema("X", field("n", type = "integer"))
  model <- validation_model(sch = sch)
  calls <- 0L
  local_mocked_bindings(perform_chat_request = function(x, request_body, ...) {
    calls <<- calls + 1L
    prompt <- tail(request_body[["messages"]], 1L)[[1L]][["content"]]
    if (prompt == "transport") {
      abort("transport failed")
    }
    validation_fixture(prompt)
  })
  input <- c('{"n":1}', '{"n":"1"}', 'transport')
  expect_warning(
    out <- llmapply(
      input,
      model,
      on_validation_failure = "abort",
      verbosity = 0L
    ),
    "transport failed"
  )
  expect_identical(as.vector(out), c(input[[1L]], NA_character_, NA_character_))
  expect_identical(
    validation_results(out)@status,
    c("valid", "invalid", "unavailable")
  )
  expect_identical(validation_results(out)@output[[2L]], input[[2L]])
  expect_identical(attr(out, "errors")[["index"]], c(2L, 3L))
  calls <- 0L
  expect_error(
    llmapply(
      input,
      model,
      on_validation_failure = "abort",
      on_error = "abort",
      verbosity = 0L
    ),
    class = "llm_output_validation_error"
  )
  expect_equal(calls, 2L)
  calls <- 0L
  expect_error(
    llmapply(input, model, validate_output = NA, verbosity = 0L),
    "TRUE or FALSE"
  )
  expect_equal(calls, 0L)
  local_mocked_bindings(.validate_output_text = function(...) {
    abort("engine failed", class = "llm_validation_engine_error")
  })
  expect_error(
    llmapply(input, model, verbosity = 0L),
    class = "llm_validation_engine_error"
  )
})

test_that("agent validation annotates final answers before memory commit", {
  sch <- schema("X", field("n", type = "integer"))
  cfg <- config_OpenAI("test", validate_model = FALSE)
  agent <- create_agent(
    cfg,
    output_schema = sch,
    use_memory = TRUE,
    verbosity = 0L
  )
  local_mocked_bindings(perform_chat_request = function(...) {
    validation_fixture('{"n":"10"}')
  })
  expect_error(
    generate(agent, "prompt", on_validation_failure = "abort", verbosity = 0L),
    class = "llm_output_validation_error"
  )
  expect_false(any(vapply(
    get_messages(agent@state),
    function(m) m@role == "assistant",
    logical(1L)
  )))
  out <- generate(
    agent,
    "prompt",
    on_validation_failure = "collect",
    verbosity = 0L
  )
  expect_identical(validation_results(out)@status, "invalid")
  final <- tail(get_messages(agent@state), 1L)[[1L]]
  expect_identical(final@content, '{"n":"10"}')
  expect_identical(validation_results(final), validation_results(out))
  expect_identical(as.vector(responses(out)), '{"n":"10"}')
  expect_identical(validation_results(responses(out)), validation_results(out))
  expect_identical(validate_output(out, sch)@status, "invalid")
  batch <- agentapply(
    c("a", "b"),
    agent,
    on_validation_failure = "collect",
    verbosity = 0L
  )
  expect_identical(validation_results(batch)@status, c("invalid", "invalid"))
  expect_identical(as.vector(batch), rep('{"n":"10"}', 2L))
})

test_that("Anthropic synthetic output is an answer, preserving JSON containers and nulls", {
  sch <- schema(
    "Structured",
    field("n", type = "number"),
    field("a", type = "array", items = "integer"),
    field("optional", required = FALSE)
  )
  raw <- paste0(
    '{"content":[{"type":"text","text":"Here you go"},',
    '{"type":"tool_use","id":"synthetic","name":"respond_with_structured_output",',
    '"input":{"n":0.123456789,"a":[1],"o":{},"optional":null}}],"stop_reason":"tool_use"}'
  )
  local_mocked_bindings(perform_chat_request = function(...) {
    httr2::response(
      200L,
      headers = list(`content-type` = "application/json"),
      body = charToRaw(raw)
    )
  })
  model <- validation_model("anthropic", sch)
  out <- generate(
    model,
    "prompt",
    on_validation_failure = "collect",
    verbosity = 0L
  )
  expect_null(out@tool_calls)
  expect_match(out@content, '"n":0.123456789', fixed = TRUE)
  expect_match(out@content, '"a":[1]', fixed = TRUE)
  expect_match(out@content, '"o":{}', fixed = TRUE)
  expect_match(out@content, '"optional":null', fixed = TRUE)
  expect_identical(validation_results(out)@status, "invalid")
  expect_identical(validation_results(out)@issues[["path"]], "/optional")
  expect_length(out@metadata[["raw_content"]], 2L)
  agent <- create_agent(model@config, output_schema = sch, verbosity = 0L)
  history <- generate(
    agent,
    "prompt",
    on_validation_failure = "collect",
    verbosity = 0L
  )
  expect_identical(validation_results(history)@status, "invalid")
  expect_identical(as.vector(responses(history)), out@content)
})

test_that("agents validate only the final answer and report unfinished tool rounds separately", {
  sch <- schema("X", field("n", type = "integer"))
  tool_runs <- 0L
  tool <- create_custom_tool(
    name = "Count",
    function_name = "count",
    description = "Count",
    parameters = list(tool_param("x", "number", "Count")),
    impl = function(x) {
      tool_runs <<- tool_runs + 1L
      x
    }
  )
  cfg <- config_OpenAI("test", validate_model = FALSE)
  agent <- create_agent(
    cfg,
    output_schema = sch,
    tools = list(tool),
    allow_custom_tools = TRUE,
    max_tool_rounds = 1L,
    verbosity = 0L
  )
  call <- list(
    id = "call1",
    type = "function",
    `function` = list(name = "count", arguments = '{"x":1}')
  )
  requests <- 0L
  validations <- 0L
  original <- .validate_output_text
  local_mocked_bindings(
    perform_chat_request = function(...) {
      requests <<- requests + 1L
      if (requests == 1L) {
        validation_fixture("I will use a tool.", tool_calls = list(call))
      } else {
        validation_fixture('{"n":1}')
      }
    },
    .validate_output_text = function(...) {
      validations <<- validations + 1L
      original(...)
    }
  )
  out <- generate(
    agent,
    "prompt",
    on_validation_failure = "abort",
    verbosity = 0L
  )
  expect_identical(validation_results(out)@status, "valid")
  expect_equal(validations, 1L)
  expect_equal(tool_runs, 1L)
  expect_equal(requests, 2L)
  local_mocked_bindings(perform_chat_request = function(...) {
    validation_fixture("More tools", tool_calls = list(call))
  })
  out <- generate(
    agent,
    "prompt",
    on_validation_failure = "collect",
    verbosity = 0L
  )
  expect_identical(validation_results(out)@status, "unavailable")
  expect_identical(validate_output(out, sch)@status, "unavailable")
})

test_that("validator cache is bounded and saved reports contain no live engine", {
  for (i in seq_len(18L)) {
    sch <- schema("Bound", field(paste0("cache_bound_", i)))
    .prepare_output_validation(sch, TRUE, "collect")
  }
  expect_length(.output_validator_cache[["keys"]], 16L)
  sch <- schema("Saved", field("n", type = "integer"))
  report <- validate_output('{"n":"10"}', sch)
  path <- tempfile(fileext = ".rds")
  saveRDS(report, path)
  restored <- readRDS(path)
  expect_identical(restored, report)
  expect_identical(
    validate_output(restored@output, restored@schema)@status,
    "invalid"
  )
})

test_that("JSON property names cannot be interpreted as JavaScript prototypes", {
  sch <- schema("X", field("n", type = "integer"))
  expect_identical(
    validate_output('{"__proto__":{"n":10}}', sch)@status,
    "invalid"
  )
  special <- schema("Special", field("__proto__"), field("constructor"))
  expect_identical(
    validate_output(
      '{"__proto__":"literal","constructor":"literal"}',
      special
    )@status,
    "valid"
  )
  expect_identical(validate_output('{}', special)@status, "invalid")
  object_key <- schema("Object key", field("__proto__", type = "object"))
  expect_identical(validate_output('{}', object_key)@status, "invalid")
  expect_identical(
    validate_output('{"__proto__":{}}', object_key)@status,
    "valid"
  )
  expect_identical(
    validate_output(
      jsonlite::toJSON(list(n = 10), auto_unbox = TRUE),
      sch
    )@status,
    "valid"
  )
  expect_identical(
    validate_output(
      '{"n":10,"text":"quote: \\" and newline: \\n"}',
      sch
    )@status,
    "valid"
  )
})
