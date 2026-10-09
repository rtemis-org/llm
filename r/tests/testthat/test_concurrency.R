# Concurrency contracts are checked against a local threaded HTTP fixture.
local_provider <- function(env = parent.frame()) {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai", "2.7.0")
  testthat::skip_if_not_installed("processx")
  python <- Sys.which("python3")
  testthat::skip_if(
    !nzchar(python),
    "python3 is required for the local provider fixture"
  )
  port <- tempfile()
  server <- processx::process$new(
    python,
    c(
      testthat::test_path("fixtures", "concurrency", "server.py"),
      port
    ),
    stdout = "|",
    stderr = "|"
  )
  withr::defer(
    {
      server$kill()
      unlink(port)
    },
    envir = env
  )
  for (i in seq_len(100L)) {
    if (file.exists(port) || !server$is_alive()) {
      break
    }
    Sys.sleep(0.05)
  }
  if (!file.exists(port)) {
    stop("Fixture server failed: ", server$read_all_error())
  }
  url <- paste0("http://127.0.0.1:", readLines(port, warn = FALSE))
  list(
    url = url,
    llm = create_OpenAI(
      "fixture",
      api_key = "fixture",
      base_url = paste0(url, "/v1")
    ),
    stats = function() {
      httr2::request(url) |> httr2::req_perform() |> httr2::resp_body_json()
    }
  )
}

strip_batch_attributes <- function(x) {
  for (key in c("elapsed", "batch_elapsed", "errors")) {
    attr(x, key) <- NULL
  }
  x
}

test_that("concurrency is a positive integer checked before provider construction", {
  for (bad in list(0, -1, 1.1, NA, Inf, NaN, NULL, TRUE, c(1L, 2L), "2")) {
    expect_error(llmapply("x", "model", concurrency = bad), "concurrency")
    expect_error(agentapply("x", "model", concurrency = bad), "concurrency")
    expect_error(dmapply("x", "model", concurrency = bad), "concurrency")
  }
  llm <- create_OpenAI("fixture", api_key = "fixture")
  expect_error(map("x", llm, concurrency = 0), "concurrency")
  expect_length(llmapply(character(), llm, concurrency = 1, verbosity = 0), 0L)
  if (requireNamespace("mirai", quietly = TRUE)) {
    expect_length(
      llmapply(character(), llm, concurrency = 2, verbosity = 0),
      0L
    )
  }
})

test_that("parallel requests overlap, stay bounded, and return in input order", {
  p <- local_provider()
  x <- c(a = "slow", b = "fast", c = "third", d = "fourth")
  out <- llmapply(x, p$llm, concurrency = 2L, verbosity = 0L)
  expect_identical(strip_batch_attributes(out), x)
  expect_identical(names(attr(out, "elapsed")), names(x))
  expect_true(all(attr(out, "elapsed") > 0))
  expect_true(attr(out, "batch_elapsed") >= max(attr(out, "elapsed")))
  info <- p$stats()
  expect_equal(info$maximum, 2L)
  times <- vapply(info$calls, `[[`, numeric(1L), "finished")
  names(times) <- vapply(info$calls, `[[`, character(1L), "prompt")
  expect_lt(times[["fast"]], times[["slow"]])
  expect_true(all(vapply(
    info$calls,
    function(call) is.null(call$body$concurrency),
    logical(1L)
  )))
})

test_that("concurrent failures, validation, and map methods preserve their slots", {
  p <- local_provider()
  expect_warning(
    out <- map(
      as.list(c("first", "fail", "last")),
      p$llm,
      concurrency = 2L,
      verbosity = 0L
    ),
    "Element 2 failed"
  )
  expect_null(out[[2L]])
  expect_equal(attr(out, "errors")$index, 2L)
  expect_equal(responses(out), c("first", NA_character_, "last"))
  shape <- schema("Answer", field("answer", "Answer"))
  # The schema is a model setting for the apply wrapper.
  structured <- create_OpenAI(
    "fixture",
    api_key = "fixture",
    base_url = paste0(p$url, "/v1"),
    output_schema = shape
  )
  out <- llmapply(
    c("valid", "invalid"),
    structured,
    concurrency = 2L,
    verbosity = 0L,
    on_validation_failure = "collect"
  )
  expect_identical(
    strip_batch_attributes(out),
    structure(
      c('{"answer":"ok"}', 'invalid'),
      validation = attr(out, "validation")
    )
  )
  expect_equal(unname(validation_results(out)@status), c("valid", "invalid"))
  expect_warning(
    failed <- llmapply(
      c("valid", "invalid"),
      structured,
      concurrency = 2L,
      verbosity = 0L,
      on_validation_failure = "abort"
    ),
    NA
  )
  expect_true(is.na(failed[[2L]]))
  expect_equal(attr(failed, "errors")$index, 2L)
})

test_that("every agent input gets fresh state and retains its complete tool loop", {
  p <- local_provider()
  impl <- function(x, y) x + y
  environment(impl) <- baseenv()
  tool <- create_custom_tool(
    "Addition",
    "add_numbers",
    "Add two numbers",
    parameters = list(
      tool_param("x", "number", "First", required = TRUE),
      tool_param("y", "number", "Second", required = TRUE)
    ),
    impl = impl
  )
  agent <- create_agent(
    p$llm@config,
    system_prompt = "Use the tool.",
    use_memory = TRUE,
    tools = list(tool),
    allow_custom_tools = TRUE,
    verbosity = 0L
  )
  append_message(agent@state, InputMessage(content = "template-only secret"))
  before <- get_messages(agent)
  x <- c("slow", "second", "third", "fourth")
  out <- agentapply(
    x,
    agent,
    concurrency = 2L,
    verbosity = 0L,
    extract_responses = FALSE
  )
  expect_equal(get_messages(agent), before)
  expect_length(out, 4L)
  for (i in seq_along(out)) {
    roles <- vapply(out[[i]], function(m) m@role, character(1L))
    expect_equal(
      unname(roles),
      c("system", "user", "assistant", "tool", "assistant")
    )
    expect_match(responses(out)[[i]], x[[i]], fixed = TRUE)
    expect_match(responses(out)[[i]], "5")
    expect_equal(reasoning(out[[i]][[3L]]), "fixture reasoning")
  }
  info <- p$stats()
  expect_equal(length(info$calls), 8L)
  for (call in info$calls) {
    users <- Filter(function(m) m$role == "user", call$body$messages)
    expect_length(users, 1L)
    expect_equal(users[[1L]]$content, call$prompt)
  }
  one <- map("single", agent, concurrency = 2L, verbosity = 0L)
  expect_equal(get_messages(agent), before)
  expect_length(one[[1L]], 5L)
  temporary <- agentapply(
    "temporary",
    agent,
    concurrency = 2L,
    verbosity = 0L,
    commit_to_memory = FALSE,
    extract_responses = FALSE
  )
  expect_length(temporary[[1L]], 5L)
  expect_equal(get_messages(agent), before)
})

test_that("decision questions and schema filling use the same task bound", {
  p <- local_provider()
  config <- config_OllamaDecision(
    "fixture",
    base_url = p$url,
    validate_model = FALSE
  )
  dm <- create_DecisionModel(config)
  qs <- list(yes = noul("Is this true?"))
  out <- dmapply(
    c(a = "slow", b = "second", c = "third"),
    dm,
    questions = qs,
    concurrency = 2L,
    verbosity = 0L,
    extract_responses = FALSE
  )
  expect_length(out, 3L)
  expect_true(all(vapply(out, S7_inherits, logical(1L), class = Decision)))
  expect_equal(p$stats()$maximum, 2L)
  many <- stats::setNames(rep(qs, 65L), paste0("q", seq_len(65L)))
  out <- dmapply(
    c("chunked-a", "chunked-b"),
    dm,
    questions = many,
    concurrency = 2L,
    verbosity = 0L
  )
  expect_true(!is.null(attr(out, "batch_elapsed")))
  calls <- Filter(function(z) startsWith(z$prompt, "chunked"), p$stats()$calls)
  expect_equal(
    sort(vapply(calls, function(z) length(z$body$questions), integer(1L))),
    c(1L, 1L, 64L, 64L)
  )
  shape <- schema("Decision", field("answer", "Answer", enum = c("yes", "no")))
  shaped <- create_DecisionModel(config, output_schema = shape)
  out <- dmapply(c("a", "b"), shaped, concurrency = 2L, verbosity = 0L)
  expect_equal(
    unname(strip_batch_attributes(out)),
    c('{"answer":"yes"}', '{"answer":"yes"}'),
    ignore_attr = "validation"
  )
})

test_that("HTTP retries are bounded and do not replay completed inputs", {
  p <- local_provider()
  out <- llmapply(c("retry", "okay"), p$llm, concurrency = 2L, verbosity = 0L)
  expect_equal(strip_batch_attributes(out), c("retry", "okay"))
  calls <- p$stats()$calls
  expect_equal(
    sum(vapply(calls, function(z) z$prompt == "retry", logical(1L))),
    2L
  )
  expect_equal(
    sum(vapply(calls, function(z) z$prompt == "okay", logical(1L))),
    1L
  )
  expect_warning(
    out <- llmapply("always_busy", p$llm, concurrency = 2L, verbosity = 0L),
    "failed"
  )
  expect_true(is.na(out[[1L]]))
  calls <- p$stats()$calls
  expect_equal(
    sum(vapply(calls, function(z) z$prompt == "always_busy", logical(1L))),
    3L
  )
  expect_warning(
    out <- llmapply("long_cooldown", p$llm, concurrency = 2L, verbosity = 0L),
    "failed"
  )
  expect_true(is.na(out[[1L]]))
  calls <- p$stats()$calls
  expect_equal(
    sum(vapply(calls, function(z) z$prompt == "long_cooldown", logical(1L))),
    1L
  )
})

test_that("abort stops dispatch and leaves user-owned workers untouched", {
  p <- local_provider()
  profile <- paste0("user-", Sys.getpid())
  mirai::daemons(1L, .compute = profile)
  on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)
  expect_error(
    llmapply(
      c("fail", "slow", rep("never", 8)),
      p$llm,
      concurrency = 2L,
      on_error = "abort",
      verbosity = 0L
    ),
    "fixture failure"
  )
  calls <- p$stats()$calls
  expect_false(any(vapply(calls, function(z) z$prompt == "never", logical(1L))))
  expect_equal(mirai::mirai(42L, .compute = profile)[], 42L)
  expect_false(any(grepl("rtemis-llm-batch-", list.files(tempdir()))))
})

test_that("agent retries retain completed tools and security logs are merged", {
  p <- local_provider()
  counter <- tempfile()
  logfile <- tempfile()
  on.exit(unlink(c(counter, logfile)), add = TRUE)
  impl <- function(x, y) {
    cat("called\n", file = counter, append = TRUE)
    x + y
  }
  environment(impl) <- list2env(list(counter = counter), parent = baseenv())
  tool <- create_custom_tool(
    "Addition",
    "add_numbers",
    "Add",
    parameters = list(
      tool_param("x", "number", "First", required = TRUE),
      tool_param("y", "number", "Second", required = TRUE)
    ),
    impl = impl
  )
  agent <- create_agent(
    p$llm@config,
    tools = list(tool),
    allow_custom_tools = TRUE,
    logfile = logfile,
    verbosity = 0L
  )
  out <- agentapply("tool_retry", agent, concurrency = 2L, verbosity = 0L)
  expect_match(out[[1L]], "5")
  expect_equal(readLines(counter), "called")
  expect_length(p$stats()$calls, 3L)
  warnings <- character()
  out <- withCallingHandlers(
    agentapply(
      c("unauthorized-a", "unauthorized-b", "unauthorized-c"),
      agent,
      concurrency = 2L,
      verbosity = 0L
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 3L)
  expect_true(all(grepl("not in the agent", warnings)))
  expect_true(all(is.na(out)))
  lines <- readLines(logfile)
  expect_length(lines, 3L)
  expect_true(all(vapply(lines, jsonlite::validate, logical(1L))))
})

test_that("cooldowns are shared, finite, and names do not contain request content", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  expect_equal(.batch_cooldown(directory), 0)
  .publish_batch_cooldown(directory, 10)
  first <- .batch_cooldown(directory)
  saveRDS(first + 1, file.path(directory, "cooldown-other"))
  expect_equal(.batch_cooldown(directory), first + 1)
  expect_true(all(grepl("^cooldown-", list.files(directory))))
  seen <- 0L
  httr2::local_mocked_responses(function(req) {
    seen <<- seen + 1L
    httr2::response_json(
      503L,
      body = list(error = "busy"),
      headers = list(`Retry-After` = "0")
    )
  })
  unlink(list.files(directory, full.names = TRUE))
  response <- .batch_request(httr2::request("http://fixture"), directory)
  expect_equal(seen, 3L)
  expect_equal(httr2::resp_status(response), 503L)
})

test_that("provider adapters and relative image paths work in workers", {
  p <- local_provider()
  testthat::local_mocked_bindings(ollama_check_model = function(...) invisible(TRUE))
  models <- list(
    Ollama(
      config = config_Ollama("fixture", base_url = p$url),
      system_prompt = "Answer"
    ),
    create_Anthropic("fixture", base_url = p$url, api_key = "fixture"),
    create_Apple(base_url = paste0(p$url, "/v1"), validate_model = FALSE)
  )
  for (model in models) {
    out <- llmapply(c("a", "b"), model, concurrency = 2L, verbosity = 0L)
    expect_equal(strip_batch_attributes(out), c("a", "b"))
  }
  paths <- c(
    red = file.path("fixtures", "red.png"),
    blue = file.path("fixtures", "blue.jpg")
  )
  out <- llmapply(
    "image",
    p$llm,
    image_path = paths,
    concurrency = 2L,
    verbosity = 0L
  )
  expect_equal(strip_batch_attributes(out), c(red = "image", blue = "image"))
  calls <- Filter(function(z) z$prompt == "image", p$stats()$calls)
  urls <- vapply(
    calls,
    function(z) z$body$messages[[2L]]$content[[2L]]$image_url$url,
    character(1L)
  )
  expect_length(unique(urls), 2L)
})

test_that("invalid questions fail before dispatch and nested pools are refused", {
  dm <- mock_decision_model()
  expect_error(
    dmapply("x", dm, questions = list(), concurrency = 2L),
    "non-empty list"
  )
  withr::local_options(list(rtemis.llm.batch_directory = tempdir()))
  llm <- create_OpenAI("fixture", api_key = "fixture")
  if (requireNamespace("mirai", quietly = TRUE)) {
    expect_error(llmapply("x", llm, concurrency = 2L), "cannot be nested")
  }
})
