# test_llmapply.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# %% Settings ----
model_name <- "gemma4:e4b"

system_prompt <- paste(
  "Convert the following color to its hexadecimal code.",
  "Respond using the provided schema."
)

output_schema <- schema(
  name = "ColorSchema",
  field(
    name = "color",
    type = "string",
    description = "The name of the color in English"
  ),
  field(
    name = "hex",
    type = "string",
    description = "The hexadecimal code of the color"
  )
)

# %% responses() ----
test_that("responses() returns assistant content from a single LLMMessage", {
  m <- LLMMessage(content = "hello", model_name = "m")
  testthat::expect_identical(responses(m), "hello")
})

test_that("responses() returns NA for a non-assistant Message", {
  m <- SystemMessage(content = "sys")
  testthat::expect_identical(responses(m), NA_character_)
})

test_that("responses() handles a flat list of Messages", {
  ms <- list(
    LLMMessage(content = "a", model_name = "m"),
    SystemMessage(content = "sys"),
    LLMMessage(content = "c", model_name = "m")
  )
  testthat::expect_identical(responses(ms), c("a", NA_character_, "c"))
})

test_that("responses() handles a list of lists of Messages", {
  x <- list(
    list(
      SystemMessage(content = "sys"),
      InputMessage(content = "user"),
      LLMMessage(content = "a", model_name = "m")
    ),
    list(
      SystemMessage(content = "sys"),
      InputMessage(content = "user")
    )
  )
  testthat::expect_identical(responses(x), c("a", NA_character_))
})

test_that("responses() aborts on unrecognized input", {
  testthat::expect_error(responses(42), "Could not extract responses")
})

# %% reasoning() ----
test_that("reasoning() returns NA for Messages without reasoning", {
  m <- SystemMessage(content = "sys")
  testthat::expect_identical(reasoning(m), NA_character_)
})

test_that("reasoning() returns the reasoning trace of an LLMMessage", {
  m <- LLMMessage(content = "ans", reasoning = "thought", model_name = "m")
  testthat::expect_identical(reasoning(m), "thought")
})

test_that("reasoning() returns NA for an LLMMessage with NULL reasoning", {
  m <- LLMMessage(content = "ans", model_name = "m")
  testthat::expect_identical(reasoning(m), NA_character_)
})

test_that("reasoning() handles a flat list of Messages", {
  ms <- list(
    LLMMessage(content = "a", reasoning = "r1", model_name = "m"),
    SystemMessage(content = "sys"),
    LLMMessage(content = "c", model_name = "m")
  )
  testthat::expect_identical(
    reasoning(ms),
    c("r1", NA_character_, NA_character_)
  )
})

test_that("reasoning() handles a list of lists of Messages", {
  x <- list(
    list(
      SystemMessage(content = "sys"),
      LLMMessage(content = "a", reasoning = "r1", model_name = "m")
    ),
    list(
      SystemMessage(content = "sys"),
      InputMessage(content = "user")
    )
  )
  testthat::expect_identical(reasoning(x), c("r1", NA_character_))
})

# %% llmapply argument validation ----
test_that("llmapply aborts when an Agent is passed as model_or_llm", {
  skip_if_ollama_model_missing(model_name)
  agent <- create_agent(
    llmconfig = config_Ollama(model_name = model_name),
    use_memory = FALSE
  )
  testthat::expect_error(
    llmapply("hi", agent),
    "Agent"
  )
})

test_that("llmapply aborts when build-path args are supplied with a built LLM", {
  skip_if_ollama_model_missing(model_name)
  llm <- create_Ollama(model_name = model_name)
  testthat::expect_error(
    llmapply("hi", llm, system_prompt = "override"),
    "Cannot supply"
  )
})

test_that("llmapply aborts on unknown backend", {
  testthat::expect_error(
    llmapply("hi", "some-model", backend = "bogus"),
    "should be one of"
  )
})

test_that("llmapply aborts on unsupported model_or_llm type", {
  testthat::expect_error(
    llmapply("hi", 42),
    "must be a single model-name string"
  )
})

# %% llmapply integration ----
test_that("llmapply works with a model name (string path)", {
  skip_if_ollama_model_missing(model_name)
  hex <- llmapply(
    c("teal", "orange", "burgundy"),
    model_or_llm = model_name,
    system_prompt = "Return the hexadecimal code for the following color in format #FFFFFF"
  )
  expect_length(hex, 3)
  expect_type(hex, "character")
})

test_that("llmapply works with a pre-built LLM object", {
  skip_if_ollama_model_missing(model_name)
  llm <- create_Ollama(
    model_name = model_name,
    system_prompt = "Return the hexadecimal code for the following color in format #FFFFFF"
  )
  hex <- llmapply(c("teal", "orange"), llm)
  expect_length(hex, 2)
})

test_that("llmapply returns raw Messages when extract_responses = FALSE", {
  skip_if_ollama_model_missing(model_name)
  out <- llmapply(
    c("teal", "orange"),
    model_or_llm = model_name,
    system_prompt = "Return the hexadecimal code for the following color in format #FFFFFF",
    extract_responses = FALSE
  )
  expect_length(out, 2)
  expect_true(all(vapply(out, S7_inherits, logical(1L), class = Message)))
})

test_that("llmapply works with output_schema", {
  skip_if_ollama_model_missing(model_name)
  x <- c("teal", "orange", "burgundy")
  hex <- llmapply(
    x,
    model_or_llm = model_name,
    system_prompt = system_prompt,
    output_schema = output_schema
  )
  expect_length(hex, length(x))
})

# %% agentapply argument validation ----
test_that("agentapply aborts when an LLM is passed as model_or_agent", {
  skip_if_ollama_model_missing(model_name)
  llm <- create_Ollama(model_name = model_name)
  testthat::expect_error(
    agentapply("hi", llm),
    "LLM"
  )
})

test_that("agentapply aborts when build-path args are supplied with a built Agent", {
  skip_if_ollama_model_missing(model_name)
  agent <- create_agent(
    llmconfig = config_Ollama(model_name = model_name),
    use_memory = FALSE
  )
  testthat::expect_error(
    agentapply("hi", agent, use_memory = TRUE),
    "Cannot supply"
  )
})

# %% agentapply integration ----
test_that("agentapply works with a pre-built Agent object", {
  skip_if_ollama_model_missing(model_name)
  agent <- create_agent(
    llmconfig = config_Ollama(model_name = model_name),
    system_prompt = "Return the hexadecimal code for the following color in format #FFFFFF",
    use_memory = FALSE
  )
  hex <- agentapply(c("teal", "orange"), agent)
  expect_length(hex, 2)
})

test_that("agentapply works with a model name (string path)", {
  skip_if_ollama_model_missing(model_name)
  hex <- agentapply(
    c("teal", "orange"),
    model_or_agent = model_name,
    system_prompt = "Return the hexadecimal code for the following color in format #FFFFFF"
  )
  expect_length(hex, 2)
})


# %% Stubs for per-call error handling ----
# Backend-free LLM and Agent whose generate() fails on named prompts, so the
# `on_error` paths can be exercised without a server.
StubLLM <- new_class(
  "StubLLM",
  parent = LLM,
  properties = list(fail_on = class_character),
  constructor = function(fail_on = character()) {
    new_object(LLM(system_prompt = "stub"), fail_on = fail_on)
  }
)

method(get_model_name, StubLLM) <- function(x) "stub-model"

method(generate, StubLLM) <- function(
  x,
  prompt,
  temperature = NULL,
  top_p = NULL,
  max_tokens = NULL,
  stop = NULL,
  think = NULL,
  output_schema = NULL,
  verbosity = 1L,
  ...
) {
  if (prompt %in% x@fail_on) {
    abort("stub failure for ", prompt)
  }
  LLMMessage(
    content = toupper(prompt),
    reasoning = paste0("because ", prompt),
    model_name = "stub-model"
  )
}

StubAgent <- new_class(
  "StubAgent",
  parent = Agent,
  properties = list(fail_on = class_character),
  constructor = function(fail_on = character()) {
    new_object(
      Agent(
        llmconfig = config_OpenAI(
          model_name = "stub-model",
          base_url = "http://localhost:1234/v1",
          validate_model = FALSE
        ),
        use_memory = FALSE,
        verbosity = 0L
      ),
      fail_on = fail_on
    )
  }
)

method(generate, StubAgent) <- function(
  x,
  prompt,
  temperature = NULL,
  top_p = NULL,
  max_tokens = NULL,
  stop = NULL,
  think = NULL,
  output_schema = NULL,
  verbosity = 1L,
  ...
) {
  if (prompt %in% x@fail_on) {
    abort("stub failure for ", prompt)
  }
  list(
    SystemMessage(content = "stub"),
    InputMessage(content = prompt),
    LLMMessage(content = toupper(prompt), model_name = "stub-model")
  )
}

# Collect every warning a batch emits without letting it reach the reporter.
count_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(cond) {
      warnings <<- c(warnings, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}


# %% on_error ----
test_that("llmapply keeps the batch by default", {
  stub <- StubLLM(fail_on = "b")
  caught <- count_warnings(llmapply(c("a", "b", "c"), stub, verbosity = 0L))
  testthat::expect_equal(
    caught[["value"]],
    c("A", NA_character_, "C"),
    ignore_attr = "errors"
  )
  testthat::expect_length(caught[["warnings"]], 1L)
})

test_that("on_error = 'abort' propagates a failed call", {
  stub <- StubLLM(fail_on = "b")
  testthat::expect_error(
    llmapply(c("a", "b", "c"), stub, verbosity = 0L, on_error = "abort"),
    "stub failure for b"
  )
})

test_that("on_error = 'na' keeps the batch and marks the failures", {
  stub <- StubLLM(fail_on = c("b", "d"))
  caught <- count_warnings(
    llmapply(c("a", "b", "c", "d"), stub, verbosity = 0L, on_error = "na")
  )
  testthat::expect_equal(
    caught[["value"]],
    c("A", NA_character_, "C", NA_character_),
    ignore_attr = "errors"
  )
  # One warning per failure, each naming its element.
  testthat::expect_length(caught[["warnings"]], 2L)
  testthat::expect_match(caught[["warnings"]][[1L]], "Element 2 failed")
  testthat::expect_match(caught[["warnings"]][[2L]], "Element 4 failed")
})

test_that("the errors attribute names exactly the failing indices", {
  stub <- StubLLM(fail_on = c("b", "d"))
  caught <- count_warnings(
    llmapply(c("a", "b", "c", "d"), stub, verbosity = 0L, on_error = "na")
  )
  errors <- attr(caught[["value"]], "errors")
  testthat::expect_s3_class(errors, "data.frame")
  testthat::expect_identical(errors[["index"]], c(2L, 4L))
  testthat::expect_true(all(grepl("stub failure", errors[["message"]])))
})

test_that("on_error = 'na' with no failures returns a zero-row errors table", {
  stub <- StubLLM()
  out <- llmapply(c("a", "b"), stub, verbosity = 0L, on_error = "na")
  testthat::expect_equal(out, c("A", "B"), ignore_attr = "errors")
  testthat::expect_identical(nrow(attr(out, "errors")), 0L)
})

test_that("on_error = 'abort' leaves the result free of an errors attribute", {
  stub <- StubLLM()
  out <- llmapply(c("a", "b"), stub, verbosity = 0L, on_error = "abort")
  testthat::expect_null(attr(out, "errors"))
})

test_that("extract_responses = FALSE leaves NULL in the failed slots", {
  stub <- StubLLM(fail_on = "b")
  caught <- count_warnings(
    llmapply(
      c("a", "b", "c"),
      stub,
      verbosity = 0L,
      on_error = "na",
      extract_responses = FALSE
    )
  )
  out <- caught[["value"]]
  testthat::expect_length(out, 3L)
  testthat::expect_null(out[[2L]])
  testthat::expect_identical(responses(out), c("A", NA_character_, "C"))
  testthat::expect_identical(
    reasoning(out),
    c("because a", NA_character_, "because c")
  )
})

test_that("map() on a list input honors on_error", {
  stub <- StubLLM(fail_on = "b")
  caught <- count_warnings(
    map(list("a", "b"), stub, verbosity = 0L, on_error = "na")
  )
  testthat::expect_identical(
    responses(caught[["value"]]),
    c("A", NA_character_)
  )
})

test_that("agentapply threads on_error through to the nested result", {
  stub <- StubAgent(fail_on = "b")
  caught <- count_warnings(
    agentapply(c("a", "b", "c"), stub, verbosity = 0L, on_error = "na")
  )
  testthat::expect_equal(
    caught[["value"]],
    c("A", NA_character_, "C"),
    ignore_attr = "errors"
  )
  testthat::expect_identical(attr(caught[["value"]], "errors")[["index"]], 2L)
})

test_that("agentapply keeps the batch by default", {
  stub <- StubAgent(fail_on = "b")
  caught <- count_warnings(agentapply(c("a", "b"), stub, verbosity = 0L))
  testthat::expect_equal(
    caught[["value"]],
    c("A", NA_character_),
    ignore_attr = "errors"
  )
})

test_that("agentapply's on_error = 'abort' propagates a failed call", {
  stub <- StubAgent(fail_on = "b")
  testthat::expect_error(
    agentapply(c("a", "b"), stub, verbosity = 0L, on_error = "abort"),
    "stub failure for b"
  )
})

test_that("map() defaults to keeping the batch", {
  stub <- StubLLM(fail_on = "b")
  caught <- count_warnings(map(c("a", "b"), stub, verbosity = 0L))
  testthat::expect_identical(
    responses(caught[["value"]]),
    c("A", NA_character_)
  )
})


# %% responses() / reasoning() with NULL slots ----
test_that("responses() maps NULL slots to NA", {
  flat <- list(LLMMessage(content = "a", model_name = "m"), NULL)
  testthat::expect_identical(responses(flat), c("a", NA_character_))
  nested <- list(
    list(
      SystemMessage(content = "sys"),
      LLMMessage(content = "a", model_name = "m")
    ),
    NULL
  )
  testthat::expect_identical(responses(nested), c("a", NA_character_))
})

test_that("reasoning() maps NULL slots to NA", {
  flat <- list(
    LLMMessage(content = "a", reasoning = "r1", model_name = "m"),
    NULL
  )
  testthat::expect_identical(reasoning(flat), c("r1", NA_character_))
  nested <- list(
    list(LLMMessage(content = "a", reasoning = "r1", model_name = "m")),
    NULL
  )
  testthat::expect_identical(reasoning(nested), c("r1", NA_character_))
})


# %% logprobs() / token_probs() ----
# One token position with two alternatives, in the shape both backends land in
# `metadata[["logprobs"]]`.
logprob_metadata <- function(entries) {
  list(logprobs = entries)
}

yes_no_entry <- list(
  token = "Yes",
  logprob = log(0.8),
  top_logprobs = list(
    list(token = "Yes", logprob = log(0.8)),
    list(token = " yes", logprob = log(0.1)),
    list(token = "No", logprob = log(0.1))
  )
)

with_logprobs <- function(content = "Yes", entries = list(yes_no_entry)) {
  LLMMessage(
    content = content,
    model_name = "m",
    metadata = logprob_metadata(entries)
  )
}

test_that("logprobs() returns a data.table of tokens and probabilities", {
  dt <- logprobs(with_logprobs())
  testthat::expect_s3_class(dt, "data.table")
  testthat::expect_identical(dt[["position"]], 1L)
  testthat::expect_identical(dt[["token"]], "Yes")
  testthat::expect_equal(dt[["prob"]], 0.8)
  # No alternatives column unless asked for.
  testthat::expect_false("alternatives" %in% names(dt))
})

test_that("logprobs(top = TRUE) adds the alternatives per position", {
  dt <- logprobs(with_logprobs(), top = TRUE)
  alts <- dt[["alternatives"]][[1L]]
  testthat::expect_s3_class(alts, "data.table")
  testthat::expect_identical(alts[["token"]], c("Yes", " yes", "No"))
  testthat::expect_equal(alts[["prob"]], c(0.8, 0.1, 0.1))
})

test_that("logprobs() returns NULL for a message that carries none", {
  testthat::expect_null(logprobs(LLMMessage(content = "a", model_name = "m")))
})

test_that("logprobs() maps over a list, preserving length and NULL slots", {
  out <- logprobs(list(with_logprobs(), NULL))
  testthat::expect_length(out, 2L)
  testthat::expect_s3_class(out[[1L]], "data.table")
  testthat::expect_null(out[[2L]])
})

test_that("token_probs() sums alternatives that differ only in case or space", {
  # "Yes" (0.8) and " yes" (0.1) are the same answer, tokenized differently.
  p <- token_probs(with_logprobs(), c("Yes", "No"))
  testthat::expect_equal(p[["Yes"]], 0.9)
  testthat::expect_equal(p[["No"]], 0.1)
})

test_that("token_probs() returns NA for a candidate outside the top-k", {
  p <- token_probs(with_logprobs(), c("Yes", "Maybe"))
  testthat::expect_equal(p[["Yes"]], 0.9)
  testthat::expect_true(is.na(p[["Maybe"]]))
})

test_that("token_probs() returns a matrix over a list of Messages", {
  p <- token_probs(list(with_logprobs(), NULL), c("Yes", "No"))
  testthat::expect_true(is.matrix(p))
  testthat::expect_identical(dim(p), c(2L, 2L))
  testthat::expect_identical(colnames(p), c("Yes", "No"))
  # Indexing to a scalar keeps the column name, so compare unnamed.
  testthat::expect_equal(unname(p[1L, "Yes"]), 0.9)
  testthat::expect_true(all(is.na(p[2L, ])))
})

test_that("token_probs() reads a later position when asked", {
  second <- list(
    yes_no_entry,
    list(
      token = ".",
      logprob = log(0.5),
      top_logprobs = list(list(token = ".", logprob = log(0.5)))
    )
  )
  p <- token_probs(with_logprobs(entries = second), ".", position = 2L)
  testthat::expect_equal(p[[1L]], 0.5)
})

test_that("logprobs() and token_probs() handle the Agent nesting", {
  nested <- list(
    list(SystemMessage(content = "sys"), with_logprobs()),
    NULL
  )
  testthat::expect_equal(logprobs(nested)[[1L]][["prob"]], 0.8)
  testthat::expect_null(logprobs(nested)[[2L]])
  testthat::expect_equal(unname(token_probs(nested, "Yes")[1L, "Yes"]), 0.9)
})

test_that("logprobs() and token_probs() abort on unrecognized input", {
  testthat::expect_error(logprobs(42), "Could not extract log probabilities")
  testthat::expect_error(
    token_probs(42, "Yes"),
    "Could not extract token probabilities"
  )
})

test_that("token_probs() validates its arguments", {
  testthat::expect_error(token_probs(with_logprobs(), c("Yes", NA)))
  testthat::expect_error(token_probs(with_logprobs(), "Yes", position = 0L))
})
