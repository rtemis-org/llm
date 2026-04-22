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
  testthat::expect_identical(reasoning(ms), c("r1", NA_character_, NA_character_))
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
