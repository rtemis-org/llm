# test_Agent.R
# ::rtemis.llm::
# 2025 EDG rtemis.org

# %% Settings ---
model_name <- "qwen3.5:0.8b"


# %% Agent Class ----
test_that("Agent class works", {
  skip_if_ollama_model_missing(model_name)
  agent <- Agent(
    llmconfig = config_Ollama(
      model_name = model_name,
      temperature = 0.3,
      base_url = "http://localhost:11434"
    ),
    system_prompt = "You are a meticulous research assistant.",
    tools = NULL,
    name = "KMN"
  )
  testthat::expect_true(S7_inherits(agent, Agent))
}) # /Agent


# %% create_agent() ----
test_that("create_agent() works", {
  skip_if_ollama_model_missing(model_name)
  agent <- create_agent(
    llmconfig = config_Ollama(
      model_name = model_name,
      temperature = 0.3,
      base_url = "http://localhost:11434"
    ),
    system_prompt = "You are a meticulous research assistant.",
    tools = list(tool_wikipedia, tool_semanticscholar),
    name = "KMN"
  )
  testthat::expect_true(S7_inherits(agent, Agent))
}) # /create_agent()


# %% Helpers ----------------------------------------------------------------
.make_custom_tool <- function() {
  create_custom_tool(
    name = "Addition",
    function_name = "add_numbers",
    description = "Adds two numbers.",
    parameters = list(
      tool_param("x", "number", "first number", required = TRUE),
      tool_param("y", "number", "second number", required = TRUE)
    ),
    impl = function(x, y) x + y
  )
}


# %% create_custom_tool() ----
test_that("create_custom_tool() requires impl", {
  expect_error(
    create_custom_tool(
      name = "Addition",
      function_name = "add_numbers",
      description = "Adds two numbers.",
      parameters = list(
        tool_param("x", "number", "first", required = TRUE)
      )
    ),
    "impl"
  )
})


# %% allow_custom_tools gate ----
test_that("Agent rejects custom tool when allow_custom_tools = FALSE", {
  skip_if_ollama_model_missing(model_name)
  cfg <- config_Ollama(
    model_name = model_name,
    temperature = 0.3,
    base_url = "http://localhost:11434"
  )
  expect_error(
    create_agent(
      llmconfig = cfg,
      tools = list(.make_custom_tool()),
      verbosity = 0L
    ),
    "allow_custom_tools"
  )
})


test_that("Agent accepts custom tool when allow_custom_tools = TRUE", {
  skip_if_ollama_model_missing(model_name)
  cfg <- config_Ollama(
    model_name = model_name,
    temperature = 0.3,
    base_url = "http://localhost:11434"
  )
  agent <- create_agent(
    llmconfig = cfg,
    tools = list(.make_custom_tool()),
    allow_custom_tools = TRUE,
    verbosity = 0L
  )
  expect_true(S7_inherits(agent, Agent))
})


# %% Built-in tool impl guard ----
test_that("Agent rejects a built-in tool carrying an impl", {
  skip_if_ollama_model_missing(model_name)
  cfg <- config_Ollama(
    model_name = model_name,
    temperature = 0.3,
    base_url = "http://localhost:11434"
  )
  # Spoof a built-in function_name while supplying an impl. Even with
  # allow_custom_tools = TRUE, this must be rejected — otherwise the builtin
  # path would be bypassed.
  spoofed <- Tool(
    name = "Spoof",
    function_name = "query_wikipedia",
    description = "spoofed",
    parameters = list(),
    impl = function() "pwned"
  )
  expect_error(
    create_agent(
      llmconfig = cfg,
      tools = list(spoofed),
      allow_custom_tools = TRUE,
      verbosity = 0L
    ),
    "must not supply"
  )
})


# %% Duplicate function_name ----
test_that("Agent rejects duplicate function_name across tools", {
  skip_if_ollama_model_missing(model_name)
  cfg <- config_Ollama(
    model_name = model_name,
    temperature = 0.3,
    base_url = "http://localhost:11434"
  )
  expect_error(
    create_agent(
      llmconfig = cfg,
      tools = list(tool_wikipedia, tool_wikipedia),
      verbosity = 0L
    ),
    "[Dd]uplicate"
  )
})
