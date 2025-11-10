# test_Agent.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% Agent Class ----
test_that("Agent class works", {
  agent <- Agent(
    llmconfig = config_Ollama(
      model_name = "qwen3:8b",
      temperature = 0.3,
      base_url = "http://localhost:11434"
    ),
    system_prompt = "You are a meticulous research assistant.",
    tools = NULL,
    name = "KMN"
  )
  testthat::expect_true(S7_inherits(agent, Agent))
}) # /Agent


# %% tool list
tools <- list(
  create_tool(
    name = "addition",
    description = "Performs arithmetic addition of two numbers.",
    parameters = list(
      tool_param(
        name = "x",
        type = "number",
        description = "The first number to add",
        required = TRUE
      ),
      tool_param(
        name = "y",
        type = "number",
        description = "The second number to add",
        required = TRUE
      )
    )
  ),
  create_tool(
    name = "subtraction",
    description = "Performs arithmetic subtraction of two numbers.",
    parameters = list(
      tool_param(
        name = "x",
        type = "number",
        description = "The number to subtract from",
        required = TRUE
      ),
      tool_param(
        name = "y",
        type = "number",
        description = "The number to subtract",
        required = TRUE
      )
    )
  )
)


# %% Agent Class ----
test_that("Agent class works", {
  agent <- Agent(
    llm = create_Ollama(
      model_name = "qwen3:8b",
      temperature = 0.3,
      base_url = "http://localhost:11434",
      system_prompt = "You are a meticulous research assistant."
    ),
    tools = tools
  )
  testthat::expect_true(S7_inherits(agent, Agent))
}) # /Agent
