# test_Agent.R
# ::rtemis.llm::
# 2025 EDG rtemis.org

# %% Settings ---
model_name <- "qwen3.5:0.8b"


# %% Agent Class ----
test_that("Agent class works", {
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
