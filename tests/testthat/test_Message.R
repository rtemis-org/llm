# test_Message.R
# ::rtemis.llm::
# 2025- EDG rtemis.org

# %% Settings ----
model_name <- "qwen3.5:0.8b"


# %% Message Class ----
test_that("Message class works", {
  msg <- Message(
    role = "llm",
    name = "Preprocessor",
    content = "Hello.",
    metadata = list(project = "kmn")
  )
  testthat::expect_true(S7_inherits(msg, Message))
}) # /Message


# %% SystemMessage Class ----
test_that("SystemMessage class works", {
  msg <- SystemMessage(
    name = "KMN",
    content = "You are a meticulous research assistant.",
    metadata = list(project = "kmn")
  )
  testthat::expect_true(S7_inherits(msg, SystemMessage))
}) # /SystemMessage


# %% InputMessage Class ----
test_that("InputMessage class works", {
  msg <- InputMessage(
    content = "Process this data.",
    metadata = list(project = "kmn")
  )
  testthat::expect_true(S7_inherits(msg, InputMessage))
}) # /InputMessage


# %% InputMessage + image_path Class ----
test_that("InputMessage class works", {
  msg <- InputMessage(
    content = "Process this data.",
    metadata = list(project = "kmn"),
    image_path = "path/to/image.png"
  )
  testthat::expect_true(S7_inherits(msg, InputMessage))
}) # /InputMessage


# %% LLMMessage Class (no reasoning) ----
test_that("LLMMessage class works", {
  msg <- LLMMessage(
    name = "Preprocessor",
    content = "Hello.",
    metadata = list(project = "kmn"),
    model_name = model_name
  )
  testthat::expect_true(S7_inherits(msg, LLMMessage))
}) # /LLMMessage

# %% LLMMessage Class ----
test_that("LLMMessage class works", {
  msg <- LLMMessage(
    name = "Preprocessor",
    content = "Hello.",
    metadata = list(project = "kmn"),
    model_name = model_name,
    reasoning = "I think therefore I am."
  )
  testthat::expect_true(S7_inherits(msg, LLMMessage))
}) # /LLMMessage


# %% OllamaMessage Class ----
test_that("OllamaMessage class works", {
  msg <- OllamaMessage(
    name = "Preprocessor",
    content = "Hello.",
    metadata = list(project = "kmn"),
    model_name = model_name,
    reasoning = "I think therefore I am."
  )
  testthat::expect_true(S7_inherits(msg, OllamaMessage))
  testthat::expect_equal(msg@metadata[["provider"]], "Ollama")
}) # /OllamaMessage


# %% AgentMessage Class ----
test_that("AgentMessage class works", {
  msg <- AgentMessage(
    name = "KMN",
    content = "Processing data.",
    metadata = list(task = "data cleaning")
  )
  testthat::expect_true(S7_inherits(msg, AgentMessage))
}) # /AgentMessage


# %% create_llm_message.OllamaConfig ----
test_that("create_llm_message.OllamaConfig works", {
  config <- OllamaConfig(
    model_name = model_name,
    temperature = 0.5,
    base_url = "http://localhost:11434"
  )
  m <- create_llm_message(
    config,
    content = "I respond because I think.",
    reasoning = "I think, therefore I respond.",
    name = "SuperAgent",
    tool_calls = NULL
  )
  testthat::expect_true(S7_inherits(m, OllamaMessage))
  testthat::expect_equal(m@name, "SuperAgent")
  testthat::expect_equal(m@content, "I respond because I think.")
  testthat::expect_equal(m@reasoning, "I think, therefore I respond.")
}) # /create_llm_message.OllamaConfig


# %%create_llm_message.Agent ----
test_that("create_llm_message.Agent works", {
  agent <- create_agent(
    llmconfig = config_Ollama(
      model_name = model_name,
      temperature = 0.3,
    ),
    system_prompt = "You are a meticulous research assistant.",
    tools = NULL,
    name = "KMN"
  )
  m <- create_llm_message(
    agent,
    content = "I respond because I think.",
    reasoning = "I think, therefore I respond.",
    tool_calls = NULL
  )
  testthat::expect_true(S7_inherits(m, OllamaMessage))
  testthat::expect_equal(m@name, "KMN")
  testthat::expect_equal(m@content, "I respond because I think.")
  testthat::expect_equal(m@reasoning, "I think, therefore I respond.")
}) # /create_llm_message.Agent
