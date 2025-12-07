# test_AgentState.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% AgentState Class ----
test_that("AgentState class works", {
  state <- AgentState()
  testthat::expect_true(S7_inherits(state, AgentState))
}) # /AgentState

# %% InMemoryAgentState ----
test_that("InMemoryAgentState class works", {
  state <- InMemoryAgentState()
  testthat::expect_true(S7_inherits(state, InMemoryAgentState))
}) # /InMemoryAgentState

# %% append_message.AgentState + get_messages.AgentState ----

test_that("append_message and get_messages for AgentState work", {
  imas <- InMemoryAgentState()
  sm <- SystemMessage(
    name = "KMN",
    content = "You are a meticulous research assistant."
  )
  im <- InputMessage(
    name = "User1",
    content = "Hello, agent!"
  )
  tool_response <- ToolMessage(
    name = "addition",
    content = "The result of adding 2 and 3 is 5."
  )
  im2 <- InputMessage(
    name = "User1",
    content = "The tools returned: The result of adding 2 and 3 is 5. Use the tool response to answer the original question."
  )
  append_message(imas, sm)
  testthat::expect_equal(length(get_messages(imas)), 1)
  append_message(imas, im)
  testthat::expect_equal(length(get_messages(imas)), 2)
  append_message(imas, tool_response)
  testthat::expect_equal(length(get_messages(imas)), 3)
  append_message(imas, im2)
  msgs <- get_messages(imas)
  testthat::expect_equal(length(msgs), 4)
}) # /append_message + get_messages
