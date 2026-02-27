# test_AgentMemory.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% AgentMemory Class ----
test_that("AgentMemory class works", {
  state <- AgentMemory()
  testthat::expect_true(S7_inherits(state, AgentMemory))
}) # /AgentMemory

# %% InProcessAgentMemory ----
test_that("InProcessAgentMemory class works", {
  state <- InProcessAgentMemory()
  testthat::expect_true(S7_inherits(state, InProcessAgentMemory))
}) # /InProcessAgentMemory

# %% append_message.AgentMemory + get_messages.AgentMemory ----

test_that("append_message and get_messages for AgentMemory work", {
  imas <- InProcessAgentMemory()
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
