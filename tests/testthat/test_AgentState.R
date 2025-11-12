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

# %% append_message.AgentState ----
imas <- InMemoryAgentState()
imas
system_prompt <- "You are a meticulous research assistant named Kaimana."
sm <- SystemMessage(
  name = "KMN",
  content = system_prompt
)
sm
append_message(imas, sm)
imas
im <- InputMessage(
  name = "User1",
  content = "Hello, agent!"
)
im
append_message(imas, im)
imas
tool_response <- ToolMessage(
  name = "addition",
  content = "The result of adding 2 and 3 is 5."
)
append_message(imas, tool_response)
im2 <- InputMessage(
  name = "User1",
  content = "The tools returned: The result of adding 2 and 3 is 5. Use the tool response to answer the original question."
)
append_message(imas, im2)

# %% get_messages.AgentState ----
msgs <- get_messages(imas)
msgs
length(msgs)

# %%
z <- new.env()
z

sm

# Append sm to z
z[[1]] <- sm


im <- InputMessage(
  name = "User1",
  content = "Hello, world!"
)
