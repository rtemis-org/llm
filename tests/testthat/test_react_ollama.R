# test_react_ollama.R
# ::kaimana::
# 2025 EDG rtemis.org

# Setup ----
model_name <- "qwen3:8b"
model_name <- "ibm/granite3.3:8b"
tool_names <- "wikipedia"
verbosity <- 1L

# ReAct no tool needed ----
system_prompt <- paste(
  "You are a meticulous AI Assitant and your name is Kaimana.",
  "You were built at the Laboratory of Computational Medicine.",
)

agent <- create_ReAct(
  model_name = model_name,
  system_prompt = system_prompt,
  tool_names = tool_names,
  temperature = 0.1
)
agent
agent@state[["messages"]]

query <- "What is your name and who built you?"
res <- invoke(agent, query, verbosity = verbosity)
res
print(res@response[5])
res

# Test multiple turns ----
res2 <- invoke(agent, "My name is Zlorg. Pick a color.")


# Overwrite agent ----
agent <- create_ReAct(
  model_name = model_name,
  system_prompt = system_prompt,
  tool_names = tool_names,
  temperature = 0.1
)
agent
invoke(agent, "What is my name?")
invoke(agent, "What is your name?")

# ReAct wikipedia test ----
system_prompt <-
  "You are a meticulous research assistant that uses tools to answer queries grounded in reliable sources."
query <- "Summarize the history of California from Wikipedia."
query <- "Search Wikipedia and summarize the history of California in 4 bullet points."
agent <- ReAct(
  model_name = model_name,
  system_prompt = system_prompt,
  tool_names = tool_names,
  temperature = 0.1
)
agent

x <- agent

names(x@tools)
names(x@tools[[1]])

tools <- list(
  tool_wikipedia
)
