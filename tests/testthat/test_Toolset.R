# test_Toolset.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% ToolParameter Class ----
test_that("ToolParameter class works", {
  tool_param <- ToolParameter(
    name = "query",
    type = "character",
    description = "The query to run",
    required = TRUE
  )
  expect_true(S7_inherits(tool_param, ToolParameter))
  expect_equal(tool_param@name, "query")
  expect_equal(tool_param@type, "character")
  expect_equal(tool_param@description, "The query to run")
  expect_true(tool_param@required)
}) # /ToolParameter class


# %% definte_create_tool() ----
tp1 <- tool_param(
  name = "query",
  type = "character",
  description = "The query to run",
  required = TRUE
)
test_that("define_create_tool() works", {
  expect_true(S7_inherits(tp1, ToolParameter))
}) # /define_create_tool()


# %% Tool Class ----
tp2 <- tool_param(
  name = "limit",
  type = "numeric",
  description = "The maximum number of results to return",
  required = FALSE
)
test_that("Tool class works", {
  tool <- create_tool(
    name = "SuperSearch",
    description = "A tool for searching the web",
    parameters = list(tp1, tp2)
  )
  expect_true(S7_inherits(tool, Tool))
  expect_equal(tool@name, "SuperSearch")
  expect_equal(tool@description, "A tool for searching the web")
  expect_type(tool@parameters, "list")
}) # /Tool class


# %% create_tool() ----
test_that("create_tool() works", {
  tool <- create_tool(
    name = "Pythia",
    description = "A tool for answering questions about the future",
    parameters = list(
      tool_param(
        name = "question",
        type = "character",
        description = "The question to answer",
        required = TRUE
      ),
      tool_param(
        name = "years_ahead",
        type = "numeric",
        description = "Number of years to look ahead",
        required = FALSE
      )
    )
  )
  expect_true(S7_inherits(tool, Tool))
  expect_equal(tool@name, "Pythia")
  expect_equal(
    tool@description,
    "A tool for answering questions about the future"
  )
  expect_type(tool@parameters, "list")
}) # /create_tool()


# %% Toolset Class ----
tool1 <- create_tool(
  name = "tool_one",
  description = "First tool",
  parameters = list(tp1)
)
tool2 <- create_tool(
  name = "tool_two",
  description = "Second tool",
  parameters = list(tp2)
)
test_that("Toolset class works", {
  toolset_instance <- Toolset(
    tools = list(tool1, tool2)
  )
  expect_true(S7_inherits(toolset_instance, Toolset))
  expect_type(toolset_instance@tools, "list")
  expect_equal(length(toolset_instance@tools), 2)
}) # /Toolset class


# %% toolset() ----
tset <- toolset(tool1, tool2)
test_that("toolset() works", {
  expect_true(S7_inherits(tset, Toolset))
  expect_type(tset@tools, "list")
  expect_equal(length(tset@tools), 2)
}) # /toolset()
