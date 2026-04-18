# test-tools.R
# ::rtemis.llm::
# 2025 EDG rtemis.org

# %% ToolParameter Class ----
test_that("ToolParameter class works", {
  tool_param <- ToolParameter(
    name = "x",
    type = "number",
    description = "The first number to add",
    required = TRUE
  )
  expect_true(S7_inherits(tool_param, ToolParameter))
  expect_equal(tool_param@name, "x")
  expect_equal(tool_param@type, "number")
  expect_equal(tool_param@description, "The first number to add")
  expect_true(tool_param@required)
}) # /ToolParameter


# %% tool_param() ----
test_that("tool_param() works", {
  param <- tool_param(
    name = "y",
    type = "number",
    description = "The second number to add",
    required = FALSE
  )
  expect_true(S7_inherits(param, ToolParameter))
  expect_equal(param@name, "y")
  expect_equal(param@type, "number")
  expect_equal(param@description, "The second number to add")
  expect_false(param@required)
}) # /tool_param


# %% Tool Class ----
test_that("Tool class works", {
  tool <- Tool(
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
  )
  expect_true(S7_inherits(tool, Tool))
  expect_equal(tool@name, "addition")
  expect_equal(tool@description, "Performs arithmetic addition of two numbers.")
  expect_equal(length(tool@parameters), 2)
}) # /Tool


# %% create_tool() ----
tool_addition <- create_tool(
  name = "Addition",
  function_name = "add_numbers",
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
)
test_that("create_tool() works", {
  expect_true(S7_inherits(tool_addition, Tool))
  expect_equal(tool_addition@name, "Addition")
  expect_equal(length(tool_addition@parameters), 2)
}) # /create_tool()
