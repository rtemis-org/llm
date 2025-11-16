# test-tools.R
# ::kaimana::
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


# %% wikipedia_search() ----
query <- "tau"
wiki_results <- wikipedia_search(query = query, limit = 5L, lang = "en")
test_that("wikipedia_search() returns expected results", {
  expect_s3_class(wiki_results, "data.frame")
  expect_equal(nrow(wiki_results), 5L)
  expect_true(all(
    c("title", "snippet", "pageid") %in% colnames(wiki_results)
  ))
})

# %% wikipedia_get() ----
article <- wikipedia_get(title = wiki_results[1, "title"])
test_that("wikipedia_get() returns expected results", {
  expect_type(article, "list")
  expect_true(all(c("title", "text") %in% names(article)))
  expect_equal(article[["title"]], wiki_results[1, "title"])
  expect_true(nchar(article[["text"]]) > 0)
})
