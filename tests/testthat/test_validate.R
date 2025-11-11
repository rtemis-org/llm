# test_validate.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% validate_function ----
test_that("validate_function() works", {
  # Valid function
  expect_null(validate_function("query_semanticscholar"))
  # Invalid function
  expect_error(
    validate_function("nonexistent_tool_function"),
    "Unauthorized tool call: nonexistent_tool_function"
  )
}) # /validate_function
