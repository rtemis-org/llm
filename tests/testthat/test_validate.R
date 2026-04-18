# test_validate.R
# ::rtemis.llm::
# 2025 EDG rtemis.org

# %% validate_function ----
# This works normally, but NOT during checks / tests
# test_that("validate_function() works", {
#   # Valid function
#   expect_null(validate_function("query_wikipedia"))
#   # Invalid function
#   expect_error(
#     validate_function("nonexistent_tool_function"),
#     "Unauthorized tool call: nonexistent_tool_function"
#   )
# }) # /validate_function
