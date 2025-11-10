# test_schema.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% schema() ----
test_that("schema() works", {
  schema_list <- schema(
    frequency = list(
      type = "numeric",
      description = "Frequency of the oscillator"
    ),
    amplitude = list(
      type = "numeric",
      description = "Amplitude of the oscillator"
    ),
    oscillator_type = list(
      type = "string",
      description = "Type of the oscillator"
    ),
    required = c("frequency", "amplitude"),
    output = "list"
  )
  testthat::expect_type(schema_list, "list")
  schema_json <- schema(
    frequency = list(
      type = "numeric",
      description = "Frequency of the oscillator"
    ),
    amplitude = list(
      type = "numeric",
      description = "Amplitude of the oscillator"
    ),
    oscillator_type = list(
      type = "string",
      description = "Type of the oscillator"
    ),
    required = c("frequency", "amplitude"),
    output = "json"
  )
  testthat::expect_type(schema_json, "character")
}) # /schema
