# test_schema.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% schema() ----
test_that("schema() works", {
  # Create an R list defining the schema
  schema_list <- schema(
    frequency = field("numeric", "Frequency of the oscillator"),
    amplitude = field("numeric", "Amplitude of the oscillator"),
    oscillator_type = field("string", "Type of the oscillator"),
    required = c("frequency", "amplitude"),
    output = "list"
  )
  testthat::expect_type(schema_list, "list")
  # Create a JSON string defining the schema
  schema_json <- schema(
    frequency = field("numeric", "Frequency of the oscillator"),
    amplitude = field("numeric", "Amplitude of the oscillator"),
    oscillator_type = field("string", "Type of the oscillator"),
    required = c("frequency", "amplitude"),
    output = "json"
  )
  testthat::expect_type(schema_json, "character")
}) # /schema


# %% Agent with output_schema ----
test_that("Agent with output_schema works", {
  output_schema <- schema(
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
  agent <- Agent(
    llmconfig = config_Ollama(
      model_name = "qwen3:8b",
      temperature = 0.3,
      base_url = "http://localhost:11434"
    ),
    tools = NULL,
    name = "SchemaAgent",
    output_schema = output_schema
  )
  testthat::expect_true(S7_inherits(agent, Agent))
  testthat::expect_equal(agent@output_schema, output_schema)
}) # /Agent with output_schema
