# test_schema.R
# ::rtemis.llm::
# 2025- EDG rtemis.org

# %% Field ----
test_that("Field succeeds with correct input", {
  field1 <- Field(
    name = "city_name",
    type = "string",
    description = "City name",
    required = TRUE
  )
  testthat::expect_true(S7_inherits(field1, Field))
})

test_that("Field fails with incorrect input", {
  testthat::expect_error(
    Field(
      name = "city_name",
      type = "any",
      description = "City name",
      required = TRUE
    )
  )
})


# %% Schema ----
test_that("Schema succeeds with correct input", {
  field1 <- Field(
    name = "city_name",
    type = "string",
    description = "City name",
    required = TRUE
  )
  field2 <- Field(
    name = "population",
    type = "number",
    description = "Population in millions",
    required = FALSE
  )
  schema1 <- Schema(
    name = "CitySchema",
    fields = list(field1, field2)
  )
  testthat::expect_true(S7_inherits(schema1, Schema))
})


test_that("Schema fails with incorrect input", {
  testthat::expect_error(
    Schema(
      name = "InvalidSchema",
      fields = list(
        list(type = "string", description = "City name", required = TRUE),
        list(type = "number", description = "Population", required = FALSE)
      )
    ),
    "Field"
  )
})


# %% field() ----
test_that("field() succeeds with correct input", {
  field1 <- field(
    name = "city_name",
    type = "string",
    description = "City name",
    required = TRUE
  )
  testthat::expect_true(S7_inherits(field1, Field))
})

# library(testthat)
# %% schema() ----
test_that("schema() works", {
  schema_obj <- schema(
    frequency = field(
      name = "frequency",
      type = "number",
      description = "Frequency of the oscillator"
    ),
    amplitude = field(
      name = "amplitude",
      type = "number",
      description = "Amplitude of the oscillator"
    ),
    oscillator_type = field(
      name = "oscillator_type",
      type = "string",
      description = "Type of the oscillator"
    )
  )
  testthat::expect_true(S7_inherits(schema_obj, Schema))

  # Create an R list defining the schema
  schema_list <- as_list(schema_obj)
  testthat::expect_type(schema_list, "list")
  testthat::expect_equal(schema_list[["type"]], "object")
  testthat::expect_true("frequency" %in% names(schema_list[["properties"]]))

  # Create a JSON string defining the schema
  schema_json <- to_json(
    schema(
      name = "OscillatorSchema",
      frequency = field(
        name = "frequency",
        type = "number",
        description = "Frequency of the oscillator"
      ),
      amplitude = field(
        name = "amplitude",
        type = "number",
        description = "Amplitude of the oscillator"
      ),
      oscillator_type = field(
        name = "oscillator_type",
        type = "string",
        description = "Type of the oscillator"
      )
    )
  )
  testthat::expect_type(schema_json, "character")
}) # /schema


# %% to_json.Field ----
test_that("to_json.Field succeeds", {
  field_json <- to_json(field("answer", "Answer text.", "string"))
  field_list <- jsonlite::fromJSON(field_json, simplifyVector = FALSE)
  testthat::expect_equal(field_list[["type"]], "string")
  testthat::expect_equal(field_list[["description"]], "Answer text.")
})


# %% to_json.Schema ----
test_that("to_json.Schema succeeds", {
  schema_obj <- schema(
    name = "TestSchema",
    answer = field(
      name = "answer",
      type = "string",
      description = "Answer text"
    )
  )
  schema_json <- to_json(schema_obj)
  schema_list <- jsonlite::fromJSON(schema_json, simplifyVector = FALSE)
  testthat::expect_equal(schema_list[["type"]], "object")
  testthat::expect_true("answer" %in% names(schema_list[["properties"]]))
  testthat::expect_equal(
    schema_list[["properties"]][["answer"]][["type"]],
    "string"
  )
  testthat::expect_equal(
    schema_list[["properties"]][["answer"]][["description"]],
    "Answer text"
  )
})


# %% Agent with output_schema ----
test_that("Agent with output_schema works", {
  output_schema <- schema(
    name = "OscillatorSchema",
    frequency = field(
      name = "frequency",
      type = "number",
      description = "Frequency of the oscillator"
    ),
    amplitude = field(
      name = "amplitude",
      type = "number",
      description = "Amplitude of the oscillator"
    ),
    oscillator_type = field(
      name = "oscillator_type",
      type = "string",
      description = "Type of the oscillator"
    )
  )
  agent <- Agent(
    llmconfig = config_OpenAI(
      model_name = "local-model",
      temperature = 0.3,
      base_url = "http://localhost:1234/v1",
      validate_model = FALSE
    ),
    tools = NULL,
    name = "SchemaAgent",
    output_schema = output_schema
  )
  testthat::expect_true(S7_inherits(agent, Agent))
  testthat::expect_true(S7_inherits(agent@output_schema, Schema))
  testthat::expect_equal(agent@output_schema, output_schema)
}) # /Agent with output_schema
