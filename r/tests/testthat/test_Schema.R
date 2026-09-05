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


# %% Field enum ----
test_that("field() round-trips an enum through as_list() and to_json()", {
  f <- field(
    "support",
    "Support level",
    type = "string",
    enum = c("none", "partial", "complete")
  )
  f_list <- as_list(f)
  testthat::expect_equal(
    as.character(f_list[["enum"]]),
    c("none", "partial", "complete")
  )
  f_json <- jsonlite::fromJSON(to_json(f), simplifyVector = FALSE)
  testthat::expect_equal(
    unlist(f_json[["enum"]]),
    c("none", "partial", "complete")
  )
})

test_that("a single permitted value stays a JSON array", {
  f_json <- to_json(field("support", "Support level", enum = "none"))
  testthat::expect_match(f_json, '"enum":["none"]', fixed = TRUE)
})

test_that("an integer field's enum is emitted as JSON numbers", {
  f <- field("grade", "Grade", type = "integer", enum = c("1", "2", "3"))
  testthat::expect_identical(as.integer(as_list(f)[["enum"]]), 1:3)
  testthat::expect_match(to_json(f), '"enum":[1,2,3]', fixed = TRUE)
})

test_that("as_list.Schema() nests enum inside the property, not the schema", {
  sch <- schema(
    "SupportSchema",
    field("support", "Support level", enum = c("none", "complete"))
  )
  sch_list <- as_list(sch)
  testthat::expect_null(sch_list[["enum"]])
  testthat::expect_equal(
    as.character(sch_list[["properties"]][["support"]][["enum"]]),
    c("none", "complete")
  )
})

test_that("Field rejects enum on a type that cannot carry one", {
  testthat::expect_error(
    field("flag", "Flag", type = "boolean", enum = c("yes", "no")),
    "boolean"
  )
  testthat::expect_error(
    field("tags", "Tags", type = "array", enum = c("a", "b")),
    "array"
  )
})

test_that("Field rejects an empty, missing, or duplicated enum", {
  testthat::expect_error(field("support", "Support", enum = character(0)))
  testthat::expect_error(
    field("support", "Support", enum = c("none", NA_character_))
  )
  testthat::expect_error(field("support", "Support", enum = c("a", "a")))
})

test_that("Field rejects enum values that do not fit the declared type", {
  testthat::expect_error(
    field("grade", "Grade", type = "integer", enum = c("a", "b")),
    "integer"
  )
  # as.integer() would silently truncate this one.
  testthat::expect_error(
    field("grade", "Grade", type = "integer", enum = c("1.5", "2")),
    "integer"
  )
})


# %% enum backend pass-through ----
test_that("enum reaches the Ollama request body's `format` field", {
  testthat::local_mocked_bindings(
    ollama_check_model = function(x) invisible(NULL),
    .package = "rtemis.llm"
  )
  sch <- schema(
    "SupportSchema",
    field("support", "Support level", enum = c("none", "partial", "complete"))
  )
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "hi"),
    echo = FALSE,
    verbosity = 0L
  )
  request_body <- build_chat_request_body(
    config_Ollama(model_name = "gemma4:e4b"),
    state = state,
    output_schema = sch
  )
  testthat::expect_equal(
    as.character(
      request_body[["format"]][["properties"]][["support"]][["enum"]]
    ),
    c("none", "partial", "complete")
  )
})

test_that("clean_openai_schema() leaves enum untouched", {
  sch_list <- as_list(schema(
    "SupportSchema",
    field("support", "Support level", enum = c("none", "partial", "complete"))
  ))
  cleaned <- clean_openai_schema(sch_list)
  testthat::expect_equal(
    as.character(cleaned[["properties"]][["support"]][["enum"]]),
    c("none", "partial", "complete")
  )
})

test_that("the Anthropic structured-output tool keeps enum", {
  sch_list <- as_list(schema(
    "SupportSchema",
    field("support", "Support level", enum = c("none", "partial", "complete"))
  ))
  tool_spec <- .anthropic_structured_output_tool(sch_list)
  testthat::expect_equal(
    as.character(
      tool_spec[["input_schema"]][["properties"]][["support"]][["enum"]]
    ),
    c("none", "partial", "complete")
  )
})
