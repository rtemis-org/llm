# test_km-client.R
# ::kaimana-r::
# 2025 EDG rtemis.org

# library(testthat)

# >>> These tests require access to a running Kaimana REST API <<<
# that's why they are skipped by default and are only run manually
if (FALSE) {
  # %% Test km_health_check()
  test_that("km_health_check works", {
    skip_on_cran()
    res <- km_health_check()
    expect_type(res, "list")
    expect_true("status" %in% names(res))
  })

  # %% Test
  test_that("km_get_models works", {
    skip_on_cran()
    res <- km_get_models()
    expect_s3_class(res, "data.frame")
    expect_true(all(c("id", "name") %in% names(res)))
  })

  # %% Test km_generate() without thinking
  test_that("km_generate works", {
    skip_on_cran()
    res <- km_generate("What is your name?", model_name = "granite3.3:8b")
    expect_type(res, "list")
    expect_true("response" %in% names(res))
  })

  # %% Test km_generate() with thinking
  test_that("km_generate works with thinking", {
    skip_on_cran()
    res <- km_generate(
      "Pick a color.",
      model_name = "qwen3:8b",
      temperature = 0.5
    )
    expect_type(res, "list")
    expect_true("id" %in% names(res))
  })

  # %% Test make_output_schema()
  state_schema <- make_output_schema(
    capital = list(
      type = "string",
      description = "The state capital"
    ),
    largest_city = list(
      type = "string",
      description = "The largest city in the state"
    ),
    population = list(
      type = "number",
      description = "The state's population in millions"
    ),
    area = list(
      type = "number",
      description = "The state's area in square kilometers"
    ),
    required = "all"
  )
  test_that("make_output_schema works", {
    expect_type(state_schema, "list")
    expect_equal(state_schema[["type"]], "object")
    expect_true(all(
      c("capital", "largest_city", "population", "area") %in%
        names(state_schema[["properties"]])
    ))
    expect_equal(
      state_schema[["required"]],
      c("capital", "largest_city", "population", "area")
    )
  })

  # %% Test km_generate() with thinking and structured output
  test_that("km_generate works with thinking", {
    skip_on_cran()
    res <- km_generate(
      "Tell me about California.",
      output_schema = state_schema,
      model_name = "qwen3:8b",
      temperature = 0.5
    )
    expect_type(res, "list")
    expect_true("id" %in% names(res))
  })

  # %% Simple schema
  person_schema <- make_output_schema(
    first_name = list(
      type = "string",
      description = "The person's name"
    ),
    last_name = list(
      type = "string",
      description = "The person's last name"
    ),
    email = list(
      type = "string",
      description = "The person's email address"
    ),
    required = c("first_name", "last_name", "email")
  )

  res_person <- km_generate(
    prompt = "Here is what we know about the person: Julia Artiste works at Infogen and can be reached at ja@ig.com",
    output_schema = person_schema,
    model_name = "qwen3:8b",
    temperature = 0.1,
    url = "http://localhost:31650",
    verbosity = 1L
  )
} # disabled because it requires running kaimana-py server

# %% Test format_ai_message()
test_that("format_ai_message works", {
  x <- "<think>
Okay, the user asked, \"What is your name?\" I need to respond with my name, which is Kaimana. Let me make sure I'm following the guidelines. The user mentioned that I should only provide relevant and factual information. Since the question is straightforward, I should answer directly. I don't need to add any extra details beyond stating my name. Let me check if there's any other information required, but the user just wants to know my name. Alright, I'll respond with \"
My name is Kaimana.\" That's it.
</think>

My name is Kaimana."
  res <- format_ai_message(x)
  expect_type(res, "list")
  expect_true(all(c("thinking", "response") %in% names(res)))
  expect_type(res$thinking, "character")
  expect_type(res$response, "character")
})

# %% Test AIMessage class
test_that("AIMessage class works", {
  x <- "<think>
Okay, the user asked, \"What is your name?\" I need to respond with my name, which is Kaimana. Let me make sure I'm following the guidelines. The user mentioned that I should only provide relevant and factual information. Since the question is straightforward, I should answer directly. I don't need to add any extra details beyond stating my name. Let me check if there's any other information required, but the user just wants to know my name. Alright, I'll respond with \"
My name is Kaimana.\" That's it.
</think>

My name is Kaimana."
  aimsg <- AIMessage(x)
  expect_true(S7_inherits(aimsg, AIMessage))
  expect_type(aimsg[["thinking"]], "character")
  expect_type(aimsg[["content"]], "character")
})
