# test_llmapply.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# %% Settings ----
model_name <- "gemma4:e4b"

system_prompt <- paste(
  "Convert the following color to its hexadecimal code.",
  "Respond using the provided schema."
)

output_schema <- schema(
  name = "ColorSchema",
  field(
    name = "color",
    type = "string",
    description = "The name of the color in English"
  ),
  field(
    name = "hex",
    type = "string",
    description = "The hexadecimal code of the color"
  )
)

# %% llmapply ----
test_that("llmapply works without output_schema", {
  skip_if_ollama_model_missing(model_name)
  hex <- llmapply(
    c("teal", "orange", "burgundy"),
    system_prompt = "Return the hexadecimal code for the following color in format #FFFFFF",
    model_name = model_name
  )
  expect_length(hex, 3)
})

test_that("llmapply works with output_schema", {
  skip_if_ollama_model_missing(model_name)
  x <- c("teal", "orange", "burgundy")
  hex <- llmapply(x, system_prompt, model_name, output_schema = output_schema)
  expect_length(hex, length(x))
})
