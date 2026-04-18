# test_llmapply.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# %% Settings ----
model_name <- "gpt-oss:20b"

# %% llmapply ----
library(rtemis.llm)
model_name <- "gemma4:e4b"
system_prompt <- paste(
  "Convert the following color to its hexadecimal code.",
  "Respond using the provided schema."
)
output_schema <- schema(
  name = "ColorSchema",
  field(name = "color", type = "string", description = "The name of the color in English"),
  field(name = "hex", type = "string", description = "The hexadecimal code of the color")
)
x <- c("teal", "orange", "burgundy")
hex <- llmapply(x, system_prompt, model_name, output_schema = output_schema)
hex

# %%
library(rtemis.llm)
hex <- llmapply(
  c("teal", "orange", "burgundy"),
  system_prompt = "Return the hexadecimal code for the following color in format #FFFFFF",
  model_name = "gemma4:e4b"
)
hex

# %% dev ----
model_name <- "gemma4:e4b"
system_prompt <- paste(
  "Convert the following color to its hexadecimal code.",
  "Respond using the provided schema."
)
output_schema <- schema(
  name = "ColorSchema",
  field(name = "color", type = "string", description = "The name of the color in English"),
  field(name = "hex", type = "string", description = "The hexadecimal code of the color")
)
agent <- create_agent(
  config_Ollama(model_name),
  system_prompt = system_prompt,
  use_memory = FALSE,
  output_schema = output_schema
)
hex_teal <- generate(agent, "teal")

# test_that("llmapply works", {
#   skip_if_ollama_model_missing(model_name)
})
