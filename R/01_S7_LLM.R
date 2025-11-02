# LLM.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% LLM Class ----

#' LLM Class
#'
#' A stateless LLM with support for reasoning & structured output.
#'
#' @name LLM
#' @title LLM Class
#' @description
#' Class for interacting with LLMs directly.
#'
#' @field model_name Character: The name of the LLM model to use.
#' @field system_prompt Character: The system prompt to use.
#' @field temperature Numeric: The temperature for the model.
#'
#' @author EDG
#' @export
LLM <- new_class(
  "LLM",
  properties = list(
    model_name = class_character,
    system_prompt = class_character,
    temperature = class_numeric,
    output_schema = class_list
  )
) # kaimana::LLM


# create LLM ----
#' Create a LLM Object
#'
#' @param model_name Character: The name of the LLM model to use. Must be an Ollama model.
#' @param system_prompt Character: The system prompt to use.
#' @param temperature Numeric: The temperature for the model.
#' @param output_schema List: An optional output schema.
#'
#' @return A LLM object.
#'
#' @author EDG
#' @export
create_LLM <- function(
  model_name,
  system_prompt,
  temperature = 0.1,
  output_schema = NULL
) {
  ollama_check_model(model_name)
  LLM(
    model_name = model_name,
    system_prompt = system_prompt,
    temperature = temperature,
    output_schema = output_schema
  )
} # /create_LLM


# repr method for LLM ----
method(repr, LLM) <- function(x, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name("LLM", output_type = output_type),
    fmt("Model: ", bold = TRUE, output_type = output_type),
    highlight(x@model_name, output_type = output_type),
    fmt("\nTemperature: ", bold = TRUE, output_type = output_type),
    highlight(x@temperature, output_type = output_type),
    fmt(
      " (default value; can be overridden in invoke)",
      muted = TRUE,
      output_type = output_type
    ),
    fmt("\nSystem Prompt: ", bold = TRUE, output_type = output_type),
    highlight(x@system_prompt, output_type = output_type),
    if (!is.null(x@output_schema)) {
      paste0(
        fmt("\nOutput Schema: \n", bold = TRUE, output_type = output_type),
        highlight(
          # => Replace with repr_ls
          paste(capture.output(str(x@output_schema)), collapse = "\n"),
          output_type = output_type
        )
      )
    } else {
      ""
    }
  )
} # /repr.LLM


# Print method for LLM ----
method(print, LLM) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /print.LLM


# invoke method for LLM ----
method(invoke, LLM) <- function(
  x,
  prompt,
  temperature = NULL,
  output_schema = NULL
) {
  ollama_check_model(x@model_name)
  if (is.null(temperature)) {
    temperature <- x@temperature
  }
  format <- if (!is.null(output_schema)) {
    output_schema
  } else if (!is.null(x@output_schema)) {
    x@output_schema
  } else {
    list()
  }
  resp <- ollamar::chat(
    model = x@model_name,
    messages = ollamar::create_messages(
      ollamar::create_message(x@system_prompt, role = "system"),
      ollamar::create_message(prompt, role = "user")
    ),
    format = format
  )
  AIResponse(
    ollamar::create_message(
      ollamar::resp_process(resp, output = "text"),
      role = "assistant"
    )
  )
} # /invoke.LLM
