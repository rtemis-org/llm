# LLM.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% LLM Class ----

#' LLM Class
#'
#' A stateless LLM with support for reasoning.
#'
#' @name LLM
#' @title LLM Class
#' @description
#' Class for interacting with LLMs directly.
#'
#' @field model_name Character: The name of the LLM model to use.
#' @field system_prompt Character: The system prompt to use.
#'
#' @author EDG
#' @export
LLM <- new_class(
  "LLM",
  properties = list(
    model_name = class_character,
    system_prompt = class_character
  )
) # kaimana::LLM


# create LLM ----
#' Create a LLM Object
#'
#' @param model_name Character: The name of the LLM model to use. Must be an Ollama model.
#' @param system_prompt Character: The system prompt to use.
#'
#' @return A LLM object.
#'
#' @author EDG
#' @export
create_LLM <- function(model_name, system_prompt) {
  ollama_check_model(model_name)
  LLM(
    model_name = model_name,
    system_prompt = system_prompt
  )
} # /create_LLM


# repr method for LLM ----
method(repr, LLM) <- function(x, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name("LLM", output_type = output_type),
    fmt("Model: ", bold = TRUE, output_type = output_type),
    highlight(x@model_name, output_type = output_type)
  )
} # /repr.LLM


# Print method for LLM ----
method(print, LLM) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /print.LLM


# invoke method for LLM ----
method(invoke, LLM) <- function(
  x,
  prompt
) {
  ollama_check_model(x@model_name)
  resp <- ollamar::chat(
    model = x@model_name,
    messages = ollamar::create_messages(
      ollamar::create_message(x@system_prompt, role = "system"),
      ollamar::create_message(prompt, role = "user")
    )
  )
  AIResponse(
    ollamar::create_message(
      ollamar::resp_process(resp, output = "text"),
      role = "assistant"
    )
  )
} # /invoke.LLM
