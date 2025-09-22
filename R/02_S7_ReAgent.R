# ReAgent.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% ReasoningResponse Class ----
#' ReasoningResponse Class
#'
#' @name ReasoningResponse
#' @title ReasoningResponse Class
#' @description
#' Class for reasoning responses containing reasoning steps and final response.
#'
#' @field response List of format `
#' list(role = "reasoning", content = <character>, role = "response", content = <character>)`
#' containing reasoning and final response.

# %% ReAgent Class ----

#' ReAgent Class
#'
#' A stateless LLM with support for reasoning.
#'
#' Lather, Rinse, Repeat
#'
#' @name ReAgent
#' @title ReAgent Class
#' @description
#' Class for ReAgent agents
#'
#' @field model_name Character: The name of the LLM model to use.
#' @field system_prompt Character: The system prompt to use.
#'
#' @author EDG
#' @export
ReAgent <- new_class(
  "ReAgent",
  properties = list(
    model_name = class_character,
    system_prompt = class_character,
    output_format = class_list
  )
) # kaimana::ReAgent


# create ReAgent ----
#' Create a ReAgent Object
#'
#' @param model_name Character: The name of the LLM model to use. Must be an Ollama model.
#' @param system_prompt Character: The system prompt to use.
#'
#' @return A ReAgent object.
#'
#' @author EDG
#' @export
create_ReAgent <- function(model_name, system_prompt) {
  ollama_check_model(model_name)
  ReAgent(
    model_name = model_name,
    system_prompt = system_prompt
  )
} # /create_ReAgent


# repr method for ReAgent ----
method(repr, ReAgent) <- function(x, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name("ReAgent", output_type = output_type),
    fmt("Model: ", bold = TRUE, output_type = output_type),
    highlight(x@model_name, output_type = output_type)
  )
} # /repr.ReAgent


# Print method for ReAgent ----
method(print, ReAgent) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /print.ReAgent


# invoke method for ReAgent ----
method(invoke, ReAgent) <- function(
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
} # /invoke.ReAgent
