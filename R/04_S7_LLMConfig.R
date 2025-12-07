# LLM.R
# ::kaimana::
# 2025 EDG rtemis.org

# References:
# Ollama API: https://docs.ollama.com/
# Ollama tool calling: https://docs.ollama.com/capabilities/tool-calling

# %% Constants ----
TEMPERATURE_DEFAULT <- 0.3
SYSTEM_PROMPT_DEFAULT <-
  "You are a meticulous research assistant. Your responses are always grounded in facts."
OLLAMA_URL_DEFAULT <- "http://localhost:11434"


# --- Internal API ---------------------------------------------------------------------------------
# %% LLMConfig Superclass ----
#' @title LLMConfig
#'
#' @description
#' LLM configuration superclass.
#'
#' @field model_name Character: The name of the LLM model to use.
#' @field system_prompt Character: The system prompt to use.
#' @field temperature Numeric: The temperature for the model.
#' @field backend Character: The backend to use.
#'
#' @author EDG
#' @noRd
LLMConfig <- new_class(
  "LLMConfig",
  properties = list(
    model_name = class_character,
    temperature = class_numeric,
    backend = class_character,
    base_url = class_character
  ),
  constructor = function(
    model_name,
    temperature,
    backend,
    base_url
  ) {
    # --- Validate inputs ---
    # Temperature must be numeric between 0.0 and 2.0
    if (temperature < 0.0 || temperature > 2.0) {
      cli::cli_abort("{.var temperature} must be between 0.0 and 2.0.")
    }
    new_object(
      S7_object(),
      model_name = model_name,
      temperature = temperature,
      backend = backend,
      base_url = base_url
    )
  }
) # /kaimana::LLMConfig


# %% as_list.LLMConfig ----
#' as_list method for LLMConfig
#'
#' @param x LLMConfig object
#'
#' @return List representation of LLMConfig
#'
#' @author EDG
#' @noRd
method(as_list, LLMConfig) <- function(x) {
  list(
    model_name = x@model_name,
    temperature = x@temperature,
    backend = x@backend,
    base_url = x@base_url
  )
} # /as_list.LLMConfig


# %% OllamaConfig Class ----
#' @title OllamaConfig Class
#'
#' @description
#' Ollama configuration class.
#'
#' @author EDG
#' @noRd
OllamaConfig <- new_class(
  "OllamaConfig",
  parent = LLMConfig,
  constructor = function(
    model_name,
    temperature,
    base_url
  ) {
    ollama_check_model(model_name)
    new_object(
      LLMConfig(
        model_name = model_name,
        temperature = temperature,
        backend = "ollama",
        base_url = base_url
      )
    )
  }
) # /kaimana::OllamaConfig


# %% repr.LLMConfig ----
# repr method for LLMConfig ----
method(repr, LLMConfig) <- function(x, pad = 0L, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name(
      sub(".*::", "", class(x)[1]),
      pad = pad,
      output_type = output_type
    ),
    repr_ls(
      as_list(x),
      pad = pad,
      print_class = FALSE,
      output_type = output_type
    )
  )
} # /repr.LLMConfig


# %% print.LLMConfig ----
# Print method for LLMConfig ----
method(print, LLMConfig) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /print.LLMConfig
