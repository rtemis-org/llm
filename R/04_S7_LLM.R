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


# %% repr.OllamaConfig ----
# repr method for OllamaConfig ----
method(repr, OllamaConfig) <- function(x, pad = 0L, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name("OllamaConfig", pad = pad, output_type = output_type),
    fmt("Model Name: ", bold = TRUE, output_type = output_type),
    highlight(x@model_name, output_type = output_type),
    "\n",
    fmt("Temperature: ", bold = TRUE, output_type = output_type),
    highlight(x@temperature, output_type = output_type),
    "\n",
    fmt("Base URL: ", bold = TRUE, output_type = output_type),
    highlight(x@base_url, output_type = output_type),
    "\n"
  )
} # /repr.OllamaConfig


# %% config_Ollama() ----
#' Create an OllamaConfig Object
#'
#' @param model_name Character: The name of the LLM model to use. Must be an Ollama model.
#' @param temperature Numeric: The temperature for the model.
#' @param base_url Character: Base URL of Ollama server.
#'
#' @return OllamaConfig object.
#'
#' @author EDG
#' @export
config_Ollama <- function(
  model_name,
  temperature = TEMPERATURE_DEFAULT,
  base_url = OLLAMA_URL_DEFAULT
) {
  OllamaConfig(
    model_name = model_name,
    temperature = temperature,
    base_url = base_url
  )
} # /config_Ollama


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
#' @field config LLMConfig: The LLM configuration.
#' @field system_prompt Character: The system prompt to use.
#'
#' @author EDG
#' @export
LLM <- new_class(
  "LLM",
  properties = list(
    system_prompt = class_character
  )
) # kaimana::LLM


# %% Ollama Class ----
#' @title Ollama Class
#'
#' @description
#' Ollama LLM class.
#'
#' @author EDG
#' @noRd
Ollama <- new_class(
  "Ollama",
  parent = LLM,
  properties = list(
    name = new_union(NULL | class_character),
    config = OllamaConfig,
    output_schema = new_union(NULL | class_list)
  ),
  constructor = function(
    name = NULL,
    config,
    system_prompt,
    output_schema = NULL
  ) {
    ollama_check_model(config@model_name)
    new_object(
      LLM(
        system_prompt = system_prompt
      ),
      name = name,
      config = config,
      output_schema = output_schema
    )
  }
) # /kaimana::Ollama


# %% create_Ollama() ----
#' Create an Ollama Object
#'
#' @param model_name Character: The name of the LLM model to use. Must be an Ollama model.
#' @param system_prompt Character: The system prompt to use.
#' @param temperature Numeric: The temperature for the model.
#' @param output_schema List: An optional output schema.
#' @param name Character or NULL: An optional name for the Ollama object.
#' @param base_url Character: Base URL of Ollama server.
#'
#' @return Ollama object.
#'
#' @author EDG
#' @export
create_Ollama <- function(
  model_name,
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  temperature = TEMPERATURE_DEFAULT,
  output_schema = NULL,
  name = NULL,
  base_url = OLLAMA_URL_DEFAULT
) {
  ollama_check_model(model_name)
  Ollama(
    name = name,
    config = OllamaConfig(
      model_name = model_name,
      temperature = temperature,
      base_url = base_url
    ),
    system_prompt = system_prompt,
    output_schema = output_schema
  )
} # /create_Ollama


# %% rerpr.LLM() ----
# repr method for LLM ----
method(repr, LLM) <- function(x, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name("LLM", output_type = output_type),
    if (!is.null(x@name)) {
      paste(
        fmt("Name: ", bold = TRUE, output_type = output_type),
        highlight(x@name, output_type = output_type),
        "\n"
      )
    },
    fmt("Model: ", bold = TRUE, output_type = output_type),
    highlight(x@config@model_name, output_type = output_type),
    "\n",
    fmt("System Prompt: ", bold = TRUE, output_type = output_type),
    highlight(x@system_prompt, output_type = output_type),
    "\n",
    fmt("Temperature: ", bold = TRUE, output_type = output_type),
    highlight(x@config@temperature, output_type = output_type),
    "\n",
    if (!is.null(x@output_schema)) {
      paste0(
        fmt("Output Schema: \n", bold = TRUE, output_type = output_type),
        highlight(
          # => Replace with repr_ls
          paste(capture.output(str(x@output_schema)), collapse = "\n"),
          output_type = output_type
        ),
        "\n"
      )
    }
  )
} # /repr.LLM


# %% print.LLM ----
# Print method for LLM ----
method(print, LLM) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /print.LLM


# %% generate.Ollama() ----
#' Generate method for Ollama
#'
#' @param x Ollama object
#' @param prompt Character: The prompt to send to the model.
#' @param verbosity Integer: Verbosity level.
#'
#' @return AIResponse object
#' @author EDG
#'
#' @noRd
method(generate, Ollama) <- function(x, prompt, tools = NULL, verbosity = 1L) {
  # Check input
  check_inherits(prompt, "character")
  # Request
  request_body <- list(
    model = x@config@model_name,
    system = x@system_prompt,
    prompt = prompt,
    stream = FALSE,
    options = list(
      temperature = x@config@temperature
    )
  )
  if (verbosity > 0) {
    output_type <- get_output_type()
    cat(repr_bracket(x@config@model_name), "working...")
  }
  # Perform request
  resp <- httr2::request(paste0(x@config@base_url, "/api/generate")) |>
    httr2::req_body_json(request_body) |>
    httr2::req_perform(verbosity = verbosity - 1L)
  # Check for errors
  httr2::resp_check_status(resp)
  if (verbosity > 0) {
    # Replace working message with done
    cat("\r", repr_bracket(x@config@model_name), "done.    \n")
  }
  as_OllamaMessage(httr2::resp_body_json(resp))
} # /kaimana::generate.Ollama


# %% invoke.LLM() ----
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
