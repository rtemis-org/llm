# utils_ollama.R
# ::kaimana::
# 2025 EDG rtemis.org

# TOC:
#   - ollama_list_models
#   - ollama_get_model_info

# References
# https://hauselin.github.io/ollama-r/

#' List Ollama Models
#'
#' @return Character vector: Model names.
#'
#' @author EDG
#' @export
ollama_list_models <- function() {
  ollamar::list_models(output = "text")
} # /ollama_list_models


#' Get Ollama Model Info
#'
#' @param model Character: Name of model. If NULL, all models are returned.
#' @param output Character: Type of output to return: "df", "resp", "jsonlist", "raw", "text"
#'
#' @return Depending on `output`.
#'
#' @author EDG
#' @export
ollama_get_model_info <- function(model = NULL, output = "df") {
  models <- ollamar::list_models(output = output)
  if (is.null(model)) {
    return(models)
  } else {
    model_info <- models[models$name == model, , drop = FALSE]
    if (nrow(model_info) == 0) {
      stop("Model ", model, " not found.")
    }
    if (nrow(model_info) == 0) {
      stop("Model ", model, " not found.")
    }
    return(model_info)
  }
} # /ollama_get_model_info

#' Check Ollama Model is Available
#'
#' @param model Character: Name of model.
#'
#' @return Logical: TRUE if model is available, FALSE otherwise.
#'
#' @author EDG
#' @export
ollama_check_model <- function(model) {
  if (model %in% ollama_list_models()) {
    invisible(NULL)
  } else {
    stop(
      "Model ",
      model,
      " is not available. Please check the model name and install if necessary."
    )
  }
} # /ollama_check_model

#' Message Ollama
#'
#' @param model Character: Name of model to use.
#' @param system Character: Prompt to send to model (as system message).
#' @param user Character: Content to send to model (as user message).
#' @param output_type Character: Type of output to return: "df", "jsonlist", "raw", "resp", "text",
#' or "tools".  Default is "text".
#'
#' @return Character: Model response.
#'
#' @author EDG
#' @export
ollama_chat <- function(
  model,
  system = NULL,
  user = "Hello.",
  output_type = "text"
) {
  ollama_check_model(model)
  messages <- Filter(
    Negate(is.null),
    list(
      if (!is.null(system)) list(role = "system", content = system),
      list(role = "user", content = user)
    )
  )
  response <- ollamar::chat(
    model = model,
    messages = messages
  )
  ollamar::resp_process(resp = response, output = output_type)
} # /ollama_chat


#' Generate Ollama Response
#'
#' @param model Character: Name of model to use.
#' @param prompt Character: Prompt to send to model.
#' @param output_schema List: Format of the response. Default is NULL. See Details for example.
#' @param output_type Character: Type of output to return: "jsonlist", "raw", "df", "text", "req"
#'
#' @return Character: Model response depending on `output_type`.
#'
#' @author EDG
#' @export
#'
#' @details
#' The `output_schema` argument is a list that defines the expected format of the response.
#' For example, a (rather generic) prompt of "Tell me about Hawaii." with output_schema:
#' \preformatted{
#' list(
#'   Name = list(type = "string"),
#'   Capital = list(type = "string"),
#'   Languages = list(
#'     type = "array",
#'     items = list(type = "string")
#'   )
#' )
#' }
#'
#' would return a response like:
#' \preformatted{
#' {
#'   "Name": "Hawaii",
#'   "Capital": "Honolulu",
#'   "Languages": [
#'     "English",
#'     "Hawaiian"
#'   ]
#' }
#' }
ollama_generate <- function(
  model_name,
  system_prompt = "",
  prompt,
  temperature = 0.1,
  output_schema = NULL,
  output = "resp"
) {
  # Check if the model is available
  ollama_check_model(model_name)

  if (!is.null(output_schema)) {
    format <- list(
      type = "object",
      properties = output_schema
    )
  } else {
    format <- list()
  }

  check_inherits(format, "list")

  response <- ollamar::generate(
    model = model_name,
    system = system_prompt,
    prompt = prompt,
    format = format,
    output = output,
    temperature = temperature
  )

  # Process the response based on the output type
  ollamar::resp_process(resp = response, output = output)
} # /ollama_generate
