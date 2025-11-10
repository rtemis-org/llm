# utils_ollama.R
# ::kaimana::
# 2025 EDG rtemis.org

# TOC:
#   - ollama_list_models
#   - ollama_get_model_info

# References
# https://hauselin.github.io/ollama-r/

# %% .ollama_api_tags() ----
.ollama_api_tags <- function(
  base_url = OLLAMA_URL_DEFAULT,
  output = c("list", "json")
) {
  output <- match.arg(output)
  req <- httr2::request(paste0(base_url, "/api/tags")) |>
    httr2::req_method("GET")
  res <- httr2::req_perform(req)
  # Check response status
  httr2::resp_check_status(res)
  if (output == "list") {
    httr2::resp_body_json(res)
  } else {
    httr2::resp_body_string(res)
  }
} # /kaimana::.ollama_api_tags

# %% ollama_list_models() ----
#' List Ollama Models
#'
#' @return Character vector: Model names.
#'
#' @author EDG
#' @export
ollama_list_models <- function(
  base_url = OLLAMA_URL_DEFAULT
) {
  res <- .ollama_api_tags(base_url = base_url, output = "list")
  # Format response using httr2
  sapply(res[["models"]], function(x) x[["model"]])
} # /kaimana::ollama_list_models


# %% ollama_get_model_info() ----
#' Get Ollama Model Info
#'
#' @param model Character: Name of model. If NULL, all models are returned.
#' @param base_url Character: Base URL of Ollama server.
#'
#' @return List with model info if `x` is defined, data.table with all models' info otherwise.
#'
#' @author EDG
#' @export
ollama_get_model_info <- function(x = NULL, base_url = OLLAMA_URL_DEFAULT) {
  models <- .ollama_api_tags(base_url = base_url, output = "list")[["models"]]
  models <- data.table(do.call(rbind, lapply(models, unlist)))
  if (is.null(x)) {
    out <- models
  } else {
    out <- models[name == x]
    if (nrow(out) == 0) {
      cli::cli_abort("Model ", x, " not found.")
    }
    out <- as.list(out)
  }
  out
} # /ollama_get_model_info


# %% ollama_check_model() ----
#' Check Ollama Model is Available
#'
#' @param x Character: Name of model.
#'
#' @return NULL, invisibly if model is available; otherwise throws an error.
#'
#' @author EDG
#' @export
ollama_check_model <- function(x) {
  if (x %in% ollama_list_models()) {
    invisible(NULL)
  } else {
    cli::cli_abort(
      "Model ",
      x,
      " is not available. Please check the model name and pull it if necessary.\n",
      "List available models with `ollama_list_models()`."
    )
  }
} # /kaimana::ollama_check_model


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
