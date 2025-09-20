# km_client.R
# ::kaimana-r::
# 2025 EDG rtemis.org

# Kaimana client utilities

# Constants ----
KM_HEALTH_ENDPOINT <- "/health"
KM_MODELS_ENDPOINT <- "/models"
KM_GENERATE_ENDPOINT <- "/generate"

# Format AI message ----
#' Format AI response
#'
#' Format AI response, extracting thinking step, if present
#'
#' @param x Character: AI response
#' @param verbosity Integer: Verbosity level.
#'
#' @return List of characters
#'
#' @author EDG
#' @keywords internal
#' @noRd

format_ai_message <- function(x) {
  # If there are <think> </think> tags, extract thinking steps
  if (grepl("<think>", x)) {
    thinking <- gsub(".*<think>(.*)</think>.*", "\\1", x)
    response <- gsub(".*</think>(.*)", "\\1", x)
  } else {
    thinking <- NULL
    response <- x
  }
  list(thinking = thinking, response = response)
} # kaimana::format_ai_message


# AIMessage Class ----
#' @title AIMessage
#'
#' @description
#' Class for AI messages
#'
#' @field thinking Character: The thinking process of the AI.
#' @field content Character: The content of the AI message.
#'
#' @author EDG
AIMessage <- new_class(
  "AIMessage",
  properties = list(
    thinking = class_character,
    content = class_character,
    metadata = class_list
  ),
  constructor = function(response, metadata = list()) {
    resp <- format_ai_message(response)
    new_object(
      S7_object(),
      thinking = resp[["thinking"]],
      content = resp[["response"]],
      metadata = metadata
    )
  }
) # kaimana::AIMessage

# [[ method for AIMessage ----
method(`[[`, AIMessage) <- function(x, name) {
  prop(x, name)
} # kaimana::[[.AIMessage

# repr AIMessage ----
method(repr, AIMessage) <- function(x, output_type = NULL) {
  output_type <- get_output_type(output_type)
  show_ls(x, output_type = output_type)
} # kaimana::repr.AIMessage

# Print AIMessage ----
method(print, AIMessage) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # kaimana::print.AIMessage

# km_health_check ----
#' Run Health Check on Kaimana REST API
#'
#' @param url Character: URL of the health check endpoint.
#' @param verbosity Integer: Verbosity level.
#'
#' @return List: Parsed response from the health check endpoint.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' km_health_check()
#' }

km_health_check <- function(
  url = "http://localhost:31650",
  verbosity = 1L
) {
  url <- paste0(url, KM_HEALTH_ENDPOINT)
  health_check <- httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  if (verbosity > 0L) {
    printls(health_check)
  }
  invisible(health_check)
} # /kaimana::km_health_check


# km_get_models ----
#' Get Models available in Kaimana REST API
#'
#' @param url Character: URL of the models endpoint.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Data frame: List of models available in the Kaimana API.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' km_get_models()
#' }

km_get_models <- function(
  url = "http://localhost:31650/models",
  verbosity = 1L
) {
  resp <- httr2::request(url) |>
    httr2::req_perform()
  # Check response status
  if (resp[["status_code"]] != 200) {
    cli::cli_abort("Failed to retrieve models from {url}")
  }
  # Clean up list
  resp <- httr2::resp_body_json(resp)
  # Must contain element "models"
  if (!"models" %in% names(resp)) {
    cli::cli_abort("Response from {url} does not contain 'models'")
  }
  out <- data.frame(do.call(rbind, lapply(resp[["models"]], unlist)))
  if (verbosity > 0L) {
    printdf(out)
  }
  invisible(out)
} # /kaimana::km_get_models


# km_generate ----
#' Generate a response from Kaimana REST API
#'
#' @param prompt Character: User prompt for the API.
#' @param output_schema List: output schema defined using [setup_output_schema].
#' @param url Character: URL of the generate endpoint.
#' @param model_name Character: Model to use for the API.
#' @param temperature Numeric: Sampling temperature.
#' @param verbosity Integer: Verbosity level.
#'
#' @details
#' This functions uses the `{url}/generate` and `{url}/generate/structured` endpoints depending on
#' the presence of an output schema.
#'
#' @return List: Parsed response from the API.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' km_generate("Hello, world!", model_name = "granite3.3:8b")
#' }

km_generate <- function(
  prompt,
  output_schema = NULL,
  model_name = "granite3.3:8b",
  temperature = 0.1,
  url = "http://localhost:31650",
  verbosity = 1L
) {
  url <- paste0(url, "/generate")
  # Use structured endpoint if output_schema is provided
  if (is.null(output_schema)) {
    # Free-form output
    resp <- httr2::request(url) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(
        list(
          message = prompt,
          config = list(
            model_name = model_name,
            temperature = temperature
          )
        )
      )
  } else {
    # Structured output
    url <- paste0(url, "/structured")
    resp <- httr2::request(url) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(
        list(
          message = prompt,
          config = list(
            model_name = model_name,
            temperature = temperature
          ),
          output_schema = output_schema
        )
      )
  }
  resp <- resp |>
    httr2::req_perform()
  # Check response status
  if (resp[["status_code"]] != 200) {
    # Health check
    health <- km_health_check(
      url = paste0(url, "/health"),
      verbosity = verbosity
    )
    cli::cli_abort("Failed to generate response from {url}")
  }
  resp <- httr2::resp_body_json(resp)
  if (verbosity > 0L) {
    printls(resp)
  }
  invisible(resp)
} # /kaimana::km_generate
