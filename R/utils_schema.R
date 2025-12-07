# utils_schema.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% schema() ----
#' Define output schema for LLM responses
#'
#' @param ... Named lists defining the schema fields and their types in the form:
#' `name = list(type = "type", description = "description")`, e.g.:
#' `frequency = list(type = "numeric", description = "Frequency of the oscillator")`
#' @param required Optional character vector of names of required fields. If NULL, all fields are
#' required. Use empty character ("") for no required fields.
#' @param output_type Character: "json" or "list". The output format of the schema definition.
#'
#' @return Character JSON string (if output = "json") or list (if output = "list")
#'
#' @details
#' The `required` field needs to remain an array after conversion to JSON, so we use
#' `I()` to inhibit conversion to JSON object.
#' You can use `jsonlite::toJSON(your_schema, auto_unbox = TRUE, pretty = TRUE)`
#' to verify the output if needed. If `required` is a single string, it should return as
#' an array with one element, e.g. `"required": ["frequency"]`, NOT `"required": "frequency"`.
#'
#' @author EDG
#' @export

schema <- function(..., required = NULL, output_type = c("json", "list")) {
  output_type <- match.arg(output_type)
  out <- list(
    type = "object",
    properties = list(
      ...
    ),
    required = if (is.null(required)) {
      I(names(list(...)))
    } else {
      I(required)
    }
  )
  if (output_type == "json") {
    jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE)
  } else {
    out
  }
} # /kaimana::schema


# %% field() ----
#' Define a schema field
#'
#' @param type Character: "string", "integer", "numeric", "boolean", "array", "object". The field type.
#' @param description Character: A brief description of the field.
#'
#' @return List defining the schema field
#'
#' @details
#' Use this function to define each individual field when calling [schema].
#' This is a convenience function to offer type validation and allow
#' `field("number", "Frequency of the oscillator")` syntax.
#' instead of
#' `list(type = "number", description = "Frequency of the oscillator")`.
#'
#' @author EDG
#' @export
field <- function(
  type = c("string", "number", "boolean", "array", "object"),
  description
) {
  type <- match.arg(type)
  list(
    type = type,
    description = description
  )
} # /kaimana::field
