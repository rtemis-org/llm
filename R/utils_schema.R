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

schema <- function(..., output_type = c("list", "json")) {
  output_type <- match.arg(output_type)
  fields <- list(...)
  # Get required field names based on `required` value in each field
  required <- I(names(fields)[sapply(fields, function(x) x$required)])
  # Remove `required` from each field
  fields <- lapply(fields, function(x) x[-which(names(x) == "required")])
  out <- list(
    type = "object",
    properties = fields,
    required = required
  )
  if (output_type == "json") {
    jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE)
  } else {
    class(out) <- c("Schema", "list")
    out
  }
} # /kaimana::schema


# %% repr.Schema ----
repr.Schema <- function(x, pad = 0L, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name("Schema", output_type = output_type),
    "\n",
    repr_ls(x, pad = pad, output_type = output_type)
  )
}


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
  description,
  required = TRUE
) {
  type <- match.arg(type)
  list(
    type = type,
    description = description,
    required = required
  )
} # /kaimana::field
