# structured.R
# ::kaimana::
# 2025 EDG rtemis.org

#' Setup Output Schema
#'
#' Helper function to create a schema for structured output
#'
#' @param ... Named arguments, each of which must be a list with elements "type" and "description".
#' "type" must be a valid JSON Schema type: "string", "number", "integer", "boolean", "array", or "object".
#' "description" is a string containing a short, informative description of the field.
#' @param required Character vector: Names of required fields or "all".
#'
#' @return A list representing the schema
#'
#' @author EDG
#'
#' @export
make_output_schema <- function(..., required = "all") {
  properties <- list(...)
  if (length(required) == 1L && required == "all") {
    required <- names(properties)
  }
  schema <- list(
    type = "object",
    properties = properties,
    required = required
  )
} # /kaimana::make_output_schema
