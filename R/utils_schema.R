# utils_schema.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% schema() ----
#' Define output schema for LLM responses
#'
#' @param ... Named lists defining the schema fields and their types in the form:
#' `name = list(type = "type", description = "description")`, e.g.:
#' `frequency = list(type = "numeric", description = "Frequency of the oscillator")`
#' @param required Character vector of names of required fields. If NULL, all fields are required.
#' Use empty character ("") for no required fields.
#' @param output Character: "json" or "list". The output format of the schema definition.
#'
#' @return Character JSON string (if output = "json") or list (if output = "list")
#'
#' @author EDG
#' @export

schema <- function(..., required = NULL, output = c("json", "list")) {
  out <- list(
    type = "object",
    properties = list(
      ...
    ),
    required = if (is.null(required)) {
      names(list(...))
    } else {
      required
    }
  )
  if (output == "json") {
    jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE)
  } else {
    out
  }
} # /kaimana::schema
