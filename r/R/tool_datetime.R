#' Get Current Date and Time
#'
#' Gets the current date and time in ISO format.
#'
#' @param output_type Character: "json" or "list".
#'
#' @return A JSON string with fields "date", "time", and "timezone".
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_current_datetime <- function(output_type = c("json", "list")) {
  output_type <- match.arg(output_type)
  dt <- list(
    date = Sys.Date(),
    time = format(Sys.time(), "%H:%M:%S"),
    timezone = Sys.timezone()
  )
  if (output_type == "json") {
    jsonlite::toJSON(
      dt,
      auto_unbox = TRUE
    )
  } else {
    dt
  }
} # /get_current_datetime


#' @rdname tools
#' @format NULL
#' @export
tool_datetime <- create_tool(
  name = "Get Current Date and Time",
  function_name = "get_current_datetime",
  description = "Get the current date and time in ISO format.",
  parameters = list()
)
