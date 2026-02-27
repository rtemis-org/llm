# AIResponse.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% AIResponse ----
#' @title AIResponse Class
#'
#' @description
#' Class for AI responses that can include thinking steps, tool calls, structured output, and free
#' text.
#'
#' @field response List of named lists including thinking, tool calls, structured output, and text output.
#'
#' @author EDG
AIResponse <- new_class(
  "AIResponse",
  properties = list(
    response = class_list
  )
) # kaimana::AIResponse


# %% repr.AIResponse ----
method(repr, AIResponse) <- function(x, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  # Get each message in the response
  out <- ""
  for (msg1 in x@response) {
    role <- msg1[["role"]]
    content <- msg1[["content"]]

    if (role == "system") {
      out <- paste0(
        out,
        fmt("System\n", bold = TRUE),
        content,
        "\n"
      )
    } # /system

    if (role == "user") {
      out <- paste0(
        out,
        fmt("User\n", col = col_input, bold = TRUE),
        content,
        "\n"
      )
    } # /user

    if (role == "assistant") {
      # If there are <think> </think> tags, extract thinking steps
      if (grepl("<think>", content)) {
        thinking <- gsub(".*<think>(.*)</think>.*", "\\1", content)
        response <- gsub(".*</think>(.*)", "\\1", content)
        out <- paste0(
          out,
          fmt("Thinking\n", col = col_reasoning, bold = TRUE),
          trimws(thinking),
          "\n"
        )
      } else {
        response <- content
      }
      assistant_name <- if (!is.null(msg1[["name"]])) {
        msg1[["name"]]
      } else {
        "Assistant"
      }
      out <- paste0(
        out,
        fmt(paste0(assistant_name, "\n"), col = col_agent, bold = TRUE),
        trimws(response),
        "\n"
      )
    } # /assistant

    if (role == "tool") {
      tool_name <- msg1[["name"]]
      out <- paste0(
        out,
        fmt("Tool: ", col = col_tool, bold = TRUE),
        fmt(tool_name, col = col_tool, bold = TRUE),
        "\n",
        content,
        "\n"
      )
    } # /tool
  } # /for (msg in x@response)
  out
} # /kaimana::repr.AIResponse


# %% print.AIResponse ----
method(print, AIResponse) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # kaimana::print.AIResponse


# %% as.list.AIResponse ----
method(as.list, AIResponse) <- function(x, ...) {
  x@response
} # kaimana::as.list.AIResponse
