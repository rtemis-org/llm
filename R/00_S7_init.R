# S7 utils
# ::kaimana::
# 2025 EDG rtemis.org

#' Get content
#'
#' @param x An object of class AIResponse or ReasoningResponse
#'
#' @return Character if content is text, data.table if content is structured
#'
#' @author EDG
#' @export
get_content <- new_generic("get_content", "x")

# References
# ollamar: https://cran.r-project.org/web/packages/ollamar/vignettes/ollamar.html
# vignette uses `class` for defining parameter types, but JSON scemas use `type`
# Tool schemas for Ollama
tool_wikipedia <- list(
  type = "function",
  `function` = list(
    name = "query_wikipedia",
    description = "Search Wikipedia and return structured results",
    parameters = list(
      type = "object",
      properties = list(
        query = list(class = "character", description = "The search query"),
        limit = list(
          class = "integer",
          description = "Maximum number of results",
          default = 3L
        ),
        language = list(
          class = "character",
          description = "Language code (en, fr, ...)",
          default = "en"
        )
      ),
      required = list("query")
    )
  )
) # /kaimana::tool_wikipedia


tool_duckduckgo <- list(
  type = "function",
  `function` = list(
    name = "query_duckduckgo",
    description = "Search DuckDuckGo and return structured results",
    parameters = list(
      type = "object",
      properties = list(
        query = list(
          class = "character",
          description = "The search query. Best to use individual words or terms."
        ),
        ia = list(
          class = "character",
          description = "Instant Answer type (web, images, etc.)",
          default = NULL
        ),
        return_all = list(
          class = "logical",
          description = "If TRUE, return all fields from the API response",
          default = FALSE
        ),
        output = list(
          class = "character",
          description = "Output format: 'json' or 'data.table'",
          enum = c("json", "data.table"),
          default = "data.table"
        )
      ),
      required = list("query")
    )
  )
) # /kaimana::tool_duckduckgo

# Tool list ----
react_tools <- list(
  wikipedia = tool_wikipedia
)

#' Print available tools
#'
#' @return Character vector: Names of available tools
#' @author E.D. Gennatas
#' @export
available_tools <- function() {
  names(react_tools)
} # /kaimana::available_tools


# %% invoke generic ----
#' Invoke Method
#'
#' Generic method for invoking LLMs and agents
#'
#' @param x An object of class LLM, ReAgent, or ReAct
#' @param query Character: The query or prompt to pass to the model or agent
#' @param ... Additional arguments passed to specific methods
#'
#' @return An AIResponse object
#'
#' @author EDG
#' @export
invoke <- new_generic("invoke", "x")


# %% AIThinking Class ----
#' @title AIThinking Class
#'
#' @description
#' Class for AI thinking steps
#'
#' @field content Character: The thinking content.
#' @field metadata List: Metadata about the thinking step.
#'
#' @author EDG
AIThinking <- new_class(
  "AIThinking",
  properties = list(
    content = class_character,
    metadata = class_list
  ),
  constructor = function(content, metadata = list()) {
    new_object(
      S7_object(),
      content = content,
      metadata = metadata
    )
  }
) # kaimana::AIThinking


# %% AIResponse Class ----
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

# repr method for AIResponse ----
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
        fmt("User\n", col = col_user, bold = TRUE),
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
          fmt("Thinking\n", col = col_thinking, bold = TRUE),
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
        fmt(paste0(assistant_name, "\n"), col = col_assistant, bold = TRUE),
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


# Print AIResponse ----
method(print, AIResponse) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # kaimana::print.AIResponse


#as.list.AIResponse ----
method(as.list, AIResponse) <- function(x, ...) {
  x@response
} # kaimana::as.list.AIResponse
