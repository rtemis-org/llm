# ReAct.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% ReActResponse Class ----
#' @title ReActResponse Class
#'
#' @description
#' Class for ReAct agent responses
#'
#' @field system_prompt Character: The system prompt used.
#' @field query Character: The query used.
#' @field response List: The response from the agent.

# %%  ReAct Class ----

#' @name ReAct
#'
#' @title ReAct Class
#'
#' @description
#' Class for ReAct agents
#'
#' @field model_name Character: The name of the LLM model to use.
#' @field state Environment: An environment to hold the state of the agent, including messages.
#' The system prompt is automatically added as the first message.
#' @field tools Character: Names of tools to use.
#' @field max_tool_calls Integer: Maximum number of tool calls per query.
#' @field temperature Numeric: The temperature for the model.
#'
#' @details
#' The agent keeps track of the conversation state in the `state` environment,
#' allowing for context-aware responses across multiple turns.
#'
#' @author EDG
#'
#' @noRd
ReAct <- new_class(
  name = "ReAct",
  properties = list(
    model_name = class_character,
    state = class_environment,
    tools = class_list,
    max_tool_calls = class_integer,
    temperature = class_numeric
  ),
  constructor = function(
    model_name,
    system_prompt,
    tool_names,
    temperature,
    max_tool_calls = 5L
  ) {
    state <- new.env(parent = emptyenv())
    # Initialize messages with system prompt within state environment
    state[["messages"]] <- list(
      list(role = "system", content = system_prompt)
    )
    if (!all(tool_names %in% names(react_tools))) {
      cli::cli_abort(
        "Some tool names are not recognized. Available tools are: ",
        paste(names(react_tools), collapse = ", ")
      )
    }
    new_object(
      S7_object(),
      model_name = model_name,
      state = state,
      tools = unname(react_tools[tool_names]), # tools list MUST be unnamed
      max_tool_calls = max_tool_calls,
      temperature = temperature
    )
  }
) # /kaimana::ReAct


# repr method for ReAct ----
method(repr, ReAct) <- function(x, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  out <- paste0(
    repr_S7name("ReAct"),
    fmt("Model: ", bold = TRUE, output_type = output_type),
    x@model_name,
    "\n",
    fmt("Tools: ", bold = TRUE, output_type = output_type),
    if (length(x@tools) == 0L) {
      "None"
    } else {
      paste(
        sapply(x@tools, function(tool) tool[["function"]][["name"]]),
        collapse = ", "
      )
    },
    "\n",
    fmt("Temperature: ", bold = TRUE, output_type = output_type),
    x@temperature,
    "\n",
    fmt("Max Tool Calls: ", bold = TRUE, output_type = output_type),
    x@max_tool_calls,
    "\n"
    # fmt("State Messages:\n", bold = TRUE, output_type = output_type),
    # repr(
    #   AIResponse(response = x@state[["messages"]]),
    #   output_type = output_type
    # )
  )
  out
} # /kaimana::repr.ReAct


# Print ReAct ----
method(print, ReAct) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # kaimana::print.ReAct


# invoke method for ReAct ----
#' @name invoke.ReAct
#'
#' @title Invoke ReAct Agent
#'
#' @param x ReAct object
#' @param query Character: Query to pass to the agent.
#' @param verbosity Integer: Verbosity level.
#'
#' @return AIResponse object
#'
#' @author EDG
method(invoke, ReAct) <- function(x, query, image_path = NULL, verbosity = 1L) {
  if (verbosity > 0L) {
    msg("Agent is working...")
  }
  if (is.null(image_path)) {
    # Append query to messages
    x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- list(
      role = "user",
      content = query
    )
  } else {
    # Append query to messages
    x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- list(
      role = "user",
      content = query,
      images = image_path
    )
  }

  turns <- 0L
  while (turns < x@max_tool_calls) {
    turns <- turns + 1L
    res <- ollamar::chat(
      model = x@model_name,
      messages = x@state[["messages"]],
      tools = x@tools,
      temperature = x@temperature,
      stream = FALSE,
      output = "resp"
    )
    tool_calls <- ollamar::resp_process(res, output = "tools")
    if (is.null(tool_calls) || length(tool_calls) == 0L) {
      break
    }
    # Perform tool calls
    # Consider passing verbosity value to tool calls
    for (tool_call in tool_calls) {
      tool_out <- do.call(
        tool_call[["name"]],
        tool_call[["arguments"]]
      )
      # Append tool call and output to messages
      x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- list(
        role = "tool",
        name = tool_call[["name"]],
        content = tool_out
      )
    }
    # Pass updated messages back to model
    # Model must decide whether it has enough information to answer or
    # needs to make another tool call
    res <- ollamar::chat(
      model = x@model_name,
      messages = x@state[["messages"]],
      tools = x@tools,
      temperature = x@temperature,
      stream = FALSE,
      output = "resp"
    )
    tool_calls <- ollamar::resp_process(res, output = "tools")
    if (is.null(tool_calls) || length(tool_calls) == 0L) {
      break
    }
  } # /while (turns < x@max_tool_calls)
  # Append final response to messages
  x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- list(
    role = "assistant",
    content = ollamar::resp_process(res, output = "text")
  )
  # Overwrite
  AIResponse(response = x@state[["messages"]])
} # /kaimana::invoke.ReAct


#' Create ReAct agent with Ollama
#'
#' @param model_name Character: The name of the LLM model to use.
#' @param state Environment: An environment to hold the state of the agent, including messages.
#' The system prompt is automatically added as the first message.
#' @param tools Character: Names of tools to use.
#' @param max_tool_calls Integer: Maximum number of tool calls per query.
#' @param temperature Numeric: The temperature for the model.
#'
#' @return ReAct object
#'
#' @author EDG
#' @export
create_ReAct <- function(
  model_name,
  system_prompt,
  tool_names,
  temperature = 0.7,
  max_tool_calls = 5L
) {
  ReAct(
    model_name = model_name,
    system_prompt = system_prompt,
    tool_names = tool_names,
    temperature = temperature,
    max_tool_calls = max_tool_calls
  )
} # /kaimana::create_ReAct
