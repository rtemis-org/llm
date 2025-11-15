# Message.R
# ::kaimana::
# 2025 EDG rtemis.org

# References:
# https://rconsortium.github.io/S7/
# subclasses inherit properties from parent classes, need only define new properties

# Description:
# This file defines the Message class hierarchy for the kaimana package,
# including subclasses SystemMessage, InputMessage, LLMMessage, and ToolMessage.
# Finally, it defines the InMemoryAgentState class to encapsulate the state of an Agent, which includes
# a list of Message objects and associated metadata.

# %% Constants ----
SYSTEM_MESSAGE_ROLE <- "system"
INPUT_MESSAGE_ROLE <- "user"
AGENT_MESSAGE_ROLE <- "user"
LLM_MESSAGE_ROLE <- "assistant"
TOOL_MESSAGE_ROLE <- "tool"


# %% Message Class ----
#' @title Message Class
#'
#' @description
#' Class for messages exchanged with an Agent.
#'
#' @field role Character: The role of the message sender (e.g., "system", "user", "llm").
#' @field name Optional character: The name of the message sender.
#' @field content Character: The content of the message.
#' @field timestamp POSIXct: The timestamp when the message was created.
#' @field metadata Optional list: Metadata associated with the message.
#'
#' @author EDG
#' @noRd
Message <- new_class(
  "Message",
  properties = list(
    content = class_character,
    role = class_character,
    name = new_union(NULL | class_character),
    timestamp = class_POSIXct,
    metadata = new_union(NULL | class_list)
  ),
  constructor = function(
    content = character(0L),
    role = character(0L),
    name = NULL,
    metadata = NULL
  ) {
    new_object(
      S7_object(),
      content = content,
      role = role,
      name = name,
      timestamp = Sys.time(),
      metadata = metadata
    )
  }
) # /kaimana::Message


# %% repr.Message ----
#' repr method for Message
#'
#' @param x Message object
#' @param output_type Character: The output type, e.g. "console" or "html"
#'
#' @return Character representation of Message
#'
#' @author EDG
#' @noRd
method(repr, Message) <- function(x, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  # RHS based on class
  rhs <- switch(sub(".*::"))
  {
    case("SystemMessage"):"System"
    case("InputMessage"):"Input"
    case("LLMMessage"):"Response"
    case("ToolMessage"):"Tool Response"
    case("AgentMessage"):"Agent"
    default:""
  }
  name <- if (!is.null(x@name)) {
    paste0(x@name, " (", x@role, ")")
  } else {
    x@role
  }
  paste0(
    fmt("[", thin = TRUE, output_type = output_type),
    fmt(name, bold = TRUE, output_type = output_type),
    fmt("]", thin = TRUE, output_type = output_type),
    "\n",
    x@content
  )
} # /kaimana::repr.Message


# %% print.Message ----
method(print, Message) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /kaimana::print.Message


# %% as_list.Message ----
method(as_list, Message) <- function(x) {
  list(
    role = x@role,
    name = x@name,
    content = x@content,
    timestamp = x@timestamp,
    metadata = x@metadata
  )
} # /kaimana::as_list.Message


# %% SystemMessage Class ----
#' @title SystemMessage Class
#'
#' @description
#' Message subclass for system messages. These are usually prompts that set the behavior of the agent.
#'
#' @author EDG
#' @noRd
SystemMessage <- new_class(
  "SystemMessage",
  parent = Message,
  constructor = function(
    content,
    name = NULL,
    metadata = NULL
  ) {
    new_object(
      Message(),
      content = content,
      role = SYSTEM_MESSAGE_ROLE,
      name = name,
      metadata = metadata
    )
  }
) # /kaimana::SystemMessage


# %% repr.SystemMessage ----
# repr method for SystemMessage
method(repr, SystemMessage) <- function(x, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  name <- if (!is.null(x@name)) {
    paste0(x@name, " ")
  } else {
    NULL
  }
  paste0(
    fmt(
      paste0(".: ", name, "System :.\n"),
      col = col_system,
      bold = TRUE,
      output_type = output_type
    ),
    x@content
  )
} # /kaimana::repr.SystemMessage


# %% print.SystemMessage ----
method(print, SystemMessage) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /kaimana::print.SystemMessage


# %% InputMessage Class ----
#' @title InputMessage Class
#'
#' @description
#' Message subclass for input messages. These are usually from the user, but not necessarily.
#'
#' @author EDG
#' @noRd
InputMessage <- new_class(
  "InputMessage",
  parent = Message,
  properties = list(
    image_path = new_union(NULL | class_character)
  ),
  constructor = function(
    content,
    name = NULL,
    image_path = NULL,
    metadata = NULL
  ) {
    new_object(
      Message(),
      content = content,
      role = INPUT_MESSAGE_ROLE,
      name = name,
      image_path = image_path,
      metadata = metadata
    )
  }
) # /kaimana::InputMessage


# %% repr.InputMessage ----
# repr method for InputMessage
method(repr, InputMessage) <- function(x, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  name <- if (!is.null(x@name)) {
    paste0(x@name, " ")
  } else {
    NULL
  }
  paste0(
    fmt(
      paste0(".: ", name, "Input :.\n"),
      col = col_input,
      bold = TRUE,
      output_type = output_type
    ),
    x@content
  )
} # /kaimana::repr.InputMessage


# %% print.InputMessage ----
method(print, InputMessage) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /kaimana::print.InputMessage


# %% as_list.InputMessage ----
method(as_list, InputMessage) <- function(x) {
  list(
    role = x@role,
    name = x@name,
    content = x@content,
    image_path = x@image_path,
    timestamp = format(x@timestamp, "%Y-%m-%dT%H:%M:%SZ"),
    metadata = x@metadata
  )
} # /kaimana::as_list.InputMessage


# %% LLMMessage Class ----
#' @title LLMMessage Class
#'
#' @description
#' Message subclass for LLM-generated messages
#'
#' @author EDG
#' @noRd
LLMMessage <- new_class(
  "LLMMessage",
  parent = Message,
  properties = list(
    reasoning = new_union(NULL | class_character),
    tool_calls = new_union(NULL | class_list),
    model_name = class_character
  ),
  constructor = function(
    content,
    name = NULL,
    metadata = NULL,
    model_name,
    reasoning = NULL,
    tool_calls = NULL
  ) {
    new_object(
      Message(),
      content = content,
      role = LLM_MESSAGE_ROLE,
      name = name,
      metadata = metadata,
      model_name = model_name,
      reasoning = reasoning,
      tool_calls = tool_calls
    )
  }
) # /kaimana::LLMMessage


# %% repr.LLMMessage ----
method(repr, LLMMessage) <- function(x, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  name <- if (!is.null(x@name)) {
    paste0(x@name, " ")
  } else {
    NULL
  }
  out <- paste0(
    if (!is.null(x@reasoning)) {
      paste0(
        fmt(
          paste0(".: ", name, "Reasoning :.\n"),
          col = col_reasoning,
          bold = TRUE,
          output_type = output_type
        ),
        x@reasoning,
        "\n"
      )
    },
    if (!is.null(x@content) && nchar(x@content) > 0L) {
      paste0(
        fmt(
          paste0(".: ", name, "Response :.\n"),
          col = col_llm,
          bold = TRUE,
          output_type = output_type
        ),
        x@content
      )
    },
    if (!is.null(x@tool_calls)) {
      paste0(
        "\n",
        fmt(
          paste0(".: ", name, "Tool Call :.\n"),
          col = col_tool,
          bold = TRUE,
          output_type = output_type
        ),
        jsonlite::toJSON(x@tool_calls, pretty = TRUE, auto_unbox = TRUE)
      )
    } else {
      ""
    }
  )
} # /kaimana::repr.LLMMessage


# %% print.LLMMessage ----
method(print, LLMMessage) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /kaimana::print.LLMMessage


# %% as_list.LLMMessage ----
method(as_list, LLMMessage) <- function(x) {
  list(
    role = x@role,
    name = x@name,
    reasoning = x@reasoning,
    content = x@content,
    tool_calls = x@tool_calls,
    timestamp = format(x@timestamp, "%Y-%m-%dT%H:%M:%SZ"),
    metadata = x@metadata,
    model_name = x@model_name
  )
} # /kaimana::as_list.LLMMessage


# %% OllamaMessage ----
#' @title OllamaMessage Class
#'
#' @description
#' LLMMessage subclass for Ollama messages
#'
#' @author EDG
#' @noRd
OllamaMessage <- new_class(
  "OllamaMessage",
  parent = LLMMessage,
  constructor = function(
    content,
    name = NULL,
    metadata = NULL,
    model_name,
    reasoning = NULL,
    tool_calls = NULL
  ) {
    metadata[["provider"]] <- "Ollama"
    new_object(
      LLMMessage(
        content = content,
        name = name,
        metadata = metadata,
        model_name = model_name,
        reasoning = reasoning,
        tool_calls = tool_calls
      )
    )
  }
) # /kaimana::OllamaMessage


# %% AgentMessage ----
#' @title AgentMessage Class
#'
#' @description
#' Message subclass for agent messages that are programmatically created by the agent logic,
#' NOT from the LLM.
#'
#' @author EDG
#' @noRd
AgentMessage <- new_class(
  "AgentMessage",
  parent = Message,
  constructor = function(
    content,
    name = NULL,
    metadata = NULL
  ) {
    new_object(
      Message(),
      content = content,
      role = AGENT_MESSAGE_ROLE,
      name = name,
      metadata = metadata
    )
  }
) # /kaimana::AgentMessage


# %% repr.AgentMessage ----
# repr method for AgentMessage
method(repr, AgentMessage) <- function(x, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  name <- if (!is.null(x@name)) {
    x@name
  } else {
    "Agent"
  }
  out <- paste0(
    fmt(
      paste0(".: ", name, " Message :.\n"),
      col = col_agent,
      bold = TRUE,
      output_type = output_type
    ),
    x@content
  )
} # /kaimana::repr.AgentMessage


# %% print.AgentMessage ----
# Print method for AgentMessage ----
method(print, AgentMessage) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /kaimana::print.AgentMessage


# %% as_OllamaMessage.list() ----
#' @title Convert to OllamaMessage
#'
#' @description
#' Convert R list to OllamaMessage Object
#'
#' @param x A list to convert, the output of `generate` on an `Ollama` object.
#'
#' @return A OllamaMessage object
#'
#' @author EDG
#' @noRd
method(as_OllamaMessage, class_list) <- function(x) {
  # Get all other elements except "model", "response, and "thinking"
  metadata <- x[setdiff(names(x), c("model", "response", "thinking"))]
  reasoning <- if (!is.null(x[["thinking"]])) {
    x[["thinking"]]
  } else {
    NULL
  }
  OllamaMessage(
    name = x[["name"]],
    content = x[["response"]],
    metadata = metadata,
    model_name = x[["model"]],
    reasoning = reasoning
  )
} # /kaimana::as_OllamaMessage.list


# %% ToolMessage Class ----
#' @title ToolMessage Class
#'
#' @description
#' Message subclass for Tool messages
#'
#' @author EDG
#' @noRd
ToolMessage <- new_class(
  "ToolMessage",
  parent = Message,
  constructor = function(
    content,
    name,
    metadata = list()
  ) {
    new_object(
      Message(),
      role = TOOL_MESSAGE_ROLE,
      name = name,
      content = content,
      metadata = metadata
    )
  }
) # /kaimana::ToolMessage


# %% repr.ToolMessage ----
method(repr, ToolMessage) <- function(x, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  name <- if (!is.null(x@name)) {
    paste0(x@name, " ")
  } else {
    NULL
  }
  paste0(
    fmt(
      paste0(".: ", name, "Tool Response :.\n"),
      col = col_tool,
      bold = TRUE,
      output_type = output_type
    ),
    x@content
  )
} # /kaimana::repr.ToolMessage


# %% print.ToolMessage ----
method(print, ToolMessage) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /kaimana::print.ToolMessage
