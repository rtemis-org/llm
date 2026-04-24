# References:
# https://rconsortium.github.io/S7/

# Description:
# This file defines the Message class hierarchy for the rtemis.llm package,
# including subclasses SystemMessage, InputMessage, LLMMessage, and ToolMessage.
# Finally, it defines the InProcessAgentMemory class to encapsulate the state of an Agent, which includes
# a list of Message objects and associated metadata.

# %% Constants ----
SYSTEM_MESSAGE_ROLE <- "system"
INPUT_MESSAGE_ROLE <- "user"
AGENT_MESSAGE_ROLE <- "user"
LLM_MESSAGE_ROLE <- "assistant"
TOOL_MESSAGE_ROLE <- "tool"


# %% Message ----
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
    name = optional(S7::class_character),
    timestamp = class_POSIXct,
    metadata = optional(S7::class_list)
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
)


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
  # RHS based on class
  rhs <- switch(
    sub(".*::", "", class(x)[1]),
    SystemMessage = "System",
    InputMessage = "Input",
    LLMMessage = "Response",
    ToolMessage = "Tool Response",
    AgentMessage = "Agent Message",
    "Message"
  )
  name <- if (is.null(x@name)) {
    rhs
  } else {
    paste(x@name, rhs)
  }
  .color <- switch(
    rhs,
    System = col_system,
    Input = col_input,
    Response = col_llm,
    `Tool Response` = col_tool,
    `Agent Message` = col_agent,
    NULL
  )
  paste0(
    repr_bracket(name, col = .color, output_type = output_type),
    " ",
    x@content
  )
}


# %% print.Message ----
method(print, Message) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
}


# %% as_list.Message ----
method(as_list, Message) <- function(x) {
  list(
    role = x@role,
    name = x@name,
    content = x@content,
    timestamp = x@timestamp,
    metadata = x@metadata
  )
}


# %% SystemMessage ----
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
)


# %% repr.SystemMessage ----
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
    repr_bracket(
      paste0(name, "System"),
      col = col_system,
      output_type = output_type
    ),
    " ",
    x@content
  )
}

# %% print.SystemMessage ----
method(print, SystemMessage) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
}


# %% InputMessage ----
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
    image_path = optional(S7::class_character)
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
)


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
    repr_bracket(
      paste0(name, "Input"),
      col = col_input,
      output_type = output_type
    ),
    " ",
    x@content,
    if (!is.null(x@image_path)) {
      paste0(
        "\n",
        fmt(
          "\nImage Path: ",
          col = col_input,
          bold = TRUE,
          output_type = output_type
        ),
        x@image_path
      )
    }
  )
}


# %% print.InputMessage ----
method(print, InputMessage) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
}


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
}


# %% LLMMessage ----
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
    reasoning = optional(S7::class_character),
    tool_calls = optional(S7::class_list),
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
)


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
  paste0(
    if (!is.null(x@reasoning)) {
      paste0(
        repr_bracket(
          paste0(name, "Reasoning"),
          col = col_reasoning,
          output_type = output_type
        ),
        " ",
        x@reasoning,
        "\n\n"
      )
    },
    if (!is.null(x@content) && nchar(trimws(x@content)) > 0L) {
      paste0(
        repr_bracket(
          paste0(name, "Response"),
          col = col_llm,
          output_type = output_type
        ),
        " ",
        x@content
      )
    },
    if (!is.null(x@tool_calls)) {
      paste0(
        repr_bracket(
          paste0(name, "Tool Call"),
          col = col_tool,
          output_type = output_type
        ),
        "\n",
        jsonlite::toJSON(x@tool_calls, pretty = TRUE, auto_unbox = TRUE)
      )
    }
  )
}


# %% print.LLMMessage ----
method(print, LLMMessage) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
}


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
}


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
)


# %% OpenAIMessage ----
#' @title OpenAIMessage Class
#'
#' @description
#' LLMMessage subclass for OpenAI-compatible messages.
#'
#' @author EDG
#' @noRd
OpenAIMessage <- new_class(
  "OpenAIMessage",
  parent = LLMMessage,
  constructor = function(
    content,
    name = NULL,
    metadata = NULL,
    model_name,
    reasoning = NULL,
    tool_calls = NULL,
    provider = "OpenAI-compatible"
  ) {
    if (is.null(metadata)) {
      metadata <- list()
    }
    metadata[["provider"]] <- provider
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
)


# %% AnthropicMessage ----
#' @title AnthropicMessage Class
#'
#' @description
#' LLMMessage subclass for Anthropic messages.
#'
#' @author EDG
#' @noRd
AnthropicMessage <- new_class(
  "AnthropicMessage",
  parent = LLMMessage,
  constructor = function(
    content,
    name = NULL,
    metadata = NULL,
    model_name,
    reasoning = NULL,
    tool_calls = NULL
  ) {
    if (is.null(metadata)) {
      metadata <- list()
    }
    metadata[["provider"]] <- "Anthropic"
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
)


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
)


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
    repr_bracket(
      paste(name, "Message"),
      col = col_agent,
      output_type = output_type
    ),
    "\n",
    x@content
  )
}


# %% print.AgentMessage ----
# Print method for AgentMessage ----
method(print, AgentMessage) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
}


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
}


# %% ToolMessage ----
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
  properties = list(
    tool_call_id = optional(S7::class_character)
  ),
  constructor = function(
    content,
    name,
    tool_call_id = NULL,
    metadata = list()
  ) {
    new_object(
      Message(),
      role = TOOL_MESSAGE_ROLE,
      name = name,
      content = content,
      tool_call_id = tool_call_id,
      metadata = metadata
    )
  }
)


# %% as_list.ToolMessage ----
method(as_list, ToolMessage) <- function(x) {
  list(
    role = x@role,
    name = x@name,
    tool_call_id = x@tool_call_id,
    content = x@content,
    timestamp = format(x@timestamp, "%Y-%m-%dT%H:%M:%SZ"),
    metadata = x@metadata
  )
}


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
    repr_bracket(
      paste0(name, "Tool Response"),
      col = col_tool,
      output_type = output_type
    ),
    "\n",
    x@content
  )
}


# %% print.ToolMessage ----
method(print, ToolMessage) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
}


# %% as.list.Message ----
#' Convert Message to List
#'
#' @param x Message object
#' @param ... Additional arguments (not used)
#'
#' @return A list representation of the Message object
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running Ollama server and gemma4:e4b model
#'   \dontrun{
#'   llm <- create_Ollama("gemma4:e4b")
#'   res <- generate(llm, "How can anything exist?")
#'   as.list(res)
#' }
as.list.Message <- method(as.list, Message) <- function(x, ...) {
  as_list(x)
}
