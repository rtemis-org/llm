# Message.R
# ::kaimana::

# References:
# https://rconsortium.github.io/S7/
# subclasses inherit properties from parent classes, need only define new properties

# Description:
# This file defines the Message class hierarchy for the kaimana package,
# including subclasses SystemMessage, InputMessage, LLMMessage, and ToolMessage.
# Finally, it defines the InMemoryAgentState class to encapsulate the state of an Agent, which includes
# a list of Message objects and associated metadata.

# %% Constants ----
MESSAGE_ROLE_SYSTEM <- "system"
MESSAGE_ROLE_INPUT <- "user"
MESSAGE_ROLE_AGENT <- "user"
MESSAGE_ROLE_LLM <- "assistant"
MESSAGE_ROLE_TOOL <- "tool"


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
    role = class_character,
    name = new_union(NULL | class_character),
    content = class_character,
    timestamp = class_POSIXct,
    metadata = new_union(NULL | class_list)
  ),
  constructor = function(
    role = character(0L),
    name = NULL,
    content = character(0L),
    metadata = NULL
  ) {
    new_object(
      S7_object(),
      role = role,
      name = name,
      content = content,
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
  name <- if (!is.null(x@name)) {
    paste0(x@name, " (", x@role, ")")
  } else {
    x@role
  }
  paste0(
    fmt(paste0(".: ", name, " :."), bold = TRUE, output_type = output_type),
    "\n",
    x@content
  )
} # /kaimana::repr.Message


# %% print.Message ----
# Print method for Message ----
method(print, Message) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /kaimana::print.Message


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
    name,
    content,
    metadata = NULL
  ) {
    new_object(
      Message(),
      role = MESSAGE_ROLE_SYSTEM,
      name = name,
      content = content,
      metadata = metadata
    )
  }
) # /kaimana::SystemMessage


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
    name = MESSAGE_ROLE_INPUT,
    content,
    image_path = NULL,
    metadata = NULL
  ) {
    new_object(
      Message(),
      role = MESSAGE_ROLE_INPUT,
      name = name,
      content = content,
      image_path = image_path,
      metadata = metadata
    )
  }
) # /kaimana::InputMessage


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
    model_name = class_character,
    reasoning = new_union(NULL | class_character),
    tool_calls = new_union(NULL | class_list)
  ),
  constructor = function(
    name = NULL,
    content,
    metadata = NULL,
    model_name,
    reasoning = NULL,
    tool_calls = NULL
  ) {
    new_object(
      Message(),
      role = MESSAGE_ROLE_LLM,
      name = name,
      content = content,
      metadata = metadata,
      model_name = model_name,
      reasoning = reasoning,
      tool_calls = tool_calls
    )
  }
) # /kaimana::LLMMessage


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
    name = NULL,
    content,
    metadata = NULL,
    model_name,
    reasoning = NULL,
    tool_calls = NULL
  ) {
    metadata[["provider"]] <- "Ollama"
    new_object(
      LLMMessage(
        name = name,
        content = content,
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
    name = NULL,
    content,
    metadata = NULL
  ) {
    new_object(
      Message(),
      role = MESSAGE_ROLE_AGENT,
      name = name,
      content = content,
      metadata = metadata
    )
  }
) # /kaimana::AgentMessage


# %% repr.LLMMessage ----
# repr method for LLMMessage
method(repr, LLMMessage) <- function(x, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  name <- if (!is.null(x@name)) {
    paste0(fmt(x@name, bold = TRUE, output_type = output_type), " ")
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
    paste0(
      fmt(
        paste0(".: ", name, "Response :.\n"),
        col = col_agent,
        bold = TRUE,
        output_type = output_type
      ),
      x@content
    )
  )
} # /kaimana::repr.LLMMessage


# %% print.LLMMessage ----
# Print method for LLMMessage ----
method(print, LLMMessage) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /kaimana::print.LLMMessage


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
    name,
    content,
    metadata = list()
  ) {
    new_object(
      Message(),
      role = MESSAGE_ROLE_TOOL,
      name = name,
      content = content,
      metadata = metadata
    )
  }
) # /kaimana::ToolMessage


# %% AgentState Class ----
#' @title AgentState Class
#'
#' @description
#' AgentState superclass.
#'
#' @author EDG
#' @noRd
AgentState <- new_class(
  "AgentState"
) # /kaimana::AgentState


# %% InMemoryAgentState Class ----
#' @title InMemoryAgentState Class
#'
#' @description
#' Class for Agent state, which includes a list of Message objects and metadata.
#'
#' @field state Environment: Contains the list of messages in the Agent's state.
#' @field metadata List: The metadata associated with the Agent's state.
InMemoryAgentState <- new_class(
  "InMemoryAgentState",
  parent = AgentState,
  properties = list(
    state = class_environment,
    metadata = new_union(NULL | class_list)
  ),
  constructor = function(
    metadata = NULL
  ) {
    state <- new.env(parent = emptyenv())
    state[["messages"]] <- list()
    new_object(
      AgentState(),
      state = state,
      metadata = metadata
    )
  },
  validator = function(self) {
    # Ensure all elements in messages are of class Message
    if (length(self@state[["messages"]]) > 0) {
      for (i in seq_along(self@state[["messages"]])) {
        msg <- self@state[["messages"]][[i]]
        if (!S7_inherits(msg, Message)) {
          cli::cli_abort(
            "All elements in 'messages' must be of class 'Message'."
          )
        }
      }
    } # /validate state messages
  }
) # /kaimana::InMemoryAgentState


# %% append_message.InMemoryAgentState ----
#' append_message method for `InMemoryAgentState`
#'
#' @param x `InMemoryAgentState` object.
#' @param message `Message` object to append.
#' @param verbosity Integer: Verbosity level.
#'
#' @return NULL. The method modifies the state in place.
#'
#' @author EDG
#' @noRd
method(append_message, InMemoryAgentState) <- function(
  x,
  message,
  verbosity = 1L
) {
  # Check message inherits from Message
  check_is_S7(message, Message)
  x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- message
  if (verbosity > 0L) {
    cat(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      repr_bracket("InMemoryAgentState"),
      "Appended message from",
      fmt(message@name %||% message@role, col = highlight_col, bold = TRUE),
      "\n"
    )
  }
} # /kaimana::append_message.InMemoryAgentState


# %% get_messages.InMemoryAgentState ----
#' get_messages method for InMemoryAgentState
#'
#' @param x InMemoryAgentState object
#'
#' @return List of Message objects
#'
#' @author EDG
#' @noRd
method(get_messages, InMemoryAgentState) <- function(x) {
  x@state[["messages"]]
} # /kaimana::get_messages.InMemoryAgentState


# %% get_message_list ----
#' Get message list for LLM input
#'
#' Get messages as a list of named lists for LLM API input after removing ToolMessages
#'
#' @param x InMemoryAgentState object
#' @return List of lists representing messages for LLM input
#'
#' @author EDG
#' @noRd
method(get_message_list, InMemoryAgentState) <- function(x) {
  # Remove tool messages; these are packaged with custom prompts as AgentMessage (role = "user")
  msgs <- Filter(
    function(msg) {
      !S7_inherits(msg, ToolMessage)
    },
    get_messages(x)
  )
  lapply(msgs, function(msg) {
    list(
      role = msg@role,
      content = msg@content
    )
  })
} # /kaimana::get_message_list.InMemoryAgentState


# %% repr.InMemoryAgentState ----
#' repr method for InMemoryAgentState
#'
#' @param x InMemoryAgentState object
#' @param output_type Character: The output type, e.g. "console" or "html"
#'
#' @return Character representation of InMemoryAgentState
#'
#' @author EDG
#' @noRd
method(repr, InMemoryAgentState) <- function(x, pad = 0L, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  paste0(
    repr_S7name("InMemoryAgentState", pad = pad, output_type = output_type),
    fmt(
      length(x@state[["messages"]]),
      col = highlight_col,
      pad = pad + 2L,
      output_type = output_type
    ),
    ngettext(length(x@state[["messages"]]), " message.\n", " messages.\n"),
    fmt(
      length(x@metadata),
      col = highlight_col,
      pad = pad + 2L,
      output_type = output_type
    ),
    " metadata",
    ngettext(length(x@metadata), " item.\n", " items.\n")
  )
} # /kaimana::repr.InMemoryAgentState


# %% print.InMemoryAgentState ----
# Print method for InMemoryAgentState ----
method(print, InMemoryAgentState) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /kaimana::print.InMemoryAgentState
