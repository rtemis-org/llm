# Message.R
# ::kaimana::

# References:
# https://rconsortium.github.io/S7/
# subclasses inherit properties from parent classes, need only define new properties

# %% Constants ----
MESSAGE_ROLE_SYSTEM <- "system"
MESSAGE_ROLE_USER <- "user"
MESSAGE_ROLE_LLM <- "llm"
MESSAGE_ROLE_TOOL <- "tool"


# %% Message Class ----
#' @title Message Class
#'
#' @description
#' Class for messages exchanged with the AI model.
#'
#' @field role Character: The role of the message sender (e.g., "system", "user", "assistant").
#' @field name Character or NULL: The name of the message sender.
#' @field content Character: The content of the message.
#' @field metadata List or NULL: Additional metadata associated with the message.
#'
#' @author EDG
#' @noRd
Message <- new_class(
  "Message",
  properties = list(
    role = class_character,
    name = new_union(NULL | class_character),
    content = class_character,
    metadata = new_union(NULL | class_list)
  )
) # /kaimana::Message


# %% InputMessage Class ----
#' @title InputMessage Class
#'
#' @description
#' Message subclass for input messages
#'
#' @author EDG
#' @noRd
InputMessage <- new_class(
  "InputMessage",
  parent = Message,
  constructor = function(
    name,
    content,
    metadata = list()
  ) {
    new_object(
      Message(),
      role = MESSAGE_ROLE_USER,
      name = name,
      content = content,
      metadata = metadata
    )
  }
) # /kaimana::InputMessage


# %% LLMMessage Class ----
#' @title LLMMessage Class
#'
#' @description
#' Message subclass for AI messages
#'
#' @author EDG
#' @noRd
LLMMessage <- new_class(
  "LLMMessage",
  parent = Message,
  properties = list(
    model_name = class_character,
    reasoning = new_union(NULL | class_character)
  ),
  constructor = function(
    name,
    content,
    metadata,
    model_name,
    reasoning
  ) {
    new_object(
      Message(),
      role = MESSAGE_ROLE_LLM,
      name = name,
      content = content,
      metadata = metadata,
      model_name = model_name,
      reasoning = reasoning
    )
  }
) # /kaimana::LLMMessage


# %% OllamaMessage ----
OllamaMessage <- new_class(
  "OllamaMessage",
  parent = LLMMessage,
  constructor = function(
    name,
    content,
    metadata = list(),
    model_name,
    reasoning
  ) {
    metadata[["provider"]] <- "Ollama"
    new_object(
      LLMMessage(
        name = name,
        content = content,
        metadata = metadata,
        model_name = model_name,
        reasoning = reasoning
      )
    )
  }
) # /kaimana::OllamaMessage


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
          col = col_thinking,
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
        col = col_llm,
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


# %% AgentMessage Class ----
#' @title AgentMessage Class
#'
#' @description
#' Message subclass for Agent messages
#'
#' @author EDG
#' @noRd
AgentMessage <- new_class(
  "AgentMessage",
  parent = Message
) # /kaimana::AgentMessage
