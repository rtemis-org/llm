# State.R
# ::kaimana::

# References:
# https://rconsortium.github.io/S7/
# subclasses inherit properties from parent classes, need only define new properties

# Description:
# This file defines the InMemoryAgentState class to encapsulate the state of an Agent, which includes
# a list of Message objects and associated metadata.

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
#' @return The updated `InMemoryAgentState` object, invisibly.
#'
#' @author EDG
#' @noRd
method(append_message, InMemoryAgentState) <- function(
  x,
  message,
  echo = TRUE,
  verbosity = 1L
) {
  # Check message inherits from Message
  S7::check_is_S7(message, Message)
  if (echo && verbosity > 0L) {
    print(message)
  }
  x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- message
  if (verbosity > 0L) {
    cat(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      repr_bracket("InMemoryAgentState"),
      "Appended",
      class(message)[1],
      "from",
      fmt(message@name %||% message@role, col = highlight_col, bold = TRUE),
      "\n"
    )
  }
  invisible(x)
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
method(get_messages, InMemoryAgentState) <- function(x, last = FALSE) {
  if (last) {
    return(tail(x@state[["messages"]], 1))
  }
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


# %% View.Message ----
View.Message <- function(x, title = x@role) {
  View(as_list(x), title = title)
} # /kaimana::View.Message
