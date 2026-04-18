# Description:
# This file defines the InProcessAgentMemory class to encapsulate the state of an Agent, which includes
# a list of Message objects and associated metadata.

# %% AgentMemory ----
#' @title AgentMemory Class
#'
#' @description
#' AgentMemory superclass.
#'
#' @author EDG
#' @noRd
AgentMemory <- new_class(
  "AgentMemory"
)


# %% InProcessAgentMemory ----
#' @title InProcessAgentMemory Class
#'
#' @description
#' Class for Agent state, which includes a list of Message objects and metadata.
#'
#' @field state Environment: Contains the list of messages in the Agent's state.
#' @field metadata List: The metadata associated with the Agent's state.
#'
#' @author EDG
#' @noRd
InProcessAgentMemory <- new_class(
  "InProcessAgentMemory",
  parent = AgentMemory,
  properties = list(
    state = class_environment,
    metadata = optional(S7::class_list)
  ),
  constructor = function(
    metadata = NULL
  ) {
    state <- new.env(parent = emptyenv())
    state[["messages"]] <- list()
    new_object(
      AgentMemory(),
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
)


# %% append_message.InProcessAgentMemory ----
#' append_message method for `InProcessAgentMemory`
#'
#' @param x `InProcessAgentMemory` object.
#' @param message `Message` object to append.
#' @param verbosity Integer: Verbosity level.
#'
#' @return The updated `InProcessAgentMemory` object, invisibly.
#'
#' @author EDG
#' @noRd
method(append_message, InProcessAgentMemory) <- function(
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
  msg(
    repr_bracket("InProcessAgentMemory"),
    "Appended",
    class(message)[1],
    "from",
    fmt(message@name %||% message@role, col = highlight_col, bold = TRUE),
    verbosity = verbosity
  )
  invisible(x)
}


# %% get_messages.InProcessAgentMemory ----
#' get_messages method for InProcessAgentMemory
#'
#' @param x InProcessAgentMemory object
#'
#' @return List of Message objects
#'
#' @author EDG
#' @noRd
method(get_messages, InProcessAgentMemory) <- function(x, last = FALSE) {
  if (last) {
    return(tail(x@state[["messages"]], 1))
  }
  out <- x@state[["messages"]]
  setNames(out, sapply(out, function(msg) msg@role))
}


# %% get_message_list.InProcessAgentMemory ----
#' Get message list for LLM input
#'
#' Get messages as a list of named lists for LLM API input after removing ToolMessages
#'
#' @param x InProcessAgentMemory object
#' @return List of lists representing messages for LLM input
#'
#' @author EDG
#' @noRd
method(get_message_list, InProcessAgentMemory) <- function(x) {
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
}


# %% repr.InProcessAgentMemory ----
#' repr method for InProcessAgentMemory
#'
#' @param x InProcessAgentMemory object
#' @param output_type Character: The output type, e.g. "console" or "html"
#'
#' @return Character representation of InProcessAgentMemory
#'
#' @author EDG
#' @noRd
method(repr, InProcessAgentMemory) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  paste0(
    repr_S7name("InProcessAgentMemory", pad = pad, output_type = output_type),
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
}


# %% print.InProcessAgentMemory ----
method(print, InProcessAgentMemory) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
}


# %% View.Message ----
View.Message <- function(x, title = x@role) {
  View(as_list(x), title = title)
}
