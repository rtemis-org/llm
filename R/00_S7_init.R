# S7 utils
# ::kaimana::
# 2025 EDG rtemis.org

# --- Generics -------------------------------------------------------------------------------------
# %% generate generic ----
#' Generate Method
#'
#' Generic method for generating text or structured output from LLMs and Agents
#'
#' @param x An object of class LLM or Agent.
#' @param prompt Character: The prompt or query to pass to the model or agent.
#'
#' @return `Message` object or list.
#'
#' @author EDG
#' @export
generate <- new_generic("generate", "x")


# %% as_Message generic ----
#' Convert to Message
#'
#' Generic method to convert various objects to kaimana Message objects
#'
#' @param x An object to convert
#'
#' @return A Message object
#'
#' @author EDG
#' @export
as_Message <- new_generic("as_Message", "x")


# %% as_OllamaMessage generic ----
#' Convert to OllamaMessage
#'
#' Generic method to convert to `OllamaMessage` object
#'
#' @param x An object to convert
#'
#' @return An OllamaMessage object
#'
#' @author EDG
#' @export
as_OllamaMessage <- new_generic("as_OllamaMessage", "x")


# %% get_content generic ----
#' Get content
#'
#' @param x An object of class AIResponse or ReasoningResponse
#'
#' @return Character if content is text, data.table if content is structured
#'
#' @author EDG
#' @export
get_content <- new_generic("get_content", "x")


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


# %% as_list generic ----
#' Convert to R list
#'
#' Generic method to convert various objects to R lists
#'
#' @param x An object to convert
#'
#' @return A named R list
#'
#' @author EDG
#' @export
as_list <- new_generic("as_list", "x")


# %% append_message generic ----
#' Append message
#'
#' Generic method to append a `Message` object to an `AgentState`
#'
#' @param x An `AgentState` object.
#' @param message A `Message` object to append.
#'
#' @return The updated `AgentState` object, invisibly.
#'
#' @author EDG
#' @export
append_message <- new_generic("append_message", "x")


# %% get_messages generic ----
#' Get messages
#'
#' Generic method to retrieve messages from `AgentState` objects
#'
#' @param x An `AgentState` object.
#'
#' @return A list of `Message` objects.
#'
#' @author EDG
#' @export
get_messages <- new_generic("get_messages", "x")


# %% get_message_list generic ----
#' Get message list
#'
#' Generic method to retrieve messages as a list of named lists for LLM APIs
#'
#' @param x An `AgentState` object.
#'
#' @return A list of named lists representing messages.
#'
#' @author EDG
#' @export
get_message_list <- new_generic("get_message_list", "x")


# %% create_llm_message generic ----
#' Create agent message
#'
#' Generic method to create an agent message for different backends
#'
#' @param x An `Agent` object.
#' @param content Character: The content of the message.
#' @param reasoning Optional character: The reasoning trace.
#'
#' @return An `LLMMessage` object.
#'
#' @author EDG
#' @export
create_llm_message <- new_generic("create_llm_message", "x")


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
