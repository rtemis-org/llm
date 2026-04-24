# --- Generics -------------------------------------------------------------------------------------

# %% get_model_name ----
get_model_name <- new_generic("get_model_name", "x")


# %% map ----
#' Map
#'
#' @param x A character vector or list to map over.
#' @param f An `LLM` or `Agent` object.
#' @param ... Additional arguments passed to `generate()`.
#'
#' @details
#' Use [responses] to retrieve just the content from the assistant messages, or [reasoning] to
#' retrieve the reasoning traces (if enabled).
#'
#' @return A list of `Message` objects (for `LLM`) or list of lists of `Message` objects
#' (for `Agent`).
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running Ollama server and gemma4:e4b model
#' \dontrun{
#'   llm <- create_Ollama(
#'     "gemma4:e4b",
#'     system_prompt = "Convert color to hex code using the format #FFFFFF"
#'   )
#'   x <- c("ocean teal", "california poppy orange", "bougainvillea pink")
#'   hex <- map(x, llm)
#'   hex
#' }
map <- new_generic("map", c("x", "f"), function(x, f, ...) S7_dispatch())


# %% to_json ----
to_json <- new_generic("to_json", "x")

# %% generate ----
#' Generate Method
#'
#' Generic method for generating text or structured output from LLMs and Agents.
#'
#' @param x An object of class LLM or Agent.
#' @param prompt Character: The prompt to pass to the model or agent.
#' @param temperature Optional numeric \[0, 2\]: Per-call sampling temperature.
#' @param top_p Optional numeric \[0, 1\]: Nucleus sampling cutoff.
#' @param max_tokens Optional integer \[1, Inf): Maximum tokens to generate. For Anthropic,
#' this overrides the config-level value (which is required); for Ollama this maps to
#' `options.num_predict`; for OpenAI-compatible backends this maps to `max_tokens`.
#' @param stop Optional character: Stop sequence(s). Mapped to `stop_sequences` on Anthropic
#' and `options.stop` on Ollama.
#' @param think Optional logical or character: Whether to enable model thinking
#' (reasoning trace) for this call. Character values target `gpt-oss`-style local models.
#' @param output_schema Optional Schema: Output schema to enforce on this call's response.
#' If omitted, the object's default schema (if any) is used.
#' @param verbosity Integer: Verbosity level.
#' @param ... Additional backend-specific per-call arguments. See Details.
#'
#' @details
#' The system prompt is set once at agent (or LLM) construction time and is **not**
#' overridable per call. Construct a new agent if you need a different system prompt.
#'
#' Backend-specific extra arguments accepted via `...`:
#' - **Ollama**: `top_k` (integer), `seed` (integer)
#' - **OpenAI**: `seed` (integer)
#' - **Anthropic**: `top_k` (integer)
#'
#' Any argument set to `NULL` (the default) falls back to the value baked into the
#' underlying `LLMConfig` at construction time.
#'
#' @return `Message` object or list of `Message` objects (for `Agent`).
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running Ollama server and gemma4:e4b model
#' \dontrun{
#'   agent <- create_agent(
#'     config_Ollama(
#'       model_name = "gemma4:e4b",
#'       temperature = 0.2
#'     )
#'   )
#'   generate(agent, "What is your name?", temperature = 0.7)
#' }
generate <- new_generic(
  "generate",
  "x",
  function(
    x,
    prompt,
    temperature = NULL,
    top_p = NULL,
    max_tokens = NULL,
    stop = NULL,
    think = NULL,
    output_schema = NULL,
    verbosity = 1L,
    ...
  ) {
    S7_dispatch()
  }
)


# %% as_Message ----
#' Convert to Message
#'
#' Generic method to convert various objects to rtemis.llm Message objects
#'
#' @param x An object to convert
#'
#' @return A Message object
#'
#' @author EDG
#' @keywords internal
#' @noRd
as_Message <- new_generic("as_Message", "x")


# %% as_OllamaMessage ----
#' Convert to OllamaMessage
#'
#' Generic method to convert to `OllamaMessage` object
#'
#' @param x An object to convert
#'
#' @return An OllamaMessage object
#'
#' @author EDG
#' @keywords internal
#' @noRd
as_OllamaMessage <- new_generic("as_OllamaMessage", "x")


# %% get_content ----
#' Get content
#'
#' @param x An object of class AIResponse or ReasoningResponse
#'
#' @return Character if content is text, data.table if content is structured
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_content <- new_generic("get_content", "x")


# %% as_list ----
#' Convert to R list
#'
#' Generic method to convert various objects to R lists
#'
#' @param x An object to convert
#' @param ... Additional arguments for specific methods
#'
#' @return A named R list
#'
#' @author EDG
#' @export
#'
#' @examples
#' decay_time <- field("decay_time", "Time from peak amplitude to sustain level", type = "number")
#' as_list(decay_time)
as_list <- new_generic("as_list", "x")


# %% append_message ----
#' Append message
#'
#' Generic method to append a `Message` object to an `AgentMemory`
#'
#' @param x An `AgentMemory` object.
#' @param message A `Message` object to append.
#'
#' @return The updated `AgentMemory` object, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
append_message <- new_generic("append_message", "x")


# %% get_messages ----
#' Get messages
#'
#' Generic method to retrieve messages from `AgentMemory` objects
#'
#' @param x An `AgentMemory` object.
#'
#' @return A list of `Message` objects.
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_messages <- new_generic("get_messages", "x")


# %% get_message_list ----
#' Get message list
#'
#' Generic method to retrieve messages as a list of named lists for LLM APIs
#'
#' @param x An `AgentMemory` object.
#'
#' @return A list of named lists representing messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_message_list <- new_generic("get_message_list", "x")


# %% create_llm_message ----
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
#' @keywords internal
#' @noRd
create_llm_message <- new_generic("create_llm_message", "x")


# %% build_chat_messages ----
#' Build Chat Messages
#'
#' Generic method to build provider-specific chat messages from agent memory
#'
#' @param x An `LLMConfig` object.
#'
#' @return A list of provider-specific message lists.
#'
#' @author EDG
#' @keywords internal
#' @noRd
build_chat_messages <- new_generic("build_chat_messages", "x")


# %% build_chat_request_body ----
#' Build Chat Request Body
#'
#' Generic method to build provider-specific chat request bodies.
#'
#' @param x An `LLMConfig` object.
#'
#' @return A named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
build_chat_request_body <- new_generic("build_chat_request_body", "x")


# %% perform_chat_request ----
#' Perform Chat Request
#'
#' Generic method to perform provider-specific chat API requests.
#'
#' @param x An `LLMConfig` object.
#'
#' @return An `httr2_response` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
perform_chat_request <- new_generic("perform_chat_request", "x")


# %% parse_chat_response ----
#' Parse Chat Response
#'
#' Generic method to parse provider-specific chat API responses.
#'
#' @param x An `LLMConfig` object.
#'
#' @return A named list with normalized response fields.
#'
#' @author EDG
#' @keywords internal
#' @noRd
parse_chat_response <- new_generic("parse_chat_response", "x")


# %% decode_tool_arguments ----
#' Decode Tool Arguments
#'
#' Generic method to decode provider-specific tool call arguments.
#'
#' @param x An `LLMConfig` object.
#'
#' @return A named list of tool arguments.
#'
#' @author EDG
#' @keywords internal
#' @noRd
decode_tool_arguments <- new_generic("decode_tool_arguments", "x")


# %% build_tool_message ----
#' Build Tool Message
#'
#' Generic method to build provider-specific tool response messages.
#'
#' @param x An `LLMConfig` object.
#'
#' @return A `ToolMessage` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
build_tool_message <- new_generic("build_tool_message", "x")


# %% build_response_format ----
#' Build Response Format
#'
#' Generic method to build provider-specific structured output request fields.
#'
#' @param x An `LLMConfig` object.
#'
#' @return A named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
build_response_format <- new_generic("build_response_format", "x")


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
#' @noRd
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
)

# %% utils ----
# %% .check_scalar_character() ----
#' Check Scalar Character
#'
#' @param x Object: Object to check.
#' @param name Character: Argument name to report.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_scalar_character <- function(x, name) {
  if (
    !is.character(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !nzchar(trimws(x))
  ) {
    cli::cli_abort("{.var {name}} must be a non-empty character scalar.")
  }
  invisible(NULL)
}


# %% .is_named_list() ----
#' Test Named List
#'
#' @param x Object: Object to test.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.is_named_list <- function(x) {
  is.list(x) &&
    length(x) == length(names(x)) &&
    all(nzchar(names(x)))
}


# %% .clean_base_url() ----
#' Clean Base URL
#'
#' @param x Character: Base URL.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.clean_base_url <- function(x) {
  .check_scalar_character(x, "base_url")
  sub("/+$", "", trimws(x))
}

# %% available_tools ----
#' Print built-in tools available for use by agents
#'
#' Prints the R handle (`tool_*`), the `function_name` the model sees, and the
#' description of every built-in `Tool` exported by the package. Derived at
#' call time from the namespace — no hardcoded list.
#'
#' @return Invisibly, a named list of `Tool` objects keyed by their R handle.
#'
#' @author EDG
#' @export
#'
#' @examples
#' available_tools()
available_tools <- function() {
  ns <- asNamespace("rtemis.llm")
  exports <- getNamespaceExports(ns)
  is_tool_export <- vapply(
    exports,
    function(nm) {
      obj <- get(nm, envir = ns, inherits = FALSE)
      S7_inherits(obj, Tool)
    },
    logical(1)
  )
  tool_handles <- sort(exports[is_tool_export])
  tools <- stats::setNames(
    lapply(tool_handles, get, envir = ns, inherits = FALSE),
    tool_handles
  )
  cat(fmt("\n  Built-in tools:\n\n"))
  for (handle in tool_handles) {
    tool <- tools[[handle]]
    cat(
      "* ",
      highlight(handle),
      " (function_name: ",
      tool@function_name,
      ")\n  ",
      tool@description,
      "\n\n",
      sep = ""
    )
  }
  invisible(tools)
}
