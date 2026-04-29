# %% --- S7 Properties -----------------------------------------------------------------------------

# %% Logical ----
#' Logical scalar S7 property
#'
#' S7 property accepting a single non-NA logical value.
#'
#' @return An S7 property object.
#' @author EDG
#' @noRd
logical_scalar <- new_property(
  class_logical,
  validator = function(value) {
    if (length(value) != 1L || is.na(value)) {
      return("must be a logical scalar (TRUE or FALSE)")
    }
    NULL
  }
)


# %% Character ----
#' Non-empty character scalar S7 property
#'
#' S7 property accepting a single non-NA, non-empty (after trimming whitespace) string.
#'
#' @return An S7 property object.
#' @author EDG
#' @noRd
character_scalar <- new_property(
  class_character,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
      return("must be a non-empty character scalar")
    }
    NULL
  }
)


#' Optional non-empty character scalar S7 property
#'
#' S7 property accepting `NULL` or a single non-NA, non-empty (after trimming whitespace) string.
#'
#' @return An S7 property object.
#' @author EDG
#' @noRd
optional_character_scalar <- new_property(
  class = new_union(class_character, NULL),
  default = NULL,
  validator = function(value) {
    if (
      !is.null(value) &&
        (length(value) != 1L || is.na(value) || !nzchar(trimws(value)))
    ) {
      return("must be NULL or a non-empty character scalar")
    }
    NULL
  }
)


#' Positive integer scalar S7 property
#'
#' S7 property accepting a single non-NA integer value strictly greater than zero (e.g. `1L`).
#'
#' @return An S7 property object.
#' @author EDG
#' @noRd
pos_integer_scalar <- new_property(
  class_integer,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || value <= 0L) {
      return("must be a positive integer scalar (> 0, e.g. 1L)")
    }
    NULL
  }
)


# %% Bounded double scalars ----
#' Probability scalar S7 property
#'
#' S7 property accepting a single finite double in \eqn{[0, 1]}.
#'
#' @return An S7 property object.
#' @author EDG
#' @noRd
prob_scalar <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || value < 0 || value > 1) {
      return("must be a finite double in [0, 1]")
    }
    NULL
  }
)


# %% Factory ----
#' Create a bounded double S7 property
#'
#' Returns a `new_property()` for a double scalar constrained to a given interval.
#' Useful for bounds not covered by the pre-built properties.
#'
#' @param lower Numeric scalar. Lower bound. Default `-Inf`.
#' @param upper Numeric scalar. Upper bound. Default `Inf`.
#' @param lower_open Logical scalar. If `TRUE`, lower bound is exclusive `(lower, ...]`.
#'   Default `FALSE`.
#' @param upper_open Logical scalar. If `TRUE`, upper bound is exclusive `[..., upper)`.
#'   Default `FALSE`.
#' @param nullable Logical scalar. If `TRUE`, `NULL` is also accepted. Default `FALSE`.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' # Learning rate in (0, 1]
#' lr_prop <- bounded_double_property(0, 1, lower_open = TRUE)
bounded_double_property <- function(
  lower = -Inf,
  upper = Inf,
  lower_open = FALSE,
  upper_open = FALSE,
  nullable = FALSE
) {
  lower_sym <- if (lower_open) "(" else "["
  upper_sym <- if (upper_open) ")" else "]"
  bound_desc <- paste0(
    "must be a finite double in ",
    lower_sym,
    lower,
    ", ",
    upper,
    upper_sym
  )

  check_lower <- if (lower_open) {
    function(v) v > lower
  } else {
    function(v) v >= lower
  }
  check_upper <- if (upper_open) {
    function(v) v < upper
  } else {
    function(v) v <= upper
  }

  cls <- if (nullable) new_union(class_double, NULL) else class_double

  new_property(
    class = cls,
    validator = function(value) {
      if (is.null(value)) {
        return(NULL)
      }
      if (length(value) != 1L || is.na(value) || !is.finite(value)) {
        return(paste0(bound_desc, " (must be a finite scalar)"))
      }
      if (!check_lower(value) || !check_upper(value)) {
        return(bound_desc)
      }
      NULL
    }
  )
}


# %% --- Checks ------------------------------------------------------------------------------------
# %% check_scalar_character() ----
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
check_scalar_character <- function(x, name) {
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


# %% check_optional_scalar_character ----
#' Check Optional Scalar Character
#'
#' @param x Optional Character: Value to check.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects.
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' check_optional_scalar_character(NULL, "my_arg") # Passes
#' check_optional_scalar_character("hello", "my_arg") # Passes
#' # Throw error:
#' try(check_optional_scalar_character(c("hello", "world"), "my_arg"))
#' try(check_optional_scalar_character(123, "my_arg"))
check_optional_scalar_character <- function(
  x,
  arg_name = deparse(substitute(x))
) {
  check_character(x, allow_null = TRUE, arg_name = arg_name)
  if (!is.null(x) && length(x) != 1L) {
    cli::cli_abort(
      "{.var {arg_name}} must be NULL or a single string."
    )
  }
  invisible()
}


# %% check_double_scalar ----
#' Check double scalar
#'
#' @param x Numeric: Value to check. Must be a single non-NA number (integer inputs are accepted).
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_double_scalar(3.14)
#' check_double_scalar(1L)
#' # Throw error:
#' try(check_double_scalar(NA_real_))
#' try(check_double_scalar(c(1.0, 2.0)))
check_double_scalar <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.var {arg_name}} must be numeric.")
  }
  if (length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.var {arg_name}} must be a single non-NA number.")
  }
  invisible()
} # /rtemis.core::check_double_scalar


# %% check_optional_pos_double_scalar ----
#' Check optional positive double scalar
#'
#' @param x Optional Numeric: Value to check. Must be `NULL` or a single finite number
#'   strictly greater than zero.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @noRd
#'
#' @examples
#' check_optional_pos_double_scalar(NULL)
#' check_optional_pos_double_scalar(2.5)
#' # Throw error:
#' try(check_optional_pos_double_scalar(0))
check_optional_pos_double_scalar <- function(
  x,
  arg_name = deparse(substitute(x))
) {
  if (is.null(x)) {
    return(invisible())
  }
  check_pos_double_scalar(x, arg_name = arg_name)
  invisible()
} # /rtemis.core::check_optional_pos_double_scalar


# %% check_pos_double_scalar ----
#' Check positive double scalar
#'
#' @param x Numeric: Value to check. Must be a single finite number strictly greater than zero.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_pos_double_scalar(0.001)
#' check_pos_double_scalar(100)
#' # Throw error:
#' try(check_pos_double_scalar(0))
#' try(check_pos_double_scalar(-1))
#' try(check_pos_double_scalar(Inf))
check_pos_double_scalar <- function(x, arg_name = deparse(substitute(x))) {
  check_double_scalar(x, arg_name = arg_name)
  if (!is.finite(x) || x <= 0) {
    cli::cli_abort("{.var {arg_name}} must be a finite number greater than 0.")
  }
  invisible()
} # /rtemis.core::check_pos_double_scalar


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


# %% --- Classes -----------------------------------------------------------------------------------

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
    content = character_scalar,
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


# %% --- Utils -------------------------------------------------------------------------------------

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
  check_scalar_character(x, "base_url")
  sub("/+$", "", trimws(x))
}

# %% available_tools ----
#' Print built-in tools available for use by agents
#'
#' Prints the R handle (`tool_*`), the `function_name` the model sees, and the
#' description of every built-in `Tool` exported by the package. Derived at
#' call time from the namespace — no hardcoded list.
#'
#' @param verbosity Integer: Verbosity level.
#'
#' @return A named list of `Tool` objects keyed by their R handle, invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' available_tools()
available_tools <- function(verbosity = 1L) {
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
  if (verbosity > 0L) {
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
  }
  invisible(tools)
}
