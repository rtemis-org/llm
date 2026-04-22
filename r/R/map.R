# %% responses ----
#' Extract response(s) from a Message or list of Messages
#'
#' Returns the assistant content from a single `Message`, from a flat list of `Message` objects
#' (e.g. the output of [llmapply] with `extract_responses = FALSE`), or from a list of lists of
#' `Message` objects (e.g. the output of `map()` on an `Agent` with `extract_responses = FALSE`).
#'
#' @param x `Message` object, list of `Message` objects, or list of lists of `Message` objects.
#'
#' @return Character vector of assistant responses. Returns `NA_character_` in slots where no
#'   assistant message is present, so that the length of the result matches the length of `x`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running Ollama server and gemma4:e4b model
#' \dontrun{
#'   llmapply(
#'     c("burgundy", "crimson", "maroon", "ruby", "scarlet"),
#'     "gemma4:e4b",
#'     system_prompt = "Return the hexadecimal code for the color provided in format #FFFFFF"
#'     temperature = 0.2
#'   ) |> responses()
#' }
responses <- function(x) {
  # Single Message object
  if (S7_inherits(x, Message)) {
    if (x@role == "assistant") {
      return(x@content)
    }
    return(NA_character_)
  }

  # Flat list of Message objects (output of map on an LLM)
  if (is.list(x) && all(vapply(x, S7_inherits, logical(1L), class = Message))) {
    return(
      vapply(
        x,
        function(m) if (m@role == "assistant") m@content else NA_character_,
        character(1L)
      )
    )
  }

  # List of lists of Message objects (output of map on an Agent)
  if (
    is.list(x) &&
      all(vapply(
        x,
        function(item) {
          is.list(item) &&
            all(vapply(item, S7_inherits, logical(1L), class = Message))
        },
        logical(1L)
      ))
  ) {
    return(
      vapply(
        x,
        function(messages) {
          asst <- Filter(function(m) m@role == "assistant", messages)
          if (length(asst) > 0L) asst[[length(asst)]]@content else NA_character_
        },
        character(1L)
      )
    )
  }

  cli::cli_abort(
    c(
      "!" = "Could not extract responses from {.arg x}.",
      "i" = "Pass a {.cls Message}, a list of {.cls Message} objects, or a list of lists of {.cls Message} objects (the output of {.fn map} / {.fn llmapply} / {.fn agentapply} with {.code extract_responses = FALSE})."
    )
  )
}


# %% reasoning ----
#' Extract reasoning trace(s) from a Message or list of Messages
#'
#' Returns the assistant's reasoning trace (if any). Only `LLMMessage` objects carry a `reasoning`
#' field; all other `Message` subclasses return `NA_character_`. Messages whose reasoning is unset
#' (`NULL`) also return `NA_character_`.
#'
#' @param x `Message` object, list of `Message` objects, or list of lists of `Message` objects.
#'
#' @return Character vector of reasoning traces, with `NA_character_` in slots where no reasoning
#'   is available.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running Ollama server and gemma4:e4b model
#' \dontrun{
#'   llmapply(
#'     c("burgundy", "crimson", "maroon", "ruby", "scarlet"),
#'     "gemma4:e4b",
#'     system_prompt = "Return the hexadecimal code for the color provided in format #FFFFFF"
#'     temperature = 0.2
#'   ) |> reasoning()
#' }
reasoning <- function(x) {
  one <- function(m) {
    if (S7_inherits(m, LLMMessage) && !is.null(m@reasoning)) {
      m@reasoning
    } else {
      NA_character_
    }
  }

  # Single Message object
  if (S7_inherits(x, Message)) {
    return(one(x))
  }

  # Flat list of Message objects
  if (is.list(x) && all(vapply(x, S7_inherits, logical(1L), class = Message))) {
    return(vapply(x, one, character(1L)))
  }

  # List of lists of Message objects: return the first assistant message's reasoning per element
  if (
    is.list(x) &&
      all(vapply(
        x,
        function(item) {
          is.list(item) &&
            all(vapply(item, S7_inherits, logical(1L), class = Message))
        },
        logical(1L)
      ))
  ) {
    return(
      vapply(
        x,
        function(messages) {
          asst <- Filter(function(m) m@role == "assistant", messages)
          if (length(asst) > 0L) one(asst[[length(asst)]]) else NA_character_
        },
        character(1L)
      )
    )
  }

  cli::cli_abort(
    c(
      "!" = "Could not extract reasoning from {.arg x}.",
      "i" = "Pass a {.cls Message}, a list of {.cls Message} objects, or a list of lists of {.cls Message} objects (the output of {.fn map} / {.fn llmapply} / {.fn agentapply} with {.code extract_responses = FALSE})."
    )
  )
}


# %% .map_iter ----
# Internal: shared iteration body for map methods. Uses cli progress bar and forwards
# backend-specific per-call args via `...` to `generate()`.
.map_iter <- function(x, f, verbosity, ...) {
  lapply(
    cli::cli_progress_along(
      x,
      "Processing",
      format = "{cli::pb_spin} [{cli::pb_current}/{cli::pb_total}] {cli::pb_bar} {cli::pb_eta_str}"
    ),
    function(i) {
      generate(f, x[[i]], verbosity = verbosity - 1L, ...)
    }
  )
}


# %% map.(class_character, LLM | Agent) ----
method(map, list(class_character, LLM | Agent)) <- function(
  x,
  f,
  verbosity = 1L,
  ...
) {
  .map_iter(x, f, verbosity, ...)
}


# %% map.(class_list, LLM | Agent) ----
method(map, list(class_list, LLM | Agent)) <- function(
  x,
  f,
  verbosity = 1L,
  ...
) {
  .map_iter(x, f, verbosity, ...)
}


# %% .check_build_conflict ----
# Internal: abort if both a pre-built object and build-path arguments were supplied.
.check_build_conflict <- function(call, build_args, object_name) {
  passed <- setdiff(names(call)[-1L], "")
  conflicts <- intersect(passed, build_args)
  if (length(conflicts) > 0L) {
    cli::cli_abort(
      c(
        "!" = "Cannot supply {.arg {conflicts}} when {.arg {object_name}} is already a built object.",
        "i" = "Either pass a model name string together with build-path arguments, or pass a pre-built object with no build-path arguments."
      )
    )
  }
  invisible(NULL)
}


# %% llmapply ----
#' Apply an LLM over a vector of prompts
#'
#' `llmapply` is the `lapply`-style entry point for running a single prompt against an `LLM`
#' repeatedly over a vector of inputs. Pass either a model name (in which case an `LLM` is built
#' on the fly using `backend`, `system_prompt`, `output_schema`) or a pre-built `LLM` object.
#'
#' Per-call overrides such as `temperature`, `top_p`, `max_tokens`, `stop`, `think`, plus
#' backend-specific options like `top_k` or `seed`, are forwarded via `...` to [generate]. Vectors
#' passed via `...` are **not** yet recycled across `x` — they are forwarded as-is to each call.
#'
#' @param x Character or list: Values to iterate over. Each element forms the user prompt for one
#'   call to the LLM.
#' @param model_or_llm Character or LLM: Either the name of a model (a string) or a pre-built
#'   `LLM` object (for example from [create_Ollama], [create_OpenAI], or [create_Claude]).
#' @param backend Character \{"ollama", "openai", "claude"\}: Backend to use when `model_or_llm`
#'   is a string. Ignored when `model_or_llm` is an `LLM` object.
#' @param system_prompt Character: System prompt to use when building the `LLM` from a model name.
#'   Ignored when `model_or_llm` is an `LLM` object.
#' @param output_schema Optional Schema: Output schema to enforce, created with [schema]. When
#'   `model_or_llm` is a string, this is baked into the built `LLM`. When `model_or_llm` is a
#'   pre-built `LLM`, supplying this here is a conflict and will error.
#' @param verbosity Integer \[0, Inf): Verbosity level. The per-call verbosity is `verbosity - 1L`.
#' @param extract_responses Logical: If `TRUE`, return a character vector of assistant responses
#'   (with `NA_character_` for missing assistant content). If `FALSE`, return the raw list of
#'   `Message` objects from each call.
#' @param ... Additional per-call arguments forwarded to [generate] (e.g. `temperature`, `top_p`,
#'   `max_tokens`, `stop`, `think`, `top_k`, `seed`).
#'
#' @return If `extract_responses = TRUE`, a character vector the same length as `x`. Otherwise, a
#'   list of `Message` objects.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running Ollama server and gemma4:e4b model
#' \dontrun{
#'   llmapply(
#'     c("burgundy", "crimson", "maroon", "ruby", "scarlet"),
#'     "gemma4:e4b",
#'     system_prompt = "Return the hexadecimal code for the color provided in format #FFFFFF"
#'     temperature = 0.2
#'   )
#' }
llmapply <- function(
  x,
  model_or_llm,
  backend = c("ollama", "openai", "claude"),
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  output_schema = NULL,
  verbosity = 1L,
  extract_responses = TRUE,
  ...
) {
  call <- match.call()
  backend <- match.arg(backend)

  if (S7_inherits(model_or_llm, Agent)) {
    cli::cli_abort(
      c(
        "!" = "{.arg model_or_llm} is an {.cls Agent}, not an {.cls LLM}.",
        "i" = "Use {.fn agentapply} to run an {.cls Agent} over a vector of prompts."
      )
    )
  }

  if (S7_inherits(model_or_llm, LLM)) {
    .check_build_conflict(
      call,
      build_args = c("backend", "system_prompt", "output_schema"),
      object_name = "model_or_llm"
    )
    llm <- model_or_llm
  } else if (is.character(model_or_llm) && length(model_or_llm) == 1L) {
    llm <- switch(
      backend,
      ollama = create_Ollama(
        model_name = model_or_llm,
        system_prompt = system_prompt,
        output_schema = output_schema
      ),
      openai = create_OpenAI(
        model_name = model_or_llm,
        system_prompt = system_prompt,
        output_schema = output_schema
      ),
      claude = create_Claude(
        model_name = model_or_llm,
        system_prompt = system_prompt,
        output_schema = output_schema
      )
    )
  } else {
    cli::cli_abort(
      c(
        "!" = "{.arg model_or_llm} must be a single model-name string or a built {.cls LLM} object.",
        "i" = "Got {.cls {class(model_or_llm)}}."
      )
    )
  }

  out <- map(x, llm, verbosity = verbosity, ...)
  if (extract_responses) responses(out) else out
}


# %% agentapply ----
#' Apply an Agent over a vector of prompts
#'
#' `agentapply` is the `lapply`-style entry point for running a single prompt against an `Agent`
#' repeatedly over a vector of inputs. Pass either a model name (in which case an `Agent` is built
#' on the fly using `backend`, `system_prompt`, `tools`, `use_memory`, `max_tool_rounds`,
#' `output_schema`) or a pre-built `Agent` object.
#'
#' Unlike [llmapply], this function can carry tools and memory. The default is `use_memory = FALSE`
#' because the common case for vectorized calls is independent queries.
#'
#' @param x Character or list: Values to iterate over.
#' @param model_or_agent Character or Agent: Either the name of a model (a string) or a pre-built
#'   `Agent` object from [create_agent].
#' @param backend Character \{"ollama", "openai", "claude"\}: Backend to use when
#'   `model_or_agent` is a string. Ignored when `model_or_agent` is an `Agent` object.
#' @param system_prompt Character: System prompt for the on-the-fly `Agent`.
#' @param tools Optional list of Tool objects: Tools available to the on-the-fly `Agent`.
#' @param use_memory Logical: Whether the on-the-fly `Agent` should keep conversation memory.
#' @param max_tool_rounds Integer \[1, Inf): Maximum number of tool call rounds per query.
#' @param output_schema Optional Schema: Output schema for the on-the-fly `Agent`.
#' @param verbosity Integer \[0, Inf): Verbosity level.
#' @param extract_responses Logical: If `TRUE`, return a character vector of assistant responses.
#'   If `FALSE`, return the raw list of lists of `Message` objects.
#' @param ... Additional per-call arguments forwarded to [generate].
#'
#' @return If `extract_responses = TRUE`, a character vector the same length as `x`. Otherwise, a
#'   list of lists of `Message` objects.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running Ollama server and gemma4:e4b model
#' \dontrun{
#'   agentapply(
#'     c("today", "yesterday", "tomorrow"),
#'     "gemma4:e4b",
#'     system_prompt = "Return the date in ISO format",
#'     tools = list(tool_datetime),
#'     temperature = 0.2
#'   )
#' }
agentapply <- function(
  x,
  model_or_agent,
  backend = c("ollama", "openai", "claude"),
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  tools = NULL,
  use_memory = FALSE,
  max_tool_rounds = 3L,
  output_schema = NULL,
  verbosity = 1L,
  extract_responses = TRUE,
  ...
) {
  call <- match.call()
  backend <- match.arg(backend)

  if (S7_inherits(model_or_agent, LLM)) {
    cli::cli_abort(
      c(
        "!" = "{.arg model_or_agent} is an {.cls LLM}, not an {.cls Agent}.",
        "i" = "Use {.fn llmapply} to run an {.cls LLM} over a vector of prompts."
      )
    )
  }

  if (S7_inherits(model_or_agent, Agent)) {
    .check_build_conflict(
      call,
      build_args = c(
        "backend",
        "system_prompt",
        "tools",
        "use_memory",
        "max_tool_rounds",
        "output_schema"
      ),
      object_name = "model_or_agent"
    )
    agent <- model_or_agent
  } else if (is.character(model_or_agent) && length(model_or_agent) == 1L) {
    llmconfig <- switch(
      backend,
      ollama = config_Ollama(model_name = model_or_agent),
      openai = config_OpenAI(model_name = model_or_agent),
      claude = config_Claude(model_name = model_or_agent)
    )
    agent <- create_agent(
      llmconfig = llmconfig,
      system_prompt = system_prompt,
      use_memory = use_memory,
      tools = tools,
      max_tool_rounds = max_tool_rounds,
      output_schema = output_schema,
      verbosity = verbosity
    )
  } else {
    cli::cli_abort(
      c(
        "!" = "{.arg model_or_agent} must be a single model-name string or a built {.cls Agent} object.",
        "i" = "Got {.cls {class(model_or_agent)}}."
      )
    )
  }

  out <- map(x, agent, verbosity = verbosity, ...)
  if (extract_responses) responses(out) else out
}
