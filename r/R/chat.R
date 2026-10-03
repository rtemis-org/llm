# chat.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# spec: llm/console-chat

# %% chat ----
#' Chat with an LLM or Agent in the console
#'
#' `chat` runs a multi-turn conversation in the R console. Each line you type is sent with
#' [generate] to an `Agent` that keeps the conversation in memory, and the reply is printed before
#' the next prompt, until you type `/exit`.
#'
#' Pass a model name (an `Agent` is built on the fly using `backend`, `system_prompt`, `tools`,
#' `max_tool_rounds`), an `LLM` from [create_Ollama], [create_OpenAI], [create_Anthropic], or
#' [create_Apple] (wrapped in an `Agent` with memory, using the LLM's configuration, system prompt,
#' and output schema), or an `Agent` from [create_agent] (used as given; its memory must be on).
#' Chatting with an existing `Agent` continues its conversation, and the `Agent` is updated in
#' place.
#'
#' Lines starting with `/` are commands and are not sent to the model:
#'
#' - `/image <path>`: attach a local image to the next message; repeat to attach several. See
#'   `image_path` in [generate].
#' - `/clear`: forget the conversation, keeping the system prompt.
#' - `/help`: list the commands.
#' - `/exit` or `/quit`: end the chat. Ctrl-C (Esc in RStudio) at the prompt also ends it.
#'
#' Interrupting while a reply is on its way, or a failed request, discards that turn: the message
#' and any partial exchange are removed from memory and the chat continues.
#'
#' @param x Character, LLM, or Agent: A model name, an `LLM`, or an `Agent`.
#' @param backend Character \{"ollama", "openai", "anthropic"\}: Backend to use when `x` is a model
#'   name.
#' @param system_prompt Character: System prompt for the on-the-fly `Agent`.
#' @param tools Optional list of Tool objects: Tools available to the on-the-fly `Agent`.
#' @param max_tool_rounds Integer \[1, Inf): Maximum number of tool call rounds per message, for the
#'   on-the-fly `Agent`.
#' @param verbosity Integer \[0, Inf): Verbosity level. At 1, the banner, progress and tool calls are
#'   shown; at 0, only the replies.
#' @param ... Additional per-call arguments forwarded to [generate] on every turn, such as
#'   `temperature` or `think`.
#'
#' @return The `Agent`, invisibly, with the conversation in its memory.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires an interactive session, a running Ollama server and the gemma4:e4b model
#' \dontrun{
#'   agent <- chat("gemma4:e4b")
#'   # After /exit, the conversation is in the agent's memory
#'   agent
#' }
chat <- function(
  x,
  backend = c("ollama", "openai", "anthropic"),
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  tools = NULL,
  max_tool_rounds = 3L,
  verbosity = 1L,
  ...
) {
  call <- match.call()
  backend <- match.arg(backend)
  if (!interactive()) {
    abort(
      "chat() needs an interactive R session.\n",
      "Use generate() with an Agent to hold a conversation from a script."
    )
  }
  agent <- .chat_agent(
    x,
    backend = backend,
    system_prompt = system_prompt,
    tools = tools,
    max_tool_rounds = max_tool_rounds,
    call = call,
    verbosity = verbosity
  )
  .chat_loop(agent, read_line = readline, verbosity = verbosity, ...)
}


# %% .chat_agent ----
#' Resolve the Agent a chat runs on
#'
#' @return `Agent` with memory.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.chat_agent <- function(
  x,
  backend,
  system_prompt,
  tools,
  max_tool_rounds,
  call,
  verbosity = 1L
) {
  build_args <- c("backend", "system_prompt", "tools", "max_tool_rounds")
  .refuse_decision_model(x, "x")
  if (S7_inherits(x, Agent)) {
    .check_build_conflict(call, build_args = build_args, object_name = "x")
    if (!x@use_memory) {
      abort(
        "`x` is an Agent without memory.\n",
        "chat() carries the conversation in memory; ",
        "create the Agent with `use_memory = TRUE`."
      )
    }
    return(x)
  }
  if (S7_inherits(x, LLM)) {
    .check_build_conflict(call, build_args = build_args, object_name = "x")
    # An LLM is stateless: generate() builds a fresh memory on every call.
    return(create_agent(
      llmconfig = x@config,
      system_prompt = x@system_prompt,
      use_memory = TRUE,
      output_schema = x@output_schema,
      name = x@name,
      verbosity = verbosity
    ))
  }
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
    llmconfig <- switch(
      backend,
      ollama = config_Ollama(model_name = x),
      openai = config_OpenAI(model_name = x),
      anthropic = config_Anthropic(model_name = x)
    )
    return(create_agent(
      llmconfig = llmconfig,
      system_prompt = system_prompt,
      use_memory = TRUE,
      tools = tools,
      max_tool_rounds = max_tool_rounds,
      verbosity = verbosity
    ))
  }
  abort(
    "`x` must be a model name, an LLM, or an Agent.\n",
    "Got <",
    paste(class(x), collapse = "/"),
    ">."
  )
}


# %% .chat_loop ----
#' Read, send, and print until the user exits
#'
#' @param agent `Agent` with memory.
#' @param read_line Function: Takes a prompt string and returns the line typed. Tests pass a
#'   scripted reader.
#' @param verbosity Integer: Verbosity level.
#' @param ... Forwarded to [generate] on every turn.
#'
#' @return `agent`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.chat_loop <- function(agent, read_line = readline, verbosity = 1L, ...) {
  state <- agent@state
  if (!S7_inherits(state, InProcessAgentMemory)) {
    abort(
      "chat() supports Agents with in-process memory only.\n",
      "Got <",
      class(state)[1],
      ">."
    )
  }
  if (verbosity > 0L) {
    .chat_say(
      repr_bracket(agent@llmconfig@model_name),
      " Type a message, /help for commands, /exit to quit."
    )
  }
  # Requests show a spinner instead of "working..." / "done." for the chat's duration.
  spinner_was <- .spinner_state[["enabled"]]
  .spinner_state[["enabled"]] <- TRUE
  on.exit(.spinner_state[["enabled"]] <- spinner_was, add = TRUE)
  image_path <- NULL
  repeat {
    # Ctrl-C at the prompt ends the chat.
    line <- tryCatch(read_line(.chat_prompt()), interrupt = function(e) NULL)
    if (is.null(line)) {
      .chat_say("")
      break
    }
    line <- trimws(line)
    if (!nzchar(line)) {
      next
    }
    if (startsWith(line, "/")) {
      command <- sub("\\s.*$", "", line)
      argument <- trimws(substring(line, nchar(command) + 1L))
      if (command %in% c("/exit", "/quit")) {
        break
      } else if (command == "/help") {
        .chat_help()
      } else if (command == "/clear") {
        .chat_keep_messages(state, .chat_n_system(state))
        image_path <- NULL
        .chat_say("Conversation cleared.")
      } else if (command == "/image") {
        path <- path.expand(argument)
        checked <- tryCatch(
          {
            # The reason is printed below; the error log line would repeat it.
            suppressMessages(.check_image_path(path))
            TRUE
          },
          error = function(e) {
            .chat_say(conditionMessage(e))
            FALSE
          }
        )
        if (checked) {
          image_path <- c(image_path, path)
          .chat_say(
            "Attached ",
            basename(path),
            " to the next message (",
            length(image_path),
            ngettext(length(image_path), " image).", " images).")
          )
        }
      } else {
        .chat_say("Unknown command ", command, ". Type /help for commands.")
      }
      next
    }
    reply <- .chat_turn(
      agent,
      prompt = line,
      image_path = image_path,
      verbosity = verbosity,
      ...
    )
    # Images belong to the turn that sent them, whether or not it succeeded.
    image_path <- NULL
    if (!is.null(reply)) {
      print(reply)
    }
  } # /repeat
  if (verbosity > 0L) {
    n <- length(state@state[["messages"]])
    .chat_say(
      "Chat ended with ",
      n,
      ngettext(n, " message", " messages"),
      " in memory."
    )
  }
  invisible(agent)
}


# %% .chat_turn ----
#' Send one message and return the reply
#'
#' A failed or interrupted turn is removed from memory, so the conversation stays as it was before
#' the message was sent.
#'
#' @return The last `Message` of the conversation, or NULL if the turn was discarded.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.chat_turn <- function(agent, prompt, image_path, verbosity, ...) {
  state <- agent@state
  n_before <- length(state@state[["messages"]])
  discard <- function(what) {
    .chat_keep_messages(state, n_before)
    .chat_say(what, "\nThe message was not kept.")
    NULL
  }
  tryCatch(
    {
      # Warnings raised inside a loop are otherwise deferred until the chat ends.
      out <- withCallingHandlers(
        generate(
          agent,
          prompt,
          image_path = image_path,
          verbosity = verbosity,
          ...
        ),
        warning = function(w) {
          .chat_say("Warning: ", conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      out[[length(out)]]
    },
    interrupt = function(e) discard("\nInterrupted."),
    error = function(e) discard(paste0("Error: ", conditionMessage(e)))
  )
}


# %% .chat_n_system ----
# Number of leading system messages in memory.
.chat_n_system <- function(state) {
  is_system <- vapply(
    state@state[["messages"]],
    function(m) S7_inherits(m, SystemMessage),
    logical(1)
  )
  if (all(is_system)) length(is_system) else which.min(is_system) - 1L
}


# %% .chat_keep_messages ----
# Truncate memory to its first `n` messages, in place.
.chat_keep_messages <- function(state, n) {
  state@state[["messages"]] <- state@state[["messages"]][seq_len(n)]
  invisible(state)
}


# %% .chat_prompt ----
# A heavy angle (U+276F) sets the chat apart from R's own "> ". Uncolored: ANSI
# escapes in a readline prompt throw off line editing in the terminal.
.chat_prompt <- function() {
  if (isTRUE(l10n_info()[["UTF-8"]])) "\u276F " else ">> "
}


# %% .chat_help ----
.chat_help <- function() {
  .chat_say(
    "Commands:\n",
    "  /image <path>  Attach an image to the next message\n",
    "  /clear         Forget the conversation, keep the system prompt\n",
    "  /help          Show this help\n",
    "  /exit, /quit   End the chat (also Ctrl-C at the prompt)"
  )
}


# %% .chat_say ----
# The chat's own lines go to the console with the replies, not to the message stream.
.chat_say <- function(...) {
  cat(..., "\n", sep = "")
}
