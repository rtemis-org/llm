# %% responses ----
#' Extract response(s) from a Message or list of Messages
#'
#' Returns the assistant content from a single `Message`, from a flat list of `Message` objects
#' (e.g. the output of [llmapply] with `extract_responses = FALSE`), or from a list of lists of
#' `Message` objects (e.g. the output of `map()` on an `Agent` with `extract_responses = FALSE`).
#'
#' @param x `Message` object, list of `Message` objects, or list of lists of `Message` objects.
#'   `NULL` elements are allowed: [llmapply] and [agentapply] leave one in the slot of every call
#'   that failed under `on_error = "na"`.
#'
#' @return Character vector of assistant responses. Returns `NA_character_` in slots where no
#'   assistant message is present (including failed calls), so that the length of the result
#'   matches the length of `x`.
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
#'     system_prompt = "Return the hexadecimal code for the color provided in format #FFFFFF",
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

  # Flat list of Message objects (output of map on an LLM). A NULL slot is a call
  # that failed under `on_error = "na"`; it keeps its position as NA so the
  # result still lines up with the input.
  if (is.list(x) && all(vapply(x, .is_null_or_message, logical(1L)))) {
    return(
      vapply(
        x,
        function(m) {
          if (!is.null(m) && m@role == "assistant") m@content else NA_character_
        },
        character(1L)
      )
    )
  }

  # List of lists of Message objects (output of map on an Agent)
  if (
    is.list(x) &&
      all(vapply(x, .is_null_or_message_list, logical(1L)))
  ) {
    return(
      vapply(
        x,
        function(messages) {
          if (is.null(messages)) {
            return(NA_character_)
          }
          asst <- Filter(function(m) m@role == "assistant", messages)
          if (length(asst) > 0L) asst[[length(asst)]]@content else NA_character_
        },
        character(1L)
      )
    )
  }

  abort(
    "Could not extract responses from `x`.\n",
    "Pass a Message, a list of Message objects, or a list of lists of ",
    "Message objects (the output of map() / llmapply() / agentapply() ",
    "with `extract_responses = FALSE`)."
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
#'   `NULL` elements are allowed: [llmapply] and [agentapply] leave one in the slot of every call
#'   that failed under `on_error = "na"`.
#'
#' @return Character vector of reasoning traces, with `NA_character_` in slots where no reasoning
#'   is available (including failed calls).
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
#'     system_prompt = "Return the hexadecimal code for the color provided in format #FFFFFF",
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

  # Flat list of Message objects. `one()` maps a NULL slot -- a call that failed
  # under `on_error = "na"` -- to NA, like any message without a reasoning trace.
  if (is.list(x) && all(vapply(x, .is_null_or_message, logical(1L)))) {
    return(vapply(x, one, character(1L)))
  }

  # List of lists of Message objects: return the first assistant message's reasoning per element
  if (
    is.list(x) &&
      all(vapply(x, .is_null_or_message_list, logical(1L)))
  ) {
    return(
      vapply(
        x,
        function(messages) {
          if (is.null(messages)) {
            return(NA_character_)
          }
          asst <- Filter(function(m) m@role == "assistant", messages)
          if (length(asst) > 0L) one(asst[[length(asst)]]) else NA_character_
        },
        character(1L)
      )
    )
  }

  abort(
    "Could not extract reasoning from `x`.\n",
    "Pass a Message, a list of Message objects, or a list of lists of ",
    "Message objects (the output of map() / llmapply() / agentapply() ",
    "with `extract_responses = FALSE`)."
  )
}


# %% .logprobs_of ----
# Internal: the normalized token-logprob list carried on one Message, or NULL.
# Both backends that support it land the same per-token shape in
# `metadata[["logprobs"]]` (see parse_chat_response.OpenAIConfig), so there is
# no per-provider branch here.
.logprobs_of <- function(m) {
  if (!S7_inherits(m, Message)) {
    return(NULL)
  }
  lp <- m@metadata[["logprobs"]]
  if (length(lp) == 0L) NULL else lp
}


# %% .logprobs_dt ----
# Internal: one message's token-logprob list as a data.table. `top` adds the
# alternatives at each position as a list column of data.tables.
.logprobs_dt <- function(lp, top) {
  token <- vapply(lp, function(e) as.character(e[["token"]]), character(1L))
  logprob <- vapply(lp, function(e) as.numeric(e[["logprob"]]), numeric(1L))
  out <- data.table(
    position = seq_along(lp),
    token = token,
    logprob = logprob,
    prob = exp(logprob)
  )
  if (top) {
    out[["alternatives"]] <- lapply(lp, function(e) {
      alts <- e[["top_logprobs"]]
      if (length(alts) == 0L) {
        return(data.table(
          token = character(0L),
          logprob = numeric(0L),
          prob = numeric(0L)
        ))
      }
      alt_logprob <- vapply(
        alts,
        function(a) as.numeric(a[["logprob"]]),
        numeric(1L)
      )
      data.table(
        token = vapply(
          alts,
          function(a) as.character(a[["token"]]),
          character(1L)
        ),
        logprob = alt_logprob,
        prob = exp(alt_logprob)
      )
    })
  }
  out
}


# %% logprobs ----
#' Extract token log probabilities from a Message or list of Messages
#'
#' Returns the per-token log probabilities of the assistant's response, when the call was made
#' with `logprobs = TRUE`. Reading a probability off the first token is better calibrated than
#' asking a model to emit a number, so this is the basis for scoring a binary question.
#'
#' Supported on Ollama and OpenAI-compatible backends; Anthropic does not return log
#' probabilities, so its messages yield `NULL` rather than an error.
#'
#' @param x `Message` object, list of `Message` objects, or list of lists of `Message` objects.
#' @param top Logical: If `TRUE`, add an `alternatives` list column holding the top-k alternative
#'   tokens considered at each position, each as a data.table of `token`, `logprob` and `prob`.
#'   The alternatives are only present when the call also set `top_logprobs`.
#'
#' @return For a single `Message`, a data.table of `position`, `token`, `logprob` and `prob`, or
#'   `NULL` when the message carries no log probabilities. For a list, a list of those, one per
#'   element, so the length of the result matches the length of `x`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running Ollama server and gemma4:e4b model
#' \dontrun{
#'   llm <- create_Ollama("gemma4:e4b", system_prompt = "Answer Yes or No only.")
#'   msg <- generate(
#'     llm, "Is the sky blue?",
#'     think = FALSE, logprobs = TRUE, top_logprobs = 5L
#'   )
#'   logprobs(msg)
#' }
logprobs <- function(x, top = FALSE) {
  check_logical_scalar(top, "top")
  one <- function(m) {
    lp <- .logprobs_of(m)
    if (is.null(lp)) NULL else .logprobs_dt(lp, top)
  }

  # Single Message object
  if (S7_inherits(x, Message)) {
    return(one(x))
  }

  # Flat list of Message objects
  if (is.list(x) && all(vapply(x, .is_null_or_message, logical(1L)))) {
    return(lapply(x, one))
  }

  # List of lists of Message objects: the last assistant message per element
  if (is.list(x) && all(vapply(x, .is_null_or_message_list, logical(1L)))) {
    return(lapply(x, function(messages) {
      if (is.null(messages)) {
        return(NULL)
      }
      asst <- Filter(function(m) m@role == "assistant", messages)
      if (length(asst) > 0L) one(asst[[length(asst)]]) else NULL
    }))
  }

  abort(
    "Could not extract log probabilities from `x`.\n",
    "Pass a Message, a list of Message objects, or a list of lists of ",
    "Message objects (the output of map() / llmapply() / agentapply() ",
    "with `extract_responses = FALSE`)."
  )
}


# %% .token_prob_row ----
# Internal: probability of each of `tokens` at one position of one message.
# Tokenizers emit leading spaces and vary in case ("Yes", " Yes", "yes"), so
# candidates are matched on trimmed, case-folded text, and every alternative
# that matches is summed -- they are mutually exclusive outcomes, so the total
# is the probability of that answer however it was tokenized.
.token_prob_row <- function(lp, tokens, position) {
  out <- rep(NA_real_, length(tokens))
  names(out) <- tokens
  if (is.null(lp) || position > length(lp)) {
    return(out)
  }
  entry <- lp[[position]]
  alts <- entry[["top_logprobs"]]
  if (length(alts) == 0L) {
    # No alternatives requested: the emitted token is all that is known.
    alts <- list(entry)
  }
  alt_token <- tolower(trimws(vapply(
    alts,
    function(a) as.character(a[["token"]]),
    character(1L)
  )))
  alt_prob <- exp(vapply(
    alts,
    function(a) as.numeric(a[["logprob"]]),
    numeric(1L)
  ))
  wanted <- tolower(trimws(tokens))
  for (i in seq_along(wanted)) {
    hit <- alt_token == wanted[[i]]
    if (any(hit)) {
      out[[i]] <- sum(alt_prob[hit])
    }
  }
  out
}


# %% token_probs ----
#' Probability of specific tokens at one position
#'
#' Reads the probability of each candidate answer directly off the model's token distribution,
#' rather than parsing a value the model emitted. Asking a binary question and taking `P("Yes")`
#' at the first position is far better calibrated than asking the model for a number.
#'
#' Candidates are matched on trimmed, case-folded token text, and the probabilities of every
#' matching alternative are summed, so `"Yes"` picks up `" Yes"` and `"yes"` too. A candidate that
#' does not appear among the returned alternatives is `NA`, not zero: its probability is unknown,
#' only bounded above by the smallest one returned. Raise `top_logprobs` if candidates you care
#' about come back `NA`.
#'
#' @param x `Message` object, list of `Message` objects, or list of lists of `Message` objects.
#' @param tokens Character: Candidate tokens to report probabilities for.
#' @param position Integer \[1, Inf): Token position to read. Position 1 is the first generated
#'   token, which is the answer only when reasoning is off - a thinking model spends its opening
#'   tokens on the reasoning channel.
#'
#' @return For a single `Message`, a named numeric vector, one element per token in `tokens`. For
#'   a list, a numeric matrix with one row per element of `x` and one column per token.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running Ollama server and gemma4:e4b model
#' \dontrun{
#'   llm <- create_Ollama("gemma4:e4b", system_prompt = "Answer Yes or No only.")
#'   msg <- generate(
#'     llm, "Is the sky blue?",
#'     think = FALSE, logprobs = TRUE, top_logprobs = 5L
#'   )
#'   token_probs(msg, c("Yes", "No"))
#' }
token_probs <- function(x, tokens, position = 1L) {
  check_character(tokens, allow_null = FALSE, arg_name = "tokens")
  check_pos_integer_scalar(position, "position")
  position <- as.integer(position)
  one <- function(m) .token_prob_row(.logprobs_of(m), tokens, position)

  # Single Message object
  if (S7_inherits(x, Message)) {
    return(one(x))
  }

  rows <- if (is.list(x) && all(vapply(x, .is_null_or_message, logical(1L)))) {
    lapply(x, one)
  } else if (
    is.list(x) && all(vapply(x, .is_null_or_message_list, logical(1L)))
  ) {
    lapply(x, function(messages) {
      if (is.null(messages)) {
        return(.token_prob_row(NULL, tokens, position))
      }
      asst <- Filter(function(m) m@role == "assistant", messages)
      if (length(asst) > 0L) {
        one(asst[[length(asst)]])
      } else {
        .token_prob_row(NULL, tokens, position)
      }
    })
  } else {
    abort(
      "Could not extract token probabilities from `x`.\n",
      "Pass a Message, a list of Message objects, or a list of lists of ",
      "Message objects (the output of map() / llmapply() / agentapply() ",
      "with `extract_responses = FALSE`)."
    )
  }
  out <- matrix(
    unlist(rows, use.names = FALSE),
    nrow = length(rows),
    ncol = length(tokens),
    byrow = TRUE,
    dimnames = list(names(x), tokens)
  )
  out
}


# %% .is_null_or_message ----
# Internal: TRUE for a Message or for the NULL left behind by a call that failed
# under `on_error = "na"`.
.is_null_or_message <- function(x) {
  is.null(x) || S7_inherits(x, Message)
}


# %% .is_null_or_message_list ----
# Internal: TRUE for a list of Messages (one Agent call's messages) or for the
# NULL left behind by a call that failed under `on_error = "na"`.
.is_null_or_message_list <- function(x) {
  is.null(x) ||
    (is.list(x) && all(vapply(x, S7_inherits, logical(1L), class = Message)))
}


# %% .map_errors ----
# Internal: build the `errors` attribute from the failures collected by
# `.map_iter()`. Always a data.frame, zero-row when nothing failed, so callers
# can read it without a NULL guard.
.map_errors <- function(index, message) {
  data.frame(index = index, message = message, stringsAsFactors = FALSE)
}


# %% .keep_errors ----
# Internal: carry the `errors` attribute across `responses()`, which returns a
# fresh character vector and so drops the attributes of its input.
.keep_errors <- function(out, from) {
  errors <- attr(from, "errors")
  if (!is.null(errors)) {
    attr(out, "errors") <- errors
  }
  out
}


# %% .map_iter ----
# Internal: shared iteration body for map methods. Uses rtemis.core's nested progress API and
# forwards backend-specific per-call args via `...` to `generate()`.
.map_iter <- function(x, f, verbosity, on_error = c("na", "abort"), ...) {
  on_error <- match.arg(on_error)
  label <- repr_bracket(get_model_name(f))
  # `kind` labels the node in the message-sink envelope, so a consumer reading
  # the progress stream can tell an LLM batch apart from any other loop.
  if (on_error == "abort") {
    return(progress_lapply(
      x,
      function(el) {
        generate(f, el, verbosity = verbosity - 1L, ...)
      },
      label = label,
      kind = "llm_map",
      verbosity = verbosity
    ))
  }

  # `on_error = "na"`: every completed call is paid for, so one failure must not
  # discard the rest of the batch. Iterate over indices rather than elements so
  # that a failure can be reported against the position the caller retries by.
  error_index <- integer(0L)
  error_message <- character(0L)
  out <- progress_lapply(
    seq_along(x),
    function(i) {
      tryCatch(
        generate(f, x[[i]], verbosity = verbosity - 1L, ...),
        error = function(e) {
          error_index[[length(error_index) + 1L]] <<- i
          error_message[[length(error_message) + 1L]] <<- conditionMessage(e)
          warn(
            "Element ",
            i,
            " failed: ",
            conditionMessage(e),
            "\nIt is NA in the result; retry the indices in ",
            'attr(result, "errors").',
            use_warning = TRUE
          )
          NULL
        }
      )
    },
    label = label,
    kind = "llm_map",
    verbosity = verbosity
  )
  names(out) <- names(x)
  attr(out, "errors") <- .map_errors(error_index, error_message)
  out
}


# %% map.(class_character, LLM | Agent) ----
method(map, list(class_character, LLM | Agent)) <- function(
  x,
  f,
  verbosity = 1L,
  on_error = c("na", "abort"),
  ...
) {
  .map_iter(x, f, verbosity, on_error = match.arg(on_error), ...)
}


# %% map.(class_list, LLM | Agent) ----
method(map, list(class_list, LLM | Agent)) <- function(
  x,
  f,
  verbosity = 1L,
  on_error = c("na", "abort"),
  ...
) {
  .map_iter(x, f, verbosity, on_error = match.arg(on_error), ...)
}


# %% .check_build_conflict ----
# Internal: abort if both a pre-built object and build-path arguments were supplied.
.check_build_conflict <- function(call, build_args, object_name) {
  passed <- setdiff(names(call)[-1L], "")
  conflicts <- intersect(passed, build_args)
  if (length(conflicts) > 0L) {
    abort(
      "Cannot supply ",
      paste0("`", conflicts, "`", collapse = ", "),
      " when `",
      object_name,
      "` is already a built object.\n",
      "Either pass a model name string together with build-path arguments, ",
      "or pass a pre-built object with no build-path arguments."
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
#' Progress is reported through rtemis.core's nested progress API: one status line labelled with
#' the model name, ticking once per element, with an ETA. Set `verbosity = 0L` to silence it. When
#' a message sink is set (see `rtemis.core::set_msg_sink()`), progress is forwarded as structured
#' events instead of being drawn, and nests under any enclosing progress node.
#'
#' @param x Character or list: Values to iterate over. Each element forms the user prompt for one
#'   call to the LLM.
#' @param model_or_llm Character or LLM: Either the name of a model (a string) or a pre-built
#'   `LLM` object (for example from [create_Ollama], [create_OpenAI], or [create_Anthropic]).
#' @param backend Character \{"ollama", "openai", "anthropic"\}: Backend to use when `model_or_llm`
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
#' @param on_error Character \{"na", "abort"\}: What to do when a single call fails. `"na"` warns,
#'   keeps that element's slot as `NA_character_` (or `NULL` when `extract_responses = FALSE`), and
#'   carries on, so a run of thousands of calls is not lost to one timeout. `"abort"` propagates
#'   the error and discards every result in the batch.
#' @param ... Additional per-call arguments forwarded to [generate] (e.g. `temperature`, `top_p`,
#'   `max_tokens`, `stop`, `think`, `top_k`, `seed`, and for Ollama `num_ctx` and `keep_alive`).
#'
#' @return If `extract_responses = TRUE`, a character vector the same length as `x`. Otherwise, a
#'   list of `Message` objects. Under `on_error = "na"` the result carries an `errors` attribute:
#'   a data.frame of `index` and `message`, one row per failed call, so failures can be retried by
#'   position rather than found by scanning for `NA`.
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
#'     system_prompt = "Return the hexadecimal code for the color provided in format #FFFFFF",
#'     temperature = 0.2
#'   )
#' }
llmapply <- function(
  x,
  model_or_llm,
  backend = c("ollama", "openai", "anthropic"),
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  output_schema = NULL,
  verbosity = 1L,
  extract_responses = TRUE,
  on_error = c("na", "abort"),
  ...
) {
  call <- match.call()
  backend <- match.arg(backend)
  on_error <- match.arg(on_error)

  if (S7_inherits(model_or_llm, Agent)) {
    abort(
      "`model_or_llm` is an Agent, not an LLM.\n",
      "Use agentapply() to run an Agent over a vector of prompts."
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
      anthropic = create_Anthropic(
        model_name = model_or_llm,
        system_prompt = system_prompt,
        output_schema = output_schema
      )
    )
  } else {
    abort(
      "`model_or_llm` must be a single model-name string or a built LLM object.\n",
      "Got <",
      paste(class(model_or_llm), collapse = "/"),
      ">."
    )
  }

  out <- map(x, llm, verbosity = verbosity, on_error = on_error, ...)
  if (extract_responses) .keep_errors(responses(out), out) else out
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
#' Progress is reported through rtemis.core's nested progress API: one status line labelled with
#' the model name, ticking once per element, with an ETA. Set `verbosity = 0L` to silence it. When
#' a message sink is set (see `rtemis.core::set_msg_sink()`), progress is forwarded as structured
#' events instead of being drawn, and nests under any enclosing progress node.
#'
#' @param x Character or list: Values to iterate over.
#' @param model_or_agent Character or Agent: Either the name of a model (a string) or a pre-built
#'   `Agent` object from [create_agent].
#' @param backend Character \{"ollama", "openai", "anthropic"\}: Backend to use when
#'   `model_or_agent` is a string. Ignored when `model_or_agent` is an `Agent` object.
#' @param system_prompt Character: System prompt for the on-the-fly `Agent`.
#' @param tools Optional list of Tool objects: Tools available to the on-the-fly `Agent`.
#' @param use_memory Logical: Whether the on-the-fly `Agent` should keep conversation memory.
#' @param max_tool_rounds Integer \[1, Inf): Maximum number of tool call rounds per query.
#' @param output_schema Optional Schema: Output schema for the on-the-fly `Agent`.
#' @param verbosity Integer \[0, Inf): Verbosity level.
#' @param extract_responses Logical: If `TRUE`, return a character vector of assistant responses.
#'   If `FALSE`, return the raw list of lists of `Message` objects.
#' @param on_error Character \{"na", "abort"\}: What to do when a single call fails. `"na"` warns,
#'   keeps that element's slot as `NA_character_` (or `NULL` when `extract_responses = FALSE`), and
#'   carries on, so a run of thousands of calls is not lost to one timeout. `"abort"` propagates
#'   the error and discards every result in the batch.
#' @param ... Additional per-call arguments forwarded to [generate].
#'
#' @return If `extract_responses = TRUE`, a character vector the same length as `x`. Otherwise, a
#'   list of lists of `Message` objects. Under `on_error = "na"` the result carries an `errors`
#'   attribute: a data.frame of `index` and `message`, one row per failed call, so failures can be
#'   retried by position rather than found by scanning for `NA`.
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
  backend = c("ollama", "openai", "anthropic"),
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  tools = NULL,
  use_memory = FALSE,
  max_tool_rounds = 3L,
  output_schema = NULL,
  verbosity = 1L,
  extract_responses = TRUE,
  on_error = c("na", "abort"),
  ...
) {
  call <- match.call()
  backend <- match.arg(backend)
  on_error <- match.arg(on_error)

  if (S7_inherits(model_or_agent, LLM)) {
    abort(
      "`model_or_agent` is an LLM, not an Agent.\n",
      "Use llmapply() to run an LLM over a vector of prompts."
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
      anthropic = config_Anthropic(model_name = model_or_agent)
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
    abort(
      "`model_or_agent` must be a single model-name string or a built Agent object.\n",
      "Got <",
      paste(class(model_or_agent), collapse = "/"),
      ">."
    )
  }

  out <- map(x, agent, verbosity = verbosity, on_error = on_error, ...)
  if (extract_responses) .keep_errors(responses(out), out) else out
}
