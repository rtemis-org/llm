# %% build_chat_messages.OllamaConfig ----
#' Build Ollama Chat Messages
#'
#' @param x OllamaConfig: Ollama configuration.
#' @param state AgentMemory: Agent memory.
#'
#' @return List of message lists.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_chat_messages, OllamaConfig) <- function(x, state) {
  get_message_list(state)
}


# %% build_chat_messages.OpenAIConfig ----
#' Build OpenAI-compatible Chat Messages
#'
#' @param x OpenAIConfig: OpenAI-compatible configuration.
#' @param state AgentMemory: Agent memory.
#'
#' @return List of message lists.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_chat_messages, OpenAIConfig) <- function(x, state) {
  # `get_messages()` names the list by role for readable inspection, and
  # `lapply()` preserves those names, which `toJSON()` then emits as an object
  # keyed by role instead of the array the API requires -- a 400 on every
  # request. `get_message_list()` unnames for the same reason on the Ollama
  # path; this adapter builds its own list and must do it too.
  unname(lapply(
    get_messages(state),
    function(msg) {
      if (S7_inherits(msg, InputMessage) && !is.null(msg@image_path)) {
        abort(
          "OpenAI-compatible image inputs are not implemented yet.\n",
          "Use a text-only prompt or add a provider-specific image adapter first."
        )
      }
      if (S7_inherits(msg, ToolMessage)) {
        if (is.null(msg@tool_call_id)) {
          abort(
            "OpenAI-compatible tool messages require `tool_call_id`.\n",
            "Use provider adapter tool handling to append tool responses."
          )
        }
        return(list(
          role = msg@role,
          tool_call_id = msg@tool_call_id,
          content = msg@content
        ))
      }
      out <- list(
        role = msg@role,
        content = msg@content
      )
      if (S7_inherits(msg, LLMMessage) && !is.null(msg@tool_calls)) {
        out[["tool_calls"]] <- msg@tool_calls
      }
      out
    }
  ))
}


# %% build_response_format.OllamaConfig ----
#' Build Ollama Response Format
#'
#' @param x OllamaConfig: Ollama configuration.
#' @param output_schema Optional Schema: Output schema.
#'
#' @return Optional list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_response_format, OllamaConfig) <- function(
  x,
  output_schema = NULL
) {
  if (is.null(output_schema)) {
    return(NULL)
  }
  as_list(output_schema)
}


# %% build_response_format.OpenAIConfig ----
#' Build OpenAI-compatible Response Format
#'
#' @param x OpenAIConfig: OpenAI-compatible configuration.
#' @param output_schema Optional Schema: Output schema.
#'
#' @return Optional list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_response_format, OpenAIConfig) <- function(
  x,
  output_schema = NULL
) {
  if (is.null(output_schema)) {
    return(NULL)
  }
  list(
    type = "json_schema",
    json_schema = list(
      name = "rtemis_llm_output",
      strict = TRUE,
      schema = clean_openai_schema(as_list(output_schema))
    )
  )
}


# %% .check_keep_alive() ----
#' Validate the Ollama keep_alive Argument
#'
#' Ollama accepts either a duration string such as `"10m"` or a number of
#' seconds, where a negative value means "keep the model loaded indefinitely".
#' A union of two types with a backend-specific meaning, so it stays here rather
#' than in rtemis.core alongside the general-purpose checks.
#'
#' @param x Object: Value to validate, or `NULL`.
#' @param arg_name Character: Argument name to report in error messages.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_keep_alive <- function(x, arg_name = "keep_alive") {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  valid <- length(x) == 1L &&
    !is.na(x) &&
    (is.character(x) || is.numeric(x))
  if (!valid) {
    abort(
      "`",
      arg_name,
      "` must be a duration string such as \"10m\", or a number of seconds."
    )
  }
  invisible(NULL)
}
# /.check_keep_alive


# %% .check_logprobs_args() ----
#' Validate the logprobs Argument Pair
#'
#' `top_logprobs` asks for alternatives at each position, which a backend only
#' returns alongside the token log probabilities themselves. Sent on its own it
#' fails late and differently per backend: OpenAI rejects the request outright,
#' while Ollama drops the field and returns a response with no logprobs at all,
#' so `logprobs()` and `token_probs()` come back empty with nothing to explain
#' why. Both are reported here instead, before the call is paid for.
#'
#' @param logprobs Object: Value of the `logprobs` argument.
#' @param top_logprobs Object: Value of the `top_logprobs` argument.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_logprobs_args <- function(logprobs, top_logprobs) {
  if (is.null(top_logprobs) || isTRUE(logprobs)) {
    return(invisible(NULL))
  }
  abort(
    "`top_logprobs` requires `logprobs = TRUE`.\n",
    "Alternative tokens are only returned alongside the log probabilities ",
    "of the generated tokens."
  )
}
# /.check_logprobs_args


# %% .check_call_options() ----
#' Validate Per-Call Request Options
#'
#' Every per-call knob a caller can pass to `generate()` reaches the request
#' builders through `...`, so nothing between the user and the wire has checked
#' it: the config classes validate their own defaults via `prop_*()`, but an
#' override supplied at call time bypasses those declarations entirely. Each
#' argument is checked here against the same bounds its config property
#' carries, before a request is built and paid for.
#'
#' Backends that disagree on a bound take it as an argument: `temperature` is
#' capped at 2 by Ollama and OpenAI but at 1 by Anthropic, and `top_logprobs`
#' is capped at 20 by OpenAI but unbounded by Ollama.
#'
#' @param temperature Optional numeric: Sampling temperature.
#' @param top_p Optional numeric \[0, 1\]: Nucleus sampling cutoff.
#' @param max_tokens Optional integer \[1, Inf): Maximum tokens to generate.
#' @param stop Optional character: Stop sequence(s).
#' @param top_k Optional integer \[1, Inf): Top-K sampling cutoff.
#' @param seed Optional integer: Sampling seed.
#' @param num_ctx Optional integer \[1, Inf): Context window size, in tokens.
#' @param keep_alive Optional character or numeric: How long to keep the model loaded.
#' @param logprobs Optional logical: Whether to return token log probabilities.
#' @param top_logprobs Optional integer: How many alternative tokens to return.
#' @param temperature_max Numeric: Backend's upper bound for `temperature`.
#' @param top_logprobs_max Numeric: Backend's upper bound for `top_logprobs`.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_call_options <- function(
  temperature = NULL,
  top_p = NULL,
  max_tokens = NULL,
  stop = NULL,
  top_k = NULL,
  seed = NULL,
  num_ctx = NULL,
  keep_alive = NULL,
  logprobs = NULL,
  top_logprobs = NULL,
  temperature_max = 2,
  top_logprobs_max = Inf
) {
  check_optional_bounded_double_scalar(
    temperature,
    lower = 0,
    upper = temperature_max,
    arg_name = "temperature"
  )
  check_optional_prob_scalar(top_p, arg_name = "top_p")
  check_optional_pos_integer_scalar(max_tokens, arg_name = "max_tokens")
  check_optional_pos_integer_scalar(top_k, arg_name = "top_k")
  check_optional_pos_integer_scalar(num_ctx, arg_name = "num_ctx")
  check_optional_bounded_integer_scalar(
    top_logprobs,
    lower = 0,
    upper = top_logprobs_max,
    arg_name = "top_logprobs"
  )
  check_optional_integer_scalar(seed, arg_name = "seed")
  check_character(stop, arg_name = "stop")
  if (!is.null(stop) && length(stop) == 0L) {
    abort("`stop` must name at least one sequence.")
  }
  .check_keep_alive(keep_alive)
  check_optional_logical_scalar(logprobs, arg_name = "logprobs")
  .check_logprobs_args(logprobs, top_logprobs)
  invisible(NULL)
}
# /.check_call_options


# %% build_chat_request_body.OllamaConfig ----
#' Build Ollama Chat Request Body
#'
#' @param x OllamaConfig: Ollama configuration.
#' @param state AgentMemory: Agent memory.
#' @param tools Optional list: Tools.
#' @param output_schema Optional Schema: Output schema.
#' @param think Optional logical: Whether to enable thinking.
#' @param use_tools Logical: Whether tools are enabled.
#' @param temperature Optional numeric \[0, 2\]: Per-call temperature override.
#' @param top_p Optional numeric \[0, 1\]: Nucleus sampling cutoff.
#' @param max_tokens Optional integer \[1, Inf): Maximum tokens to generate
#' (mapped to Ollama's `options.num_predict`).
#' @param stop Optional character: Stop sequence(s).
#' @param top_k Optional integer \[1, Inf): Top-K sampling cutoff.
#' @param seed Optional integer: Sampling seed for deterministic output.
#' @param num_ctx Optional integer \[1, Inf): Context window size, in tokens
#' (mapped to Ollama's `options.num_ctx`).
#' @param keep_alive Optional character or numeric: How long to keep the model loaded after the
#' request - a duration string such as `"10m"`, or seconds as a number.
#' @param logprobs Optional logical: Whether to return log probabilities for the generated tokens.
#' @param top_logprobs Optional integer \[0, Inf): How many alternative tokens to return log
#' probabilities for at each position. Requires `logprobs = TRUE`.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_chat_request_body, OllamaConfig) <- function(
  x,
  state,
  tools = NULL,
  output_schema = NULL,
  think = NULL,
  use_tools = TRUE,
  temperature = NULL,
  top_p = NULL,
  max_tokens = NULL,
  stop = NULL,
  top_k = NULL,
  seed = NULL,
  num_ctx = NULL,
  keep_alive = NULL,
  logprobs = NULL,
  top_logprobs = NULL
) {
  .check_call_options(
    temperature = temperature,
    top_p = top_p,
    max_tokens = max_tokens,
    stop = stop,
    top_k = top_k,
    seed = seed,
    num_ctx = num_ctx,
    keep_alive = keep_alive,
    logprobs = logprobs,
    top_logprobs = top_logprobs
  )
  effective_think <- think %||% x@think
  .check_ollama_think(effective_think, "think")
  options <- list(
    temperature = temperature %||% x@temperature
  )
  if (!is.null(top_p)) {
    options[["top_p"]] <- top_p
  }
  if (!is.null(top_k)) {
    options[["top_k"]] <- as.integer(top_k)
  }
  if (!is.null(seed)) {
    options[["seed"]] <- as.integer(seed)
  }
  if (!is.null(max_tokens)) {
    options[["num_predict"]] <- as.integer(max_tokens)
  }
  if (!is.null(stop)) {
    options[["stop"]] <- as.character(stop)
  }
  if (!is.null(num_ctx)) {
    options[["num_ctx"]] <- as.integer(num_ctx)
  }
  request_body <- list(
    model = x@model_name,
    messages = build_chat_messages(x, state),
    stream = FALSE,
    options = options
  )
  # `keep_alive` is a top-level request field, not a sampling option.
  if (!is.null(keep_alive)) {
    request_body[["keep_alive"]] <- keep_alive
  }
  # So are `logprobs` and `top_logprobs`. Nested under `options` the Ollama
  # server ignores them silently -- no error, and no `logprobs` in the response.
  if (!is.null(logprobs)) {
    request_body[["logprobs"]] <- logprobs
  }
  if (!is.null(top_logprobs)) {
    request_body[["top_logprobs"]] <- as.integer(top_logprobs)
  }
  if (!is.null(effective_think)) {
    request_body[["think"]] <- effective_think
  }
  if (!is.null(output_schema)) {
    request_body[["format"]] <- build_response_format(x, output_schema)
  }
  if (!is.null(tools) && use_tools) {
    request_body[["tools"]] <- lapply(tools, as_list)
  }
  request_body
}


# %% build_chat_request_body.OpenAIConfig ----
#' Build OpenAI-compatible Chat Request Body
#'
#' @param x OpenAIConfig: OpenAI-compatible configuration.
#' @param state AgentMemory: Agent memory.
#' @param tools Optional list: Tools.
#' @param output_schema Optional Schema: Output schema.
#' @param think Optional logical: Whether to enable thinking.
#' @param use_tools Logical: Whether tools are enabled.
#' @param temperature Optional numeric \[0, 2\]: Per-call temperature override.
#' @param top_p Optional numeric \[0, 1\]: Nucleus sampling cutoff.
#' @param max_tokens Optional integer \[1, Inf): Maximum tokens to generate.
#' @param stop Optional character: Stop sequence(s).
#' @param seed Optional integer: Sampling seed for deterministic output.
#' @param logprobs Optional logical: Whether to return log probabilities for the generated tokens.
#' @param top_logprobs Optional integer \[0, 20\]: How many alternative tokens to return log
#' probabilities for at each position. Requires `logprobs = TRUE`.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_chat_request_body, OpenAIConfig) <- function(
  x,
  state,
  tools = NULL,
  output_schema = NULL,
  think = NULL,
  use_tools = TRUE,
  temperature = NULL,
  top_p = NULL,
  max_tokens = NULL,
  stop = NULL,
  seed = NULL,
  logprobs = NULL,
  top_logprobs = NULL
) {
  .check_call_options(
    temperature = temperature,
    top_p = top_p,
    max_tokens = max_tokens,
    stop = stop,
    seed = seed,
    logprobs = logprobs,
    top_logprobs = top_logprobs,
    top_logprobs_max = 20
  )
  request_body <- list(
    model = x@model_name,
    messages = build_chat_messages(x, state),
    stream = FALSE,
    temperature = temperature %||% x@temperature
  )
  if (!is.null(top_p)) {
    request_body[["top_p"]] <- top_p
  }
  if (!is.null(max_tokens)) {
    request_body[["max_tokens"]] <- as.integer(max_tokens)
  }
  if (!is.null(stop)) {
    request_body[["stop"]] <- as.character(stop)
  }
  if (!is.null(seed)) {
    request_body[["seed"]] <- as.integer(seed)
  }
  if (!is.null(logprobs)) {
    request_body[["logprobs"]] <- logprobs
  }
  if (!is.null(top_logprobs)) {
    request_body[["top_logprobs"]] <- as.integer(top_logprobs)
  }
  if (!is.null(tools) && use_tools) {
    request_body[["tools"]] <- lapply(tools, as_list)
  }
  response_format <- build_response_format(x, output_schema)
  if (!is.null(response_format)) {
    request_body[["response_format"]] <- response_format
  }
  request_body <- add_openai_thinking_options(
    request_body,
    config = x,
    think = think
  )
  if (!is.null(x@extra_body)) {
    request_body[names(x@extra_body)] <- x@extra_body
  }
  if (isTRUE(x@zero_data_retention)) {
    provider <- request_body[["provider"]] %||% list()
    if (!is.list(provider)) {
      abort(
        "`extra_body[[\"provider\"]]` must be a list when ",
        "`zero_data_retention = TRUE`."
      )
    }
    provider[["zdr"]] <- TRUE
    request_body[["provider"]] <- provider
  }
  request_body
}


# %% perform_chat_request.OllamaConfig ----
#' Perform Ollama Chat Request
#'
#' @param x OllamaConfig: Ollama configuration.
#' @param request_body List: Request body.
#' @param verbosity Integer: Verbosity level.
#'
#' @return httr2_response.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(perform_chat_request, OllamaConfig) <- function(
  x,
  request_body,
  verbosity = 1L
) {
  resp <- httr2::request(paste0(x@base_url, "/api/chat")) |>
    httr2::req_body_json(request_body) |>
    httr2::req_user_agent("rtemis (www.rtemis.org)") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(verbosity = max(verbosity - 1L, 0L))
  .check_http_response(resp, "Ollama")
  resp
}


# %% perform_chat_request.OpenAIConfig ----
#' Perform OpenAI-compatible Chat Request
#'
#' @param x OpenAIConfig: OpenAI-compatible configuration.
#' @param request_body List: Request body.
#' @param verbosity Integer: Verbosity level.
#'
#' @return httr2_response.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(perform_chat_request, OpenAIConfig) <- function(
  x,
  request_body,
  verbosity = 1L
) {
  req <- httr2::request(paste0(x@base_url, "/chat/completions")) |>
    httr2::req_body_json(request_body) |>
    httr2::req_user_agent("rtemis.llm-r Agent (www.rtemis.org)") |>
    httr2::req_timeout(x@timeout) |>
    .add_openai_headers(x)
  resp <- httr2::req_perform(req, verbosity = max(verbosity - 1L, 0L))
  .check_http_response(resp, .openai_provider_name(x))
  resp
}


# %% parse_chat_response.OllamaConfig ----
#' Parse Ollama Chat Response
#'
#' @param x OllamaConfig: Ollama configuration.
#' @param resp httr2_response: Response object.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(parse_chat_response, OllamaConfig) <- function(x, resp) {
  res <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  message <- res[["message"]]
  # An empty thinking string means "no reasoning trace", and `reasoning` is
  # optional, so it is carried as NULL -- as the OpenAI path already does.
  reasoning <- message[["thinking"]]
  list(
    content = message[["content"]] %||% "",
    reasoning = if (!is.null(reasoning) && nzchar(reasoning)) reasoning,
    tool_calls = message[["tool_calls"]],
    refusal = NULL,
    metadata = res[setdiff(names(res), "message")]
  )
}


# %% parse_chat_response.OpenAIConfig ----
#' Parse OpenAI-compatible Chat Response
#'
#' @param x OpenAIConfig: OpenAI-compatible configuration.
#' @param resp httr2_response: Response object.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(parse_chat_response, OpenAIConfig) <- function(x, resp) {
  res <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  if (is.null(res[["choices"]][[1]])) {
    abort(
      "OpenAI-compatible response did not include any choices.\n",
      "Check that the server implements the Chat Completions response shape."
    )
  }
  choice <- res[["choices"]][[1]]
  message <- choice[["message"]]
  content <- clean_openai_message_content(message)
  reasoning <- extract_openai_reasoning(message, content = content)
  metadata <- res[setdiff(names(res), "choices")]
  metadata[["finish_reason"]] <- choice[["finish_reason"]]
  metadata[["request_id"]] <- httr2::resp_header(resp, "x-request-id")
  # Ollama returns its token log probabilities as a flat top-level list, which
  # the Ollama parser sweeps into metadata for free; OpenAI nests the same
  # per-token shape under `choices[[1]]$logprobs$content`. Unwrap it here so
  # `metadata[["logprobs"]]` means the same thing on both backends and
  # `logprobs()` needs no per-provider branch. (The sibling `$refusal` trace is
  # not carried: it describes a refusal message, not the response content.)
  token_logprobs <- choice[["logprobs"]][["content"]]
  if (!is.null(token_logprobs)) {
    metadata[["logprobs"]] <- token_logprobs
  }
  list(
    content = content,
    reasoning = reasoning,
    tool_calls = message[["tool_calls"]],
    refusal = message[["refusal"]],
    metadata = metadata
  )
}


# %% decode_tool_arguments.OllamaConfig ----
#' Decode Ollama Tool Arguments
#'
#' @param x OllamaConfig: Ollama configuration.
#' @param tool_call List: Tool call.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(decode_tool_arguments, OllamaConfig) <- function(x, tool_call) {
  args <- tool_call[["function"]][["arguments"]] %||% list()
  if (!is.list(args)) {
    abort("Ollama tool arguments must be a list.")
  }
  args
}


# %% decode_tool_arguments.OpenAIConfig ----
#' Decode OpenAI-compatible Tool Arguments
#'
#' @param x OpenAIConfig: OpenAI-compatible configuration.
#' @param tool_call List: Tool call.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(decode_tool_arguments, OpenAIConfig) <- function(x, tool_call) {
  args <- tool_call[["function"]][["arguments"]] %||% "{}"
  if (is.character(args)) {
    args <- tryCatch(
      jsonlite::fromJSON(args, simplifyVector = FALSE),
      error = function(e) {
        abort(
          "Could not decode arguments for tool '",
          tool_call[["function"]][["name"]],
          "'.\n",
          "Check that the model returned valid JSON function arguments.",
          parent = e
        )
      }
    )
  }
  if (!is.list(args)) {
    abort(
      "Decoded tool arguments must be a named list.\n",
      "Check that the model returned a JSON object for function arguments."
    )
  }
  args
}


# %% build_tool_message.OllamaConfig ----
#' Build Ollama Tool Message
#'
#' @param x OllamaConfig: Ollama configuration.
#' @param tool_call List: Tool call.
#' @param tool_name Character: Tool name.
#' @param tool_response Object: Tool response.
#'
#' @return ToolMessage.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_tool_message, OllamaConfig) <- function(
  x,
  tool_call,
  tool_name,
  tool_response
) {
  ToolMessage(
    name = tool_name,
    content = .tool_response_to_character(tool_response)
  )
}


# %% build_tool_message.OpenAIConfig ----
#' Build OpenAI-compatible Tool Message
#'
#' @param x OpenAIConfig: OpenAI-compatible configuration.
#' @param tool_call List: Tool call.
#' @param tool_name Character: Tool name.
#' @param tool_response Object: Tool response.
#'
#' @return ToolMessage.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_tool_message, OpenAIConfig) <- function(
  x,
  tool_call,
  tool_name,
  tool_response
) {
  tool_call_id <- tool_call[["id"]]
  if (is.null(tool_call_id)) {
    abort(
      "OpenAI-compatible tool calls must include an `id`.\n",
      "Check that the server implements the Chat Completions tool-call response shape."
    )
  }
  ToolMessage(
    name = tool_name,
    tool_call_id = tool_call_id,
    content = .tool_response_to_character(tool_response)
  )
}


# %% build_chat_messages.AnthropicConfig ----
#' Build Anthropic Chat Messages
#'
#' @param x AnthropicConfig: Anthropic configuration.
#' @param state AgentMemory: Agent memory.
#'
#' @return List of Anthropic-shaped message lists.
#'
#' @details
#' System messages are filtered out because Anthropic passes system as a top-level
#' `system` field. Consecutive `ToolMessage`s are merged into a single user
#' message containing multiple `tool_result` content blocks, as required by the
#' Anthropic Messages API.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_chat_messages, AnthropicConfig) <- function(x, state) {
  msgs <- get_messages(state)
  out <- list()
  pending_tool_results <- list()
  flush_tool_results <- function() {
    if (length(pending_tool_results) > 0L) {
      out[[length(out) + 1L]] <<- list(
        role = "user",
        content = pending_tool_results
      )
      pending_tool_results <<- list()
    }
  }
  for (msg in msgs) {
    if (S7_inherits(msg, SystemMessage)) {
      flush_tool_results()
      next
    }
    if (S7_inherits(msg, ToolMessage)) {
      if (is.null(msg@tool_call_id)) {
        abort(
          "Anthropic tool messages require `tool_call_id`.\n",
          "Use provider adapter tool handling to append tool responses."
        )
      }
      pending_tool_results[[length(pending_tool_results) + 1L]] <- list(
        type = "tool_result",
        tool_use_id = msg@tool_call_id,
        content = msg@content
      )
      next
    }
    flush_tool_results()
    if (S7_inherits(msg, LLMMessage)) {
      out[[length(out) + 1L]] <- list(
        role = "assistant",
        content = .anthropic_assistant_blocks_from_message(msg)
      )
      next
    }
    if (S7_inherits(msg, InputMessage)) {
      if (!is.null(msg@image_path)) {
        abort(
          "Anthropic image inputs are not implemented yet.\n",
          "Use a text-only prompt or add a provider-specific image adapter first."
        )
      }
      out[[length(out) + 1L]] <- list(
        role = "user",
        content = list(list(type = "text", text = msg@content))
      )
      next
    }
    if (S7_inherits(msg, AgentMessage)) {
      out[[length(out) + 1L]] <- list(
        role = "user",
        content = list(list(type = "text", text = msg@content))
      )
      next
    }
    # Fallback: treat as user text
    out[[length(out) + 1L]] <- list(
      role = "user",
      content = list(list(type = "text", text = msg@content))
    )
  }
  flush_tool_results()
  out
}


# %% build_response_format.AnthropicConfig ----
#' Build Anthropic Response Format
#'
#' @param x AnthropicConfig: Anthropic configuration.
#' @param output_schema Optional Schema: Output schema.
#'
#' @return Optional named list with `tools` and `tool_choice` fields.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_response_format, AnthropicConfig) <- function(
  x,
  output_schema = NULL
) {
  if (is.null(output_schema)) {
    return(NULL)
  }
  list(
    tools = list(
      .anthropic_structured_output_tool(as_list(output_schema))
    ),
    tool_choice = list(
      type = "tool",
      name = ANTHROPIC_STRUCTURED_OUTPUT_TOOL_NAME
    )
  )
}


# %% build_chat_request_body.AnthropicConfig ----
#' Build Anthropic Chat Request Body
#'
#' @param x AnthropicConfig: Anthropic configuration.
#' @param state AgentMemory: Agent memory.
#' @param tools Optional list: Tools.
#' @param output_schema Optional Schema: Output schema.
#' @param think Optional logical: Whether to enable extended thinking.
#' @param use_tools Logical: Whether tools are enabled.
#' @param temperature Optional numeric \[0, 1\]: Per-call temperature override.
#' @param top_p Optional numeric \[0, 1\]: Nucleus sampling cutoff.
#' @param max_tokens Optional integer \[1, Inf): Per-call max_tokens override.
#' @param stop Optional character: Stop sequence(s) (mapped to `stop_sequences`).
#' @param top_k Optional integer \[1, Inf): Top-K sampling cutoff.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_chat_request_body, AnthropicConfig) <- function(
  x,
  state,
  tools = NULL,
  output_schema = NULL,
  think = NULL,
  use_tools = TRUE,
  temperature = NULL,
  top_p = NULL,
  max_tokens = NULL,
  stop = NULL,
  top_k = NULL
) {
  # Anthropic caps `temperature` at 1, not 2.
  .check_call_options(
    temperature = temperature,
    top_p = top_p,
    max_tokens = max_tokens,
    stop = stop,
    top_k = top_k,
    temperature_max = 1
  )
  request_body <- list(
    model = x@model_name,
    messages = build_chat_messages(x, state),
    max_tokens = as.integer(max_tokens %||% x@max_tokens),
    temperature = temperature %||% x@temperature
  )
  if (!is.null(top_p)) {
    request_body[["top_p"]] <- top_p
  }
  if (!is.null(top_k)) {
    request_body[["top_k"]] <- as.integer(top_k)
  }
  if (!is.null(stop)) {
    request_body[["stop_sequences"]] <- as.character(stop)
  }
  system_prompt <- .anthropic_system_from_state(state)
  if (!is.null(system_prompt) && nzchar(system_prompt)) {
    request_body[["system"]] <- system_prompt
  }
  tool_specs <- list()
  if (!is.null(tools) && use_tools) {
    tool_specs <- lapply(tools, .tool_to_anthropic_schema)
  }
  response_format <- build_response_format(x, output_schema)
  if (!is.null(response_format)) {
    tool_specs <- c(tool_specs, response_format[["tools"]])
    request_body[["tool_choice"]] <- response_format[["tool_choice"]]
  }
  if (length(tool_specs) > 0L) {
    request_body[["tools"]] <- tool_specs
  }
  budget <- resolve_anthropic_thinking_budget(x, think = think)
  if (!is.null(budget)) {
    request_body[["thinking"]] <- list(
      type = "enabled",
      budget_tokens = budget
    )
  }
  if (!is.null(x@extra_body)) {
    request_body[names(x@extra_body)] <- x@extra_body
  }
  request_body
}


# %% perform_chat_request.AnthropicConfig ----
#' Perform Anthropic Chat Request
#'
#' @param x AnthropicConfig: Anthropic configuration.
#' @param request_body List: Request body.
#' @param verbosity Integer: Verbosity level.
#'
#' @return httr2_response.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(perform_chat_request, AnthropicConfig) <- function(
  x,
  request_body,
  verbosity = 1L
) {
  req <- httr2::request(paste0(x@base_url, "/messages")) |>
    httr2::req_body_json(request_body) |>
    httr2::req_user_agent("rtemis.llm-r Agent (www.rtemis.org)") |>
    httr2::req_timeout(x@timeout) |>
    .add_anthropic_headers(x)
  resp <- httr2::req_perform(req, verbosity = max(verbosity - 1L, 0L))
  .check_anthropic_response(resp)
  resp
}


# %% parse_chat_response.AnthropicConfig ----
#' Parse Anthropic Chat Response
#'
#' @param x AnthropicConfig: Anthropic configuration.
#' @param resp httr2_response: Response object.
#'
#' @return Named list with normalized response fields.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(parse_chat_response, AnthropicConfig) <- function(x, resp) {
  res <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  content_blocks <- res[["content"]]
  if (is.null(content_blocks)) {
    content_blocks <- list()
  }
  text_content <- .anthropic_text_from_content(content_blocks)
  reasoning <- .anthropic_reasoning_from_content(content_blocks)
  tool_calls <- .anthropic_tool_calls_from_content(content_blocks)
  stop_reason <- res[["stop_reason"]]
  refusal <- if (identical(stop_reason, "refusal")) {
    text_content
  } else {
    NULL
  }
  metadata <- res[setdiff(names(res), c("content"))]
  metadata[["raw_content"]] <- content_blocks
  metadata[["request_id"]] <- httr2::resp_header(resp, "request-id") %||%
    httr2::resp_header(resp, "x-request-id")
  list(
    content = text_content,
    reasoning = reasoning,
    tool_calls = tool_calls,
    refusal = refusal,
    metadata = metadata
  )
}


# %% decode_tool_arguments.AnthropicConfig ----
#' Decode Anthropic Tool Arguments
#'
#' @param x AnthropicConfig: Anthropic configuration.
#' @param tool_call List: Tool call.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(decode_tool_arguments, AnthropicConfig) <- function(x, tool_call) {
  args <- tool_call[["function"]][["arguments"]]
  if (is.character(args) && length(args) == 1L) {
    args <- tryCatch(
      jsonlite::fromJSON(args, simplifyVector = FALSE),
      error = function(e) {
        abort(
          "Could not decode arguments for tool '",
          tool_call[["function"]][["name"]],
          "'.\n",
          "Check that the model returned valid JSON tool arguments.",
          parent = e
        )
      }
    )
  }
  if (is.null(args)) {
    args <- list()
  }
  if (!is.list(args)) {
    abort(
      "Decoded Anthropic tool arguments must be a named list.\n",
      "Check that the model returned a JSON object for tool input."
    )
  }
  args
}


# %% build_tool_message.AnthropicConfig ----
#' Build Anthropic Tool Message
#'
#' @param x AnthropicConfig: Anthropic configuration.
#' @param tool_call List: Tool call.
#' @param tool_name Character: Tool name.
#' @param tool_response Object: Tool response.
#'
#' @return ToolMessage.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_tool_message, AnthropicConfig) <- function(
  x,
  tool_call,
  tool_name,
  tool_response
) {
  tool_call_id <- tool_call[["id"]]
  if (is.null(tool_call_id)) {
    abort(
      "Anthropic tool calls must include an `id`.\n",
      "Check that the server returned a 'tool_use' content block with an id."
    )
  }
  ToolMessage(
    name = tool_name,
    tool_call_id = tool_call_id,
    content = .tool_response_to_character(tool_response)
  )
}
