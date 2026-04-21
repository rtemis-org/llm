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
  lapply(
    get_messages(state),
    function(msg) {
      if (S7_inherits(msg, InputMessage) && !is.null(msg@image_path)) {
        cli::cli_abort(c(
          "OpenAI-compatible image inputs are not implemented yet.",
          i = "Use a text-only prompt or add a provider-specific image adapter first."
        ))
      }
      if (S7_inherits(msg, ToolMessage)) {
        if (is.null(msg@tool_call_id)) {
          cli::cli_abort(c(
            "OpenAI-compatible tool messages require {.field tool_call_id}.",
            i = "Use provider adapter tool handling to append tool responses."
          ))
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
  )
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


# %% build_chat_request_body.OllamaConfig ----
#' Build Ollama Chat Request Body
#'
#' @param x OllamaConfig: Ollama configuration.
#' @param state AgentMemory: Agent memory.
#' @param tools Optional list: Tools.
#' @param output_schema Optional Schema: Output schema.
#' @param think Optional logical: Whether to enable thinking.
#' @param use_tools Logical: Whether tools are enabled.
#' @param temperature Optional numeric: Per-call temperature override.
#' @param top_p Optional numeric \[0, 1\]: Nucleus sampling cutoff.
#' @param max_tokens Optional integer \[1, Inf): Maximum tokens to generate
#' (mapped to Ollama's `options.num_predict`).
#' @param stop Optional character: Stop sequence(s).
#' @param top_k Optional integer \[1, Inf): Top-K sampling cutoff.
#' @param seed Optional integer: Sampling seed for deterministic output.
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
  seed = NULL
) {
  effective_think <- think %||% x@think
  .check_ollama_think(effective_think, "think")
  options <- list(
    temperature = temperature %||% x@temperature
  )
  if (!is.null(top_p)) options[["top_p"]] <- top_p
  if (!is.null(top_k)) options[["top_k"]] <- as.integer(top_k)
  if (!is.null(seed)) options[["seed"]] <- as.integer(seed)
  if (!is.null(max_tokens)) options[["num_predict"]] <- as.integer(max_tokens)
  if (!is.null(stop)) options[["stop"]] <- as.character(stop)
  request_body <- list(
    model = x@model_name,
    messages = build_chat_messages(x, state),
    stream = FALSE,
    options = options
  )
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
#' @param temperature Optional numeric: Per-call temperature override.
#' @param top_p Optional numeric \[0, 1\]: Nucleus sampling cutoff.
#' @param max_tokens Optional integer \[1, Inf): Maximum tokens to generate.
#' @param stop Optional character: Stop sequence(s).
#' @param seed Optional integer: Sampling seed for deterministic output.
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
  seed = NULL
) {
  request_body <- list(
    model = x@model_name,
    messages = build_chat_messages(x, state),
    stream = FALSE,
    temperature = temperature %||% x@temperature
  )
  if (!is.null(top_p)) request_body[["top_p"]] <- top_p
  if (!is.null(max_tokens)) {
    request_body[["max_tokens"]] <- as.integer(max_tokens)
  }
  if (!is.null(stop)) request_body[["stop"]] <- as.character(stop)
  if (!is.null(seed)) request_body[["seed"]] <- as.integer(seed)
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
    httr2::req_perform(verbosity = verbosity - 1L)
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
  resp <- httr2::req_perform(req, verbosity = verbosity - 1L)
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
  list(
    content = message[["content"]] %||% "",
    reasoning = message[["thinking"]],
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
    cli::cli_abort(c(
      "OpenAI-compatible response did not include any choices.",
      i = "Check that the server implements the Chat Completions response shape."
    ))
  }
  choice <- res[["choices"]][[1]]
  message <- choice[["message"]]
  content <- clean_openai_message_content(message)
  reasoning <- extract_openai_reasoning(message, content = content)
  metadata <- res[setdiff(names(res), "choices")]
  metadata[["finish_reason"]] <- choice[["finish_reason"]]
  metadata[["request_id"]] <- httr2::resp_header(resp, "x-request-id")
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
    cli::cli_abort("Ollama tool arguments must be a list.")
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
        cli::cli_abort(c(
          "Could not decode arguments for tool {.val {tool_call[[\"function\"]][[\"name\"]]}}.",
          i = "Check that the model returned valid JSON function arguments."
        ))
      }
    )
  }
  if (!is.list(args)) {
    cli::cli_abort(c(
      "Decoded tool arguments must be a named list.",
      i = "Check that the model returned a JSON object for function arguments."
    ))
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
    cli::cli_abort(c(
      "OpenAI-compatible tool calls must include an {.field id}.",
      i = "Check that the server implements the Chat Completions tool-call response shape."
    ))
  }
  ToolMessage(
    name = tool_name,
    tool_call_id = tool_call_id,
    content = .tool_response_to_character(tool_response)
  )
}


# %% build_chat_messages.ClaudeConfig ----
#' Build Claude Chat Messages
#'
#' @param x ClaudeConfig: Claude configuration.
#' @param state AgentMemory: Agent memory.
#'
#' @return List of Claude-shaped message lists.
#'
#' @details
#' System messages are filtered out because Claude passes system as a top-level
#' `system` field. Consecutive `ToolMessage`s are merged into a single user
#' message containing multiple `tool_result` content blocks, as required by the
#' Anthropic Messages API.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_chat_messages, ClaudeConfig) <- function(x, state) {
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
        cli::cli_abort(c(
          "Claude tool messages require {.field tool_call_id}.",
          i = "Use provider adapter tool handling to append tool responses."
        ))
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
        content = .claude_assistant_blocks_from_message(msg)
      )
      next
    }
    if (S7_inherits(msg, InputMessage)) {
      if (!is.null(msg@image_path)) {
        cli::cli_abort(c(
          "Claude image inputs are not implemented yet.",
          i = "Use a text-only prompt or add a provider-specific image adapter first."
        ))
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


# %% build_response_format.ClaudeConfig ----
#' Build Claude Response Format
#'
#' @param x ClaudeConfig: Claude configuration.
#' @param output_schema Optional Schema: Output schema.
#'
#' @return Optional named list with `tools` and `tool_choice` fields.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_response_format, ClaudeConfig) <- function(
  x,
  output_schema = NULL
) {
  if (is.null(output_schema)) {
    return(NULL)
  }
  list(
    tools = list(
      .claude_structured_output_tool(as_list(output_schema))
    ),
    tool_choice = list(
      type = "tool",
      name = CLAUDE_STRUCTURED_OUTPUT_TOOL_NAME
    )
  )
}


# %% build_chat_request_body.ClaudeConfig ----
#' Build Claude Chat Request Body
#'
#' @param x ClaudeConfig: Claude configuration.
#' @param state AgentMemory: Agent memory.
#' @param tools Optional list: Tools.
#' @param output_schema Optional Schema: Output schema.
#' @param think Optional logical: Whether to enable extended thinking.
#' @param use_tools Logical: Whether tools are enabled.
#' @param temperature Optional numeric: Per-call temperature override.
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
method(build_chat_request_body, ClaudeConfig) <- function(
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
  request_body <- list(
    model = x@model_name,
    messages = build_chat_messages(x, state),
    max_tokens = as.integer(max_tokens %||% x@max_tokens),
    temperature = temperature %||% x@temperature
  )
  if (!is.null(top_p)) request_body[["top_p"]] <- top_p
  if (!is.null(top_k)) request_body[["top_k"]] <- as.integer(top_k)
  if (!is.null(stop)) request_body[["stop_sequences"]] <- as.character(stop)
  system_prompt <- .claude_system_from_state(state)
  if (!is.null(system_prompt) && nzchar(system_prompt)) {
    request_body[["system"]] <- system_prompt
  }
  tool_specs <- list()
  if (!is.null(tools) && use_tools) {
    tool_specs <- lapply(tools, .tool_to_claude_schema)
  }
  response_format <- build_response_format(x, output_schema)
  if (!is.null(response_format)) {
    tool_specs <- c(tool_specs, response_format[["tools"]])
    request_body[["tool_choice"]] <- response_format[["tool_choice"]]
  }
  if (length(tool_specs) > 0L) {
    request_body[["tools"]] <- tool_specs
  }
  budget <- resolve_claude_thinking_budget(x, think = think)
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


# %% perform_chat_request.ClaudeConfig ----
#' Perform Claude Chat Request
#'
#' @param x ClaudeConfig: Claude configuration.
#' @param request_body List: Request body.
#' @param verbosity Integer: Verbosity level.
#'
#' @return httr2_response.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(perform_chat_request, ClaudeConfig) <- function(
  x,
  request_body,
  verbosity = 1L
) {
  req <- httr2::request(paste0(x@base_url, "/messages")) |>
    httr2::req_body_json(request_body) |>
    httr2::req_user_agent("rtemis.llm-r Agent (www.rtemis.org)") |>
    httr2::req_timeout(x@timeout) |>
    .add_claude_headers(x)
  resp <- httr2::req_perform(req, verbosity = verbosity - 1L)
  .check_claude_response(resp)
  resp
}


# %% parse_chat_response.ClaudeConfig ----
#' Parse Claude Chat Response
#'
#' @param x ClaudeConfig: Claude configuration.
#' @param resp httr2_response: Response object.
#'
#' @return Named list with normalized response fields.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(parse_chat_response, ClaudeConfig) <- function(x, resp) {
  res <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  content_blocks <- res[["content"]]
  if (is.null(content_blocks)) {
    content_blocks <- list()
  }
  text_content <- .claude_text_from_content(content_blocks)
  reasoning <- .claude_reasoning_from_content(content_blocks)
  tool_calls <- .claude_tool_calls_from_content(content_blocks)
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


# %% decode_tool_arguments.ClaudeConfig ----
#' Decode Claude Tool Arguments
#'
#' @param x ClaudeConfig: Claude configuration.
#' @param tool_call List: Tool call.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(decode_tool_arguments, ClaudeConfig) <- function(x, tool_call) {
  args <- tool_call[["function"]][["arguments"]]
  if (is.character(args) && length(args) == 1L) {
    args <- tryCatch(
      jsonlite::fromJSON(args, simplifyVector = FALSE),
      error = function(e) {
        cli::cli_abort(c(
          "Could not decode arguments for tool {.val {tool_call[[\"function\"]][[\"name\"]]}}.",
          i = "Check that the model returned valid JSON tool arguments."
        ))
      }
    )
  }
  if (is.null(args)) {
    args <- list()
  }
  if (!is.list(args)) {
    cli::cli_abort(c(
      "Decoded Claude tool arguments must be a named list.",
      i = "Check that the model returned a JSON object for tool input."
    ))
  }
  args
}


# %% build_tool_message.ClaudeConfig ----
#' Build Claude Tool Message
#'
#' @param x ClaudeConfig: Claude configuration.
#' @param tool_call List: Tool call.
#' @param tool_name Character: Tool name.
#' @param tool_response Object: Tool response.
#'
#' @return ToolMessage.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(build_tool_message, ClaudeConfig) <- function(
  x,
  tool_call,
  tool_name,
  tool_response
) {
  tool_call_id <- tool_call[["id"]]
  if (is.null(tool_call_id)) {
    cli::cli_abort(c(
      "Claude tool calls must include an {.field id}.",
      i = "Check that the server returned a {.val tool_use} content block with an id."
    ))
  }
  ToolMessage(
    name = tool_name,
    tool_call_id = tool_call_id,
    content = .tool_response_to_character(tool_response)
  )
}
