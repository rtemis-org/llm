# References:
# Messages API: https://docs.anthropic.com/en/api/messages
# Tool use: https://docs.anthropic.com/en/docs/build-with-claude/tool-use
# Extended thinking: https://docs.anthropic.com/en/docs/build-with-claude/extended-thinking

CLAUDE_STRUCTURED_OUTPUT_TOOL_NAME <- "respond_with_structured_output"
CLAUDE_STRUCTURED_OUTPUT_TOOL_DESCRIPTION <-
  "Respond with the structured output requested by the caller."


# %% resolve_claude_api_key() ----
#' Resolve Claude API Key
#'
#' @param config ClaudeConfig: Configuration object.
#' @param error_if_missing Logical: Whether to abort if no key is found.
#'
#' @return Optional character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_claude_api_key <- function(config, error_if_missing = TRUE) {
  api_key <- config@api_key
  if (is.null(api_key) && nzchar(config@api_key_env)) {
    env_key <- Sys.getenv(config@api_key_env, unset = "")
    if (nzchar(env_key)) {
      api_key <- env_key
    }
  }
  if (is.null(api_key) && !is.null(config@keychain_service)) {
    api_key <- get_keychain_secret(service = config@keychain_service)
  }
  if (is.null(api_key) && error_if_missing) {
    cli::cli_abort(c(
      "No Anthropic API key was found.",
      i = "Set {.envvar {config@api_key_env}}, pass {.var api_key}, or configure {.var keychain_service}.",
      i = "Keys can be created at {.url https://console.anthropic.com/}."
    ))
  }
  api_key
}


# %% .add_claude_headers() ----
#' Add Claude Headers
#'
#' @param req httr2_request: Request object.
#' @param config ClaudeConfig: Configuration object.
#'
#' @return httr2_request.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.add_claude_headers <- function(req, config) {
  api_key <- resolve_claude_api_key(config)
  req <- httr2::req_headers(
    req,
    `x-api-key` = api_key,
    `anthropic-version` = config@anthropic_version
  )
  if (!is.null(config@anthropic_beta)) {
    req <- httr2::req_headers(
      req,
      `anthropic-beta` = paste(config@anthropic_beta, collapse = ",")
    )
  }
  if (!is.null(config@extra_headers)) {
    req <- do.call(httr2::req_headers, c(list(req), config@extra_headers))
  }
  req
}


# %% .check_claude_response() ----
#' Check Claude HTTP Response
#'
#' @param resp httr2_response: Response object.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_claude_response <- function(resp) {
  if (!httr2::resp_is_error(resp)) {
    return(invisible(NULL))
  }
  status <- httr2::resp_status(resp)
  request_id <- httr2::resp_header(resp, "request-id") %||%
    httr2::resp_header(resp, "x-request-id")
  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )
  error_type <- body[["error"]][["type"]]
  api_message <- body[["error"]][["message"]] %||% body[["message"]]
  cli::cli_abort(c(
    "Anthropic API request failed with HTTP status {status}.",
    if (!is.null(error_type)) i = "Error type: {.val {error_type}}.",
    if (!is.null(api_message)) ">" = api_message,
    if (!is.null(request_id)) i = "Request id: {.val {request_id}}.",
    i = "Check the model name, API key, and request options."
  ))
}


# %% clean_claude_schema() ----
#' Clean Claude Tool Input Schema
#'
#' @param x List: JSON schema.
#'
#' @return List.
#'
#' @author EDG
#' @keywords internal
#' @noRd
clean_claude_schema <- function(x) {
  if (!is.list(x)) {
    cli::cli_abort("{.var output_schema} must be a JSON Schema list.")
  }
  x <- unclass(x)
  if (identical(x[["type"]], "object")) {
    if (is.null(x[["properties"]]) || !is.list(x[["properties"]])) {
      cli::cli_abort(c(
        "{.var output_schema} object schemas must define {.field properties}.",
        i = "Use {.fun schema} and {.fun field} to create an output schema."
      ))
    }
    x[["properties"]] <- lapply(x[["properties"]], clean_claude_schema)
    required <- if (is.null(x[["required"]])) {
      character()
    } else {
      as.character(x[["required"]])
    }
    x[["required"]] <- I(required)
  } else if (!is.null(x[["items"]]) && is.list(x[["items"]])) {
    x[["items"]] <- clean_claude_schema(x[["items"]])
  }
  x
}


# %% .tool_to_claude_schema() ----
#' Convert Tool To Claude Tool Schema
#'
#' @param tool Tool: Tool object.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.tool_to_claude_schema <- function(tool) {
  if (!S7_inherits(tool, Tool)) {
    cli::cli_abort(".tool_to_claude_schema() expects a {.cls Tool} object.")
  }
  required <- sapply(
    Filter(function(p) p@required, tool@parameters),
    function(p) p@name,
    USE.NAMES = FALSE
  )
  required <- I(as.character(required))
  properties <- structure(
    lapply(
      tool@parameters,
      function(p) {
        list(
          type = p@type,
          description = p@description
        )
      }
    ),
    names = sapply(tool@parameters, function(p) p@name)
  )
  list(
    name = tool@function_name,
    description = tool@description,
    input_schema = list(
      type = "object",
      properties = properties,
      required = required
    )
  )
}


# %% .claude_structured_output_tool() ----
#' Build Claude Structured-Output Tool Spec
#'
#' @param output_schema List: JSON schema for the forced tool input.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.claude_structured_output_tool <- function(output_schema) {
  list(
    name = CLAUDE_STRUCTURED_OUTPUT_TOOL_NAME,
    description = CLAUDE_STRUCTURED_OUTPUT_TOOL_DESCRIPTION,
    input_schema = clean_claude_schema(output_schema)
  )
}


# %% .claude_tool_use_block_to_call() ----
#' Convert Claude tool_use Block To R-Level Tool Call
#'
#' @param block List: Claude `tool_use` content block.
#'
#' @return Named list matching the Agent loop's expected tool-call shape.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.claude_tool_use_block_to_call <- function(block) {
  input <- block[["input"]]
  if (is.null(input)) {
    input <- list()
  }
  list(
    id = block[["id"]],
    type = "function",
    `function` = list(
      name = block[["name"]],
      arguments = input
    )
  )
}


# %% .claude_tool_call_to_use_block() ----
#' Convert R-Level Tool Call To Claude tool_use Block
#'
#' @param tool_call List: Tool call as stored on an `LLMMessage`.
#'
#' @return Named list describing a `tool_use` content block.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.claude_tool_call_to_use_block <- function(tool_call) {
  input <- tool_call[["function"]][["arguments"]]
  if (is.character(input) && length(input) == 1L) {
    input <- tryCatch(
      jsonlite::fromJSON(input, simplifyVector = FALSE),
      error = function(e) list()
    )
  }
  if (is.null(input)) {
    input <- list()
  }
  if (length(input) == 0L) {
    input <- setNames(list(), character(0))
  }
  list(
    type = "tool_use",
    id = tool_call[["id"]],
    name = tool_call[["function"]][["name"]],
    input = input
  )
}


# %% .claude_assistant_blocks_from_message() ----
#' Build Claude Assistant Content Blocks From LLMMessage
#'
#' @param msg LLMMessage: Assistant message.
#'
#' @return List of Claude content blocks.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.claude_assistant_blocks_from_message <- function(msg) {
  raw <- msg@metadata[["raw_content"]]
  if (is.list(raw) && length(raw) > 0L) {
    return(raw)
  }
  blocks <- list()
  if (!is.null(msg@content) && nzchar(msg@content)) {
    blocks[[length(blocks) + 1L]] <- list(
      type = "text",
      text = msg@content
    )
  }
  if (!is.null(msg@tool_calls) && length(msg@tool_calls) > 0L) {
    for (tc in msg@tool_calls) {
      blocks[[length(blocks) + 1L]] <- .claude_tool_call_to_use_block(tc)
    }
  }
  if (length(blocks) == 0L) {
    blocks <- list(list(type = "text", text = ""))
  }
  blocks
}


# %% .claude_text_from_content() ----
#' Extract Visible Text From Claude Content Array
#'
#' @param content List: Claude assistant content array.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.claude_text_from_content <- function(content) {
  if (is.null(content) || length(content) == 0L) {
    return("")
  }
  text_parts <- unlist(
    lapply(
      content,
      function(block) {
        if (identical(block[["type"]], "text")) {
          return(block[["text"]] %||% "")
        }
        NULL
      }
    ),
    use.names = FALSE
  )
  raw <- paste(text_parts[nzchar(text_parts)], collapse = "\n\n")
  .split_openai_thinking_tags(raw)[["content"]]
}


# %% .claude_reasoning_from_content() ----
#' Extract Reasoning From Claude Content Array
#'
#' @param content List: Claude assistant content array.
#'
#' @return Optional character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.claude_reasoning_from_content <- function(content) {
  if (is.null(content) || length(content) == 0L) {
    return(NULL)
  }
  parts <- unlist(
    lapply(
      content,
      function(block) {
        type <- block[["type"]]
        if (identical(type, "thinking")) {
          return(block[["thinking"]] %||% block[["text"]])
        }
        if (identical(type, "redacted_thinking")) {
          return("[redacted thinking]")
        }
        NULL
      }
    ),
    use.names = FALSE
  )
  text_parts <- unlist(
    lapply(
      content,
      function(block) {
        if (identical(block[["type"]], "text")) {
          return(block[["text"]] %||% "")
        }
        NULL
      }
    ),
    use.names = FALSE
  )
  raw_text <- paste(text_parts[nzchar(text_parts)], collapse = "\n\n")
  tagged <- .split_openai_thinking_tags(raw_text)[["reasoning"]]
  parts <- c(parts, tagged)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0L) {
    NULL
  } else {
    paste(parts, collapse = "\n\n")
  }
}


# %% .claude_tool_calls_from_content() ----
#' Extract Tool Calls From Claude Content Array
#'
#' @param content List: Claude assistant content array.
#'
#' @return Optional list of tool-call structures.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.claude_tool_calls_from_content <- function(content) {
  if (is.null(content) || length(content) == 0L) {
    return(NULL)
  }
  calls <- lapply(
    content,
    function(block) {
      if (identical(block[["type"]], "tool_use")) {
        return(.claude_tool_use_block_to_call(block))
      }
      NULL
    }
  )
  calls <- calls[!vapply(calls, is.null, logical(1L))]
  if (length(calls) == 0L) {
    NULL
  } else {
    calls
  }
}


# %% resolve_claude_thinking_budget() ----
#' Resolve Claude Thinking Budget
#'
#' @param config ClaudeConfig: Configuration object.
#' @param think Optional logical: Per-call override.
#'
#' @return Optional integer.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_claude_thinking_budget <- function(config, think = NULL) {
  if (is.null(think)) {
    return(config@thinking_budget_tokens)
  }
  if (length(think) != 1L || is.na(think)) {
    cli::cli_abort("{.var think} must be a logical scalar or {.val NULL}.")
  }
  if (!isTRUE(as.logical(think))) {
    return(NULL)
  }
  if (is.null(config@thinking_budget_tokens)) {
    cli::cli_abort(c(
      "{.var think = TRUE} requires a thinking budget for Claude.",
      i = "Set {.arg thinking_budget_tokens} on {.fun config_Claude} (minimum {.val {CLAUDE_THINKING_MIN_BUDGET}})."
    ))
  }
  config@thinking_budget_tokens
}


# %% .claude_system_from_state() ----
#' Extract Top-Level System Prompt From State
#'
#' @param state AgentMemory: Agent memory.
#'
#' @return Optional character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.claude_system_from_state <- function(state) {
  for (msg in get_messages(state)) {
    if (S7_inherits(msg, SystemMessage)) {
      return(msg@content)
    }
  }
  NULL
}


# %% claude_list_models() ----
#' List Claude (Anthropic) Models
#'
#' @param base_url Character: Base URL of the Anthropic API.
#' @param api_key Optional character: API key.
#' @param api_key_env Character: Environment variable containing the API key.
#' @param keychain_service Optional character: macOS Keychain service containing the API key.
#' @param anthropic_version Character: `anthropic-version` header value.
#'
#' @return Character vector: Model ids.
#'
#' @author EDG
#' @export
claude_list_models <- function(
  base_url = CLAUDE_URL_DEFAULT,
  api_key = NULL,
  api_key_env = CLAUDE_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  anthropic_version = CLAUDE_API_VERSION_DEFAULT
) {
  config <- ClaudeConfig(
    model_name = "models",
    temperature = TEMPERATURE_DEFAULT,
    base_url = base_url,
    api_key = api_key,
    api_key_env = api_key_env,
    keychain_service = keychain_service,
    anthropic_version = anthropic_version,
    validate_model = FALSE
  )
  collected <- character()
  url <- paste0(config@base_url, "/models")
  repeat {
    req <- httr2::request(url) |>
      httr2::req_method("GET") |>
      httr2::req_user_agent("rtemis (www.rtemis.org)") |>
      .add_claude_headers(config)
    resp <- httr2::req_perform(req)
    .check_claude_response(resp)
    res <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    if (is.null(res[["data"]])) {
      cli::cli_abort(c(
        "Anthropic models endpoint did not return a {.field data} array.",
        i = "Set {.var validate_model = FALSE} if the server response shape is non-standard."
      ))
    }
    collected <- c(
      collected,
      vapply(res[["data"]], function(x) x[["id"]], character(1L))
    )
    if (!isTRUE(res[["has_more"]]) || is.null(res[["last_id"]])) {
      break
    }
    url <- paste0(
      config@base_url,
      "/models?after_id=",
      utils::URLencode(res[["last_id"]])
    )
  }
  collected
}


# %% claude_check_model() ----
#' Check Claude Model Is Available
#'
#' @param x Character: Name of the model.
#' @param base_url Character: Base URL of the Anthropic API.
#' @param api_key Optional character: API key.
#' @param api_key_env Character: Environment variable containing the API key.
#' @param keychain_service Optional character: macOS Keychain service containing the API key.
#' @param anthropic_version Character: `anthropic-version` header value.
#'
#' @return NULL, invisibly, if the model is available; otherwise throws an error.
#'
#' @author EDG
#' @export
claude_check_model <- function(
  x,
  base_url = CLAUDE_URL_DEFAULT,
  api_key = NULL,
  api_key_env = CLAUDE_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  anthropic_version = CLAUDE_API_VERSION_DEFAULT
) {
  models <- claude_list_models(
    base_url = base_url,
    api_key = api_key,
    api_key_env = api_key_env,
    keychain_service = keychain_service,
    anthropic_version = anthropic_version
  )
  if (x %in% models) {
    invisible(NULL)
  } else {
    cli::cli_abort(c(
      "Model {.val {x}} is not available from the Anthropic API.",
      i = "Check the model name or set {.var validate_model = FALSE}."
    ))
  }
}
