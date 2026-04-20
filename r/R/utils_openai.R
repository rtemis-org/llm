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


# %% .is_official_openai_url() ----
#' Test Official OpenAI URL
#'
#' @param x Character: Base URL.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.is_official_openai_url <- function(x) {
  identical(.clean_base_url(x), OPENAI_URL_DEFAULT)
}


# %% .openai_provider_name() ----
#' Get OpenAI-compatible Provider Name
#'
#' @param x OpenAIConfig: Configuration object.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.openai_provider_name <- function(x) {
  if (.is_official_openai_url(x@base_url)) {
    "OpenAI"
  } else {
    "OpenAI-compatible"
  }
}


# %% resolve_api_key() ----
#' Resolve API Key
#'
#' @param config OpenAIConfig: Configuration object.
#' @param error_if_missing Logical: Whether to abort if no key is found for official OpenAI.
#'
#' @return Optional character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_api_key <- function(config, error_if_missing = TRUE) {
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
  if (
    is.null(api_key) &&
      error_if_missing &&
      .is_official_openai_url(config@base_url)
  ) {
    cli::cli_abort(c(
      "No OpenAI API key was found.",
      i = "Set {.envvar {config@api_key_env}}, pass {.var api_key}, or configure {.var keychain_service}.",
      i = "For local OpenAI-compatible servers, set {.var base_url} to the local endpoint."
    ))
  }
  api_key
}


# %% .add_openai_headers() ----
#' Add OpenAI-compatible Headers
#'
#' @param req httr2_request: Request object.
#' @param config OpenAIConfig: Configuration object.
#'
#' @return httr2_request.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.add_openai_headers <- function(req, config) {
  api_key <- resolve_api_key(config)
  if (!is.null(api_key)) {
    req <- httr2::req_auth_bearer_token(req, api_key)
  }
  if (!is.null(config@organization)) {
    req <- httr2::req_headers(req, `OpenAI-Organization` = config@organization)
  }
  if (!is.null(config@project)) {
    req <- httr2::req_headers(req, `OpenAI-Project` = config@project)
  }
  if (!is.null(config@extra_headers)) {
    req <- do.call(httr2::req_headers, c(list(req), config@extra_headers))
  }
  req
}


# %% .check_http_response() ----
#' Check HTTP Response
#'
#' @param resp httr2_response: Response object.
#' @param provider Character: Provider name.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_http_response <- function(resp, provider) {
  if (!httr2::resp_is_error(resp)) {
    return(invisible(NULL))
  }
  status <- httr2::resp_status(resp)
  request_id <- httr2::resp_header(resp, "x-request-id")
  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )
  api_message <- NULL
  err <- body[["error"]]
  if (is.character(err) && length(err) == 1L && nzchar(err)) {
    api_message <- err
  } else if (is.list(err) && !is.null(err[["message"]])) {
    api_message <- err[["message"]]
  } else if (!is.null(body[["message"]])) {
    api_message <- body[["message"]]
  }
  cli::cli_abort(c(
    "{provider} API request failed with HTTP status {status}.",
    if (!is.null(api_message)) ">" = api_message,
    if (!is.null(request_id)) i = "Request id: {.val {request_id}}.",
    i = "Check the model name, base URL, API key, and request options."
  ))
}


# %% clean_openai_schema() ----
#' Clean OpenAI Structured Output Schema
#'
#' @param x List: JSON schema.
#'
#' @return List.
#'
#' @author EDG
#' @keywords internal
#' @noRd
clean_openai_schema <- function(x) {
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
    x[["properties"]] <- lapply(x[["properties"]], clean_openai_schema)
    if (is.null(x[["required"]])) {
      x[["required"]] <- I(names(x[["properties"]]))
    } else {
      x[["required"]] <- I(as.character(x[["required"]]))
    }
    x[["additionalProperties"]] <- FALSE
  } else if (!is.null(x[["items"]]) && is.list(x[["items"]])) {
    x[["items"]] <- clean_openai_schema(x[["items"]])
  }
  x
}


# %% .tool_response_to_character() ----
#' Convert Tool Response To Character
#'
#' @param x Object: Tool response.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.tool_response_to_character <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    return(x)
  }
  jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE)
}


# %% .collapse_openai_text() ----
#' Collapse OpenAI-Compatible Text
#'
#' @param x Object: Text-like response object.
#'
#' @return Optional character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.collapse_openai_text <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.character(x)) {
    out <- paste(x, collapse = "\n")
    return(if (nzchar(out)) out else NULL)
  }
  if (is.list(x)) {
    for (field in c(
      "text",
      "content",
      "summary",
      "reasoning",
      "reasoning_content"
    )) {
      if (!is.null(x[[field]])) {
        value <- .collapse_openai_text(x[[field]])
        if (!is.null(value)) {
          return(value)
        }
      }
    }
    out <- unlist(
      lapply(
        x,
        function(item) {
          if (is.character(item)) {
            return(item)
          }
          if (is.list(item)) {
            for (field in c(
              "text",
              "content",
              "summary",
              "reasoning",
              "reasoning_content"
            )) {
              value <- .collapse_openai_text(item[[field]])
              if (!is.null(value)) {
                return(value)
              }
            }
          }
          NULL
        }
      ),
      use.names = FALSE
    )
    out <- paste(out[nzchar(out)], collapse = "\n")
    return(if (nzchar(out)) out else NULL)
  }
  out <- as.character(x)
  if (nzchar(out)) out else NULL
}


# %% .split_openai_thinking_tags() ----
#' Split OpenAI-Compatible Thinking Tags
#'
#' @param x Optional character: Message content.
#'
#' @return List with content and reasoning fields.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.split_openai_thinking_tags <- function(x) {
  if (is.null(x)) {
    return(list(content = "", reasoning = NULL))
  }
  pattern <- "(?is)<(thinking|think)[^>]*>.*?</\\1>"
  matches <- regmatches(x, gregexpr(pattern, x, perl = TRUE))[[1]]
  if (length(matches) == 1L && identical(matches, "")) {
    return(list(content = x, reasoning = NULL))
  }
  reasoning <- gsub(
    "(?is)^<(think|thinking)[^>]*>|</(think|thinking)>$",
    "",
    matches,
    perl = TRUE
  )
  content <- trimws(gsub(pattern, "", x, perl = TRUE))
  reasoning <- paste(trimws(reasoning), collapse = "\n\n")
  list(
    content = content,
    reasoning = if (nzchar(reasoning)) reasoning else NULL
  )
}


# %% extract_openai_message_content() ----
#' Extract OpenAI-Compatible Message Content
#'
#' @param message List: Assistant message.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
extract_openai_message_content <- function(message) {
  content <- message[["content"]]
  if (is.list(content)) {
    text_parts <- unlist(
      lapply(
        content,
        function(part) {
          if (is.list(part) && identical(part[["type"]], "reasoning")) {
            return(NULL)
          }
          .collapse_openai_text(part)
        }
      ),
      use.names = FALSE
    )
    return(paste(text_parts[nzchar(text_parts)], collapse = "\n"))
  }
  .collapse_openai_text(content) %||% ""
}


# %% extract_openai_reasoning() ----
#' Extract OpenAI-Compatible Reasoning Content
#'
#' @param message List: Assistant message.
#' @param content Character: Message content.
#'
#' @return Optional character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
extract_openai_reasoning <- function(message, content = message[["content"]]) {
  fields <- c(
    "reasoning",
    "reasoning_content",
    "thinking",
    "thinking_content",
    "thought",
    "thoughts"
  )
  reasoning_parts <- unlist(
    lapply(fields, function(field) .collapse_openai_text(message[[field]])),
    use.names = FALSE
  )
  if (!is.null(message[["reasoning_details"]])) {
    reasoning_parts <- c(
      reasoning_parts,
      .collapse_openai_text(message[["reasoning_details"]])
    )
  }
  if (is.list(message[["content"]])) {
    content_reasoning <- unlist(
      lapply(
        message[["content"]],
        function(part) {
          if (is.list(part) && identical(part[["type"]], "reasoning")) {
            return(.collapse_openai_text(part))
          }
          NULL
        }
      ),
      use.names = FALSE
    )
    reasoning_parts <- c(reasoning_parts, content_reasoning)
  }
  tagged <- .split_openai_thinking_tags(extract_openai_message_content(message))
  reasoning_parts <- c(reasoning_parts, tagged[["reasoning"]])
  reasoning_parts <- reasoning_parts[
    !is.na(reasoning_parts) & nzchar(reasoning_parts)
  ]
  if (length(reasoning_parts) == 0L) {
    NULL
  } else {
    paste(unique(trimws(reasoning_parts)), collapse = "\n\n")
  }
}


# %% clean_openai_message_content() ----
#' Clean OpenAI-Compatible Message Content
#'
#' @param message List: Assistant message.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
clean_openai_message_content <- function(message) {
  content <- extract_openai_message_content(message)
  .split_openai_thinking_tags(content)[["content"]]
}


# %% resolve_openai_enable_thinking() ----
#' Resolve OpenAI-Compatible Thinking Flag
#'
#' @param config OpenAIConfig: Configuration object.
#' @param think Optional logical: Generate-call override.
#'
#' @return Optional logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_openai_enable_thinking <- function(config, think = NULL) {
  value <- think %||% config@enable_thinking
  if (is.null(value)) {
    return(NULL)
  }
  if (length(value) != 1L || is.na(value)) {
    cli::cli_abort("{.var enable_thinking} must be a logical scalar.")
  }
  as.logical(value)
}


# %% add_openai_thinking_options() ----
#' Add OpenAI-Compatible Thinking Options
#'
#' @param request_body List: Chat request body.
#' @param config OpenAIConfig: Configuration object.
#' @param think Optional logical: Generate-call override.
#'
#' @return List.
#'
#' @author EDG
#' @keywords internal
#' @noRd
add_openai_thinking_options <- function(
  request_body,
  config,
  think = NULL
) {
  enable_thinking <- resolve_openai_enable_thinking(config, think = think)
  if (is.null(enable_thinking)) {
    return(request_body)
  }
  if (.is_official_openai_url(config@base_url)) {
    cli::cli_abort(c(
      "{.var enable_thinking} is for local OpenAI-compatible servers.",
      i = "Official OpenAI reasoning controls are model-specific; pass supported fields with {.var extra_body}."
    ))
  }
  request_body[["enable_thinking"]] <- enable_thinking
  chat_template_kwargs <- request_body[["chat_template_kwargs"]] %||% list()
  chat_template_kwargs[["enable_thinking"]] <- enable_thinking
  request_body[["chat_template_kwargs"]] <- chat_template_kwargs
  request_body
}


# %% openai_list_models() ----
#' List OpenAI-compatible Models
#'
#' @param base_url Character: Base URL of the OpenAI-compatible server.
#' @param api_key Optional character: API key.
#' @param api_key_env Character: Environment variable containing the API key.
#' @param keychain_service Optional character: macOS Keychain service containing the API key.
#' @param organization Optional character: OpenAI organization id.
#' @param project Optional character: OpenAI project id.
#'
#' @return Character vector: Model ids.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running OpenAI-compatible server with /models endpoint
#' \dontrun{
#'   openai_list_models(
#'     base_url = "http://localhost:1234/v1",
#'     api_key = "test-key"
#'   )
#' }
openai_list_models <- function(
  base_url = OPENAI_URL_DEFAULT,
  api_key = NULL,
  api_key_env = OPENAI_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  organization = NULL,
  project = NULL
) {
  config <- OpenAIConfig(
    model_name = "models",
    temperature = TEMPERATURE_DEFAULT,
    base_url = base_url,
    api_key = api_key,
    api_key_env = api_key_env,
    keychain_service = keychain_service,
    organization = organization,
    project = project,
    validate_model = FALSE
  )
  req <- httr2::request(paste0(config@base_url, "/models")) |>
    httr2::req_method("GET") |>
    httr2::req_user_agent("rtemis (www.rtemis.org)") |>
    .add_openai_headers(config)
  resp <- httr2::req_perform(req)
  .check_http_response(resp, .openai_provider_name(config))
  res <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  if (is.null(res[["data"]])) {
    cli::cli_abort(c(
      "The OpenAI-compatible models endpoint did not return a {.field data} array.",
      i = "Set {.var validate_model = FALSE} for servers that do not implement {.path /models}."
    ))
  }
  sapply(res[["data"]], function(x) x[["id"]])
}


# %% openai_check_model() ----
#' Check OpenAI-compatible Model Is Available
#'
#' @param x Character: Name of model.
#' @param base_url Character: Base URL of the OpenAI-compatible server.
#' @param api_key Optional character: API key.
#' @param api_key_env Character: Environment variable containing the API key.
#' @param keychain_service Optional character: macOS Keychain service containing the API key.
#' @param organization Optional character: OpenAI organization id.
#' @param project Optional character: OpenAI project id.
#'
#' @return NULL, invisibly, if model is available; otherwise throws an error.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running OpenAI-compatible server with /models endpoint
#' \dontrun{
#'   openai_check_model(
#'     x = "local-model",
#'     base_url = "http://localhost:1234/v1",
#'     api_key = "test-key"
#'   )
#' }
openai_check_model <- function(
  x,
  base_url = OPENAI_URL_DEFAULT,
  api_key = NULL,
  api_key_env = OPENAI_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  organization = NULL,
  project = NULL
) {
  models <- openai_list_models(
    base_url = base_url,
    api_key = api_key,
    api_key_env = api_key_env,
    keychain_service = keychain_service,
    organization = organization,
    project = project
  )
  if (x %in% models) {
    invisible(NULL)
  } else {
    cli::cli_abort(c(
      "Model {.val {x}} is not available from the OpenAI-compatible server.",
      i = "Check the model name or set {.var validate_model = FALSE} for servers with incomplete {.path /models} support."
    ))
  }
}
