# %% LLM ----
#' LLM Class
#'
#' A stateless LLM with support for reasoning & structured output.
#'
#' @name LLM
#' @title LLM Class
#' @description
#' Class for interacting with LLMs directly.
#'
#' @field config LLMConfig: The LLM configuration.
#' @field system_prompt Character: The system prompt to use.
#'
#' @author EDG
#' @keywords internal
#' @noRd
LLM <- new_class(
  "LLM",
  properties = list(
    name = optional(S7::class_character),
    system_prompt = class_character
  )
) # rtemis.llm::LLM


# %% repr.LLM ----
# repr method for LLM ----
method(repr, LLM) <- function(x, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name("LLM", output_type = output_type),
    if (!is.null(x@name)) {
      paste(
        fmt("Name: ", bold = TRUE, output_type = output_type),
        highlight(x@name, output_type = output_type),
        "\n"
      )
    },
    fmt("System Prompt: ", bold = TRUE, output_type = output_type),
    highlight(x@system_prompt, output_type = output_type),
    "\n",
  )
} # /repr.LLM


# %% print.LLM ----
# Print method for LLM ----
method(print, LLM) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /print.LLM


# %% Ollama ----
#' @title Ollama Class
#'
#' @description
#' Ollama LLM class.
#'
#' @author EDG
#' @noRd
Ollama <- new_class(
  "Ollama",
  parent = LLM,
  properties = list(
    name = optional(S7::class_character),
    config = OllamaConfig,
    output_schema = optional(Schema)
  ),
  constructor = function(
    name = NULL,
    config,
    system_prompt,
    output_schema = NULL
  ) {
    ollama_check_model(config@model_name)
    new_object(
      LLM(
        system_prompt = system_prompt
      ),
      name = name,
      config = config,
      output_schema = output_schema
    )
  }
)


# %% repr.Ollama ----
method(repr, Ollama) <- function(x, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name("Ollama", output_type = output_type),
    if (!is.null(x@name)) {
      paste(
        fmt("Name: ", bold = TRUE, output_type = output_type),
        highlight(x@name, output_type = output_type),
        "\n"
      )
    },
    fmt("Model: ", bold = TRUE, output_type = output_type),
    highlight(x@config@model_name, output_type = output_type),
    "\n",
    fmt("System Prompt: ", bold = TRUE, output_type = output_type),
    highlight(x@system_prompt, output_type = output_type),
    "\n",
    fmt("Temperature: ", bold = TRUE, output_type = output_type),
    highlight(x@config@temperature, output_type = output_type),
    "\n",
    if (!is.null(x@config@think)) {
      paste0(
        fmt("Thinking: ", bold = TRUE, output_type = output_type),
        highlight(x@config@think, output_type = output_type),
        "\n"
      )
    },
    if (!is.null(x@output_schema)) {
      paste0(
        fmt("Output Schema: \n", bold = TRUE, output_type = output_type),
        repr_ls(
          as_list(x@output_schema),
          pad = 2L,
          output_type = output_type
        )
      )
    }
  )
}


# %% print.Ollama ----
method(print, Ollama) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /print.Ollama


# %% OpenAI ----
#' @title OpenAI Class
#'
#' @description
#' OpenAI-compatible chat API LLM class.
#'
#' @author EDG
#' @noRd
OpenAI <- new_class(
  "OpenAI",
  parent = LLM,
  properties = list(
    name = optional(S7::class_character),
    config = OpenAIConfig,
    output_schema = optional(Schema)
  ),
  constructor = function(
    name = NULL,
    config,
    system_prompt,
    output_schema = NULL
  ) {
    new_object(
      LLM(
        system_prompt = system_prompt
      ),
      name = name,
      config = config,
      output_schema = output_schema
    )
  }
)


# %% repr.OpenAI ----
method(repr, OpenAI) <- function(x, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name("OpenAI", output_type = output_type),
    if (!is.null(x@name)) {
      paste(
        fmt("Name: ", bold = TRUE, output_type = output_type),
        highlight(x@name, output_type = output_type),
        "\n"
      )
    },
    fmt("Model: ", bold = TRUE, output_type = output_type),
    highlight(x@config@model_name, output_type = output_type),
    "\n",
    fmt("Base URL: ", bold = TRUE, output_type = output_type),
    highlight(x@config@base_url, output_type = output_type),
    "\n",
    fmt("System Prompt: ", bold = TRUE, output_type = output_type),
    highlight(x@system_prompt, output_type = output_type),
    "\n",
    fmt("Temperature: ", bold = TRUE, output_type = output_type),
    highlight(x@config@temperature, output_type = output_type),
    "\n",
    if (!is.null(x@output_schema)) {
      paste0(
        fmt("Output Schema: \n", bold = TRUE, output_type = output_type),
        repr_ls(
          as_list(x@output_schema),
          pad = 2L,
          output_type = output_type
        )
      )
    }
  )
}


# %% print.OpenAI ----
method(print, OpenAI) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /print.OpenAI


# %% Claude ----
#' @title Claude Class
#'
#' @description
#' Anthropic Claude LLM class.
#'
#' @author EDG
#' @noRd
Claude <- new_class(
  "Claude",
  parent = LLM,
  properties = list(
    name = optional(S7::class_character),
    config = ClaudeConfig,
    output_schema = optional(Schema)
  ),
  constructor = function(
    name = NULL,
    config,
    system_prompt,
    output_schema = NULL
  ) {
    new_object(
      LLM(
        system_prompt = system_prompt
      ),
      name = name,
      config = config,
      output_schema = output_schema
    )
  }
)


# %% repr.Claude ----
method(repr, Claude) <- function(x, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name("Claude", output_type = output_type),
    if (!is.null(x@name)) {
      paste(
        fmt("Name: ", bold = TRUE, output_type = output_type),
        highlight(x@name, output_type = output_type),
        "\n"
      )
    },
    fmt("Model: ", bold = TRUE, output_type = output_type),
    highlight(x@config@model_name, output_type = output_type),
    "\n",
    fmt("Base URL: ", bold = TRUE, output_type = output_type),
    highlight(x@config@base_url, output_type = output_type),
    "\n",
    fmt("System Prompt: ", bold = TRUE, output_type = output_type),
    highlight(x@system_prompt, output_type = output_type),
    "\n",
    fmt("Temperature: ", bold = TRUE, output_type = output_type),
    highlight(x@config@temperature, output_type = output_type),
    "\n",
    if (!is.null(x@output_schema)) {
      paste0(
        fmt("Output Schema: \n", bold = TRUE, output_type = output_type),
        repr_ls(
          as_list(x@output_schema),
          pad = 2L,
          output_type = output_type
        )
      )
    }
  )
}


# %% print.Claude ----
method(print, Claude) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /print.Claude


# %% generate.Ollama ----
#' Generate method for Ollama
#'
#' @param x Ollama object
#' @param prompt Character: The prompt to send to the model.
#' @param temperature Optional numeric \[0, 2\]: Per-call temperature override.
#' @param top_p Optional numeric \[0, 1\]: Nucleus sampling cutoff.
#' @param max_tokens Optional integer \[1, Inf): Maximum tokens to generate
#' (mapped to `options.num_predict`).
#' @param stop Optional character: Stop sequence(s).
#' @param think Optional logical or character: Whether to enable thinking.
#' @param output_schema Optional Schema: Per-call output schema override.
#' @param verbosity Integer: Verbosity level.
#' @param ... Additional per-call options: `top_k` (integer), `seed` (integer).
#'
#' @return OllamaMessage object
#' @author EDG
#'
#' @noRd
method(generate, Ollama) <- function(
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
  # Check input
  check_inherits(prompt, "character")
  effective_think <- think %||% x@config@think
  .check_ollama_think(effective_think, "think")
  extra <- list(...)
  top_k <- extra[["top_k"]]
  seed <- extra[["seed"]]
  options <- list(
    temperature = temperature %||% x@config@temperature
  )
  if (!is.null(top_p)) options[["top_p"]] <- top_p
  if (!is.null(top_k)) options[["top_k"]] <- as.integer(top_k)
  if (!is.null(seed)) options[["seed"]] <- as.integer(seed)
  if (!is.null(max_tokens)) options[["num_predict"]] <- as.integer(max_tokens)
  if (!is.null(stop)) options[["stop"]] <- as.character(stop)
  # Request
  request_body <- list(
    model = x@config@model_name,
    system = x@system_prompt,
    prompt = prompt,
    stream = FALSE,
    options = options
  )
  if (!is.null(effective_think)) {
    request_body[["think"]] <- effective_think
  }
  effective_schema <- output_schema %||% x@output_schema
  if (!is.null(effective_schema)) {
    request_body[["format"]] <- as_list(effective_schema)
  }
  msg(repr_bracket(x@config@model_name), "working...", verbosity = verbosity)
  # Perform request
  resp <- httr2::request(paste0(x@config@base_url, "/api/generate")) |>
    httr2::req_body_json(request_body) |>
    httr2::req_user_agent("rtemis.llm-r LLM (www.rtemis.org)") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(verbosity = verbosity - 1L)
  # Check for errors
  .check_http_response(resp, "Ollama")

  # Replace working message with done
  msg(repr_bracket(x@config@model_name), "done.", verbosity = verbosity)
  as_OllamaMessage(httr2::resp_body_json(resp))
}


# %% generate.OpenAI ----
#' Generate method for OpenAI-compatible LLMs
#'
#' @param x OpenAI object.
#' @param prompt Character: The prompt to send to the model.
#' @param temperature Optional numeric \[0, 2\]: Per-call temperature override.
#' @param top_p Optional numeric \[0, 1\]: Nucleus sampling cutoff.
#' @param max_tokens Optional integer \[1, Inf): Maximum tokens to generate.
#' @param stop Optional character: Stop sequence(s).
#' @param think Optional logical: Whether to enable thinking options.
#' @param output_schema Optional Schema: Per-call output schema override.
#' @param verbosity Integer: Verbosity level.
#' @param ... Additional per-call options: `seed` (integer).
#'
#' @return OpenAIMessage object.
#' @author EDG
#'
#' @noRd
method(generate, OpenAI) <- function(
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
  check_inherits(prompt, "character")
  extra <- list(...)
  seed <- extra[["seed"]]
  state <- InProcessAgentMemory()
  append_message(
    state,
    SystemMessage(
      name = x@name,
      content = x@system_prompt
    ),
    echo = FALSE,
    verbosity = 0L
  )
  append_message(
    state,
    InputMessage(content = prompt),
    echo = FALSE,
    verbosity = 0L
  )
  request_body <- build_chat_request_body(
    x@config,
    state = state,
    output_schema = output_schema %||% x@output_schema,
    think = think,
    use_tools = FALSE,
    temperature = temperature,
    top_p = top_p,
    max_tokens = max_tokens,
    stop = stop,
    seed = seed
  )
  msg(repr_bracket(x@config@model_name), "working...", verbosity = verbosity)
  resp <- perform_chat_request(
    x@config,
    request_body = request_body,
    verbosity = verbosity
  )
  msg(repr_bracket(x@config@model_name), "done.", verbosity = verbosity)
  res <- parse_chat_response(x@config, resp)
  OpenAIMessage(
    name = x@name,
    content = res[["content"]],
    metadata = res[["metadata"]],
    model_name = x@config@model_name,
    reasoning = res[["reasoning"]],
    tool_calls = res[["tool_calls"]],
    provider = .openai_provider_name(x@config)
  )
}


# %% generate.Claude ----
#' Generate method for Claude
#'
#' @param x Claude object.
#' @param prompt Character: The prompt to send to the model.
#' @param temperature Optional numeric \[0, 2\]: Per-call temperature override.
#' @param top_p Optional numeric \[0, 1\]: Nucleus sampling cutoff.
#' @param max_tokens Optional integer \[1, Inf): Per-call max_tokens override.
#' @param stop Optional character: Stop sequence(s) (mapped to `stop_sequences`).
#' @param think Optional logical: Whether to enable extended thinking for this call.
#' @param output_schema Optional Schema: Per-call output schema override.
#' @param verbosity Integer: Verbosity level.
#' @param ... Additional per-call options: `top_k` (integer).
#'
#' @return ClaudeMessage object.
#' @author EDG
#'
#' @noRd
method(generate, Claude) <- function(
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
  check_inherits(prompt, "character")
  extra <- list(...)
  top_k <- extra[["top_k"]]
  state <- InProcessAgentMemory()
  append_message(
    state,
    SystemMessage(
      name = x@name,
      content = x@system_prompt
    ),
    echo = FALSE,
    verbosity = 0L
  )
  append_message(
    state,
    InputMessage(content = prompt),
    echo = FALSE,
    verbosity = 0L
  )
  request_body <- build_chat_request_body(
    x@config,
    state = state,
    output_schema = output_schema %||% x@output_schema,
    think = think,
    use_tools = FALSE,
    temperature = temperature,
    top_p = top_p,
    max_tokens = max_tokens,
    stop = stop,
    top_k = top_k
  )
  msg(repr_bracket(x@config@model_name), "working...", verbosity = verbosity)
  resp <- perform_chat_request(
    x@config,
    request_body = request_body,
    verbosity = verbosity
  )
  msg(repr_bracket(x@config@model_name), "done.", verbosity = verbosity)
  res <- parse_chat_response(x@config, resp)
  ClaudeMessage(
    name = x@name,
    content = res[["content"]],
    metadata = res[["metadata"]],
    model_name = x@config@model_name,
    reasoning = res[["reasoning"]],
    tool_calls = res[["tool_calls"]]
  )
}


# --- Public API ---------------------------------------------------------------------------------
# %% config_Ollama ----
#' Create an OllamaConfig Object
#'
#' @param model_name Character: The name of the LLM model to use. Must be an Ollama model.
#' @param temperature Numeric: The temperature for the model.
#' @param base_url Character: Base URL of Ollama server.
#' @param think Optional Logical or Character \{"low", "medium", "high"\}: Default thinking mode
#' for this config. Logical values target models like deepseek or qwen3; character values target
#' gpt-oss. Can be overridden per call.
#'
#' @return OllamaConfig object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running Ollama server and gemma4:e4b model
#' \dontrun{
#'   config_Ollama(
#'     model_name = "gemma4:e4b",
#'     temperature = 0.2
#'   )
#' }
config_Ollama <- function(
  model_name,
  temperature = TEMPERATURE_DEFAULT,
  base_url = OLLAMA_URL_DEFAULT,
  think = NULL
) {
  OllamaConfig(
    model_name = model_name,
    temperature = temperature,
    base_url = base_url,
    think = think
  )
} # /config_Ollama


# %% create_Ollama ----
#' Create an Ollama Object
#'
#' @param model_name Character: The name of the LLM model to use. Must be an Ollama model.
#' @param system_prompt Character: The system prompt to use.
#' @param temperature Numeric: The temperature for the model.
#' @param output_schema Optional Schema: An optional output schema created using [schema].
#' @param name Character or NULL: An optional name for the Ollama object.
#' @param base_url Character: Base URL of Ollama server.
#' @param think Optional Logical or Character \{"low", "medium", "high"\}: Default thinking mode.
#' Logical values target models like deepseek or qwen3; character values target gpt-oss.
#'
#' @return Ollama object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running Ollama server and gemma4:e4b model
#' \dontrun{
#'   create_Ollama(
#'     model_name = "gemma4:e4b",
#'     system_prompt = "You are professor of Drum and Bass at the Institute of Advanced Beat Studies.",
#'     temperature = 1.0
#'   )
#' }
create_Ollama <- function(
  model_name,
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  temperature = TEMPERATURE_DEFAULT,
  output_schema = NULL,
  name = NULL,
  base_url = OLLAMA_URL_DEFAULT,
  think = NULL
) {
  ollama_check_model(model_name)
  Ollama(
    name = name,
    config = OllamaConfig(
      model_name = model_name,
      temperature = temperature,
      base_url = base_url,
      think = think
    ),
    system_prompt = system_prompt,
    output_schema = output_schema
  )
}


# %% config_OpenAI ----
#' Create an OpenAI-compatible Config Object
#'
#' @param model_name Character: The name of the LLM model to use.
#' @param temperature Numeric \[0, 2\]: The temperature for the model.
#' @param base_url Character: Base URL of the OpenAI-compatible server.
#' @param api_key Optional character: API key.
#' @param api_key_env Character: Environment variable containing the API key.
#' @param keychain_service Optional character: macOS Keychain service containing the API key.
#' @param organization Optional character: OpenAI organization id.
#' @param project Optional character: OpenAI project id.
#' @param timeout Numeric (0, Inf): Request timeout in seconds.
#' @param extra_headers Optional list: Additional HTTP headers.
#' @param extra_body Optional list: Additional request body fields.
#' @param enable_thinking Optional logical: Whether to enable model thinking for compatible local
#' servers.
#' @param validate_model Logical: Whether to validate model availability using the models endpoint.
#'
#' @return OpenAIConfig object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' cfg <- config_OpenAI(
#'    model_name = "local-model",
#'    temperature = 0.4,
#'    base_url = "http://localhost:1234/v1/",
#'    validate_model = FALSE
#' )
config_OpenAI <- function(
  model_name,
  temperature = TEMPERATURE_DEFAULT,
  base_url = OPENAI_URL_DEFAULT,
  api_key = NULL,
  api_key_env = OPENAI_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  organization = NULL,
  project = NULL,
  timeout = OPENAI_TIMEOUT_DEFAULT,
  extra_headers = NULL,
  extra_body = NULL,
  enable_thinking = NULL,
  validate_model = FALSE
) {
  OpenAIConfig(
    model_name = model_name,
    temperature = temperature,
    base_url = base_url,
    api_key = api_key,
    api_key_env = api_key_env,
    keychain_service = keychain_service,
    organization = organization,
    project = project,
    timeout = timeout,
    extra_headers = extra_headers,
    extra_body = extra_body,
    enable_thinking = enable_thinking,
    validate_model = validate_model
  )
} # /config_OpenAI


# %% create_OpenAI ----
#' Create an OpenAI-compatible LLM Object
#'
#' @param model_name Character: The name of the LLM model to use.
#' @param system_prompt Character: The system prompt to use.
#' @param temperature Numeric \[0, 2\]: The temperature for the model.
#' @param output_schema Optional Schema: Output schema created using [schema].
#' @param name Optional character: Name for the LLM object.
#' @param base_url Character: Base URL of the OpenAI-compatible server.
#' @param api_key Optional character: API key.
#' @param api_key_env Character: Environment variable containing the API key.
#' @param keychain_service Optional character: macOS Keychain service containing the API key.
#' @param organization Optional character: OpenAI organization id.
#' @param project Optional character: OpenAI project id.
#' @param timeout Numeric (0, Inf): Request timeout in seconds.
#' @param extra_headers Optional list: Additional HTTP headers.
#' @param extra_body Optional list: Additional request body fields.
#' @param enable_thinking Optional logical: Whether to enable model thinking for compatible local
#' servers.
#' @param validate_model Logical: Whether to validate model availability using the models endpoint.
#'
#' @return OpenAI object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' llm <- create_OpenAI(
#'    model_name = "local-model",
#'    base_url = "http://localhost:1234/v1",
#'    system_prompt = "You are a meticulous research assistant.",
#'    validate_model = FALSE
#' )
create_OpenAI <- function(
  model_name,
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  temperature = TEMPERATURE_DEFAULT,
  output_schema = NULL,
  name = NULL,
  base_url = OPENAI_URL_DEFAULT,
  api_key = NULL,
  api_key_env = OPENAI_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  organization = NULL,
  project = NULL,
  timeout = OPENAI_TIMEOUT_DEFAULT,
  extra_headers = NULL,
  extra_body = NULL,
  enable_thinking = NULL,
  validate_model = FALSE
) {
  OpenAI(
    name = name,
    config = config_OpenAI(
      model_name = model_name,
      temperature = temperature,
      base_url = base_url,
      api_key = api_key,
      api_key_env = api_key_env,
      keychain_service = keychain_service,
      organization = organization,
      project = project,
      timeout = timeout,
      extra_headers = extra_headers,
      extra_body = extra_body,
      enable_thinking = enable_thinking,
      validate_model = validate_model
    ),
    system_prompt = system_prompt,
    output_schema = output_schema
  )
} # /create_OpenAI


# %% config_Claude ----
#' Create a ClaudeConfig Object
#'
#' @param model_name Character: The name of the Claude model to use (for example
#' `"claude-sonnet-4-6"`).
#' @param temperature Numeric \[0, 2\]: The temperature for the model.
#' @param base_url Character: Base URL of the Anthropic API.
#' @param api_key Optional character: API key.
#' @param api_key_env Character: Environment variable containing the API key.
#' @param keychain_service Optional character: macOS Keychain service containing the API key.
#' @param anthropic_version Character: Value of the required `anthropic-version` header.
#' @param anthropic_beta Optional character: Value(s) for the `anthropic-beta` header. A character
#' vector is comma-joined.
#' @param max_tokens Integer \[1, Inf): Maximum number of tokens the model may generate. Required by
#' the Messages API.
#' @param timeout Numeric (0, Inf): Request timeout in seconds.
#' @param extra_headers Optional list: Additional HTTP headers.
#' @param extra_body Optional list: Additional request body fields.
#' @param thinking_budget_tokens Optional integer \[1024, Inf): Budget for extended thinking. When
#' set, each request enables extended thinking with this budget.
#' @param validate_model Logical: Whether to validate model availability using `/models`.
#'
#' @return ClaudeConfig object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' cfg <- config <- config_Claude(
#'    model_name = "claude-sonnet-4-6",
#'    temperature = 0.4,
#'    api_key = "test-key",
#'    max_tokens = 1024L,
#'    validate_model = FALSE
#' )
config_Claude <- function(
  model_name,
  temperature = TEMPERATURE_DEFAULT,
  base_url = CLAUDE_URL_DEFAULT,
  api_key = NULL,
  api_key_env = CLAUDE_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  anthropic_version = CLAUDE_API_VERSION_DEFAULT,
  anthropic_beta = NULL,
  max_tokens = CLAUDE_MAX_TOKENS_DEFAULT,
  timeout = CLAUDE_TIMEOUT_DEFAULT,
  extra_headers = NULL,
  extra_body = NULL,
  thinking_budget_tokens = NULL,
  validate_model = FALSE
) {
  ClaudeConfig(
    model_name = model_name,
    temperature = temperature,
    base_url = base_url,
    api_key = api_key,
    api_key_env = api_key_env,
    keychain_service = keychain_service,
    anthropic_version = anthropic_version,
    anthropic_beta = anthropic_beta,
    max_tokens = max_tokens,
    timeout = timeout,
    extra_headers = extra_headers,
    extra_body = extra_body,
    thinking_budget_tokens = thinking_budget_tokens,
    validate_model = validate_model
  )
} # /config_Claude


# %% create_Claude ----
#' Create a Claude LLM Object
#'
#' @param model_name Character: The name of the Claude model to use.
#' @param system_prompt Character: The system prompt to use.
#' @param temperature Numeric \[0, 2\]: The temperature for the model.
#' @param output_schema Optional Schema: Output schema created using [schema]. Structured output is
#' implemented by forcing a single synthetic tool call whose `input_schema` is this schema.
#' @param name Optional character: Name for the LLM object.
#' @param base_url Character: Base URL of the Anthropic API.
#' @param api_key Optional character: API key.
#' @param api_key_env Character: Environment variable containing the API key.
#' @param keychain_service Optional character: macOS Keychain service containing the API key.
#' @param anthropic_version Character: Value of the required `anthropic-version` header.
#' @param anthropic_beta Optional character: Value(s) for the `anthropic-beta` header.
#' @param max_tokens Integer \[1, Inf): Maximum number of tokens the model may generate.
#' @param timeout Numeric (0, Inf): Request timeout in seconds.
#' @param extra_headers Optional list: Additional HTTP headers.
#' @param extra_body Optional list: Additional request body fields.
#' @param thinking_budget_tokens Optional integer \[1024, Inf): Extended-thinking budget.
#' @param validate_model Logical: Whether to validate model availability using `/models`.
#'
#' @return Claude object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' llm <- create_Claude(
#'    model_name = "claude-sonnet-4-6",
#'    system_prompt = "You are a meticulous research assistant.",
#'    api_key = "test-key",
#'    validate_model = FALSE
#' )
create_Claude <- function(
  model_name,
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  temperature = TEMPERATURE_DEFAULT,
  output_schema = NULL,
  name = NULL,
  base_url = CLAUDE_URL_DEFAULT,
  api_key = NULL,
  api_key_env = CLAUDE_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  anthropic_version = CLAUDE_API_VERSION_DEFAULT,
  anthropic_beta = NULL,
  max_tokens = CLAUDE_MAX_TOKENS_DEFAULT,
  timeout = CLAUDE_TIMEOUT_DEFAULT,
  extra_headers = NULL,
  extra_body = NULL,
  thinking_budget_tokens = NULL,
  validate_model = FALSE
) {
  Claude(
    name = name,
    config = config_Claude(
      model_name = model_name,
      temperature = temperature,
      base_url = base_url,
      api_key = api_key,
      api_key_env = api_key_env,
      keychain_service = keychain_service,
      anthropic_version = anthropic_version,
      anthropic_beta = anthropic_beta,
      max_tokens = max_tokens,
      timeout = timeout,
      extra_headers = extra_headers,
      extra_body = extra_body,
      thinking_budget_tokens = thinking_budget_tokens,
      validate_model = validate_model
    ),
    system_prompt = system_prompt,
    output_schema = output_schema
  )
} # /create_Claude
