# References:
# Ollama API: https://docs.ollama.com/
# Ollama tool calling: https://docs.ollama.com/capabilities/tool-calling

# %% Constants ----
TEMPERATURE_DEFAULT <- 0.3
SYSTEM_PROMPT_DEFAULT <-
  "You are a meticulous research assistant. Your responses are always grounded in facts."
OLLAMA_URL_DEFAULT <- "http://localhost:11434"
OPENAI_URL_DEFAULT <- "https://api.openai.com/v1"
OPENAI_API_KEY_ENV_DEFAULT <- "OPENAI_API_KEY"
OPENAI_TIMEOUT_DEFAULT <- 60
CLAUDE_URL_DEFAULT <- "https://api.anthropic.com/v1"
CLAUDE_API_KEY_ENV_DEFAULT <- "ANTHROPIC_API_KEY"
CLAUDE_API_VERSION_DEFAULT <- "2023-06-01"
CLAUDE_MAX_TOKENS_DEFAULT <- 4096L
CLAUDE_TIMEOUT_DEFAULT <- 60
CLAUDE_THINKING_MIN_BUDGET <- 1024L


# --- Internal API ---------------------------------------------------------------------------------
# %% LLMConfig ----
#' @title LLMConfig
#'
#' @description
#' LLM configuration superclass.
#'
#' @field model_name Character: The name of the LLM model to use.
#' @field system_prompt Character: The system prompt to use.
#' @field temperature Numeric: The temperature for the model.
#' @field backend Character: The backend to use.
#'
#' @author EDG
#' @noRd
LLMConfig <- new_class(
  "LLMConfig",
  properties = list(
    model_name = class_character,
    temperature = class_numeric,
    backend = class_character,
    base_url = class_character
  ),
  constructor = function(
    model_name,
    temperature,
    backend,
    base_url
  ) {
    # --- Validate inputs ---
    # Temperature must be numeric between 0.0 and 2.0
    if (temperature < 0.0 || temperature > 2.0) {
      cli::cli_abort("{.var temperature} must be between 0.0 and 2.0.")
    }
    new_object(
      S7_object(),
      model_name = model_name,
      temperature = temperature,
      backend = backend,
      base_url = base_url
    )
  }
)


# %% as_list.LLMConfig ----
#' as_list method for LLMConfig
#'
#' @param x LLMConfig object
#'
#' @return List representation of LLMConfig
#'
#' @author EDG
#' @noRd
method(as_list, LLMConfig) <- function(x) {
  list(
    model_name = x@model_name,
    temperature = x@temperature,
    backend = x@backend,
    base_url = x@base_url
  )
} # /as_list.LLMConfig


# %% OllamaConfig ----
#' @title OllamaConfig Class
#'
#' @description
#' Ollama configuration class.
#'
#' @author EDG
#' @noRd
OllamaConfig <- new_class(
  "OllamaConfig",
  parent = LLMConfig,
  constructor = function(
    model_name,
    temperature,
    base_url
  ) {
    ollama_check_model(model_name)
    new_object(
      LLMConfig(
        model_name = model_name,
        temperature = temperature,
        backend = "ollama",
        base_url = base_url
      )
    )
  }
)


# %% OpenAIConfig ----
#' @title OpenAIConfig Class
#'
#' @description
#' OpenAI-compatible chat API configuration class.
#'
#' @author EDG
#' @noRd
OpenAIConfig <- new_class(
  "OpenAIConfig",
  parent = LLMConfig,
  properties = list(
    api_key = optional(S7::class_character),
    api_key_env = class_character,
    keychain_service = optional(S7::class_character),
    organization = optional(S7::class_character),
    project = optional(S7::class_character),
    timeout = class_numeric,
    extra_headers = optional(S7::class_list),
    extra_body = optional(S7::class_list),
    enable_thinking = optional(S7::class_logical),
    validate_model = class_logical
  ),
  constructor = function(
    model_name,
    temperature,
    base_url,
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
    .check_scalar_character(model_name, "model_name")
    .check_scalar_character(base_url, "base_url")
    if (!is.null(api_key)) {
      .check_scalar_character(api_key, "api_key")
    }
    .check_scalar_character(api_key_env, "api_key_env")
    if (!is.null(keychain_service)) {
      .check_scalar_character(keychain_service, "keychain_service")
    }
    if (!is.null(organization)) {
      .check_scalar_character(organization, "organization")
    }
    if (!is.null(project)) {
      .check_scalar_character(project, "project")
    }
    if (length(timeout) != 1L || is.na(timeout) || timeout <= 0) {
      cli::cli_abort("{.var timeout} must be a positive numeric scalar.")
    }
    if (!is.null(extra_headers) && !.is_named_list(extra_headers)) {
      cli::cli_abort(
        "{.var extra_headers} must be a named list or {.val NULL}."
      )
    }
    if (!is.null(extra_body) && !.is_named_list(extra_body)) {
      cli::cli_abort("{.var extra_body} must be a named list or {.val NULL}.")
    }
    if (
      !is.null(enable_thinking) &&
        (length(enable_thinking) != 1L || is.na(enable_thinking))
    ) {
      cli::cli_abort(
        "{.var enable_thinking} must be a logical scalar or {.val NULL}."
      )
    }
    if (length(validate_model) != 1L || is.na(validate_model)) {
      cli::cli_abort("{.var validate_model} must be a logical scalar.")
    }
    base_url <- .clean_base_url(base_url)
    if (validate_model) {
      openai_check_model(
        x = model_name,
        base_url = base_url,
        api_key = api_key,
        api_key_env = api_key_env,
        keychain_service = keychain_service,
        organization = organization,
        project = project
      )
    }
    new_object(
      LLMConfig(
        model_name = model_name,
        temperature = temperature,
        backend = "openai-compatible",
        base_url = base_url
      ),
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
  }
)


# %% ClaudeConfig ----
#' @title ClaudeConfig Class
#'
#' @description
#' Anthropic Claude Messages API configuration class.
#'
#' @author EDG
#' @noRd
ClaudeConfig <- new_class(
  "ClaudeConfig",
  parent = LLMConfig,
  properties = list(
    api_key = optional(S7::class_character),
    api_key_env = class_character,
    keychain_service = optional(S7::class_character),
    anthropic_version = class_character,
    anthropic_beta = optional(S7::class_character),
    max_tokens = class_integer,
    timeout = class_numeric,
    extra_headers = optional(S7::class_list),
    extra_body = optional(S7::class_list),
    thinking_budget_tokens = optional(S7::class_integer),
    validate_model = class_logical
  ),
  constructor = function(
    model_name,
    temperature,
    base_url,
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
    .check_scalar_character(model_name, "model_name")
    .check_scalar_character(base_url, "base_url")
    if (!is.null(api_key)) {
      .check_scalar_character(api_key, "api_key")
    }
    .check_scalar_character(api_key_env, "api_key_env")
    if (!is.null(keychain_service)) {
      .check_scalar_character(keychain_service, "keychain_service")
    }
    .check_scalar_character(anthropic_version, "anthropic_version")
    if (!is.null(anthropic_beta)) {
      if (
        !is.character(anthropic_beta) ||
          length(anthropic_beta) == 0L ||
          any(is.na(anthropic_beta)) ||
          any(!nzchar(trimws(anthropic_beta)))
      ) {
        cli::cli_abort(
          "{.var anthropic_beta} must be a non-empty character vector or {.val NULL}."
        )
      }
    }
    if (
      length(max_tokens) != 1L ||
        is.na(max_tokens) ||
        !is.numeric(max_tokens) ||
        max_tokens <= 0 ||
        max_tokens != as.integer(max_tokens)
    ) {
      cli::cli_abort(
        "{.var max_tokens} must be a positive integer-coercible scalar."
      )
    }
    max_tokens <- as.integer(max_tokens)
    if (length(timeout) != 1L || is.na(timeout) || timeout <= 0) {
      cli::cli_abort("{.var timeout} must be a positive numeric scalar.")
    }
    if (!is.null(extra_headers) && !.is_named_list(extra_headers)) {
      cli::cli_abort(
        "{.var extra_headers} must be a named list or {.val NULL}."
      )
    }
    if (!is.null(extra_body) && !.is_named_list(extra_body)) {
      cli::cli_abort("{.var extra_body} must be a named list or {.val NULL}.")
    }
    if (!is.null(thinking_budget_tokens)) {
      if (
        length(thinking_budget_tokens) != 1L ||
          is.na(thinking_budget_tokens) ||
          !is.numeric(thinking_budget_tokens) ||
          thinking_budget_tokens != as.integer(thinking_budget_tokens)
      ) {
        cli::cli_abort(
          "{.var thinking_budget_tokens} must be a positive integer-coercible scalar or {.val NULL}."
        )
      }
      thinking_budget_tokens <- as.integer(thinking_budget_tokens)
      if (thinking_budget_tokens < CLAUDE_THINKING_MIN_BUDGET) {
        cli::cli_abort(c(
          "{.var thinking_budget_tokens} must be at least {.val {CLAUDE_THINKING_MIN_BUDGET}}.",
          i = "Extended thinking requires a minimum budget of {CLAUDE_THINKING_MIN_BUDGET} tokens."
        ))
      }
    }
    if (length(validate_model) != 1L || is.na(validate_model)) {
      cli::cli_abort("{.var validate_model} must be a logical scalar.")
    }
    base_url <- .clean_base_url(base_url)
    if (validate_model) {
      claude_check_model(
        x = model_name,
        base_url = base_url,
        api_key = api_key,
        api_key_env = api_key_env,
        keychain_service = keychain_service,
        anthropic_version = anthropic_version
      )
    }
    new_object(
      LLMConfig(
        model_name = model_name,
        temperature = temperature,
        backend = "anthropic",
        base_url = base_url
      ),
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
  }
)


# %% as_list.ClaudeConfig ----
#' as_list method for ClaudeConfig
#'
#' @param x ClaudeConfig object.
#'
#' @return List representation of ClaudeConfig.
#'
#' @author EDG
#' @noRd
method(as_list, ClaudeConfig) <- function(x) {
  api_key <- resolve_claude_api_key(x, error_if_missing = FALSE)
  list(
    model_name = x@model_name,
    temperature = x@temperature,
    backend = x@backend,
    base_url = x@base_url,
    api_key = if (!is.null(api_key)) "<redacted>" else NULL,
    api_key_env = x@api_key_env,
    keychain_service = x@keychain_service,
    anthropic_version = x@anthropic_version,
    anthropic_beta = x@anthropic_beta,
    max_tokens = x@max_tokens,
    timeout = x@timeout,
    extra_headers = x@extra_headers,
    extra_body = x@extra_body,
    thinking_budget_tokens = x@thinking_budget_tokens,
    validate_model = x@validate_model
  )
} # /as_list.ClaudeConfig


# %% as_list.OpenAIConfig ----
#' as_list method for OpenAIConfig
#'
#' @param x OpenAIConfig object.
#'
#' @return List representation of OpenAIConfig.
#'
#' @author EDG
#' @noRd
method(as_list, OpenAIConfig) <- function(x) {
  api_key <- resolve_api_key(x, error_if_missing = FALSE)
  list(
    model_name = x@model_name,
    temperature = x@temperature,
    backend = x@backend,
    base_url = x@base_url,
    api_key = if (!is.null(api_key)) "<redacted>" else NULL,
    api_key_env = x@api_key_env,
    keychain_service = x@keychain_service,
    organization = x@organization,
    project = x@project,
    timeout = x@timeout,
    extra_headers = x@extra_headers,
    extra_body = x@extra_body,
    enable_thinking = x@enable_thinking,
    validate_model = x@validate_model
  )
} # /as_list.OpenAIConfig


# %% repr.LLMConfig ----
# repr method for LLMConfig ----
method(repr, LLMConfig) <- function(x, pad = 0L, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name(
      sub(".*::", "", class(x)[1]),
      pad = pad,
      output_type = output_type
    ),
    repr_ls(
      as_list(x),
      pad = pad,
      print_class = FALSE,
      output_type = output_type,
      limit = 20L
    )
  )
} # /repr.LLMConfig


# %% print.LLMConfig ----
# Print method for LLMConfig ----
method(print, LLMConfig) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /print.LLMConfig
