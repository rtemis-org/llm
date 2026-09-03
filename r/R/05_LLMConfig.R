# Ollama References:
# Chat endpoint: https://docs.ollama.com/api/chat
# Thinking: https://docs.ollama.com/capabilities/thinking#enable-thinking-in-api-calls
# Tool calling: https://docs.ollama.com/capabilities/tool-calling#tool-calling
# OpenAI API: https://developers.openai.com/api/reference/overview
# Anthropic API: https://platform.claude.com/docs/en/api/getting-started

# %% Constants ----
TEMPERATURE_DEFAULT <- 0.3
SYSTEM_PROMPT_DEFAULT <-
  "You are a meticulous research assistant. Your responses are always grounded in facts."
OLLAMA_URL_DEFAULT <- "http://localhost:11434"
OPENAI_URL_DEFAULT <- "https://api.openai.com/v1"
OPENAI_API_KEY_ENV_DEFAULT <- "OPENAI_API_KEY"
OPENAI_TIMEOUT_DEFAULT <- 60
ANTHROPIC_URL_DEFAULT <- "https://api.anthropic.com/v1"
ANTHROPIC_API_KEY_ENV_DEFAULT <- "ANTHROPIC_API_KEY"
ANTHROPIC_API_VERSION_DEFAULT <- "2023-06-01"
ANTHROPIC_MAX_TOKENS_DEFAULT <- 4096L
ANTHROPIC_TIMEOUT_DEFAULT <- 60
ANTHROPIC_THINKING_MIN_BUDGET <- 1024L


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
    model_name = prop_string(description = "Model name"),
    temperature = prop_float(
      min = 0,
      max = 2,
      description = "Sampling temperature"
    ),
    backend = prop_string(description = "Backend name"),
    base_url = prop_string(description = "API base URL")
  ),
  constructor = function(
    model_name,
    temperature,
    backend,
    base_url
  ) {
    # `temperature`'s bounds are carried by its property declaration.
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
  properties = list(
    think = S7::new_property(
      class = S7::new_union(S7::class_logical, S7::class_character, NULL),
      default = NULL
    )
  ),
  constructor = function(
    model_name,
    temperature,
    base_url,
    think = NULL
  ) {
    ollama_check_model(model_name)
    .check_ollama_think(think, "think")
    new_object(
      LLMConfig(
        model_name = model_name,
        temperature = temperature,
        backend = "ollama",
        base_url = base_url
      ),
      think = think
    )
  }
)


# %% as_list.OllamaConfig ----
#' as_list method for OllamaConfig
#'
#' @param x OllamaConfig object.
#'
#' @return List representation of OllamaConfig.
#'
#' @author EDG
#' @noRd
method(as_list, OllamaConfig) <- function(x) {
  list(
    model_name = x@model_name,
    temperature = x@temperature,
    backend = x@backend,
    base_url = x@base_url,
    think = x@think
  )
} # /as_list.OllamaConfig


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
    temperature = prop_float(
      min = 0,
      max = 2,
      description = "Sampling temperature"
    ),
    api_key = prop_string(nullable = TRUE, description = "API key"),
    api_key_env = prop_string(
      description = "Environment variable holding the API key"
    ),
    keychain_service = prop_string(
      nullable = TRUE,
      description = "Keychain service holding the API key"
    ),
    organization = prop_string(nullable = TRUE, description = "Organization"),
    project = prop_string(nullable = TRUE, description = "Project"),
    timeout = prop_float(
      exclusive_min = 0,
      description = "Request timeout (seconds)"
    ),
    extra_headers = prop_bag(description = "Extra HTTP headers"),
    extra_body = prop_bag(description = "Extra request body fields"),
    zero_data_retention = prop_boolean(
      default = NULL,
      nullable = TRUE,
      description = "Require OpenRouter zero-data-retention routing"
    ),
    enable_thinking = prop_boolean(
      nullable = TRUE,
      description = "Enable reasoning"
    ),
    validate_model = prop_boolean(
      default = NULL,
      description = "Check the model name against the endpoint"
    )
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
    zero_data_retention = NULL,
    enable_thinking = NULL,
    validate_model = FALSE
  ) {
    check_character_scalar(model_name, "model_name")
    check_character_scalar(base_url, "base_url")
    if (!is.null(api_key)) {
      check_character_scalar(api_key, "api_key")
    }
    check_character_scalar(api_key_env, "api_key_env")
    if (!is.null(keychain_service)) {
      check_character_scalar(keychain_service, "keychain_service")
    }
    if (!is.null(organization)) {
      check_character_scalar(organization, "organization")
    }
    if (!is.null(project)) {
      check_character_scalar(project, "project")
    }
    if (length(timeout) != 1L || is.na(timeout) || timeout <= 0) {
      abort("`timeout` must be a positive numeric scalar.")
    }
    if (!is.null(extra_headers) && !.is_named_list(extra_headers)) {
      abort("`extra_headers` must be a named list or NULL.")
    }
    if (!is.null(extra_body) && !.is_named_list(extra_body)) {
      abort("`extra_body` must be a named list or NULL.")
    }
    check_optional_logical_scalar(
      zero_data_retention,
      "zero_data_retention"
    )
    if (
      !is.null(enable_thinking) &&
        (length(enable_thinking) != 1L || is.na(enable_thinking))
    ) {
      abort("`enable_thinking` must be a logical scalar or NULL.")
    }
    if (length(validate_model) != 1L || is.na(validate_model)) {
      abort("`validate_model` must be a logical scalar.")
    }
    base_url <- .clean_base_url(base_url)
    if (
      isTRUE(zero_data_retention) &&
        !grepl(
          "^https://(openrouter\\.ai|eu\\.openrouter\\.ai)(:[0-9]+)?(/|$)",
          base_url
        )
    ) {
      abort(
        "`zero_data_retention = TRUE` is only supported for OpenRouter requests.\n",
        "Use an OpenRouter base URL, or configure ZDR in the provider account/workspace."
      )
    }
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
      zero_data_retention = zero_data_retention,
      enable_thinking = enable_thinking,
      validate_model = validate_model
    )
  }
)


# %% AnthropicConfig ----
#' @title AnthropicConfig Class
#'
#' @description
#' Anthropic Messages API configuration class.
#'
#' @author EDG
#' @noRd
AnthropicConfig <- new_class(
  "AnthropicConfig",
  parent = LLMConfig,
  properties = list(
    temperature = prop_float(
      min = 0,
      max = 1,
      description = "Sampling temperature"
    ),
    api_key = prop_string(nullable = TRUE, description = "API key"),
    api_key_env = prop_string(
      description = "Environment variable holding the API key"
    ),
    keychain_service = prop_string(
      nullable = TRUE,
      description = "Keychain service holding the API key"
    ),
    anthropic_version = prop_string(description = "Anthropic API version"),
    anthropic_beta = prop_string(
      nullable = TRUE,
      description = "Anthropic beta feature header"
    ),
    max_tokens = prop_integer(
      min = 1L,
      description = "Maximum tokens to generate"
    ),
    timeout = prop_float(
      exclusive_min = 0,
      description = "Request timeout (seconds)"
    ),
    extra_headers = prop_bag(description = "Extra HTTP headers"),
    extra_body = prop_bag(description = "Extra request body fields"),
    thinking_budget_tokens = prop_integer(
      nullable = TRUE,
      min = 1L,
      description = "Token budget for extended thinking"
    ),
    validate_model = prop_boolean(
      default = NULL,
      description = "Check the model name against the endpoint"
    )
  ),
  constructor = function(
    model_name,
    temperature,
    base_url,
    api_key = NULL,
    api_key_env = ANTHROPIC_API_KEY_ENV_DEFAULT,
    keychain_service = NULL,
    anthropic_version = ANTHROPIC_API_VERSION_DEFAULT,
    anthropic_beta = NULL,
    max_tokens = ANTHROPIC_MAX_TOKENS_DEFAULT,
    timeout = ANTHROPIC_TIMEOUT_DEFAULT,
    extra_headers = NULL,
    extra_body = NULL,
    thinking_budget_tokens = NULL,
    validate_model = FALSE
  ) {
    check_character_scalar(model_name, "model_name")
    check_character_scalar(base_url, "base_url")
    if (!is.null(api_key)) {
      check_character_scalar(api_key, "api_key")
    }
    check_character_scalar(api_key_env, "api_key_env")
    if (!is.null(keychain_service)) {
      check_character_scalar(keychain_service, "keychain_service")
    }
    check_character_scalar(anthropic_version, "anthropic_version")
    if (!is.null(anthropic_beta)) {
      if (
        !is.character(anthropic_beta) ||
          length(anthropic_beta) == 0L ||
          any(is.na(anthropic_beta)) ||
          any(!nzchar(trimws(anthropic_beta)))
      ) {
        abort(
          "`anthropic_beta` must be a non-empty character vector or NULL."
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
      abort("`max_tokens` must be a positive integer-coercible scalar.")
    }
    max_tokens <- as.integer(max_tokens)
    if (length(timeout) != 1L || is.na(timeout) || timeout <= 0) {
      abort("`timeout` must be a positive numeric scalar.")
    }
    if (!is.null(extra_headers) && !.is_named_list(extra_headers)) {
      abort("`extra_headers` must be a named list or NULL.")
    }
    if (!is.null(extra_body) && !.is_named_list(extra_body)) {
      abort("`extra_body` must be a named list or NULL.")
    }
    if (!is.null(thinking_budget_tokens)) {
      if (
        length(thinking_budget_tokens) != 1L ||
          is.na(thinking_budget_tokens) ||
          !is.numeric(thinking_budget_tokens) ||
          thinking_budget_tokens != as.integer(thinking_budget_tokens)
      ) {
        abort(
          "`thinking_budget_tokens` must be a positive integer-coercible scalar or NULL."
        )
      }
      thinking_budget_tokens <- as.integer(thinking_budget_tokens)
      if (thinking_budget_tokens < ANTHROPIC_THINKING_MIN_BUDGET) {
        abort(
          "`thinking_budget_tokens` must be at least ",
          ANTHROPIC_THINKING_MIN_BUDGET,
          ".\n",
          "Extended thinking requires a minimum budget of ",
          ANTHROPIC_THINKING_MIN_BUDGET,
          " tokens."
        )
      }
    }
    if (length(validate_model) != 1L || is.na(validate_model)) {
      abort("`validate_model` must be a logical scalar.")
    }
    base_url <- .clean_base_url(base_url)
    if (validate_model) {
      anthropic_check_model(
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


# %% as_list.AnthropicConfig ----
#' as_list method for AnthropicConfig
#'
#' @param x AnthropicConfig object.
#'
#' @return List representation of AnthropicConfig.
#'
#' @author EDG
#' @noRd
method(as_list, AnthropicConfig) <- function(x) {
  api_key <- resolve_anthropic_api_key(x, error_if_missing = FALSE)
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
} # /as_list.AnthropicConfig


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
    zero_data_retention = x@zero_data_retention,
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
      limit = 20L,
      output_type = output_type
    )
  )
} # /repr.LLMConfig


# %% print.LLMConfig ----
# Print method for LLMConfig ----
method(print, LLMConfig) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /print.LLMConfig
