# 09_DecisionConfig.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# Decision models answer typed questions (a choice among named options, a noul:
# how true a statement is) with a probability per option and write no text.
# Ollama and OpenRouter serve them through the same `/v1/systemone` contract
# (spec: llm/decision-models#classes-s7).

# %% Constants ----
OPENROUTER_URL_DEFAULT <- "https://openrouter.ai/api"
OPENROUTER_API_KEY_ENV_DEFAULT <- "OPENROUTER_API_KEY"
# A cold local load is about 10 s, and every call carries the state's prefill.
OLLAMA_DECISION_TIMEOUT_DEFAULT <- 300
OPENROUTER_DECISION_TIMEOUT_DEFAULT <- 60
# The smallest provider's limits (Clef): 2 to 26 options per choice and 64
# questions per call.
DECISION_MAX_OPTIONS <- 26L
DECISION_MAX_QUESTIONS <- 64L


# %% DecisionConfig ----
#' @title DecisionConfig
#'
#' @description
#' Decision model configuration superclass: one `/v1/systemone` endpoint.
#'
#' @field model_name Character: The decision model's name on the provider.
#' @field backend Character: The provider.
#' @field base_url Character: The provider's base URL; requests go to
#'   `<base_url>/v1/systemone`.
#' @field timeout Numeric: Request timeout in seconds.
#'
#' @author EDG
#' @keywords internal
#' @noRd
DecisionConfig <- new_class(
  "DecisionConfig",
  properties = list(
    model_name = prop_string(description = "Decision model name"),
    backend = prop_string(description = "Backend name"),
    base_url = prop_string(description = "API base URL"),
    timeout = prop_float(
      exclusive_min = 0,
      description = "Request timeout (seconds)"
    )
  ),
  constructor = function(model_name, backend, base_url, timeout) {
    new_object(
      S7_object(),
      model_name = model_name,
      backend = backend,
      base_url = base_url,
      timeout = timeout
    )
  }
)


# %% OllamaDecisionConfig ----
#' @title OllamaDecisionConfig Class
#'
#' @description
#' Configuration for a decision model served by Ollama.
#'
#' @author EDG
#' @keywords internal
#' @noRd
OllamaDecisionConfig <- new_class(
  "OllamaDecisionConfig",
  parent = DecisionConfig,
  properties = list(
    validate_model = prop_boolean(
      default = NULL,
      description = "Check that the server has the model and that it is a decision model"
    )
  ),
  constructor = function(
    model_name,
    base_url = OLLAMA_URL_DEFAULT,
    timeout = OLLAMA_DECISION_TIMEOUT_DEFAULT,
    validate_model = TRUE
  ) {
    check_character_scalar(model_name, "model_name")
    base_url <- .clean_base_url(base_url)
    .check_timeout(timeout)
    check_logical_scalar(validate_model, "validate_model")
    if (validate_model) {
      .ollama_check_decision_model(model_name, base_url = base_url)
    }
    new_object(
      DecisionConfig(
        model_name = model_name,
        backend = "ollama",
        base_url = base_url,
        timeout = as.numeric(timeout)
      ),
      validate_model = validate_model
    )
  }
)


# %% OpenRouterDecisionConfig ----
#' @title OpenRouterDecisionConfig Class
#'
#' @description
#' Configuration for a decision model served by OpenRouter.
#'
#' @author EDG
#' @keywords internal
#' @noRd
OpenRouterDecisionConfig <- new_class(
  "OpenRouterDecisionConfig",
  parent = DecisionConfig,
  properties = list(
    api_key = prop_string(nullable = TRUE, description = "API key"),
    api_key_env = prop_string(
      description = "Environment variable holding the API key"
    ),
    keychain_service = prop_string(
      nullable = TRUE,
      description = "Keychain service holding the API key"
    ),
    extra_headers = prop_bag(description = "Extra HTTP headers")
  ),
  constructor = function(
    model_name,
    api_key = NULL,
    api_key_env = OPENROUTER_API_KEY_ENV_DEFAULT,
    keychain_service = NULL,
    base_url = OPENROUTER_URL_DEFAULT,
    timeout = OPENROUTER_DECISION_TIMEOUT_DEFAULT,
    extra_headers = NULL
  ) {
    check_character_scalar(model_name, "model_name")
    if (!is.null(api_key)) {
      check_character_scalar(api_key, "api_key")
    }
    check_character_scalar(api_key_env, "api_key_env")
    if (!is.null(keychain_service)) {
      check_character_scalar(keychain_service, "keychain_service")
    }
    base_url <- .clean_base_url(base_url)
    .check_timeout(timeout)
    if (!is.null(extra_headers) && !.is_named_list(extra_headers)) {
      abort("`extra_headers` must be a named list or NULL.")
    }
    new_object(
      DecisionConfig(
        model_name = model_name,
        backend = "openrouter",
        base_url = base_url,
        timeout = as.numeric(timeout)
      ),
      api_key = api_key,
      api_key_env = api_key_env,
      keychain_service = keychain_service,
      extra_headers = extra_headers
    )
  }
)


# %% .check_timeout() ----
#' Check a request timeout
#'
#' @param x Numeric: Timeout in seconds.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_timeout <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0) {
    abort("`timeout` must be a positive number of seconds.")
  }
  invisible(NULL)
}


# %% as_list.OllamaDecisionConfig ----
method(as_list, OllamaDecisionConfig) <- function(x) {
  list(
    model_name = x@model_name,
    backend = x@backend,
    base_url = x@base_url,
    timeout = x@timeout,
    validate_model = x@validate_model
  )
} # /as_list.OllamaDecisionConfig


# %% as_list.OpenRouterDecisionConfig ----
method(as_list, OpenRouterDecisionConfig) <- function(x) {
  api_key <- .resolve_openrouter_key(x, error_if_missing = FALSE)
  list(
    model_name = x@model_name,
    backend = x@backend,
    base_url = x@base_url,
    timeout = x@timeout,
    api_key = if (!is.null(api_key)) "<redacted>" else NULL,
    api_key_env = x@api_key_env,
    keychain_service = x@keychain_service,
    extra_headers = x@extra_headers
  )
} # /as_list.OpenRouterDecisionConfig


# %% repr.DecisionConfig ----
method(repr, DecisionConfig) <- function(x, pad = 0L, output_type = NULL) {
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
} # /repr.DecisionConfig


# %% print.DecisionConfig ----
method(print, DecisionConfig) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /print.DecisionConfig


# %% .decision_provider_name ----
#' The provider's name, for messages and metadata
#'
#' @param x DecisionConfig: Configuration.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.decision_provider_name <- new_generic(".decision_provider_name", "x")
method(.decision_provider_name, OllamaDecisionConfig) <- function(x) "Ollama"
method(.decision_provider_name, OpenRouterDecisionConfig) <- function(x) {
  "OpenRouter"
}


# %% .decision_auth ----
#' Add what a provider needs to authenticate a request
#'
#' @param x DecisionConfig: Configuration.
#' @param req httr2_request: Request.
#'
#' @return httr2_request.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.decision_auth <- new_generic(".decision_auth", "x", function(x, req) {
  S7_dispatch()
})
# A local server needs no key, and none found in the environment is sent to it.
method(.decision_auth, OllamaDecisionConfig) <- function(x, req) req
method(.decision_auth, OpenRouterDecisionConfig) <- function(x, req) {
  req <- httr2::req_auth_bearer_token(req, .resolve_openrouter_key(x))
  if (!is.null(x@extra_headers)) {
    req <- do.call(httr2::req_headers, c(list(req), x@extra_headers))
  }
  req
}


# %% .resolve_openrouter_key() ----
#' Resolve the OpenRouter API key
#'
#' @param config OpenRouterDecisionConfig: Configuration.
#' @param error_if_missing Logical: Whether to abort when no key is found.
#'
#' @return Optional character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.resolve_openrouter_key <- function(config, error_if_missing = TRUE) {
  api_key <- .resolve_key_sources(
    api_key = config@api_key,
    api_key_env = config@api_key_env,
    keychain_service = config@keychain_service,
    default_env = OPENROUTER_API_KEY_ENV_DEFAULT,
    provider = "OpenRouter"
  )
  if (is.null(api_key) && error_if_missing) {
    abort(
      "No OpenRouter API key was found.\n",
      "Set ",
      config@api_key_env,
      ", pass `api_key`, or configure `keychain_service`."
    )
  }
  api_key
}


# %% .ollama_capabilities() ----
#' Read a model's capabilities from Ollama
#'
#' `/api/tags` lists models without capabilities; `/api/show` reports them.
#'
#' @param model_name Character: Model name.
#' @param base_url Character: Base URL of the Ollama server.
#'
#' @return Character vector of capabilities, or NULL when the server does not
#'   have the model.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.ollama_capabilities <- function(model_name, base_url = OLLAMA_URL_DEFAULT) {
  resp <- tryCatch(
    httr2::request(paste0(base_url, "/api/show")) |>
      httr2::req_body_json(list(model = model_name)) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) {
      abort(
        "Could not reach the Ollama server at ",
        base_url,
        ".\n",
        "Start Ollama, or set `base_url` to where it runs."
      )
    }
  )
  if (httr2::resp_status(resp) == 404L) {
    return(NULL)
  }
  .check_http_response(resp, "Ollama")
  unlist(
    httr2::resp_body_json(resp, simplifyVector = FALSE)[["capabilities"]],
    use.names = FALSE
  ) %||%
    character()
}


# %% .ollama_check_decision_model() ----
#' Check that Ollama has a model and that it is a decision model
#'
#' @param model_name Character: Model name.
#' @param base_url Character: Base URL of the Ollama server.
#'
#' @return NULL, invisibly; aborts otherwise.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.ollama_check_decision_model <- function(
  model_name,
  base_url = OLLAMA_URL_DEFAULT
) {
  capabilities <- .ollama_capabilities(model_name, base_url = base_url)
  if (is.null(capabilities)) {
    abort(
      "Decision model '",
      model_name,
      "' is not available on the Ollama server at ",
      base_url,
      ".\n",
      "Pull it (for example `ollama pull clef-flash`), or list the decision ",
      "models the server has with ollama_list_decision_models()."
    )
  }
  if (!"decision" %in% capabilities) {
    abort(
      "'",
      model_name,
      "' is a language model, not a decision model.\n",
      "Use create_Ollama() for it, or choose a decision model from ",
      "ollama_list_decision_models()."
    )
  }
  invisible(NULL)
}


# --- Public API -----------------------------------------------------------------------------------
# %% config_OllamaDecision ----
#' Configure a Decision Model on Ollama
#'
#' A decision model answers typed questions with a probability for every option and writes no
#' text. Ollama serves the ones it lists with the `decision` capability through `/v1/systemone`.
#'
#' @param model_name Character: The decision model's name on the Ollama server, for example
#'   `"clef-flash"`.
#' @param base_url Character: Base URL of the Ollama server.
#' @param timeout Numeric (0, Inf): Request timeout in seconds.
#' @param validate_model Logical: If `TRUE`, check that the server has the model and that it is a
#'   decision model.
#'
#' @return `OllamaDecisionConfig` object, to pass to [create_DecisionModel()].
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires a running Ollama server with a decision model
#' \dontrun{
#'   config_OllamaDecision("clef-flash")
#' }
config_OllamaDecision <- function(
  model_name,
  base_url = OLLAMA_URL_DEFAULT,
  timeout = OLLAMA_DECISION_TIMEOUT_DEFAULT,
  validate_model = TRUE
) {
  OllamaDecisionConfig(
    model_name = model_name,
    base_url = base_url,
    timeout = timeout,
    validate_model = validate_model
  )
} # /config_OllamaDecision


# %% config_OpenRouterDecision ----
#' Configure a Decision Model on OpenRouter
#'
#' OpenRouter serves decision models (for example `"typesafe/jev-1.13"`) through the same
#' `/v1/systemone` contract as Ollama. The request, including the prompt and any images, leaves
#' the computer.
#'
#' The API key is resolved at request time: `api_key` if given, then the variable named by
#' `api_key_env` or the Keychain item named by `keychain_service`, then `OPENROUTER_API_KEY`.
#'
#' @param model_name Character: The decision model's name on OpenRouter.
#' @param api_key Optional Character: API key.
#' @param api_key_env Character: Environment variable holding the API key.
#' @param keychain_service Optional Character: macOS Keychain service holding the API key.
#' @param base_url Character: Base URL; requests go to `<base_url>/v1/systemone`.
#' @param timeout Numeric (0, Inf): Request timeout in seconds.
#' @param extra_headers Optional named list: Extra HTTP headers.
#'
#' @return `OpenRouterDecisionConfig` object, to pass to [create_DecisionModel()].
#'
#' @author EDG
#' @export
#'
#' @examples
#' config_OpenRouterDecision("typesafe/jev-1.13")
config_OpenRouterDecision <- function(
  model_name,
  api_key = NULL,
  api_key_env = OPENROUTER_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  base_url = OPENROUTER_URL_DEFAULT,
  timeout = OPENROUTER_DECISION_TIMEOUT_DEFAULT,
  extra_headers = NULL
) {
  OpenRouterDecisionConfig(
    model_name = model_name,
    api_key = api_key,
    api_key_env = api_key_env,
    keychain_service = keychain_service,
    base_url = base_url,
    timeout = timeout,
    extra_headers = extra_headers
  )
} # /config_OpenRouterDecision


# %% ollama_list_decision_models() ----
#' List Decision Models on Ollama
#'
#' @param base_url Character: Base URL of the Ollama server.
#'
#' @return Character vector: Names of the models whose capabilities include `decision`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires a running Ollama server
#' \dontrun{
#'   ollama_list_decision_models()
#' }
ollama_list_decision_models <- function(base_url = OLLAMA_URL_DEFAULT) {
  base_url <- .clean_base_url(base_url)
  models <- unlist(ollama_list_models(base_url = base_url), use.names = FALSE)
  is_decision <- vapply(
    models,
    function(m) {
      "decision" %in% .ollama_capabilities(m, base_url = base_url)
    },
    logical(1L)
  )
  models[is_decision]
} # /ollama_list_decision_models
