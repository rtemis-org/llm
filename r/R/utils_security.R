# %% Constants ----
HASH_ALGO <- "sha256"

# Package-authored tool allowlist: every `function_name` listed here is eligible
# for agent dispatch. Hashes for these functions are captured at `.onLoad` into
# `.tool_hash_cache` and re-verified on every invocation, so runtime tampering
# (e.g. via `assignInNamespace`) is caught.
AVAILABLE_TOOLS <- c(
  "get_current_datetime",
  "query_wikipedia",
  "query_semanticscholar",
  "query_arxiv",
  "query_duckduckgo_ia"
)


# %% get_keychain_secret ----
#' Get secret from macOS Keychain
#'
#' @param service The service name in Keychain (e.g. "OPENAI_API_KEY")
#' @param account The account name in Keychain (e.g. "someone@gmail.com")
#'
#' @return Character string with the secret, or `NULL` if it could not be
#' retrieved. Returning `NULL` rather than erroring lets the caller decide
#' whether a missing key is fatal.
#'
#' @details
#' Retrieves a secret from the macOS Keychain via the `security` tool.
#'
#' Failure is detected from the process exit status, not from an R condition:
#' `system2()` does not raise an R error when the child process exits non-zero,
#' it warns and returns a zero-length result. A `tryCatch(error = )` around it
#' therefore never fires, and a zero-length character is not `NULL`, so a caller
#' guarding on `is.null()` would treat a failed lookup as a successful one.
#'
#' Arguments are shell-quoted. `system2()` quotes the command but passes `args`
#' through to the shell as written, so an unquoted service name containing shell
#' metacharacters would be interpreted rather than matched.
#'
#' @keywords internal
#' @noRd
get_keychain_secret <- function(
  service = "RTEMIS_LLM_API_KEY",
  account = Sys.getenv("USER")
) {
  check_character_scalar(service, "service")
  check_character_scalar(account, "account")
  out <- tryCatch(
    suppressWarnings(system2(
      "security",
      args = c(
        "find-generic-password",
        "-a",
        shQuote(account),
        "-s",
        shQuote(service),
        "-w"
      ),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(out)) {
    return(NULL)
  }
  status <- attr(out, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    return(NULL)
  }
  out <- trimws(out)
  out <- out[nzchar(out)]
  if (length(out) == 0L) {
    return(NULL)
  }
  out[[1L]]
}
# /get_keychain_secret

# %% .resolve_key_sources() ----
#' Resolve an API key from the sources a caller named
#'
#' Tries every configured source in turn and returns the first key found.
#'
#' Naming `api_key_env` (other than the backend default) or `keychain_service`
#' is a statement about where the key lives. If a named source yields nothing,
#' that is a configuration error and this aborts, rather than returning `NULL`
#' and letting an unauthenticated request go out. The failure this prevents is
#' expensive and late: a batch run would issue every call without credentials
#' and, under `on_error = "na"`, come back as a full matrix of `NA` with no
#' indication that the cause was a typo in a variable name.
#'
#' The backend default variable being unset is *not* an error here -- it may
#' simply be unused, as with a local OpenAI-compatible server. That case is left
#' to the caller.
#'
#' @param api_key Optional character: Literal key, wins over every source.
#' @param api_key_env Character: Environment variable to read.
#' @param keychain_service Optional character: macOS Keychain service name.
#' @param default_env Character: The backend's default variable name, which is
#'   treated as ambient rather than explicitly requested.
#' @param provider Character: Backend label used in the error message, so the
#'   message stays discoverable ("No OpenAI API key ...") while also naming the
#'   source that actually failed.
#'
#' @return Character key, or `NULL` if nothing was named and nothing was found.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.resolve_key_sources <- function(
  api_key,
  api_key_env,
  keychain_service,
  default_env,
  provider = "API"
) {
  if (!is.null(api_key)) {
    return(api_key)
  }

  # Precedence: explicitly named sources outrank the backend default variable.
  # `api_key_env` cannot be blanked -- it always holds a name -- so a config
  # that names a Keychain service would otherwise be overridden by an unrelated
  # `OPENAI_API_KEY` left in the environment, and that key would be sent to
  # whatever `base_url` points at. Naming a source is the stronger statement.
  env_is_named <- nzchar(api_key_env) && !identical(api_key_env, default_env)

  # Every source is tried before anything is reported, so a variable that
  # happens to be unset does not mask a key the Keychain does hold.
  named_but_empty <- character()

  if (env_is_named) {
    env_key <- Sys.getenv(api_key_env, unset = "")
    if (nzchar(env_key)) {
      return(env_key)
    }
    named_but_empty <- c(
      named_but_empty,
      paste0("environment variable `", api_key_env, "` is unset or empty")
    )
  }

  if (!is.null(keychain_service)) {
    secret <- get_keychain_secret(service = keychain_service)
    if (!is.null(secret)) {
      return(secret)
    }
    named_but_empty <- c(
      named_but_empty,
      paste0(
        "macOS Keychain has no item for service \"",
        keychain_service,
        "\" under account \"",
        Sys.getenv("USER"),
        "\""
      )
    )
  }

  if (length(named_but_empty) > 0L) {
    abort(
      "No ",
      provider,
      " API key could be resolved from the source you named:\n",
      paste0("  - ", named_but_empty, collapse = "\n"),
      "\n\nA named key source states where the key lives, so not finding one ",
      "is a configuration error.\n",
      "Set the variable in ~/.Renviron, add the Keychain item with\n",
      "  security add-generic-password -a \"$USER\" -s <service> -w\n",
      "or pass `api_key` directly."
    )
  }

  # Nothing was named: fall back to the backend default, whose absence is
  # ambient rather than an error -- a local server needs no key.
  if (nzchar(api_key_env)) {
    env_key <- Sys.getenv(api_key_env, unset = "")
    if (nzchar(env_key)) {
      return(env_key)
    }
  }

  NULL
}

# %% report_agent_unauthorized_tool ----
#' Report security incident
#'
#' Logs a security incident with details for auditing purposes.
#'
#' @param agent `Agent`: The agent that triggered the incident.
#' @param issue Character: Description of the security issue.
#' @param tool_requested Character: The unauthorized tool that was requested.
#' @param logfile Character: Path to the log file to append the incident to.
#'
#' @return NULL. Called for side effect of logging.
#'
#' @author EDG
#' @keywords internal
#' @noRd
report_agent_unauthorized_tool <- function(
  agent,
  issue,
  tool_requested,
  logfile
) {
  check_character_scalar(logfile, "logfile")
  log_entry <- list(
    timestamp = Sys.time(),
    agent_name = agent@name,
    issue = issue,
    tool_requested = tool_requested
  )
  log_line <- jsonlite::toJSON(log_entry, auto_unbox = TRUE)
  cat(log_line, file = logfile, append = TRUE, sep = "\n")
  invisible(NULL)
}


# Internal environment holding trusted tool hashes captured at .onLoad.
# Populated by `.warm_tool_hash_cache()`; read by `validate_function()`.
.tool_hash_cache <- new.env(parent = emptyenv())


# %% hash_function ----
#' Hash function for validation
#'
#' Create a hash of the function's source code for validation purposes
#'
#' @param x Function: The function to hash.
#'
#' @return Character: The hash of the function's source code.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.hash_function <- function(x, algo = HASH_ALGO) {
  x_env_stripped <- x
  environment(x_env_stripped) <- baseenv()
  # hash after explicit serialization (instead of serialize = TRUE in digest)
  digest::digest(
    serialize(object = x_env_stripped, connection = NULL),
    algo = algo
  )
}


#' Warm the tool hash cache
#'
#' Capture the hash of every function named in `AVAILABLE_TOOLS` into
#' `.tool_hash_cache`. Intended to be called from `.onLoad`: the state
#' at load time is treated as trusted, and any later divergence (e.g.
#' `assignInNamespace` of a tool function) is caught by `validate_function()`.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.warm_tool_hash_cache <- function() {
  ns <- asNamespace("rtemis.llm")
  for (fn_name in AVAILABLE_TOOLS) {
    .tool_hash_cache[[fn_name]] <- .hash_function(
      get(fn_name, envir = ns, inherits = FALSE)
    )
  }
  invisible(NULL)
}


#' Call a tool securely
#'
#' @param tool_name character, name of the tool function
#'
#' @return NULL, invisibly. Signals an error if the tool is unknown or has
#' been tampered with since package load.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_function <- function(tool_name) {
  if (!exists(tool_name, envir = .tool_hash_cache, inherits = FALSE)) {
    stop(sprintf("Unauthorized tool call: %s", tool_name))
  }
  fn <- get(tool_name, envir = asNamespace("rtemis.llm"), inherits = FALSE)
  if (!identical(.hash_function(fn), .tool_hash_cache[[tool_name]])) {
    abort(
      "Tool hash mismatch: '",
      tool_name,
      "' may have been altered."
    )
  }
  invisible(NULL)
}
