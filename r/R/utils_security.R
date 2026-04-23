# %% Constants ----
HASH_ALGO <- "sha256"
KMN_LOG_FILE <- "rtemis.llm_security_log.jsonl"

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
#' @return Character string with the secret
#'
#' @details
#' This function retrieves a secret (like an API key) stored in the macOS Keychain.
#' It uses the `security` command-line tool to access the Keychain.
#' If the key cannot be retrieved, a warning -not an error- is issued, and `NULL` is returned.
#' We avoid throwing an error to allow the calling function to handle the absence of the key.
#'
#' @keywords internal
#' @noRd
get_keychain_secret <- function(
  service = "RTEMIS_LLM_API_KEY",
  account = Sys.getenv("USER")
) {
  cmd <- sprintf(
    "security find-generic-password -a %s -s %s -w",
    account,
    service
  )
  tryCatch(
    system(cmd, intern = TRUE),
    error = function(e) {
      cli::cli_warn(c(
        "x" = "Could not retrieve key for service {.val {service}}.",
        "!" = "Keychain may be locked, the key may not exist, or access may be denied.",
        ">" = e[["message"]]
      ))
      NULL
    }
  )
}
# /get_keychain_secret

# %% report_agent_unauthorized_tool ----
#' Report security incident
#'
#' Logs a security incident with details for auditing purposes.
#'
#' @param agent `Agent`: The agent that triggered the incident.
#' @param issue Character: Description of the security issue.
#' @param tool_requested Character: The unauthorized tool that was requested.
#' @param logfile Character: Path to the log file to append the incident to.
#'   Defaults to `KMN_LOG_FILE`.
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
  logfile = KMN_LOG_FILE
) {
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
    cli::cli_abort(
      "Tool hash mismatch: {.val {tool_name}} may have been altered."
    )
  }
  invisible(NULL)
}
