# %% Constants ----
HASH_ALGO <- "sha256"
KMN_LOG_FILE <- "rtemis.llm_security_log.jsonl"


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
#' @export
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
#'
#' @return NULL. Called for side effect of logging.
#'
#' @author EDG
#' @keywords internal
#' @noRd
report_agent_unauthorized_tool <- function(
  agent,
  issue,
  tool_requested
) {
  log_entry <- list(
    timestamp = Sys.time(),
    agent_name = agent@name,
    issue = issue,
    tool_requested = tool_requested
  )
  log_line <- jsonlite::toJSON(log_entry, auto_unbox = TRUE)
  cat(log_line, file = KMN_LOG_FILE, append = TRUE, sep = "\n")
  invisible(NULL)
}


# Internal environment to memoize verified tool hashes
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


#' Call a tool securely
#'
#' @param tool_name character, name of the tool function
#' @param ... arguments to pass to the tool
#'
#' @return result of the tool function
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_function <- function(tool_name, ...) {
  ns <- asNamespace("rtemis.llm")

  # --- Step 1: Check allowed tool names ---
  if (!tool_name %in% tool_DB[["function_name"]]) {
    stop(sprintf("Unauthorized tool call: %s", tool_name))
  }

  # --- Step 2: Get the actual function from the namespace ---
  fn <- get(tool_name, envir = ns, inherits = FALSE)

  # --- Step 3: Verify hash (memoized) ---
  expected_hash <- tool_DB[["hash"]][tool_DB[["function_name"]] == tool_name]

  current_hash <- .tool_hash_cache[[tool_name]]
  if (is.null(current_hash)) {
    current_hash <- .hash_function(fn)
    .tool_hash_cache[[tool_name]] <- current_hash
  }

  if (!identical(current_hash, expected_hash)) {
    cli::cli_abort(
      "Tool hash mismatch: {.val {tool_name}} may have been altered."
    )
  }
}
