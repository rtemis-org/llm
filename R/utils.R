# utils.R
# ::kaimana::
# 2025- EDG rtemis.org

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
  service = "KAIMANA_API_KEY",
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
