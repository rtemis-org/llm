# utils.R

col256 <- function(x, col = 183) {
  paste0("\033[38;5;", col, "m", x, "\033[0m")
}

kmnlogo <- local({
  paste0(
    "  ",
    mapply(
      col256,
      readLines(system.file(
        package = .packageName,
        "resources",
        "kaimana.utf8"
      )),
      c(159, 39, 26, 20, 17, -1)
    ),
    collapse = "\n"
  )
})


#' @keywords internal
#' @noRd
plain <- function(x) {
  paste0("\033[0m", x)
}

#' @keywords internal
#' @noRd
bold <- function(...) {
  paste0("\033[1m", paste(...), "\033[22m")
}

#' @keywords internal
#' @noRd
red <- function(..., bold = FALSE) {
  paste0("\033[", ifelse(bold, "1;", ""), "91m", paste(...), "\033[0m")
}


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
