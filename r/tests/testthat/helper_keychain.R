# The macOS Keychain tests shell out to `security`, which exists only on macOS,
# and default `account` to `Sys.getenv("USER")`, which is unset in most build
# containers (R-universe, CRAN Linux/Windows). Both make the call fail for
# reasons unrelated to what the test is checking.
skip_if_no_keychain <- function() {
  testthat::skip_on_cran()
  if (!identical(Sys.info()[["sysname"]], "Darwin")) {
    testthat::skip("macOS Keychain not available on this platform")
  }
  if (!nzchar(Sys.getenv("USER"))) {
    testthat::skip("USER is unset, so no Keychain account to look up")
  }
  invisible(NULL)
}
