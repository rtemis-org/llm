# utils.R


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
