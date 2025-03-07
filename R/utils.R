# utils.R

col256 <- function(x, col = 183) {
  paste0("\033[38;5;", col, "m", x, "\033[0m")
}

kmnlogo <- local({
  paste0("  ",
    mapply(
      col256,
      readLines(system.file(package = .packageName, "resources", "kaimana.utf8")),
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
