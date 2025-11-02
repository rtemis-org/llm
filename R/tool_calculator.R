# tool_calculator.R
# ::kaimana::
# 2025 EDG rtemis.org

#' Add two numbers
#'
#' @param a A number
#' @param b A number
#'
#' @return The sum of a and b
#' @export
add <- function(a, b) {
  a + b
}

#' Subtract two numbers
#'
#' @param a A number
#' @param b A number
#'
#' @return The difference of a and b
#' @export
subtract <- function(a, b) {
  a - b
}

#' Multiply two numbers
#'
#' @param a A number
#' @param b A number
#'
#' @return The product of a and b
#' @export
multiply <- function(a, b) {
  a * b
}

#' Divide two numbers
#'
#' @param a A number
#' @param b A number
#'
#' @return The quotient of a and b
#' @export
divide <- function(a, b) {
  if (b == 0) {
    return("Error: Division by zero")
  }
  a / b
}
