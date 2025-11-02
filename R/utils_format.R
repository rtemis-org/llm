# utils_format.R
# ::kaimana-r::
# 2025 EDG rtemis.org

# Utilities to format messages

repr_bracket <- function(x, output_type = NULL) {
  output_type <- get_output_type(output_type)

  paste0(
    fmt("[", muted = TRUE, output_type = output_type),
    highlight(x, output_type = output_type),
    fmt("]", muted = TRUE, output_type = output_type)
  )
} # /kaimana::repr_bracket


#' Convert character vector to markdown list
#'
#' @param x Character vector
#'
#' @return Character string formatted as markdown list
#'
#' @author EDG
#' @export
#'
#' @examples
#' md_char2list(c("Item 1", "Item 2", "Item 3")) |> cat()
md_char2list <- function(x) {
  paste0("- ", x, collapse = "\n")
}
