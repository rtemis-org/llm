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
