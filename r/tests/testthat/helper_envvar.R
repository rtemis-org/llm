# Set environment variables for the duration of `expr`, then restore whatever
# was there before -- including variables that were unset, which must be
# unset again rather than set to "". `NA` as a value means "unset this".
with_env <- function(vars, expr) {
  nms <- names(vars)
  previous <- Sys.getenv(nms, names = TRUE, unset = NA)
  set_env <- function(values) {
    unset <- is.na(values)
    if (any(unset)) {
      Sys.unsetenv(nms[unset])
    }
    if (any(!unset)) {
      do.call(Sys.setenv, as.list(values[!unset]))
    }
  }
  on.exit(set_env(previous), add = TRUE)
  values <- as.character(vars)
  names(values) <- nms
  set_env(values)
  expr
}
