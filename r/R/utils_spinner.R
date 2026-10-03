# utils_spinner.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# A transient spinner with elapsed time, drawn while a chat request is in flight.
# The designs, the color pulse and the `rtemis.progress_spinner` option follow
# rtemis.core's progress spinner, which has no public spinner-only API yet; this
# may move there. Glyphs are escaped so the source stays ASCII.
# spec: llm/console-chat#waiting

# %% State and designs ----
.spinner_state <- new.env(parent = emptyenv())
.spinner_state[["enabled"]] <- FALSE
.spinner_state[["active"]] <- FALSE

# rtemis.core's pulse: rtemis_colors light_orange -> red in 5 steps, ping-pong,
# precomputed so the package does not import grDevices for it.
.spinner_colors <- c(
  "#FDB808",
  "#F89818",
  "#F37829",
  "#EE5839",
  "#EA384A",
  "#EE5839",
  "#F37829",
  "#F89818"
)

.spinner_designs <- list(
  dots = c(
    "\u280B",
    "\u2819",
    "\u2839",
    "\u2838",
    "\u283C",
    "\u2834",
    "\u2826",
    "\u2827",
    "\u2807",
    "\u280F"
  ),
  dot = "\u25CF",
  blocks = c("\u2596", "\u2598", "\u259D", "\u2597")
)


# %% .spinner_enabled ----
#' Whether requests show the spinner instead of "working..." / "done."
#'
#' On only while `chat()` is running (it sets `.spinner_state$enabled`), at verbosity 1 or
#' higher, and on an ANSI console, where the line can be rewritten in place.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.spinner_enabled <- function(verbosity = 1L) {
  isTRUE(.spinner_state[["enabled"]]) &&
    verbosity > 0L &&
    identical(get_output_type(), "ansi")
}


# %% .spinner_start ----
.spinner_start <- function() {
  .spinner_state[["active"]] <- TRUE
  .spinner_state[["frame"]] <- 0L
  .spinner_state[["t_start"]] <- proc.time()[["elapsed"]]
  .spinner_state[["last_draw"]] <- -Inf
  .spinner_state[["width"]] <- 0L
  .spinner_draw()
}


# %% .spinner_tick ----
# Called from curl's progress callback: about once a second while the server is
# working, and many times a second while a response downloads.
.spinner_tick <- function() {
  if (!isTRUE(.spinner_state[["active"]])) {
    return(invisible(NULL))
  }
  if (proc.time()[["elapsed"]] - .spinner_state[["last_draw"]] >= 0.1) {
    .spinner_draw()
  }
  invisible(NULL)
}


# %% .spinner_stop ----
# Erase the line so the reply prints where the spinner was.
.spinner_stop <- function() {
  if (!isTRUE(.spinner_state[["active"]])) {
    return(invisible(NULL))
  }
  .spinner_state[["active"]] <- FALSE
  message(
    "\r",
    strrep(" ", .spinner_state[["width"]]),
    "\r",
    appendLF = FALSE
  )
  invisible(NULL)
}


# %% .spinner_draw ----
.spinner_draw <- function() {
  frames <- .spinner_designs[[getOption(
    "rtemis.progress_spinner",
    "dots"
  )]] %||%
    .spinner_designs[["dots"]]
  frame <- .spinner_state[["frame"]]
  now <- proc.time()[["elapsed"]]
  elapsed <- .spinner_elapsed(now - .spinner_state[["t_start"]])
  line <- paste0(
    fmt(
      frames[[(frame %% length(frames)) + 1L]],
      col = .spinner_colors[[(frame %% length(.spinner_colors)) + 1L]],
      output_type = "ansi"
    ),
    " ",
    gray(elapsed, output_type = "ansi")
  )
  width <- nchar(elapsed) + 2L
  pad <- max(0L, .spinner_state[["width"]] - width)
  message("\r", line, strrep(" ", pad), appendLF = FALSE)
  .spinner_state[["frame"]] <- frame + 1L
  .spinner_state[["last_draw"]] <- now
  .spinner_state[["width"]] <- width
  invisible(NULL)
}


# %% .spinner_elapsed ----
# "7s" under a minute, "1:05" after.
.spinner_elapsed <- function(seconds) {
  s <- as.integer(floor(seconds))
  if (s < 60L) {
    paste0(s, "s")
  } else {
    sprintf("%d:%02d", s %/% 60L, s %% 60L)
  }
}


# %% .req_perform_status ----
#' Perform a chat request, with the spinner when it is enabled
#'
#' R is blocked while the request runs, so the spinner advances from curl's progress callback,
#' which curl calls about once a second while waiting.
#'
#' @param req httr2_request.
#' @param verbosity Integer: Verbosity level.
#'
#' @return httr2_response.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.req_perform_status <- function(req, verbosity = 1L) {
  if (.spinner_enabled(verbosity)) {
    req <- httr2::req_options(
      req,
      noprogress = FALSE,
      xferinfofunction = function(down, up) {
        .spinner_tick()
        TRUE
      }
    )
    .spinner_start()
    on.exit(.spinner_stop(), add = TRUE)
  }
  httr2::req_perform(req, verbosity = max(verbosity - 1L, 0L))
}
