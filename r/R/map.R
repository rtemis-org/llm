# %% responses ----
#' Extract response(s) from a Message or list of Messages
#'
#' Returns one or more Message's assistant content
#'
#' @param x Message object or list with Message objects. Can be the ouput of [map] on an `LLM` or
#'   `Agent` object
#'
#' @return Character vector of assistant's responses
#'
#' @author EDG
ai_response <- function(x) {
  # Single Message object
  if (S7_inherits(x, Message)) {
    if (x@role == "assistant") {
      return(x@content)
    } else {
      return(NULL)
    }
  }

  # map.LLM output is a list of Message objects
  if (is.list(x) && all(sapply(x, S7_inherits, class = Message))) {
    return(
      vapply(
        x,
        function(m) {
          if (m@role == "assistant") m@content else NA_character_
        },
        character(1L)
      )
    )
  }

  # map.Agent output is a list of lists of Message objects
  if (
    is.list(x) &&
      all(sapply(x, function(item) {
        is.list(item) && all(sapply(item, S7_inherits, class = Message))
      }))
  ) {
    return(
      vapply(
        x,
        function(messages) {
          asst <- Filter(function(m) m@role == "assistant", messages)
          if (length(asst) > 0L) asst[[1L]]@content else NA_character_
        },
        character(1L)
      )
    )
  }
  cli::cli_abort(
    "Input not recognized. It must be either a Message object or the output of map on an LLM or Agent object."
  )
}


# %% map.(class_character, LLM | Agent) ----
method(map, list(class_character, LLM | Agent)) <- function(
  x,
  f,
  verbosity = 1L,
  ...
) {
  # Iterate over X and generate responses
  out <- lapply(
    cli::cli_progress_along(
      x,
      "Processing",
      format = "{cli::pb_spin} [{cli::pb_current}/{cli::pb_total}] {cli::pb_bar} {cli::pb_eta_str}"
    ),
    function(i) {
      generate(f, x[[i]], verbosity = verbosity - 1L, ...)
    }
  )

  out
}

# %% llmapply ----
# This is a wrapper for the map method for R users who are much more familiar with lapply than map.
#'
#' @param x A vector (atomic or list) to iterate over. Other objects will be coerced by
#'   [base::as.list]. Each element will form the user prompt for a separate call to the LLM.
#' @param llm An LLM object to use for all calls.
#' @param ... Additional arguments passed to the `generate` method for the LLM.
#'
#' @return A list of `Message` objects returned by the LLM
#' @author EDG
#' @export
llmapply <- function(
  x,
  llm,
  ...
) {
  map(x, llm, ...)
}
