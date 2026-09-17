# Apple Foundation Models are reached through the rtemis-afm bridge, which serves
# the on-device model over the OpenAI Chat Completions wire on loopback and adds
# GET /health, whose body says whether chat can work and, if not, why.
# Wire contract: ~/Code/spec/rtemis-afm/wire/spec.md

# %% .apple_health_url() ----
#' Derive the Bridge's Health URL
#'
#' The chat wire lives under `/v1`; `/health` sits beside it at the server root.
#'
#' @param base_url Character: Base URL of the bridge's OpenAI wire.
#'
#' @return Character: URL of the health endpoint.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.apple_health_url <- function(base_url) {
  paste0(sub("/v1$", "", .clean_base_url(base_url)), "/health")
}


# %% apple_health() ----
#' Read the rtemis-afm Bridge's Health
#'
#' Reads `GET /health` of a running `rtemis-afm` bridge and returns its body:
#' `status`, `version`, and `model`, which carries the served model's `id`,
#' `name`, `availability` (`"available"` or `"unavailable"`), `reason` when
#' unavailable (`"deviceNotEligible"`, `"appleIntelligenceNotEnabled"`, or
#' `"modelNotReady"`), and `context_window` in tokens.
#'
#' @param base_url Character: Base URL of the bridge's OpenAI-compatible wire. The health
#' endpoint is read from the same server, beside `/v1`.
#'
#' @return Named list: The parsed health response.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires a running rtemis-afm bridge
#' \dontrun{
#'   apple_health()
#' }
apple_health <- function(base_url = APPLE_URL_DEFAULT) {
  check_character_scalar(base_url, "base_url")
  req <- httr2::request(.apple_health_url(base_url)) |>
    httr2::req_method("GET") |>
    httr2::req_user_agent("rtemis (www.rtemis.org)") |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  # rtemis-afm answers /health with 200 and a `model` field whatever the
  # model's state; anything else on the port is some other server.
  res <- if (httr2::resp_is_error(resp)) {
    NULL
  } else {
    tryCatch(
      httr2::resp_body_json(resp, simplifyVector = FALSE),
      error = function(e) NULL
    )
  }
  if (is.null(res[["model"]])) {
    abort(
      "The server at ",
      base_url,
      " did not answer /health as rtemis-afm does (HTTP ",
      httr2::resp_status(resp),
      ").\n",
      "Point `base_url` at a running rtemis-afm bridge (default ",
      APPLE_URL_DEFAULT,
      ")."
    )
  }
  res
}


# %% apple_check_available() ----
#' Check the Apple Foundation Model Is Available
#'
#' Reads the bridge's health with [apple_health] and stops with a message that
#' says what to do when the bridge is not running, the server is not
#' `rtemis-afm`, or the model is unavailable (device not eligible, Apple
#' Intelligence turned off, model still downloading).
#'
#' @param base_url Character: Base URL of the bridge's OpenAI-compatible wire.
#'
#' @return Named list, invisibly: The health response, if the model is available;
#' otherwise throws an error.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires a running rtemis-afm bridge
#' \dontrun{
#'   apple_check_available()
#' }
apple_check_available <- function(base_url = APPLE_URL_DEFAULT) {
  health <- tryCatch(
    apple_health(base_url = base_url),
    httr2_failure = function(e) {
      abort(
        "No rtemis-afm bridge is answering at ",
        base_url,
        ".\n",
        "Start it with `rtemis-afm` in a terminal, or install it first:\n",
        "  curl -fsSL https://live.rtemis.org/afm.sh | sh\n",
        "  brew install rtemis-org/tap/rtemis-afm\n",
        "It needs an Apple silicon Mac, macOS 27 or later, and Apple Intelligence turned on."
      )
    }
  )
  model <- health[["model"]]
  if (identical(model[["availability"]], "available")) {
    return(invisible(health))
  }
  reason <- model[["reason"]]
  advice <- switch(
    if (is.null(reason)) "" else reason,
    appleIntelligenceNotEnabled = paste0(
      "Turn on Apple Intelligence in System Settings > Apple Intelligence & Siri, ",
      "then try again."
    ),
    deviceNotEligible = "The model runs only on an Apple silicon Mac with macOS 27 or later.",
    modelNotReady = paste0(
      "The model is still being downloaded or prepared by macOS. ",
      "Wait a few minutes, then try again."
    ),
    "Check `rtemis-afm status` for details."
  )
  abort(
    "The Apple Foundation Model is unavailable",
    if (!is.null(reason)) paste0(" (", reason, ")"),
    ".\n",
    advice
  )
}
