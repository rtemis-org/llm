# References
# https://hauselin.github.io/ollama-r/

# %% .ollama_api_tags() ----
.ollama_api_tags <- function(
  base_url = OLLAMA_URL_DEFAULT,
  output = c("list", "json")
) {
  output <- match.arg(output)
  req <- httr2::request(paste0(base_url, "/api/tags")) |>
    httr2::req_method("GET")
  res <- httr2::req_perform(req)
  # Check response status
  httr2::resp_check_status(res)
  if (output == "list") {
    httr2::resp_body_json(res)
  } else {
    httr2::resp_body_string(res)
  }
}

# %% ollama_list_models() ----
#' List Ollama Models
#'
#' @param base_url Character: Base URL of Ollama server.
#'
#' @return Character vector: Model names.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires a running Ollama server
#' \dontrun{
#' ollama_list_models()
#' }
ollama_list_models <- function(
  base_url = OLLAMA_URL_DEFAULT
) {
  res <- .ollama_api_tags(base_url = base_url, output = "list")
  # Format response using httr2
  sapply(res[["models"]], function(x) x[["model"]])
}


# %% ollama_get_model_info() ----
#' Get Ollama Model Info
#'
#' @param x Optional character vector: Name of model(s) to get info for. If NULL, all available
#' models' info is returned.
#' @param base_url Character: Base URL of Ollama server.
#'
#' @return data.table
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires a running Ollama server
#' \dontrun{
#'   ollama_get_model_info()
#'   ollama_get_model_info(x = "gemma4:e4b")
#' }
ollama_get_model_info <- function(x = NULL, base_url = OLLAMA_URL_DEFAULT) {
  models <- .ollama_api_tags(base_url = base_url, output = "list")[["models"]]
  models <- data.table::rbindlist(
    lapply(models, function(m) data.table(t(unlist(m)))),
    fill = TRUE
  )

  # appease R CMD check
  name <- NULL

  if (is.null(x)) {
    models
  } else {
    models[name %in% x]
  }
} # /ollama_get_model_info


# %% .check_ollama_think() ----
#' Validate Ollama think Argument
#'
#' Ollama accepts either a logical scalar (e.g. deepseek, qwen3) or a character
#' scalar in `{"low", "medium", "high"}` (e.g. gpt-oss).
#'
#' @param x Object: Value to validate.
#' @param name Character: Argument name to report in error messages.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_ollama_think <- function(x, name = "think") {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  if (length(x) != 1L || is.na(x)) {
    cli::cli_abort(
      "{.var {name}} must be a logical scalar or one of {.val low}, {.val medium}, {.val high}."
    )
  }
  if (is.logical(x)) {
    return(invisible(NULL))
  }
  if (is.character(x) && x %in% c("low", "medium", "high")) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    "{.var {name}} must be a logical scalar or one of {.val low}, {.val medium}, {.val high}."
  )
}


# %% ollama_check_model() ----
#' Check Ollama Model is Available
#'
#' @param x Character: Name of model.
#'
#' @return NULL, invisibly if model is available; otherwise throws an error.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running Ollama server
#' \dontrun{
#'   ollama_check_model("gemma4:e4b")
#' }
ollama_check_model <- function(x) {
  if (x %in% ollama_list_models()) {
    invisible(NULL)
  } else {
    cli::cli_abort(c(
      "Model {.val {x}} is not available.",
      i = "Please check the model name and pull it if necessary.",
      i = "List available models with `ollama_list_models()`."
    ))
  }
}
