# utils_ollama.R
# ::kaimana::
# 2025 EDG rtemis.org

# TOC:
#   - ollama_list_models
#   - ollama_get_model_info

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
} # /kaimana::.ollama_api_tags

# %% ollama_list_models() ----
#' List Ollama Models
#'
#' @return Character vector: Model names.
#'
#' @author EDG
#' @export
ollama_list_models <- function(
  base_url = OLLAMA_URL_DEFAULT
) {
  res <- .ollama_api_tags(base_url = base_url, output = "list")
  # Format response using httr2
  sapply(res[["models"]], function(x) x[["model"]])
} # /kaimana::ollama_list_models


# %% ollama_get_model_info() ----
#' Get Ollama Model Info
#'
#' @param model Character: Name of model. If NULL, all models are returned.
#' @param base_url Character: Base URL of Ollama server.
#'
#' @return List with model info if `x` is defined, data.table with all models' info otherwise.
#'
#' @author EDG
#' @export
ollama_get_model_info <- function(x = NULL, base_url = OLLAMA_URL_DEFAULT) {
  models <- .ollama_api_tags(base_url = base_url, output = "list")[["models"]]
  models <- data.table(do.call(rbind, lapply(models, unlist)))
  if (is.null(x)) {
    out <- models
  } else {
    out <- models[name == x]
    if (nrow(out) == 0) {
      cli::cli_abort("Model ", x, " not found.")
    }
    out <- as.list(out)
  }
  out
} # /ollama_get_model_info


# %% ollama_check_model() ----
#' Check Ollama Model is Available
#'
#' @param x Character: Name of model.
#'
#' @return NULL, invisibly if model is available; otherwise throws an error.
#'
#' @author EDG
#' @export
ollama_check_model <- function(x) {
  if (x %in% ollama_list_models()) {
    invisible(NULL)
  } else {
    cli::cli_abort(
      "Model ",
      x,
      " is not available. Please check the model name and pull it if necessary.\n",
      "List available models with `ollama_list_models()`."
    )
  }
} # /kaimana::ollama_check_model
