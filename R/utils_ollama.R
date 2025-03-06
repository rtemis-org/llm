# utils_ollama.R
# ::kaimana::
# 2025 SDG rtemis.org

# References
# https://hauselin.github.io/ollama-r/

#' List Ollama Models
#' 
#' @return Character vector: Model names.
#' 
#' @author SDG
#' @export
list_ollama_models <- function() {
  ollamar::list_models()[["name"]]
} # /list_ollama_models

#' Message Ollama
#'
#' @param model Character: Name of model to use.
#' @param system Character: Prompt to send to model (as system message).
#' @param user Character: Content to send to model (as user message).
#' @param output_type Character: Type of output to return: "df", "jsonlist", "raw", "resp", "text", 
#' or "tools".  Default is "text".
#'
#' @return Character: Model response.
#'
#' @author SDG
#' @export
msg_ollama <- function(model, system = NULL, user = "Hello.", output_type = "text") {
  messages <- Filter(Negate(is.null), list(
    if (!is.null(system)) list(role = "system", content = system),
    list(role = "user", content = user)
  ))
  response <- ollamar::chat(
    model = model,
    messages = messages
  )
  ollamar::resp_process(resp = response, output = output_type)
} # /msg_ollama
