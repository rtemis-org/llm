# %%llmapply ----
#' Batch submission of prompts to an LLM agent
#'
#' @param X A vector (atomic or list) to iterate over. Other objects will be coerced by
#'   [base::as.list]. Each element will form the user prompt for a separate call to the LLM agent.
#' @param agent An object of class `Agent` to which the prompts will be submitted.
#' @param ... Additional arguments passed to the `generate` method for the agent.
#'
#' @return A list of `Message` objects or a list of structured responses returned by the agent
#'
#' @author EDG
#' @export
llmapply <- function(X, agent, ...) {
  # Ensure X is a list for iteration
  if (!is.list(X)) {
    X <- as.list(X)
  }
}
