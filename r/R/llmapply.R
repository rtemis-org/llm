# %%llmapply ----
#' Batch submission of prompts to an LLM agent
#'
#' @param X A vector (atomic or list) to iterate over. Other objects will be coerced by
#'   [base::as.list]. Each element will form the user prompt for a separate call to the LLM agent.
#' @param system_prompt Character: The system prompt to use for all calls.
#' @param model_name Character: The name of the model to use. Ignored if `agent` is provided.
#' @param use_memory Logical: If `TRUE`, the agent will retain memory across calls. Ignored if `agent` is provided.
#' @param agent `Agent` object: Agent to use for generating responses.
#' @param ... Additional arguments passed to the `generate` method for the agent.
#'
#' @return A list of `Message` objects or a list of structured responses returned by the agent
#'
#' @details
#' Either model_name or agent must be provided. If both are provided, the agent will take precedence.
#'
#' @author EDG
#' @export
llmapply <- function(
  X,
  system_prompt,
  model_name = NULL,
  use_memory = FALSE,
  tools = NULL,
  max_tool_rounds = 3L,
  output_schema = NULL,
  name = NULL,
  verbosity = 1L,
  extract_responses = TRUE,
  agent = NULL,
  ...
) {
  # Ensure X is a list for iteration
  if (!is.list(X)) {
    X <- as.list(X)
  }
  # model_name or agent must be provided
  if (is.null(model_name) && is.null(agent)) {
    cli::cli_abort("Either {.arg model_name} or {.arg agent} must be provided.")
  }
  if (is.null(agent)) {
    agent <- create_agent(
      config_Ollama(model_name),
      system_prompt = system_prompt,
      use_memory = use_memory,
      tools = tools,
      max_tool_rounds = max_tool_rounds,
      output_schema = output_schema,
      name = name,
      verbosity = verbosity
    )
  }
  # Iterate over X and generate responses
  out <- lapply(X, function(prompt) {
    generate(agent, prompt, ...)
  })
  if (extract_responses) {
    out <- lapply(out, function(x) {
      x$assistant@content
    })
  }
  out
}
