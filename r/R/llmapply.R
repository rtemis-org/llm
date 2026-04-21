# %%llmapply ----
#' Batch submission of prompts to an LLM agent
#'
#' @param X A vector (atomic or list) to iterate over. Other objects will be coerced by
#'   [base::as.list]. Each element will form the user prompt for a separate call to the LLM agent.
#' @param system_prompt Character: The system prompt to use for all calls. Ignored if `agent` is
#'   provided.
#' @param model_name Character: The name of the model to use. Ignored if `agent` is provided.
#' @param backend Character: The LLM backend to use. One of "ollama", "claude", or "openai".
#'   Ignored if `agent` is provided.
#' @param use_memory Logical: If `TRUE`, the agent will retain memory across calls. Ignored if
#'   `agent` is provided.
#' @param tools List of `Tool` objects: Tools to use for all calls. Ignored if `agent` is provided.
#' @param max_tool_rounds Integer: Maximum number of tool use rounds per call. Ignored if `agent`
#'   is provided.
#' @param output_schema `Schema` object, named list, or JSON string defining the expected structure
#'   of the LLM response. Ignored if `agent` is provided.
#' @param name Character: Name for the agent. Ignored if `agent` is provided.
#' @param verbosity Integer: Verbosity level for messages. Ignored if `agent` is provided.
#' @param extract_responses Logical: If `TRUE`, extract the content of the assistant message from
#'   the full response. If `FALSE`, return the full response object from the agent.
#' @param agent Optional `Agent` object: Agent to use for generating responses.
#' @param ... Additional arguments passed to the `generate` method for the agent.
#'
#' @return A list of `Message` objects or a list of structured responses returned by the agent
#'
#' @details
#' If `agent` is provided, all agent-related arguments (model_name, backend, use_memory, tools,
#' max_tool_rounds, output_schema, name, verbosity) are ignored. If `agent` is not provided, a new
#' Agent will be created using the provided arguments.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires running Ollama server with the "gemma4:e4b" model
#' \dontrun{
#'   hex <- llmapply(
#'      c("teal", "orange", "burgundy"),
#'      system_prompt = "Return the hexadecimal code for the following color in format #FFFFFF",
#'      model_name = "gemma4:e4b"
#'    )
#' }
llmapply <- function(
  X,
  system_prompt,
  model_name = NULL,
  backend = c("ollama", "claude", "openai"),
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
    backend <- match.arg(backend)
    config_fn <- switch(
      backend,
      ollama = config_Ollama,
      claude = config_Claude,
      openai = config_OpenAI
    )
    agent <- create_agent(
      config_fn(model_name),
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
