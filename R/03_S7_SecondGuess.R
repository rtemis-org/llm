# SecondGuess.R
# ::kaimana::
# 2025 EDG rtemis.org

# %%  SecondGuess Class ----

#' @name SecondGuess
#'
#' @title SecondGuess Class
#'
#' @description
#' Class for SecondGuess agents
#'
#' @field model_name Character: The name of the LLM model to use.
#' @field state Environment: An environment to hold the state of the agent, including messages.
#' The system prompt is automatically added as the first message.
#' @field tools Character: Names of tools to use.
#' @field max_tool_calls Integer: Maximum number of tool calls per query.
#' @field temperature Numeric: The temperature for the model.
#'
#' @details
#' The
#'
#' @author EDG
#'
#' @noRd
SecondGuess <- new_class(
  name = "SecondGuess",
  properties = list(
    model_name_one = class_character,
    model_name_two = class_character,
    system_prompt_one = class_character,
    system_prompt_two = class_character,
    state = class_environment,
    temperature_one = class_numeric,
    temperature_two = class_numeric
  ),
  constructor = function(
    model_name_one,
    system_prompt_one,
    temperature_one,
    model_name_two,
    system_prompt_two,
    temperature_two
  ) {
    state <- new.env(parent = emptyenv())
    # Initialize messages with system prompt one within state environment
    state[["messages"]] <- list(
      list(role = "system", content = system_prompt_one)
    )
    # if (!all(tool_names %in% names(react_tools))) {
    #   cli::cli_abort(
    #     "Some tool names are not recognized. Available tools are: ",
    #     paste(names(react_tools), collapse = ", ")
    #   )
    # }
    new_object(
      S7_object(),
      model_name_one = model_name_one,
      model_name_two = model_name_two,
      temperature_one = temperature_one,
      temperature_two = temperature_two,
      system_prompt_one = system_prompt_one,
      system_prompt_two = system_prompt_two,
      state = state
    )
  }
) # /kaimana::SecondGuess


#' Create SecondGuess agent with Ollama
#'
#' @param model_name_one Character: The name of the first LLM model to use.
#' @param system_prompt_one Character: System prompt for the first model.
#' @param temperature_one Numeric: The temperature for the first model.
#' @param model_name_two Character: The name of the second LLM model to use.
#' @param system_prompt_two Character: System prompt for the second model.
#' @param temperature_two Numeric: The temperature for the second model.
#'
#' @return SecondGuess object
#'
#' @author EDG
#' @export
create_SecondGuess <- function(
  model_name_one,
  system_prompt_one,
  temperature_one = 0.7,
  model_name_two,
  system_prompt_two = "You are a meticulous AI Assitant and your name is Kaimana.",
  temperature_two = 0.7
) {
  SecondGuess(
    model_name_one = model_name_one,
    system_prompt_one = system_prompt_one,
    temperature_one = temperature_one,
    model_name_two = model_name_two,
    system_prompt_two = system_prompt_two,
    temperature_two = temperature_two
  )
} # /kaimana::create_SecondGuess


# %% repr method for SecondGuess ----
method(repr, SecondGuess) <- function(x, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  out <- paste0(
    repr_S7name("SecondGuess"),
    fmt("Model One: ", bold = TRUE, output_type = output_type),
    x@model_name_one,
    "\n",
    fmt("Model Two: ", bold = TRUE, output_type = output_type),
    x@model_name_two,
    "\n",
    fmt("Temperature One: ", bold = TRUE, output_type = output_type),
    x@temperature_one,
    "\n",
    fmt("Temperature Two: ", bold = TRUE, output_type = output_type),
    x@temperature_two,
    "\n"
  )
  out
} # /kaimana::repr.SecondGuess


# %% Print SecondGuess ----
method(print, SecondGuess) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # kaimana::print.SecondGuess


# %% invoke method for SecondGuess ----
#' @name invoke.SecondGuess
#'
#' @title Invoke SecondGuess Agent
#'
#' @param x SecondGuess object
#' @param query Character: Query to pass to the agent.
#' @param verbosity Integer: Verbosity level.
#'
#' @return AIResponse object
#'
#' @author EDG
method(invoke, SecondGuess) <- function(
  x,
  query,
  image_path = NULL,
  verbosity = 1L
) {
  if (verbosity > 0L) {
    msg("SecondGuess Agent is working...")
  }
  if (is.null(image_path)) {
    # Append query to messages
    x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- list(
      role = "user",
      content = query
    )
  } else {
    # Append query to messages
    x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- list(
      role = "user",
      content = query,
      images = image_path
    )
  }

  res_one <- ollamar::chat(
    model = x@model_name_one,
    messages = x@state[["messages"]],
    temperature = x@temperature_one,
    stream = FALSE,
    output = "resp"
  )
  # Append model one's response to messages
  x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- list(
    role = "assistant",
    name = "Assistant One",
    content = ollamar::resp_process(res_one, output = "text")
  )

  # Construct prompt for model two
  prompt_two <- paste0(
    "Here is the original user query and the first model's response:\n\n",
    "User: ",
    query,
    "\n",
    "Model One: ",
    ollamar::resp_process(res_one, output = "text"),
    "\n\n",
    "Carefully analyze the reasoning and response of Model One.\n",
    "1) Do you agree with the reasoning?\n",
    "2) Do you agree with the final response?\n",
    "3) Does the response follow clearly and correctly from the reasoning?\n",
    "Provide a final response either by repeating the original response or by correcting it, ",
    "expanding on it, or improving it in any way you see fit."
  )

  res_two <- ollamar::chat(
    model = x@model_name_two,
    messages = ollamar::create_messages(
      ollamar::create_message(content = x@system_prompt_two, role = "system"),
      ollamar::create_message(content = prompt_two, role = "user")
    ),
    temperature = x@temperature_two,
    stream = FALSE,
    output = "resp"
  )

  # Append model two's response to messages
  x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- list(
    role = "assistant",
    name = "Assistant Two",
    content = ollamar::resp_process(res_two, output = "text")
  )

  # Overwrite
  AIResponse(response = x@state[["messages"]])
} # /kaimana::invoke.SecondGuess
