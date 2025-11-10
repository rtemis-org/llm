# Agent.R
# ::kaimana::
# 2025 EDG rtemis.org

# References:
# https://docs.ollama.com/api/chat
# https://docs.ollama.com/capabilities/tool-calling#tool-calling

# %% Agent Class ----
#' @title agent
#'
#' @description
#' Class for agents that support reasoning, tool use, structured output, and state management.
#'
#' @field llmconfig LLMConfig: The LLMConfig to use.
#' @field state Optional environment: The state of the agent. If NULL, the agent is stateless, i.e.
#' there is no conversation memory and each generate() call is independent.
#' @field system_prompt Optional character: The system prompt to use.
#' @field tools Optional list of Tool objects: The tools available to the agent.
#' @field name Optional character: The name of the agent.
#'
#' @author EDG
#' @noRd
Agent <- new_class(
  "Agent",
  properties = list(
    llmconfig = LLMConfig,
    state = class_environment,
    system_prompt = new_union(NULL | class_character),
    use_memory = class_logical,
    tools = new_union(NULL | class_list),
    name = new_union(NULL | class_character)
  ),
  constructor = function(
    llmconfig,
    system_prompt = SYSTEM_PROMPT_DEFAULT,
    use_memory = TRUE,
    tools = NULL,
    name = NULL
  ) {
    state <- new.env(parent = emptyenv())
    # Initialize messages with system prompt within state environment
    state[["messages"]] <- list(
      list(role = "system", content = system_prompt)
    )
    new_object(
      S7_object(),
      llmconfig = llmconfig,
      state = state,
      system_prompt = system_prompt,
      use_memory = use_memory,
      tools = tools,
      name = name
    )
  },
  validator = function(self) {
    if (!is.null(self@tools)) {
      for (tool in self@tools) {
        if (!S7_inherits(tool, Tool)) {
          cli::cli_abort("All elements of 'tools' must be Tool objects.")
        }
      }
    }
  }
) # /kaimana::Agent


# %% create_agent() ----
#' Create an Agent
#'
#' @param llmconfig LLMConfig: The LLMConfig to use.
#' @param system_prompt Optional character: The system prompt to use.
#' @param use_memory Logical: Whether to use conversation memory.
#' @param tools Optional list of Tool objects: The tools available to the agent.
#' @param name Optional character: The name of the agent.
#'
#' @return Agent object
#' @author EDG
#' @export
create_agent <- function(
  llmconfig,
  system_prompt = NULL,
  use_memory = TRUE,
  tools = NULL,
  name = NULL
) {
  Agent(
    llmconfig = llmconfig,
    system_prompt = system_prompt,
    use_memory = use_memory,
    tools = tools,
    name = name
  )
} # /kaimana::agent


# %% repr.Agent ----
method(repr, Agent) <- function(x, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  out <- paste0(
    repr_S7name("Agent", output_type = output_type),
    repr(x@llmconfig, pad = 2L, output_type = output_type),
    fmt("System Prompt: ", bold = TRUE, pad = 2L, output_type = output_type),
    fmt(
      paste0(
        substr(x@system_prompt, 1, 60),
        if (nchar(x@system_prompt) > 60) "..." else ""
      ),
      output_type = output_type
    ),
    "\n",
    fmt("       Memory: ", bold = TRUE, pad = 2L, output_type = output_type),
    fmt(
      if (x@use_memory) "Enabled" else "Disabled",
      output_type = output_type
    ),
    "\n",
    if (!is.null(x@tools)) {
      paste0(
        fmt(
          "        Tools:\n",
          bold = TRUE,
          pad = 2L,
          output_type = output_type
        ),
        paste0(
          sapply(
            x@tools,
            function(tool) {
              paste0(
                "           - ",
                fmt(
                  tool@name,
                  bold = TRUE,
                  output_type = output_type
                ),
                ": ",
                tool@description
              )
            }
          ),
          collapse = "\n"
        )
      )
    }
  )
} # /kaimana::repr.Agent


# %% print.Agent ----
method(print, Agent) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /kaimana::print.Agent


# %% generate.Agent() ----
# Reference: https://docs.ollama.com/api/chat
#' Generate method for Agent
#'
#' @param x Agent object
#' @param prompt Character: The prompt to send to the agent.
#' @param image_path Optional character: Path to an image to include in the prompt.
#' @param output_schema Optional list: The output schema to enforce on the agent's response.
#' @param commit_to_memory Logical: Whether to commit this interaction to the agent's memory.
#' @param use_tools Logical: Whether to allow the agent to use tools.
#' @param echo Logical: Whether to echo the prompt and response.
#' @param verbosity Integer: Verbosity level.
#'
#' @return AgentMessage object
#'
#' @details
#' Memory: if agent was initialized with use_memory = TRUE, the conversation history will be
#' maintained. If any call to generate() specifies `commit_to_memory = FALSE`, the message and its
#' response will not be added to the conversation history. If the agent was initialized with
#' use_memory = FALSE, the conversation history will not be maintained.
#'
#' @author EDG
#' @noRd
method(generate, Agent) <- function(
  x,
  prompt,
  image_path = NULL,
  output_schema = NULL,
  commit_to_memory = TRUE,
  use_tools = TRUE,
  echo = TRUE,
  verbosity = 1L
) {
  # Check input
  check_inherits(prompt, "character")
  update_state <- x@use_memory && commit_to_memory
  # State management
  if (update_state) {
    if (is.null(image_path)) {
      # Append prompt to messages
      x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- list(
        role = "user",
        content = prompt
      )
    } else {
      # Append prompt + images to messages
      x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- list(
        role = "user",
        content = prompt,
        images = image_path
      )
    }
    messages <- x@state[["messages"]]
  } else {
    if (is.null(image_path)) {
      messages <- list(
        list(
          role = "system",
          content = x@system_prompt
        ),
        list(
          role = "user",
          content = prompt
        )
      )
    } else {
      messages <- list(
        list(
          role = "system",
          content = x@system_prompt
        ),
        list(
          role = "user",
          content = prompt,
          images = image_path
        )
      )
    }
  }

  # Request
  request_body <- list(
    model = x@llmconfig@model_name,
    messages = messages,
    stream = FALSE,
    options = list(
      temperature = x@llmconfig@temperature
    )
  )
  # Output schema
  if (!is.null(output_schema)) {
    request_body[["format"]] <- output_schema
  }

  # Tools
  if (!is.null(x@tools) && use_tools) {
    request_body[["tools"]] <- lapply(x@tools, as_list)
  }

  if (verbosity > 0) {
    output_type <- get_output_type()
    msg(repr_bracket(x@llmconfig@model_name), "working...")
  }

  # Perform request
  resp <- httr2::request(paste0(x@llmconfig@base_url, "/api/chat")) |>
    httr2::req_body_json(request_body) |>
    httr2::req_user_agent("kaimana-r agent (kaimana.rtemis.org)") |>
    httr2::req_perform(verbosity = verbosity - 1L)

  # Check for errors
  httr2::resp_check_status(resp)
  if (verbosity > 0) {
    # Replace working message with done
    msg(repr_bracket(x@llmconfig@model_name), "done.")
  }
  # Initial response
  res <- httr2::resp_body_json(resp)

  # Update state
  if (update_state) {
    # Append response to messages
    x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- res[[
      "message"
    ]]
  }

  # Check for tool calls
  if (!is.null(res[["message"]][["tool_calls"]])) {
    tool_names <- sapply(
      res[["message"]][["tool_calls"]],
      function(tc) tc[["function"]][["name"]]
    )
    n_tool_calls <- length(tool_names)
    if (verbosity > 0L) {
      msg(
        "Agent wants to perform",
        n_tool_calls,
        ngettext(n_tool_calls, "tool call.", " tool calls.")
      )
    }

    tool_responses <- structure(
      vector("list", n_tool_calls),
      names = tool_names
    )
    for (i in seq_along(tool_responses)) {
      if (verbosity > 0L) {
        msg("Invoking tool:", highlight(tool_names[i]))
      }
      args <- res[["message"]][["tool_calls"]][[i]][["function"]][["arguments"]]
      tool_responses[[i]] <- do.call(tool_names[i], args)
      if (verbosity > 0L) {
        msg("Tool", highlight(tool_names[i]), "returned response.")
      }
    }
    tool_response_prompt <- paste0(
      "The following tool responses were obtained:\n",
      paste0(
        sapply(
          names(tool_responses),
          function(tn) {
            paste0(
              "Tool '",
              tn,
              "' response:\n",
              jsonlite::toJSON(
                tool_responses[[tn]],
                auto_unbox = TRUE,
                pretty = TRUE
              ),
              "\n"
            )
          }
        ),
        collapse = "\n"
      )
    )
    # Remind agent of original prompt
    tool_response_prompt <- paste0(
      tool_response_prompt,
      "\n",
      "Acknowledge the tools used and include citations where applicable.",
      " Use these tool responses to answer the original query:\n\n",
      '"',
      prompt,
      '"\n'
    )
    # Now, send another prompt to the agent with the tool responses
    if (verbosity > 0L) {
      msg("Sending tool responses back to agent...")
    }
    followup_request_body <- list(
      model = x@llmconfig@model_name,
      messages = list(
        list(
          role = "system",
          content = x@system_prompt
        ),
        list(
          role = "user",
          content = prompt
        ),
        list(
          role = "tool",
          name = "tool_responses",
          content = tool_response_prompt
        )
      ),
      stream = FALSE,
      options = list(
        temperature = x@llmconfig@temperature
      )
    )
    # Output schema
    if (!is.null(output_schema)) {
      followup_request_body[["format"]] <- output_schema
    }
    followup_resp <- httr2::request(paste0(
      x@llmconfig@base_url,
      "/api/chat"
    )) |>
      httr2::req_body_json(followup_request_body) |>
      httr2::req_user_agent("kaimana-r agent (kaimana.rtemis.org)") |>
      httr2::req_perform(verbosity = verbosity - 1L)
    # Check for errors
    httr2::resp_check_status(followup_resp)
    if (verbosity > 0) {
      # Replace working message with done
      msg(repr_bracket(x@llmconfig@model_name), "done.")
    }
    out <- httr2::resp_body_json(followup_resp)

    # Update state
    if (update_state) {
      # Append response to messages
      x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- out[[
        "message"
      ]]
    }
    return(out)
  } # /tool calls
  res
} # /kaimana::generate.Agent
