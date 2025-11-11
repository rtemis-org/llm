# Agent.R
# ::kaimana::
# 2025 EDG rtemis.org

# References:
# https://docs.ollama.com/api/chat
# https://docs.ollama.com/capabilities/tool-calling#tool-calling

# --- Internal API ---------------------------------------------------------------------------------
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
    output_schema = new_union(NULL | class_list),
    name = new_union(NULL | class_character)
  ),
  constructor = function(
    llmconfig,
    system_prompt = NULL,
    use_memory = TRUE,
    tools = NULL,
    output_schema = NULL,
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
      output_schema = output_schema,
      name = name
    )
  },
  validator = function(self) {
    if (!is.null(self@tools)) {
      for (tool in self@tools) {
        # Check that each tool is a Tool object
        if (!S7_inherits(tool, Tool)) {
          cli::cli_abort("All elements of 'tools' must be Tool objects.")
        }
        # Check that tool is part of allowed tools in tool_DB
        if (!tool@name %in% names(tool_DB)) {
          cli::cli_abort(
            "Tool '{tool@name}' is not part of the allowed tool set."
          )
        }
      }
    }
  }
) # /kaimana::Agent


# %% repr.Agent ----
method(repr, Agent) <- function(x, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  out <- paste0(
    repr_S7name("Agent", output_type = output_type),
    fmt("         Name: ", bold = TRUE, pad = 2L, output_type = output_type),
    if (is.null(x@name)) {
      "(Undefined)"
    } else {
      x@name
    },
    "\n",
    fmt("System Prompt: ", bold = TRUE, pad = 2L, output_type = output_type),
    if (is.null(x@system_prompt)) {
      "(Undefined)"
    } else {
      fmt(
        paste0(
          substr(x@system_prompt, 1, 60),
          if (nchar(x@system_prompt) > 60) {
            "..."
          } else {
            ""
          }
        ),
        output_type = output_type
      )
    },
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
                  tool@function_name,
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
    }, # / if tools
    if (!is.null(x@output_schema)) {
      paste0(
        fmt(" Output Schema: \n", bold = TRUE, output_type = output_type),
        repr_ls(x@output_schema, pad = 2L, output_type = output_type)
      )
    },
    # llmconfig
    fmt("    llmconfig:\n", bold = TRUE, pad = 2L, output_type = output_type),
    repr(x@llmconfig, pad = 17L, output_type = output_type)
  ) #/ paste0
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
#' @return List with agent response.
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

  # {Request Body}
  request_body <- list(
    model = x@llmconfig@model_name,
    messages = messages,
    stream = FALSE,
    options = list(
      temperature = x@llmconfig@temperature
    )
  )

  ## Output schema
  if (!is.null(output_schema)) {
    request_body[["format"]] <- output_schema
  }

  ## Tools
  if (!is.null(x@tools) && use_tools) {
    request_body[["tools"]] <- lapply(x@tools, as_list)
  }

  if (verbosity > 0) {
    output_type <- get_output_type()
    msg(repr_bracket(x@llmconfig@model_name), "working...")
  }

  # {Perform request}
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

  # Echo response
  if (echo) {
    if (!is.null(res[["message"]][["thinking"]])) {
      cat(repr_bracket(x@name %||% "Agent"), "reasoning:\n")
      cat(res[["message"]][["thinking"]], "\n")
    }
    cat(repr_bracket(x@name %||% "Agent"), "response:\n")
    cat(res[["message"]][["content"]], "\n")
  }

  # Update state
  if (update_state) {
    # Append response to messages
    x@state[["messages"]][[length(x@state[["messages"]]) + 1]] <- res[[
      "message"
    ]]
    if (verbosity > 1L) {
      msg(
        "Agent state updated. Total messages in memory:",
        length(x@state[["messages"]])
      )
    }
  } # /update state

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
      # Check that tool exists in agent's tool list and in allowed tool_DB
      if (
        is.null(x@tools) ||
          !(tool_names[i] %in% sapply(x@tools, function(t) t@function_name)) ||
          !(tool_names[i] %in% tool_DB[["name"]])
      ) {
        # Report security incident
        report_agent_unauthorized_tool(
          agent = x,
          issue = "Unauthorized tool request",
          tool_requested = tool_names[i]
        )
        cli::cli_abort(
          "Agent requested tool '{tool_names[i]}' which is not in the agent's tool list.",
          "This incident has been reported."
        )
      }
      # Validate tool function: Throws error if hash does not match
      validate_function(tool_names[i])
      # Call tool
      if (verbosity > 0L) {
        msg("Invoking tool:", highlight(tool_names[i]))
      }
      args <- res[["message"]][["tool_calls"]][[i]][["function"]][["arguments"]]
      tool_responses[[i]] <- do.call(tool_names[i], args)
      if (verbosity > 0L) {
        msg("Tool", highlight(tool_names[i]), "returned response.")
      }
    } # /for each tool
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


# --- Public API -----------------------------------------------------------------------------------
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
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  use_memory = TRUE,
  tools = NULL,
  output_schema = NULL,
  name = NULL
) {
  Agent(
    llmconfig = llmconfig,
    system_prompt = system_prompt,
    use_memory = use_memory,
    tools = tools,
    output_schema = output_schema,
    name = name
  )
} # /kaimana::agent
