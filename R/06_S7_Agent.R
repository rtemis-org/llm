# Agent.R
# ::kaimana::
# 2025 EDG rtemis.org

# References:
# Chat endpoint: https://docs.ollama.com/api/chat
# Tool calling: https://docs.ollama.com/capabilities/tool-calling#tool-calling
# Thinking: https://docs.ollama.com/capabilities/thinking#enable-thinking-in-api-calls

# --- Internal API ---------------------------------------------------------------------------------
# %% Agent Class ----
#' @title agent
#'
#' @description
#' Class for agents that support reasoning, tool use, structured output, and state management.
#'
#' @field llmconfig LLMConfig: The LLMConfig to use.
#' @field state AgentState: The state of the agent contains the message history and metadata.
#' @field system_prompt Optional character: The system prompt to use.
#' @field use_memory Logical: Whether to store conversation history in agent state.
#' @field tools Optional list of Tool objects: The tools available to the agent.
#' @field max_tool_rounds Integer: Maximum number of tool call rounds per query.
#' @field output_schema Optional list: The output schema to enforce on the agent's response
#' @field name Optional character: The name of the agent.
#'
#' @author EDG
#' @noRd
Agent <- new_class(
  "Agent",
  properties = list(
    llmconfig = LLMConfig,
    state = AgentState,
    system_prompt = new_union(NULL | class_character),
    use_memory = class_logical,
    tools = new_union(NULL | class_list),
    max_tool_rounds = class_integer,
    output_schema = new_union(NULL | class_list),
    name = new_union(NULL | class_character)
  ),
  constructor = function(
    llmconfig,
    state = InMemoryAgentState(),
    system_prompt = NULL,
    use_memory = TRUE,
    tools = NULL,
    max_tool_rounds = 3L,
    output_schema = NULL,
    name = NULL
  ) {
    # Initialize messages with system prompt within state environment
    if (!is.null(system_prompt)) {
      append_message(
        state,
        SystemMessage(
          name = name,
          content = system_prompt
        )
      )
    }
    new_object(
      S7_object(),
      llmconfig = llmconfig,
      state = state,
      system_prompt = system_prompt,
      use_memory = use_memory,
      tools = tools,
      max_tool_rounds = max_tool_rounds,
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
        if (!tool@function_name %in% tool_DB[["function_name"]]) {
          cli::cli_abort(
            "Tool '{tool@function_name}' is not part of the allowed tool set."
          )
        }
      }
    }
  }
) # /kaimana::Agent


# %% repr.Agent ----
method(repr, Agent) <- function(x, pad = 0L, output_type = NULL) {
  if (is.null(output_type)) {
    output_type <- get_output_type()
  }
  out <- paste0(
    repr_S7name("Agent", output_type = output_type),
    fmt("         Name: ", bold = TRUE, pad = pad, output_type = output_type),
    if (is.null(x@name)) {
      "(Undefined)"
    } else {
      x@name
    },
    "\n",
    fmt("System Prompt: ", bold = TRUE, pad = pad, output_type = output_type),
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
    fmt("       Memory: ", bold = TRUE, pad = pad, output_type = output_type),
    fmt(
      if (x@use_memory) "Enabled" else "Disabled",
      output_type = output_type
    ),
    "\n",
    fmt("        Tools: ", bold = TRUE, pad = pad, output_type = output_type),
    if (is.null(x@tools)) {
      "(None)\n"
    } else {
      paste0(
        paste0(
          "\n",
          sapply(
            x@tools,
            function(tool) {
              paste0(
                "               - ",
                fmt(tool@function_name, bold = TRUE, output_type = output_type),
                ": ",
                tool@description
              )
            }
          ),
          collapse = ""
        ),
        "\n"
      )
    }, # / if tools
    if (!is.null(x@output_schema)) {
      paste0(
        fmt(" Output Schema: \n", bold = TRUE, output_type = output_type),
        repr_ls(x@output_schema, pad = pad, output_type = output_type)
      )
    },
    # llmconfig
    fmt("   LLM Config:\n", bold = TRUE, pad = pad, output_type = output_type),
    repr(x@llmconfig, pad = 15L, output_type = output_type)
  ) #/ paste0
} # /kaimana::repr.Agent


# %% print.Agent ----
method(print, Agent) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
} # /kaimana::print.Agent


# %% create_llm_message.Agent ----
#' create_llm_message method for `Agent`
#'
#' @param x `Agent` object
#' @param content Character: The content of the message.
#' @param reasoning Optional character: The reasoning trace.
#' @param tool_calls Optional list: Tool call information.
#'
#' @return An `LLMMessage` object.
#'
#' @author EDG
#' @noRd
method(create_llm_message, Agent) <- function(
  x,
  content,
  reasoning = NULL,
  tool_calls = NULL
) {
  create_llm_message(
    x@llmconfig,
    content = content,
    reasoning = reasoning,
    tool_calls = tool_calls,
    name = x@name
  )
} # /kaimana::create_llm_message.Agent


# %% create_llm_message.OllamaConfig ----
# Needs to follow Agent definition
#' create_llm_message method for Agent with Ollama backend
#'
#' @param x OllamaConfig object
#' @param content Character: The content of the message.
#' @param reasoning Optional character: The reasoning trace.
#' @param tool_calls Optional list: Tool call information.
#'
#' @return An OllamaMessage object.
#'
#' @author EDG
#' @noRd
method(create_llm_message, OllamaConfig) <- function(
  x,
  content,
  reasoning = NULL,
  tool_calls = NULL,
  name = NULL
) {
  OllamaMessage(
    content = content,
    name = name,
    metadata = list(),
    model_name = x@model_name,
    reasoning = reasoning,
    tool_calls = tool_calls
  )
} # /kaimana::create_llm_message.OllamaConfig


# %% get_messages.Agent ----
#' Get messages from Agent
#'
#' Get list of Message objects from the Agent's state
#'
#' @param x Agent object
#'
#' @return List of Message objects
#'
#' @author EDG
#' @noRd
method(get_messages, Agent) <- function(x) {
  get_messages(x@state)
} # /kaimana::get_messages.Agent


# --- Public API -----------------------------------------------------------------------------------
# %% create_agent() ----
#' Create an `Agent`
#'
#' @param llmconfig `LLMConfig`: The LLM configuration to use.
#' @param system_prompt Optional character: The system prompt to use.
#' @param use_memory Logical: Whether to use conversation memory.
#' @param tools Optional list of Tool objects: The tools available to the agent.
#' @param max_tool_rounds Integer: Maximum number of tool call rounds per query.
#' @param output_schema Optional list: The output schema to enforce on the agent's response created
#' using [schema].
#' @param name Optional character: The name of the agent.
#'
#' @return `Agent` object
#'
#' @author EDG
#' @export
create_agent <- function(
  llmconfig,
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  use_memory = TRUE,
  tools = NULL,
  max_tool_rounds = 3L,
  output_schema = NULL,
  name = NULL
) {
  Agent(
    llmconfig = llmconfig,
    system_prompt = system_prompt,
    use_memory = use_memory,
    tools = tools,
    max_tool_rounds = max_tool_rounds,
    output_schema = output_schema,
    name = name
  )
} # /kaimana::create_agent


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
#' @return List of `Message` objects representing the conversation history.
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
  # If not updating state, create a temporary InMemoryAgentState for this interaction only
  if (!update_state) {
    .tempState <- InMemoryAgentState()
    # Append system prompt
    append_message(
      .tempState,
      SystemMessage(
        name = x@name,
        content = x@system_prompt
      )
    )
  }
  running_state <- if (update_state) x@state else .tempState
  # {++} State: Append initial InputMessage
  append_message(
    running_state,
    InputMessage(
      content = prompt,
      image_path = image_path
    )
  )

  # {Initial Request Body}
  request_body <- list(
    model = x@llmconfig@model_name,
    messages = get_message_list(running_state),
    stream = FALSE,
    options = list(
      temperature = x@llmconfig@temperature
    )
  )

  ## Add Output schema
  if (!is.null(output_schema)) {
    request_body[["format"]] <- output_schema
  }

  ## Add Tools
  # "required" field must be list, even if empty or single tool (handled by as_list.Tool)
  if (!is.null(x@tools) && use_tools) {
    request_body[["tools"]] <- lapply(x@tools, as_list)
  }

  if (verbosity > 0L) {
    msg(repr_bracket(x@llmconfig@model_name), "working...")
  }

  # {>>} Perform initial request
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

  # {<<} Initial response
  res <- httr2::resp_body_json(resp)

  # Latest ollama native API return a separate "thinking" field for reasoning traces when
  # using a model that supports it.
  # Echo response
  if (echo) {
    if (!is.null(res[["message"]][["thinking"]])) {
      cat(repr_bracket(x@name %||% "Agent"), fmt("reasoning\n", bold = TRUE))
      cat(res[["message"]][["thinking"]], "\n")
    }
    cat(repr_bracket(x@name %||% "Agent"), fmt("response\n", bold = TRUE))
    cat(res[["message"]][["content"]], "\n")
  }

  # {++} Append initial response
  append_message(
    running_state,
    create_llm_message(
      x,
      content = res[["message"]][["content"]],
      reasoning = res[["message"]][["thinking"]],
      tool_calls = res[["message"]][["tool_calls"]]
    ),
    verbosity = verbosity
  )

  n_completed_tool_rounds <- 0L
  while (n_completed_tool_rounds < x@max_tool_rounds) {
    # {?>_} Check for tool calls
    if (!is.null(res[["message"]][["tool_calls"]])) {
      n_completed_tool_rounds <- n_completed_tool_rounds + 1L
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

      # Initialize tool responses
      tool_responses <- structure(
        vector("list", n_tool_calls),
        names = tool_names
      )

      # Call each tool
      # (how likely is it that an agent will call multiple tools at once rather than one at a time?)
      for (i in seq_along(tool_responses)) {
        # {/\} Check that tool exists in agent's tool list and in allowed tool_DB
        if (
          is.null(x@tools) ||
            !(tool_names[i] %in%
              sapply(x@tools, function(t) t@function_name)) ||
            !(tool_names[i] %in% tool_DB[["function_name"]])
        ) {
          # {!!} Report security incident
          report_agent_unauthorized_tool(
            agent = x,
            issue = "Unauthorized tool request",
            tool_requested = tool_names[i]
          )
          cli::cli_abort(c(
            "Agent requested tool '{.val {tool_names[i]}}' which is not in the agent's tool list.",
            i = "This incident has been reported."
          ))
        }
        # {/\!} Validate tool function: Throws error if hash does not match
        validate_function(tool_names[i])

        # {>_} Call tool
        if (verbosity > 0L) {
          msg("Invoking tool:", highlight(tool_names[i]))
        }
        args <- res[["message"]][["tool_calls"]][[i]][["function"]][[
          "arguments"
        ]]
        fn <- get(tool_names[i], envir = asNamespace("kaimana"))
        # Force output_type = "json" where supported
        if ("output_type" %in% names(formals(fn))) {
          args[["output_type"]] <- "json"
        }
        tool_responses[[i]] <- do.call(fn, args)
        if (verbosity > 0L) {
          msg("Tool", highlight(tool_names[i]), "returned response.")
        }
      } # /for each tool
      #

      # {++} Append ToolMessages to state
      for (i in seq_along(tool_responses)) {
        append_message(
          running_state,
          ToolMessage(
            name = tool_names[i],
            content = tool_responses[[i]]
          ),
          verbosity = verbosity
        )
      }

      # Prepare AgentMessage with tool responses
      tool_response_prompt <- paste0(
        "The following tool responses were obtained:\n",
        paste0(
          sapply(
            names(tool_responses),
            function(tn) {
              paste0(
                "Tool '",
                tn,
                "' response:\n\n",
                tool_responses[[tn]],
                # jsonlite::toJSON(
                #   tool_responses[[tn]],
                #   auto_unbox = TRUE,
                #   pretty = TRUE
                # ),
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
        if (x@max_tool_rounds - n_completed_tool_rounds > 0L) {
          paste0(
            " If you do not have sufficient information, you may use up to ",
            x@max_tool_rounds - n_completed_tool_rounds,
            " more tool call",
            ngettext(
              x@max_tool_rounds - n_completed_tool_rounds,
              " round",
              " rounds"
            ),
            ", if necessary."
          )
        } else {
          paste(
            "If you do not have sufficient information to answer the original query, say so",
            "clearly and suggest alternative ways to obtain the information."
          )
        },
        " Remember the original query you are answering:\n\n",
        '"',
        prompt,
        '"\n'
      )

      # {++} Append AgentMessage with tool responses
      append_message(
        running_state,
        AgentMessage(
          content = tool_response_prompt
        ),
        verbosity = verbosity
      )

      # Now, send another prompt to the agent with the tool responses
      if (verbosity > 0L) {
        msg("Sending tool responses back to agent...")
      }
      followup_request_body <- list(
        model = x@llmconfig@model_name,
        messages = get_message_list(running_state), # Removes ToolMessages, AgentMessages are sent as role = "user"
        stream = FALSE,
        options = list(
          temperature = x@llmconfig@temperature
        )
      )
      # Output schema
      if (!is.null(output_schema)) {
        followup_request_body[["format"]] <- output_schema
      }
      # {>>} Perform follow-up request
      followup_resp <- httr2::request(paste0(
        x@llmconfig@base_url,
        "/api/chat"
      )) |>
        httr2::req_body_json(followup_request_body) |>
        httr2::req_user_agent("kaimana-r Agent (kaimana.rtemis.org)") |>
        httr2::req_perform(verbosity = verbosity - 1L)
      # Check for errors
      httr2::resp_check_status(followup_resp)
      if (verbosity > 0) {
        # Replace working message with done
        msg(repr_bracket(x@llmconfig@model_name), "done.")
      }
      # {<<} Follow-up response
      res <- httr2::resp_body_json(followup_resp)

      # {++} Append response to messages as LLMMessage
      append_message(
        running_state,
        create_llm_message(
          x,
          content = res[["message"]][["content"]],
          reasoning = res[["message"]][["thinking"]],
          tool_calls = res[["message"]][["tool_calls"]]
        ),
        verbosity = verbosity
      )
    } else {
      # No tool calls, break
      break
    }
  } # /while max_tool_rounds < max_tool_rounds
  get_messages(running_state)
} # /kaimana::generate.Agent
