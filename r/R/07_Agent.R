# Agent.R
# ::rtemis.llm::
# 2025 EDG rtemis.org

# References:
# Chat endpoint: https://docs.ollama.com/api/chat
# Tool calling: https://docs.ollama.com/capabilities/tool-calling#tool-calling
# Thinking: https://docs.ollama.com/capabilities/thinking#enable-thinking-in-api-calls

# --- Internal API ---------------------------------------------------------------------------------
# %% Agent ----
#' @title agent
#'
#' @description
#' Class for agents that support reasoning, tool use, structured output, and state management.
#'
#' @field llmconfig LLMConfig: The LLMConfig to use.
#' @field state AgentMemory: The state of the agent contains the message history and metadata.
#' @field system_prompt Optional character: The system prompt to use.
#' @field use_memory Logical: Whether to store conversation history in agent state.
#' @field tools Optional list of Tool objects: The tools available to the agent.
#' @field max_tool_rounds Integer: Maximum number of tool call rounds per query.
#' @field output_schema Optional Schema: The output schema to enforce on the agent's response.
#' @field name Optional character: The name of the agent.
#'
#' @author EDG
#' @noRd
Agent <- new_class(
  "Agent",
  properties = list(
    llmconfig = LLMConfig,
    state = AgentMemory,
    system_prompt = optional(S7::class_character),
    use_memory = class_logical,
    tools = optional(S7::class_list),
    max_tool_rounds = class_integer,
    output_schema = optional(Schema),
    name = optional(S7::class_character)
  ),
  constructor = function(
    llmconfig,
    state = InProcessAgentMemory(),
    system_prompt = NULL,
    use_memory = TRUE,
    tools = NULL,
    max_tool_rounds = 3L,
    output_schema = NULL,
    name = NULL,
    verbosity = 1L
  ) {
    # Initialize messages with system prompt within state environment
    if (!is.null(system_prompt)) {
      append_message(
        state,
        SystemMessage(
          name = name,
          content = system_prompt
        ),
        verbosity = verbosity - 1L
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
)


# %% repr.Agent ----
method(repr, Agent) <- function(x, pad = 0L, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
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
        fmt("Output Schema: \n", bold = TRUE, output_type = output_type),
        repr_ls(
          as_list(x@output_schema),
          pad = pad + 15L,
          output_type = output_type,
          limit = 20L
        )
      )
    },
    # llmconfig
    fmt("   LLM Config:\n", bold = TRUE, pad = pad, output_type = output_type),
    repr(x@llmconfig, pad = pad + 15L, output_type = output_type)
  ) # / paste0
}


# %% print.Agent ----
method(print, Agent) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
}


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
  tool_calls = NULL,
  metadata = list()
) {
  create_llm_message(
    x@llmconfig,
    content = content,
    reasoning = reasoning,
    tool_calls = tool_calls,
    metadata = metadata,
    name = x@name
  )
}


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
  name = NULL,
  metadata = list()
) {
  OllamaMessage(
    content = content,
    name = name,
    metadata = metadata,
    model_name = x@model_name,
    reasoning = reasoning,
    tool_calls = tool_calls
  )
}


# %% create_llm_message.OpenAIConfig ----
#' create_llm_message method for Agent with OpenAI-compatible backend
#'
#' @param x OpenAIConfig object.
#' @param content Character: The content of the message.
#' @param reasoning Optional character: The reasoning trace.
#' @param tool_calls Optional list: Tool call information.
#' @param name Optional character: The agent name.
#' @param metadata List: Message metadata.
#'
#' @return An OpenAIMessage object.
#'
#' @author EDG
#' @noRd
method(create_llm_message, OpenAIConfig) <- function(
  x,
  content,
  reasoning = NULL,
  tool_calls = NULL,
  name = NULL,
  metadata = list()
) {
  OpenAIMessage(
    content = content,
    name = name,
    metadata = metadata,
    model_name = x@model_name,
    reasoning = reasoning,
    tool_calls = tool_calls,
    provider = .openai_provider_name(x)
  )
}


# %% create_llm_message.ClaudeConfig ----
#' create_llm_message method for Agent with Claude backend
#'
#' @param x ClaudeConfig object.
#' @param content Character: The content of the message.
#' @param reasoning Optional character: The reasoning trace.
#' @param tool_calls Optional list: Tool call information.
#' @param name Optional character: The agent name.
#' @param metadata List: Message metadata.
#'
#' @return A ClaudeMessage object.
#'
#' @author EDG
#' @noRd
method(create_llm_message, ClaudeConfig) <- function(
  x,
  content,
  reasoning = NULL,
  tool_calls = NULL,
  name = NULL,
  metadata = list()
) {
  ClaudeMessage(
    content = content,
    name = name,
    metadata = metadata,
    model_name = x@model_name,
    reasoning = reasoning,
    tool_calls = tool_calls
  )
}


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
method(get_messages, Agent) <- function(x, last = FALSE) {
  get_messages(x@state, last = last)
}


# --- Public API -----------------------------------------------------------------------------------
# %% create_agent ----
#' Create a rtemis.llm Agent
#'
#' @param llmconfig `LLMConfig`: The LLM configuration to use.
#' @param system_prompt Optional character: The system prompt to use.
#' @param use_memory Logical: Whether to use conversation memory.
#' @param tools Optional list of Tool objects: The tools available to the agent.
#' @param max_tool_rounds Integer: Maximum number of tool call rounds per query.
#' @param output_schema Optional Schema: The output schema to enforce on the agent's response created
#' using [schema].
#' @param name Optional character: The name of the agent.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `Agent` object
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires Ollama server running and gemma4:e4b model available
#' \dontrun{
#'   agent <- create_agent(
#'     config_Ollama(
#'       model_name = "gemma4:e4b",
#'       temperature = 0.2
#'     ),
#'     system_prompt = "You are professor of Trance at the Institute of Advanced Beat Studies.",
#'     use_memory = TRUE
#'   )
#' }
create_agent <- function(
  llmconfig,
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  use_memory = TRUE,
  tools = NULL,
  max_tool_rounds = 3L,
  output_schema = NULL,
  name = NULL,
  verbosity = 1L
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
}


# %% generate.Agent ----
# Reference: https://docs.ollama.com/api/chat
#' Generate method for rtemis.llm Agent
#'
#' @param x `Agent` object
#' @param prompt Character: The prompt to send to the agent.
#' @param temperature Optional numeric \[0, 2\]: Per-call temperature override.
#' @param top_p Optional numeric \[0, 1\]: Nucleus sampling cutoff.
#' @param max_tokens Optional integer \[1, Inf): Per-call maximum tokens to generate.
#' @param stop Optional character: Stop sequence(s).
#' @param image_path Optional character: Path to an image to include in the prompt.
#' @param think Optional logical: Whether to enable thinking (reasoning trace) for this call. Only
#' supported by certain models.
#' @param output_schema Optional Schema: The output schema to enforce on the agent's response.
#' Important: if NULL, the agent's default output_schema, if defined, will be used. This means that
#' the generate call's schema takes precedence over the agent's schema.
#' @param commit_to_memory Logical: Whether to commit this interaction to the agent's memory.
#' @param use_tools Logical: Whether to allow the agent to use tools.
#' @param echo Logical: Whether to echo the prompt and response.
#' @param verbosity Integer: Verbosity level.
#' @param ... Backend-specific per-call options forwarded to the request builder
#' (e.g. `top_k`, `seed`). See [generate].
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
  temperature = NULL,
  top_p = NULL,
  max_tokens = NULL,
  stop = NULL,
  image_path = NULL,
  think = NULL,
  output_schema = NULL,
  commit_to_memory = TRUE,
  use_tools = TRUE,
  echo = TRUE,
  verbosity = 1L,
  ...
) {
  # Get output schema: First check function argument, then agent's default
  if (is.null(output_schema)) {
    output_schema <- x@output_schema
  }
  # Check input
  check_inherits(prompt, "character")
  update_state <- x@use_memory && commit_to_memory
  # If not updating state, create a temporary InProcessAgentMemory for this interaction only
  if (!update_state) {
    .tempState <- InProcessAgentMemory()
    # Append system prompt
    append_message(
      .tempState,
      SystemMessage(
        name = x@name,
        content = x@system_prompt
      ),
      echo = echo,
      verbosity = verbosity
    )
  }
  running_state <- if (update_state) x@state else .tempState
  # {++} State: Append initial InputMessage
  append_message(
    running_state,
    InputMessage(
      content = prompt,
      image_path = image_path
    ),
    echo = echo,
    verbosity = verbosity
  )

  # Per-call overrides forwarded to every build_chat_request_body call
  overrides <- list(
    temperature = temperature,
    top_p = top_p,
    max_tokens = max_tokens,
    stop = stop
  )
  extra <- list(...)
  overrides <- c(overrides, extra)

  # {Initial Request Body}
  request_body <- do.call(
    build_chat_request_body,
    c(
      list(
        x@llmconfig,
        state = running_state,
        tools = x@tools,
        output_schema = output_schema,
        think = think,
        use_tools = use_tools
      ),
      overrides
    )
  )

  msg(repr_bracket(x@llmconfig@model_name), "working...", verbosity = verbosity)

  # {>>} Perform initial request
  resp <- perform_chat_request(
    x@llmconfig,
    request_body = request_body,
    verbosity = verbosity
  )
  # Replace working message with done
  msg(repr_bracket(x@llmconfig@model_name), "done.", verbosity = verbosity)

  # {<<} Initial response
  res <- parse_chat_response(x@llmconfig, resp)

  # {++} Append initial response
  append_message(
    running_state,
    create_llm_message(
      x,
      content = res[["content"]],
      reasoning = res[["reasoning"]],
      tool_calls = res[["tool_calls"]],
      metadata = res[["metadata"]]
    ),
    echo = echo,
    verbosity = verbosity
  )

  n_completed_tool_rounds <- 0L
  while (n_completed_tool_rounds < x@max_tool_rounds) {
    # {?>_} Check for tool calls
    if (!is.null(res[["tool_calls"]]) && length(res[["tool_calls"]]) > 0L) {
      n_completed_tool_rounds <- n_completed_tool_rounds + 1L
      tool_names <- sapply(
        res[["tool_calls"]],
        function(tc) tc[["function"]][["name"]]
      )
      n_tool_calls <- length(tool_names)
      msg(
        "Agent wants to perform",
        n_tool_calls,
        ngettext(n_tool_calls, "tool call.", " tool calls."),
        verbosity = verbosity
      )

      # Initialize tool responses
      tool_responses <- structure(
        vector("list", n_tool_calls),
        names = tool_names
      )

      # Call each tool
      # (how likely is it that an agent will call multiple tools at once rather than one at a time?)
      for (i in seq_along(tool_responses)) {
        # {/\} Check that tool requested by LLM exists in agent's tool list
        # Note that Agent validator already checks that all tools in agent's tool list are allowed.
        if (
          !use_tools ||
            is.null(x@tools) ||
            !(tool_names[i] %in% sapply(x@tools, function(t) t@function_name))
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
        msg("Invoking tool:", highlight(tool_names[i]), verbosity = verbosity)
        tool_call <- res[["tool_calls"]][[i]]
        args <- decode_tool_arguments(x@llmconfig, tool_call)
        if (!.is_named_list(args)) {
          cli::cli_abort(c(
            "Tool arguments for {.val {tool_names[i]}} must be a named list.",
            i = "Check the tool schema and model tool-call response."
          ))
        }
        fn <- get(tool_names[i], envir = asNamespace("rtemis.llm"))
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
          build_tool_message(
            x@llmconfig,
            tool_call = res[["tool_calls"]][[i]],
            tool_name = tool_names[i],
            tool_response = tool_responses[[i]]
          ),
          echo = echo,
          verbosity = verbosity
        )
      }

      # Ollama does not consume native role = "tool" messages in this package's current flow.
      # Preserve its existing behavior by summarizing tool results as a user-role AgentMessage.
      if (S7_inherits(x@llmconfig, OllamaConfig)) {
        tool_response_prompt <- paste0(
          sapply(
            names(tool_responses),
            function(tn) {
              paste0(
                "Tool '",
                tn,
                "' response:\n\n",
                .tool_response_to_character(tool_responses[[tn]]),
                "\n"
              )
            }
          ),
          collapse = "\n"
        )
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
        append_message(
          running_state,
          AgentMessage(
            content = tool_response_prompt
          ),
          echo = echo,
          verbosity = verbosity
        )
      }

      # Now, send another prompt to the agent with the tool responses
      if (verbosity > 0L) {
        msg("Sending tool responses back to agent...")
      }
      followup_request_body <- do.call(
        build_chat_request_body,
        c(
          list(
            x@llmconfig,
            state = running_state,
            tools = x@tools,
            output_schema = output_schema,
            use_tools = use_tools
          ),
          overrides
        )
      )
      # {>>} Perform follow-up request
      followup_resp <- perform_chat_request(
        x@llmconfig,
        request_body = followup_request_body,
        verbosity = verbosity
      )
      # Replace working message with done
      msg(repr_bracket(x@llmconfig@model_name), "done.", verbosity = verbosity)
      # {<<} Follow-up response
      res <- parse_chat_response(x@llmconfig, followup_resp)

      # {++} Append response to messages as LLMMessage
      append_message(
        running_state,
        create_llm_message(
          x,
          content = res[["content"]],
          reasoning = res[["reasoning"]],
          tool_calls = res[["tool_calls"]],
          metadata = res[["metadata"]]
        ),
        echo = echo,
        verbosity = verbosity
      )
    } else {
      # No tool calls, break
      break
    }
  } # /while max_tool_rounds < max_tool_rounds
  get_messages(running_state)
}
