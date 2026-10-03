# test_chat.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# The loop takes its reader as an argument, so these feed scripted lines to a
# real Agent whose requests are answered by a mocked `perform_chat_request`.

chat_test_config <- function() {
  config_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    validate_model = FALSE
  )
}

chat_test_agent <- function(use_memory = TRUE) {
  create_agent(
    chat_test_config(),
    system_prompt = "You are terse.",
    use_memory = use_memory
  )
}

# A reader that returns `lines` in order, then "/exit".
scripted_reader <- function(lines) {
  i <- 0L
  function(prompt) {
    i <<- i + 1L
    if (i > length(lines)) "/exit" else lines[[i]]
  }
}

openai_reply <- function(content) {
  httr2::response_json(
    body = list(
      choices = list(
        list(
          message = list(role = "assistant", content = content),
          finish_reason = "stop"
        )
      )
    )
  )
}

# Mock the request: answer "Reply <n>" and record every request body.
local_mock_chat <- function(env = parent.frame(), fail_on = integer()) {
  calls <- new.env()
  calls$bodies <- list()
  testthat::local_mocked_bindings(
    perform_chat_request = function(x, request_body, verbosity = 1L) {
      n <- length(calls$bodies) + 1L
      calls$bodies[[n]] <- request_body
      if (n %in% fail_on) {
        stop("server unavailable")
      }
      openai_reply(paste("Reply", n))
    },
    .package = "rtemis.llm",
    .env = env
  )
  calls
}

run_chat <- function(agent, lines, ...) {
  out <- NULL
  printed <- utils::capture.output(
    out <- .chat_loop(agent, read_line = scripted_reader(lines), ...)
  )
  list(agent = out, printed = printed)
}

message_classes <- function(agent) {
  vapply(
    agent@state@state[["messages"]],
    function(m) sub(".*::", "", class(m)[1]),
    character(1)
  )
}


# %% chat loop ----
test_that("chat carries the conversation across turns and returns the Agent", {
  calls <- local_mock_chat()
  agent <- chat_test_agent()
  res <- run_chat(agent, c("Hello", "", "And again"), verbosity = 0L)
  expect_identical(res$agent, agent)
  expect_equal(
    message_classes(agent),
    c(
      "SystemMessage",
      "InputMessage",
      "OpenAIMessage",
      "InputMessage",
      "OpenAIMessage"
    )
  )
  # The blank line is not sent; the second request carries the first turn.
  expect_length(calls$bodies, 2L)
  expect_length(calls$bodies[[2]][["messages"]], 4L)
  expect_true(any(grepl("Reply 1", res$printed)))
  expect_true(any(grepl("Reply 2", res$printed)))
})


test_that("per-call arguments reach every request", {
  calls <- local_mock_chat()
  run_chat(chat_test_agent(), c("a", "b"), verbosity = 0L, temperature = 1.5)
  expect_equal(
    vapply(calls$bodies, function(b) b[["temperature"]], numeric(1)),
    c(1.5, 1.5)
  )
})


test_that("/clear keeps only the system prompt", {
  calls <- local_mock_chat()
  agent <- chat_test_agent()
  res <- run_chat(agent, c("Hello", "/clear", "Hi again"), verbosity = 0L)
  expect_equal(
    message_classes(agent),
    c("SystemMessage", "InputMessage", "OpenAIMessage")
  )
  expect_length(calls$bodies[[2]][["messages"]], 2L)
  expect_true(any(grepl("cleared", res$printed)))
})


test_that("a failed request discards the turn and the chat continues", {
  calls <- local_mock_chat(fail_on = 1L)
  agent <- chat_test_agent()
  res <- run_chat(agent, c("First", "Second"), verbosity = 0L)
  expect_true(any(grepl("server unavailable", res$printed)))
  expect_equal(
    message_classes(agent),
    c("SystemMessage", "InputMessage", "OpenAIMessage")
  )
  expect_equal(agent@state@state[["messages"]][[2]]@content, "Second")
})


test_that("an interrupt during a reply discards the turn", {
  testthat::local_mocked_bindings(
    perform_chat_request = function(x, request_body, verbosity = 1L) {
      signalCondition(
        structure(
          class = c("interrupt", "condition"),
          list(message = "", call = NULL)
        )
      )
    },
    .package = "rtemis.llm"
  )
  agent <- chat_test_agent()
  res <- run_chat(agent, "Hello", verbosity = 0L)
  expect_true(any(grepl("Interrupted", res$printed)))
  expect_equal(message_classes(agent), "SystemMessage")
})


test_that("an interrupt at the prompt ends the chat", {
  calls <- local_mock_chat()
  agent <- chat_test_agent()
  reader <- function(prompt) {
    signalCondition(
      structure(
        class = c("interrupt", "condition"),
        list(message = "", call = NULL)
      )
    )
    "never sent"
  }
  utils::capture.output(
    out <- .chat_loop(agent, read_line = reader, verbosity = 0L)
  )
  expect_identical(out, agent)
  expect_length(calls$bodies, 0L)
})


test_that("unknown commands and /help are not sent to the model", {
  calls <- local_mock_chat()
  res <- run_chat(chat_test_agent(), c("/help", "/nope"), verbosity = 0L)
  expect_length(calls$bodies, 0L)
  expect_true(any(grepl("/image <path>", res$printed, fixed = TRUE)))
  expect_true(any(grepl("Unknown command /nope", res$printed, fixed = TRUE)))
})


test_that("/image attaches images to the next message only", {
  calls <- local_mock_chat()
  agent <- chat_test_agent()
  png <- test_path("fixtures", "red.png")
  gif <- test_path("fixtures", "pixel.gif")
  res <- run_chat(
    agent,
    c(
      paste("/image", png),
      paste("/image", gif),
      "/image no/such/file.png",
      "What colors?",
      "And now?"
    ),
    verbosity = 0L
  )
  expect_true(any(grepl("Image file not found", res$printed)))
  messages <- agent@state@state[["messages"]]
  expect_length(messages[[2]]@images, 2L)
  expect_null(messages[[4]]@images)
})


test_that("the banner and closing line follow verbosity", {
  local_mock_chat()
  res <- run_chat(chat_test_agent(), "Hi", verbosity = 1L)
  expect_true(any(grepl("/help for commands", res$printed)))
  expect_true(any(grepl("Chat ended with 3 messages", res$printed)))
  quiet <- run_chat(chat_test_agent(), character(), verbosity = 0L)
  expect_length(quiet$printed, 0L)
})


# %% chat agent ----
test_that("an LLM is wrapped in an Agent with memory", {
  schema_ <- schema(answer = field("string", "The answer"))
  llm <- create_OpenAI(
    model_name = "local-model",
    base_url = "http://localhost:1234/v1",
    system_prompt = "Be brief.",
    output_schema = schema_,
    name = "Bot"
  )
  agent <- .chat_agent(
    llm,
    backend = "ollama",
    system_prompt = SYSTEM_PROMPT_DEFAULT,
    tools = NULL,
    max_tool_rounds = 3L,
    call = quote(chat(llm)),
    verbosity = 0L
  )
  expect_true(S7_inherits(agent, Agent))
  expect_true(agent@use_memory)
  expect_identical(agent@llmconfig, llm@config)
  expect_equal(agent@system_prompt, "Be brief.")
  expect_equal(agent@name, "Bot")
  expect_identical(agent@output_schema, schema_)
})


test_that("an Agent is used as given, and must have memory", {
  agent <- chat_test_agent()
  expect_identical(
    .chat_agent(agent, "ollama", "", NULL, 3L, call = quote(chat(agent))),
    agent
  )
  expect_error(
    .chat_agent(
      chat_test_agent(use_memory = FALSE),
      "ollama",
      "",
      NULL,
      3L,
      call = quote(chat(agent))
    ),
    "without memory"
  )
  expect_error(
    .chat_agent(
      agent,
      "openai",
      "",
      NULL,
      3L,
      call = quote(chat(agent, backend = "openai"))
    ),
    "backend"
  )
})


test_that("chat rejects other inputs and non-interactive sessions", {
  expect_error(
    .chat_agent(42, "ollama", "", NULL, 3L, call = quote(chat(42))),
    "model name, an LLM, or an Agent"
  )
  # testthat runs non-interactively.
  expect_error(chat(chat_test_agent()), "interactive")
})
