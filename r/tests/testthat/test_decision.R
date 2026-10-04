# test_decision.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

triage_schema <- function() {
  schema(
    "Triage",
    field(
      "team",
      "Team to route to",
      enum = c("billing", "engineering", "sales")
    ),
    field("urgent", "Whether it needs an answer today", type = "boolean"),
    field("severity", "Severity", type = "integer", enum = as.character(1:5)),
    field(
      "channels",
      "Channels mentioned",
      type = "array",
      items = field("channel", enum = c("email", "phone", "chat"))
    ),
    field(
      "refund",
      "Whether a refund is requested",
      type = "boolean",
      required = FALSE
    )
  )
}


# %% is_decidable ----
test_that("is_decidable names every open field with its reason, in field order", {
  s <- schema(
    "Mixed",
    field("summary", "One-line summary"),
    field("score", type = "number"),
    field("triage", enum = c("low", "high")),
    field("count", type = "integer"),
    field("notes", type = "array", items = schema("Note", field("text")))
  )
  res <- is_decidable(s)
  expect_false(res)
  open <- attr(res, "open")
  expect_identical(open[["path"]], c("summary", "score", "count", "notes"))
  expect_identical(
    open[["reason"]],
    c(
      "free text",
      "a number",
      "an integer with no bounds, or more than 100 values",
      "a list of anything but closed values"
    )
  )
  expect_identical(attr(res, "questions"), NA_integer_)
})


test_that("is_decidable counts the first round's questions of a closed schema", {
  res <- is_decidable(triage_schema())
  expect_true(res)
  expect_identical(nrow(attr(res, "open")), 0L)
  # team, urgent, severity, 3 channels, refund? + refund
  expect_identical(attr(res, "questions"), 1L + 1L + 1L + 3L + 2L)
})


test_that("an array of a type name or of booleans is open", {
  for (items in c("string", "boolean")) {
    s <- schema("A", field("x", type = "array", items = items))
    expect_identical(
      attr(is_decidable(s), "open")[["reason"]],
      "a list of anything but closed values"
    )
  }
})


test_that("a schema with an open field is refused at construction, naming it", {
  s <- schema(
    "S",
    field("label", enum = c("a", "b")),
    field("rationale", required = FALSE)
  )
  expect_error(
    mock_decision_model(output_schema = s),
    "`rationale`: free text",
    fixed = TRUE
  )
  expect_error(create_DecisionModel(config_Ollama), "config_OllamaDecision")
})


# %% as_questions ----
test_that("as_questions returns the questions generate() asks, named by field", {
  qs <- as_questions(triage_schema())
  expect_identical(
    names(qs),
    c(
      "team",
      "urgent",
      "severity",
      "channels[email]",
      "channels[phone]",
      "channels[chat]",
      "refund?",
      "refund"
    )
  )
  expect_true(S7_inherits(qs[["team"]], Choice))
  expect_identical(
    names(qs[["team"]]@options),
    c("billing", "engineering", "sales")
  )
  expect_true(S7_inherits(qs[["refund?"]], Noul))
  # The same questions, word for word, that generate() sends.
  env <- new.env()
  httr2::local_mocked_responses(mock_systemone(env = env))
  generate(
    mock_decision_model(output_schema = triage_schema()),
    "x",
    verbosity = 0L
  )
  expect_identical(
    env[["calls"]][[1L]][["body"]][["questions"]],
    lapply(qs, as_list)
  )
})


test_that("as_questions refuses an open schema and an enum over 26 values", {
  expect_error(
    as_questions(schema("S", field("text"))),
    "`text`: free text",
    fixed = TRUE
  )
  big <- schema("S", field("pick", enum = sprintf("v%02d", 1:30)))
  expect_error(as_questions(big), "split the values")
})


test_that("edited questions from a schema can be asked with decide()", {
  qs <- as_questions(schema(
    "S",
    field("sentiment", enum = c("positive", "negative"))
  ))
  qs[["sentiment"]] <- choice(
    qs[["sentiment"]]@instructions,
    c(positive = "Approving", negative = "Critical")
  )
  env <- new.env()
  httr2::local_mocked_responses(mock_systemone(
    list(sentiment = "negative"),
    env = env
  ))
  d <- decide(mock_decision_model(), "Awful.", qs, verbosity = 0L)
  sent <- env[["calls"]][[1L]][["body"]][["questions"]][["sentiment"]][[
    "criteria"
  ]]
  expect_identical(sent, list(positive = "Approving", negative = "Critical"))
  expect_identical(d@answers[["sentiment"]][["choice"]], "negative")
})


# %% Questions ----
test_that("choice() names unnamed options by themselves and fills blank meanings", {
  q <- choice("Which?", c(a = "", b = "Bee"))
  expect_identical(q@options, c(a = "a", b = "Bee"))
  q <- choice("Which?", c("x", "y"))
  expect_identical(q@options, c(x = "x", y = "y"))
  expect_identical(
    as_list(q),
    list(
      type = "choice",
      instructions = "Which?",
      criteria = list(x = "x", y = "y")
    )
  )
  expect_error(choice("Which?", "x"), "2 to 26")
  expect_error(choice("Which?", as.character(1:27)), "2 to 26")
  expect_identical(
    as_list(noul("Is it?")),
    list(type = "noul", instructions = "Is it?")
  )
})


# %% The wire ----
test_that("decide sends the state after the context, and no images key without images", {
  env <- new.env()
  httr2::local_mocked_responses(mock_systemone(
    list(team = "billing"),
    env = env
  ))
  dm <- mock_decision_model(context = "You route support tickets.")
  d <- decide(
    dm,
    "Refund my invoice.",
    list(
      team = choice("Which team?", c("engineering", "billing")),
      refund = noul("Refund?")
    ),
    verbosity = 0L
  )
  body <- env[["calls"]][[1L]][["body"]]
  expect_identical(
    env[["calls"]][[1L]][["req"]][["url"]],
    "http://localhost:11434/v1/systemone"
  )
  expect_identical(body[["model"]], "clef-flash")
  expect_identical(
    body[["state"]],
    "You route support tickets.\n\nRefund my invoice."
  )
  expect_null(body[["images"]])
  expect_identical(d@answers[["team"]][["choice"]], "billing")
  expect_identical(d@answers[["refund"]][["p"]], 0.9)
  expect_identical(d@metadata[["calls"]], 1L)
  expect_identical(d@metadata[["usage"]][["input_tokens"]], 100)
  p <- probabilities(d)
  expect_identical(names(p), c("question", "option", "p", "chosen", "unsure"))
  expect_identical(p[question == "team" & chosen == TRUE, option], "billing")
  expect_identical(p[question == "refund", option], c("true", "false"))
})


test_that("decide sends images base64-encoded", {
  env <- new.env()
  httr2::local_mocked_responses(mock_systemone(env = env))
  dm <- mock_decision_model()
  decide(
    dm,
    "What color?",
    list(red = noul("Is it red?")),
    image_path = test_path("fixtures", "red.png"),
    verbosity = 0L
  )
  images <- env[["calls"]][[1L]][["body"]][["images"]]
  expect_length(images, 1L)
  expect_false(startsWith(images[[1L]], "data:"))
})


test_that("more than 64 questions are sent as several calls", {
  env <- new.env()
  httr2::local_mocked_responses(mock_systemone(env = env))
  qs <- stats::setNames(
    lapply(1:65, function(i) noul(paste("Is", i, "mentioned?"))),
    paste0("n", 1:65)
  )
  d <- decide(mock_decision_model(), "1 2 3", qs, verbosity = 0L)
  expect_length(env[["calls"]], 2L)
  expect_length(env[["calls"]][[1L]][["body"]][["questions"]], 64L)
  expect_length(d@answers, 65L)
  expect_identical(d@metadata[["calls"]], 2L)
})


test_that("a response with no model gets the requested one", {
  httr2::local_mocked_responses(mock_systemone(model = NULL))
  d <- decide(mock_decision_model(), "x", list(a = noul("A?")), verbosity = 0L)
  expect_identical(d@metadata[["models"]], "clef-flash")
})


test_that("a malformed answer aborts", {
  bad <- function(answer) {
    function(req) {
      httr2::response_json(body = list(model = "m", answers = list(a = answer)))
    }
  }
  dm <- mock_decision_model()
  q <- list(a = choice("Which?", c("x", "y")))
  httr2::local_mocked_responses(bad(list(type = "choice", choice = "z")))
  expect_error(decide(dm, "s", q, verbosity = 0L), "which was not an option")
  httr2::local_mocked_responses(bad(list(type = "score", value = 1)))
  expect_error(decide(dm, "s", q, verbosity = 0L), "an answer of type `score`")
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(model = "m"))
  })
  expect_error(decide(dm, "s", q, verbosity = 0L), "no `answers`")
})


test_that("a provider error is reported with its message", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(
      status_code = 400L,
      body = list(
        error = "question \"q1\": criteria must contain 2-26 candidates"
      )
    )
  })
  expect_error(
    decide(mock_decision_model(), "s", list(a = noul("A?")), verbosity = 0L),
    "criteria must contain",
    class = "rtemis_llm_api_error"
  )
})


test_that("the recorded clef-flash response reads", {
  res <- jsonlite::read_json(test_path("fixtures", "systemone_clef_flash.json"))
  questions <- list(
    q1 = as_list(choice("Team?", c("billing", "engineering", "sales"))),
    q2 = as_list(noul("Urgent?"))
  )
  answers <- rtemis.llm:::.decision_parse_answers(res, questions, "Ollama")
  expect_identical(answers[["q1"]][["choice"]], "engineering")
  expect_identical(
    names(answers[["q1"]][["probabilities"]])[[1L]],
    "engineering"
  )
  expect_equal(answers[["q2"]][["p"]], 0.816, tolerance = 1e-3)
})


# %% Filling a schema ----
test_that("generate fills a closed schema, in the harness's words", {
  env <- new.env()
  httr2::local_mocked_responses(mock_systemone(
    list(
      team = "engineering",
      urgent = 0.8,
      severity = "4",
      `channels[email]` = 0.9,
      `channels[phone]` = 0.2,
      `channels[chat]` = 0.6,
      `refund?` = 0.7,
      refund = 0.3
    ),
    env = env
  ))
  dm <- mock_decision_model(output_schema = triage_schema())
  m <- generate(dm, "It crashes. Email or chat me.", verbosity = 0L)
  expect_true(S7_inherits(m, DecisionMessage))
  expect_identical(
    jsonlite::fromJSON(m@content, simplifyVector = FALSE),
    list(
      team = "engineering",
      urgent = TRUE,
      severity = 4L,
      channels = list("email", "chat"),
      refund = FALSE
    )
  )
  expect_identical(validation_results(m)@status, "valid")
  expect_identical(responses(m), m@content, ignore_attr = TRUE)
  qs <- env[["calls"]][[1L]][["body"]][["questions"]]
  expect_identical(
    names(qs),
    c(
      "team",
      "urgent",
      "severity",
      "channels[email]",
      "channels[phone]",
      "channels[chat]",
      "refund?",
      "refund"
    )
  )
  expect_identical(
    qs[["team"]][["instructions"]],
    "Which value of the field `team` (Team to route to) does the passage support?"
  )
  expect_identical(
    qs[["urgent"]][["instructions"]],
    "Is the field `urgent` (Whether it needs an answer today) true, according to the passage?"
  )
  expect_identical(
    qs[["channels[phone]"]][["instructions"]],
    "Does the field `channels` (Channels mentioned) include `phone`, according to the passage?"
  )
  expect_identical(
    qs[["refund?"]][["instructions"]],
    "Does the passage give a value for the field `refund` (Whether a refund is requested)?"
  )
  p <- probabilities(m)
  expect_identical(
    p[path == "channels" & chosen == TRUE, option],
    c("email", "chat")
  )
  expect_identical(p[path == "refund" & chosen == TRUE, option], "false")
  expect_identical(m@metadata[["questions"]], 8L)
})


test_that("an optional field judged absent is left out and listed", {
  httr2::local_mocked_responses(mock_systemone(list(`refund?` = 0.2)))
  m <- generate(
    mock_decision_model(output_schema = triage_schema()),
    "x",
    verbosity = 0L
  )
  expect_false("refund" %in% names(jsonlite::fromJSON(m@content)))
  expect_identical(m@omitted, "refund")
  expect_identical(validation_results(m)@status, "valid")
})


test_that("a choice over more than 26 options runs as a tournament", {
  env <- new.env()
  values <- sprintf("v%02d", 1:30)
  httr2::local_mocked_responses(mock_systemone(
    list(`pick~1` = "v03", `pick~2` = "v20", pick = "v20"),
    env = env
  ))
  dm <- mock_decision_model(
    output_schema = schema("S", field("pick", enum = values))
  )
  m <- generate(dm, "x", verbosity = 0L)
  expect_length(env[["calls"]], 2L)
  first <- env[["calls"]][[1L]][["body"]][["questions"]]
  expect_identical(names(first), c("pick~1", "pick~2"))
  expect_length(first[["pick~1"]][["criteria"]], 15L)
  final <- env[["calls"]][[2L]][["body"]][["questions"]]
  expect_identical(names(final[["pick"]][["criteria"]]), c("v03", "v20"))
  expect_identical(jsonlite::fromJSON(m@content)[["pick"]], "v20")
  expect_identical(m@fields[[1L]][["by"]], "tournament")
})


test_that("a one-value enum is decided by elimination, without a call", {
  env <- new.env()
  httr2::local_mocked_responses(mock_systemone(env = env))
  s <- schema("S", field("kind", enum = "only"), field("ok", type = "boolean"))
  m <- generate(mock_decision_model(output_schema = s), "x", verbosity = 0L)
  expect_identical(names(env[["calls"]][[1L]][["body"]][["questions"]]), "ok")
  expect_identical(m@fields[[1L]][["by"]], "elimination")
  expect_false(m@fields[[1L]][["unsure"]])
})


test_that("generate refuses sampling options and a missing schema", {
  dm <- mock_decision_model()
  expect_error(generate(dm, "x", temperature = 0.2), "no sampling options")
  expect_error(generate(dm, "x", seed = 1L), "`seed`")
  expect_error(generate(dm, "x"), "decide()", fixed = TRUE)
})


# %% Batches ----
test_that("dmapply fills a schema per element and records the time of each", {
  httr2::local_mocked_responses(mock_systemone())
  dm <- mock_decision_model(
    output_schema = schema("S", field("ok", type = "boolean"))
  )
  res <- dmapply(c(a = "one", b = "two"), dm, verbosity = 0L)
  expect_identical(unname(res), rep("{\"ok\":true}", 2L), ignore_attr = TRUE)
  expect_identical(names(attr(res, "elapsed")), c("a", "b"))
  expect_true(all(attr(res, "elapsed") >= 0))
  expect_identical(validation_results(res)@status, c("valid", "valid"))
})


test_that("dmapply with questions returns the probabilities table", {
  httr2::local_mocked_responses(mock_systemone())
  res <- dmapply(
    c("one", "two"),
    mock_decision_model(),
    questions = list(a = noul("A?")),
    verbosity = 0L
  )
  expect_identical(res[["index"]], c(1L, 1L, 2L, 2L))
  expect_length(attr(res, "elapsed"), 2L)
})


test_that("dmapply refuses a built model with build arguments, and language models", {
  dm <- mock_decision_model()
  expect_error(dmapply("x", dm, context = "c"), "`context`")
  expect_error(dmapply("x", dm, verbosity = 0L), "output schema")
})


test_that("language-model entry points refuse a decision model", {
  dm <- mock_decision_model()
  expect_error(llmapply("x", dm), "dmapply()", fixed = TRUE)
  expect_error(agentapply("x", dm), "dmapply()", fixed = TRUE)
  expect_error(create_agent(dm@config), "decision model")
  expect_error(
    rtemis.llm:::.chat_agent(dm, "ollama", "", NULL, 3L, call = quote(chat(x))),
    "decision model"
  )
})


# %% Configs ----
test_that("config_OllamaDecision tells a missing model from a language model", {
  local_mocked_bindings(.ollama_capabilities = function(model_name, base_url) {
    NULL
  })
  expect_error(config_OllamaDecision("nope"), "ollama pull clef-flash")
  local_mocked_bindings(.ollama_capabilities = function(model_name, base_url) {
    c("completion", "tools")
  })
  expect_error(config_OllamaDecision("qwen3.5:9b"), "is a language model")
  local_mocked_bindings(.ollama_capabilities = function(model_name, base_url) {
    "decision"
  })
  expect_identical(config_OllamaDecision("clef-flash")@backend, "ollama")
})


test_that("OpenRouter requests carry the key and go to its endpoint", {
  env <- new.env()
  httr2::local_mocked_responses(mock_systemone(model = NULL, env = env))
  dm <- create_DecisionModel(config_OpenRouterDecision("typesafe/jev-1.13"))
  d <- with_env(
    c(OPENROUTER_API_KEY = "sk-or-test"),
    decide(dm, "x", list(a = noul("A?")), verbosity = 0L)
  )
  req <- env[["calls"]][[1L]][["req"]]
  expect_identical(req[["url"]], "https://openrouter.ai/api/v1/systemone")
  expect_identical(
    httr2::req_get_headers(req, "reveal")[["Authorization"]],
    "Bearer sk-or-test"
  )
  expect_identical(d@metadata[["provider"]], "OpenRouter")
  expect_identical(d@model_name, "typesafe/jev-1.13")
  expect_identical(
    with_env(
      c(OPENROUTER_API_KEY = "sk-or-test"),
      as_list(dm@config)[["api_key"]]
    ),
    "<redacted>"
  )
})


test_that("OpenRouter without a key aborts before the request", {
  httr2::local_mocked_responses(function(req) stop("no request expected"))
  dm <- create_DecisionModel(config_OpenRouterDecision("typesafe/jev-1.13"))
  expect_error(
    with_env(
      c(OPENROUTER_API_KEY = NA),
      decide(dm, "x", list(a = noul("A?")), verbosity = 0L)
    ),
    "No OpenRouter API key"
  )
})


# %% Live ----
test_that("clef-flash fills a triage schema", {
  skip_if_no_decision_model()
  dm <- create_DecisionModel(
    config_OllamaDecision("clef-flash"),
    output_schema = triage_schema()
  )
  m <- generate(
    dm,
    "The app crashes every time I open the billing page. I emailed twice. Fix it today.",
    verbosity = 0L
  )
  value <- jsonlite::fromJSON(m@content)
  # The passage is about a crash on the billing page: either team is defensible.
  expect_true(value[["team"]] %in% c("billing", "engineering"))
  expect_true(value[["urgent"]])
  expect_true("email" %in% value[["channels"]])
  expect_identical(validation_results(m)@status, "valid")
})


test_that("clef-flash judges an image sent with the state", {
  skip_if_no_decision_model()
  dm <- create_DecisionModel(config_OllamaDecision("clef-flash"))
  d <- decide(
    dm,
    "Look at the image.",
    list(color = choice("What color is the image?", c("red", "green", "blue"))),
    image_path = test_path("fixtures", "red.png"),
    verbosity = 0L
  )
  expect_identical(d@answers[["color"]][["choice"]], "red")
})
