# 10_DecisionModel.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# A decision model fills a closed Schema, or answers typed questions, with a
# probability per option (spec: llm/decision-models#design). It is a sibling of
# `LLM`, not a subclass: it has no system prompt, no sampling options, no turns.

# %% Question ----
#' @title Question Class
#'
#' @description
#' Superclass of the typed questions a decision model answers.
#'
#' @field instructions Character: The question.
#'
#' @author EDG
#' @keywords internal
#' @noRd
Question <- new_class(
  "Question",
  properties = list(
    instructions = prop_string(description = "The question")
  )
)


# %% Choice ----
#' @title Choice Class
#'
#' @description
#' One of 2 to 26 named options, each with what it means.
#'
#' @field options Character: Named vector; names are the options, values what
#'   each means.
#'
#' @author EDG
#' @keywords internal
#' @noRd
Choice <- new_class(
  "Choice",
  parent = Question,
  properties = list(
    options = prop_string(
      map = TRUE,
      description = "Options and their meanings"
    )
  ),
  constructor = function(instructions, options) {
    new_object(Question(instructions = instructions), options = options)
  },
  validator = function(self) {
    n <- length(self@options)
    if (n < 2L || n > DECISION_MAX_OPTIONS) {
      abort(
        "A choice takes 2 to ",
        DECISION_MAX_OPTIONS,
        " options; got ",
        n,
        ".\n",
        "Split a longer list into several choices, or fill a schema field ",
        "with an `enum`, which runs as a tournament."
      )
    }
    if (any(!nzchar(trimws(names(self@options))))) {
      abort("Every option of a choice needs a non-blank name.")
    }
    NULL
  }
)


# %% Noul ----
#' @title Noul Class
#'
#' @description
#' How true a statement is: the answer is a probability.
#'
#' @author EDG
#' @keywords internal
#' @noRd
Noul <- new_class("Noul", parent = Question)


# %% as_list.Choice ----
method(as_list, Choice) <- function(x) {
  list(
    type = "choice",
    instructions = x@instructions,
    criteria = as.list(x@options)
  )
}


# %% as_list.Noul ----
method(as_list, Noul) <- function(x) {
  list(type = "noul", instructions = x@instructions)
}


# %% repr.Question ----
method(repr, Question) <- function(x, pad = 0L, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name(
      sub(".*::", "", class(x)[1]),
      pad = pad,
      output_type = output_type
    ),
    strrep(" ", pad),
    x@instructions,
    "\n",
    if (S7_inherits(x, Choice)) {
      paste0(
        strrep(" ", pad),
        "  ",
        highlight(names(x@options), output_type = output_type),
        ifelse(
          x@options == names(x@options),
          "",
          paste0(": ", x@options)
        ),
        "\n",
        collapse = ""
      )
    }
  )
}


# %% print.Question ----
method(print, Question) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
}


# %% Decision ----
#' @title Decision Class
#'
#' @description
#' A decision model's answers to typed questions.
#'
#' @field answers List: Named by question; each `list(type = "choice", choice,
#'   probabilities, confidence)` or `list(type = "noul", p)`.
#' @field model_name Character: The model that answered.
#' @field metadata List: `provider`, `models`, `calls`, `questions`, `usage`,
#'   `elapsed` (seconds).
#'
#' @author EDG
#' @keywords internal
#' @noRd
Decision <- new_class(
  "Decision",
  properties = list(
    answers = class_list,
    model_name = prop_string(description = "Model that answered"),
    metadata = prop_bag(description = "Provider, calls, usage, elapsed")
  )
)


# %% repr.Decision ----
method(repr, Decision) <- function(x, output_type = NULL) {
  output_type <- get_output_type(output_type)
  keys <- names(x@answers)
  width <- max(nchar(keys))
  lines <- vapply(
    keys,
    function(k) {
      a <- x@answers[[k]]
      paste0(
        formatC(k, width = width),
        ": ",
        if (a[["type"]] == "noul") {
          paste0(
            "noul ",
            highlight(sprintf("%.3f", a[["p"]]), output_type = output_type)
          )
        } else {
          paste0(
            highlight(a[["choice"]], output_type = output_type),
            " (p = ",
            sprintf("%.3f", a[["probabilities"]][[a[["choice"]]]]),
            ", confidence ",
            sprintf("%.3f", a[["confidence"]]),
            ")"
          )
        },
        if (.is_unsure(a)) {
          fmt("  (unsure)", muted = TRUE, output_type = output_type)
        }
      )
    },
    character(1L)
  )
  paste0(
    repr_S7name("Decision", output_type = output_type),
    fmt("Model: ", bold = TRUE, output_type = output_type),
    highlight(x@model_name, output_type = output_type),
    "\n",
    paste0(lines, collapse = "\n"),
    "\n"
  )
}


# %% print.Decision ----
method(print, Decision) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
}


# %% DecisionModel ----
#' @title DecisionModel Class
#'
#' @description
#' A decision model: a configuration, the context placed before every prompt,
#' and the schema it fills.
#'
#' @field name Optional Character: Name.
#' @field config DecisionConfig: Configuration.
#' @field context Optional Character: Text placed before every prompt in the
#'   state the questions are judged against.
#' @field output_schema Optional Schema: The schema `generate()` fills.
#'
#' @author EDG
#' @keywords internal
#' @noRd
DecisionModel <- new_class(
  "DecisionModel",
  properties = list(
    name = prop_string(nullable = TRUE, description = "Name"),
    config = DecisionConfig,
    context = prop_string(nullable = TRUE, description = "Context"),
    output_schema = optional(Schema)
  ),
  constructor = function(
    config,
    context = NULL,
    output_schema = NULL,
    name = NULL
  ) {
    if (!is.null(output_schema)) {
      .check_decidable(output_schema)
    }
    new_object(
      S7_object(),
      name = name,
      config = config,
      context = context,
      output_schema = output_schema
    )
  }
)


# %% get_model_name.DecisionModel ----
method(get_model_name, DecisionModel) <- function(x) {
  x@config@model_name
}


# %% repr.DecisionModel ----
method(repr, DecisionModel) <- function(x, output_type = NULL) {
  output_type <- get_output_type(output_type)
  row <- function(label, value) {
    paste0(
      fmt(label, bold = TRUE, output_type = output_type),
      highlight(value, output_type = output_type),
      "\n"
    )
  }
  paste0(
    repr_S7name("DecisionModel", output_type = output_type),
    if (!is.null(x@name)) row("         Name: ", x@name),
    row("        Model: ", x@config@model_name),
    row("     Provider: ", .decision_provider_name(x@config)),
    row("     Base URL: ", x@config@base_url),
    if (!is.null(x@context)) row("      Context: ", x@context),
    if (!is.null(x@output_schema)) {
      paste0(
        fmt("Output Schema: \n", bold = TRUE, output_type = output_type),
        repr(x@output_schema, pad = 15L, output_type = output_type)
      )
    }
  )
}


# %% print.DecisionModel ----
method(print, DecisionModel) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
  invisible(x)
}


# %% DecisionMessage ----
#' @title DecisionMessage Class
#'
#' @description
#' A schema filled by a decision model. `content` is the filled JSON document,
#' so `responses()` and the output validator read it as they read an LLM's
#' structured output; `reasoning` and `tool_calls` are always NULL.
#'
#' @field fields List: One element per filled field: `path`, `value`, `label`
#'   (the option chosen, as sent on the wire; NA for a list), `p` (the
#'   probability of `value`; NA for a list), `options` (named probabilities,
#'   likeliest first), `unsure`, `by`.
#' @field omitted Character: Optional fields the passage was judged not to give.
#'
#' @author EDG
#' @keywords internal
#' @noRd
DecisionMessage <- new_class(
  "DecisionMessage",
  parent = LLMMessage,
  properties = list(
    fields = class_list,
    omitted = class_character
  ),
  constructor = function(
    content,
    name = NULL,
    metadata = NULL,
    model_name,
    fields = list(),
    omitted = character()
  ) {
    new_object(
      LLMMessage(
        content = content,
        name = name,
        metadata = metadata,
        model_name = model_name
      ),
      fields = fields,
      omitted = omitted
    )
  }
)


# %% repr.DecisionMessage ----
method(repr, DecisionMessage) <- function(x, output_type = NULL) {
  output_type <- get_output_type(output_type)
  name <- if (!is.null(x@name)) paste0(x@name, " ")
  paths <- vapply(x@fields, function(f) f[["path"]], character(1L))
  width <- max(nchar(c(paths, "")))
  lines <- vapply(
    x@fields,
    function(f) {
      paste0(
        "  ",
        formatC(f[["path"]], width = -width),
        "  ",
        if (is.na(f[["p"]])) {
          paste(
            sprintf("%s %.2f", names(f[["options"]]), f[["options"]]),
            collapse = ", "
          )
        } else {
          sprintf("p = %.3f", f[["p"]])
        },
        if (isTRUE(f[["unsure"]])) "  (unsure)"
      )
    },
    character(1L)
  )
  paste0(
    repr_bracket(
      paste0(name, "Decision"),
      col = col_llm,
      output_type = output_type
    ),
    " ",
    x@content,
    if (length(lines)) paste0("\n", paste0(lines, collapse = "\n")),
    if (length(x@omitted)) {
      paste0("\n  Not given: ", paste(x@omitted, collapse = ", "))
    }
  )
}


# %% .check_decidable() ----
#' Abort unless a decision model can fill a schema
#'
#' @param schema Schema: Output schema.
#'
#' @return The compiled schema, from `.fill_compile()`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_decidable <- function(schema) {
  if (!S7_inherits(schema, Schema)) {
    abort(
      "`output_schema` must be a Schema created with schema().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  compiled <- .fill_compile(schema)
  open <- compiled[["open"]]
  if (nrow(open)) {
    abort(
      "A decision model cannot fill this schema: every field must be a closed ",
      "set (a field with an `enum`, a boolean, or an array whose `items` is a ",
      "field with an `enum`).\n",
      paste0("  - `", open[["path"]], "`: ", open[["reason"]], collapse = "\n"),
      "\nClose these fields, drop them, or fill the schema with an LLM.",
      class = "rtemis_input_error"
    )
  }
  compiled
}


# --- Public API -----------------------------------------------------------------------------------
# %% choice() ----
#' Define a Choice Question
#'
#' A question a decision model answers by choosing one of 2 to 26 named options, with a
#' probability for each.
#'
#' @param instructions Character: The question.
#' @param options Character: The options. Name the vector to say what each option means
#'   (`c(low = "Can wait a week")`); an unnamed option means its own name.
#'
#' @return `Choice` object, for [decide()].
#'
#' @author EDG
#' @export
#'
#' @examples
#' choice(
#'   "Which team should handle this ticket?",
#'   c(billing = "Payments, invoices, refunds", engineering = "Bugs and crashes")
#' )
choice <- function(instructions, options) {
  check_character_scalar(instructions, "instructions")
  if (!is.character(options) || anyNA(options)) {
    abort(
      "`options` must be a character vector.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (is.null(names(options))) {
    names(options) <- options
  }
  if (anyDuplicated(names(options))) {
    abort("The options of a choice must have distinct names.")
  }
  # A blank meaning is refused by some providers; the option's own name is the
  # least that can be said.
  blank <- !nzchar(trimws(options))
  options[blank] <- names(options)[blank]
  Choice(instructions = instructions, options = options)
} # /choice


# %% noul() ----
#' Define a Noul Question
#'
#' A question a decision model answers with the probability that a statement is true.
#'
#' @param instructions Character: The statement or yes/no question.
#'
#' @return `Noul` object, for [decide()].
#'
#' @author EDG
#' @export
#'
#' @examples
#' noul("Does the customer ask for a refund?")
noul <- function(instructions) {
  check_character_scalar(instructions, "instructions")
  Noul(instructions = instructions)
} # /noul


# %% create_DecisionModel() ----
#' Create a Decision Model
#'
#' A decision model answers typed questions about a passage with a probability for every option,
#' and writes no text. There are two kinds of question: a [choice()] among 2 to 26 named options,
#' and a [noul()], which asks how true a statement is.
#'
#' @details
#' **Questions are the decision model's own interface.** Ask them with [decide()], or over many
#' passages with `dmapply(questions = ...)`. You write each question's wording and, for a choice,
#' what each option means. The model judges the passage against those meanings, so well-defined
#' options are usually the most accurate way to use a decision model.
#'
#' **A schema is an adapter for code written for LLMs.** [generate()] and [dmapply()] can also
#' fill a closed [schema()], returning a validated JSON document in the shape an LLM's structured
#' output has. The schema is turned into questions first. [as_questions()] returns them, so you can
#' read them, edit them and pass them to [decide()].
#' - A field with an `enum` becomes a `choice()`, asked as "Which value of the field `<name>`
#'   (`<description>`) does the passage support?". Each option means only its own label.
#' - A boolean field becomes a `noul()`.
#' - An array whose `items` is a field with an `enum` becomes one `noul()` per value. The values
#'   at or above 0.5 are kept.
#' - A field that is not required adds a `noul()` asking whether the passage gives it at all.
#'
#' Use a schema when the result must be a document: to drop a decision model into a pipeline
#' built for LLMs, or to validate the output. Use questions when the answer you want is the
#' judgment itself, and to give options their meanings. Use [is_decidable()] to check whether
#' a schema is closed.
#'
#' @param config `DecisionConfig`: From [config_OllamaDecision()] or [config_OpenRouterDecision()].
#' @param context Optional Character: Text placed before every prompt, in the state the questions
#'   are judged against. A decision model has no system prompt; put the task's standing context
#'   here.
#' @param output_schema Optional Schema: The closed schema [generate()] fills.
#' @param name Optional Character: Name.
#'
#' @return `DecisionModel` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires a running Ollama server with a decision model
#' \dontrun{
#'   triage <- schema(
#'     "Triage",
#'     field("team", "Team to route to", enum = c("billing", "engineering", "sales")),
#'     field("urgent", "Whether it needs an answer today", type = "boolean")
#'   )
#'   dm <- create_DecisionModel(config_OllamaDecision("clef-flash"), output_schema = triage)
#'   generate(dm, "The app crashes whenever I open the billing page.")
#' }
create_DecisionModel <- function(
  config,
  context = NULL,
  output_schema = NULL,
  name = NULL
) {
  if (S7_inherits(config, LLMConfig)) {
    abort(
      "`config` configures a language model.\n",
      "Use create_Ollama(), create_OpenAI() or create_agent() for it, or ",
      "config_OllamaDecision() / config_OpenRouterDecision() for a decision model."
    )
  }
  if (!S7_inherits(config, DecisionConfig)) {
    abort(
      "`config` must come from config_OllamaDecision() or config_OpenRouterDecision().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  check_optional_character_scalar(context, "context")
  check_optional_character_scalar(name, "name")
  DecisionModel(
    config = config,
    context = context,
    output_schema = output_schema,
    name = name
  )
} # /create_DecisionModel


# %% is_decidable() ----
#' Check Whether a Decision Model Can Fill a Schema
#'
#' A decision model fills only closed fields: a field with an `enum` (a choice), a boolean (a
#' noul), or an array whose `items` is a field with an `enum` (one noul per value). A field that
#' is not required adds a question about whether the passage gives it at all. Free text, numbers
#' and integers without an `enum`, and arrays of anything else are open, and keep the schema an
#' LLM's.
#'
#' @param x Schema: Created with [schema()].
#'
#' @return Logical scalar, with two attributes: `open`, a data.frame of the open fields (`path`,
#'   `reason`) in field order; and `questions`, the number of questions a fill asks before any
#'   tournament final (`NA` when the schema is open).
#'
#' @author EDG
#' @export
#'
#' @examples
#' is_decidable(schema(
#'   "Triage",
#'   field("team", enum = c("billing", "engineering")),
#'   field("summary", "One-line summary")
#' ))
is_decidable <- function(x) {
  if (!S7_inherits(x, Schema)) {
    abort(
      "`x` must be a Schema created with schema().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  compiled <- .fill_compile(x)
  open <- compiled[["open"]]
  structure(
    nrow(open) == 0L,
    open = open,
    questions = if (nrow(open)) {
      NA_integer_
    } else {
      length(.fill_first_round(compiled[["nodes"]]))
    }
  )
} # /is_decidable


# %% as_questions() ----
#' Convert a Schema to Decision Model Questions
#'
#' Returns the questions a decision model is asked when it fills `x` with [generate()]. Each is
#' worded from the field's name and description, and named after the field. Edit them, for
#' example by giving a choice's options their meanings, and ask them with [decide()] or
#' `dmapply(questions = ...)`.
#'
#' The conversion:
#' - A field with an `enum` becomes a [choice()] named after the field. Its options are the
#'   `enum` values, each meaning only its own label.
#' - A boolean field becomes a [noul()] named after the field.
#' - An array whose `items` is a field with an `enum` becomes one [noul()] per value, named
#'   `<field>[<value>]`.
#' - A field that is not required adds a [noul()] named `<field>?`, asking whether the passage
#'   gives it at all.
#'
#' @param x Schema: A closed schema (see [is_decidable()]).
#'
#' @return Named list of `Choice` and `Noul` objects, in field order.
#'
#' @details
#' A choice takes at most 26 options. [generate()] fills an `enum` with more values by asking
#' about groups of values and then choosing among the group winners. That has no single question
#' to return, so it is an error here.
#'
#' @author EDG
#' @export
#'
#' @examples
#' sentiment <- schema(
#'   "Sentiment",
#'   field("sentiment", "Sentiment of the sentence", enum = c("positive", "negative", "neutral"))
#' )
#' qs <- as_questions(sentiment)
#' qs
#' # Give the options their meanings, keeping the wording
#' qs[["sentiment"]] <- choice(
#'   qs[["sentiment"]]@instructions,
#'   c(
#'     positive = "The writer is pleased or approving overall",
#'     negative = "The writer is displeased or critical overall",
#'     neutral = "The writer states facts, or balances praise and criticism evenly"
#'   )
#' )
as_questions <- function(x) {
  compiled <- .check_decidable(x)
  too_many <- vapply(
    compiled[["nodes"]],
    function(node) {
      node[["kind"]] == "choice" &&
        length(node[["options"]][["label"]]) > DECISION_MAX_OPTIONS
    },
    logical(1L)
  )
  if (any(too_many)) {
    paths <- vapply(compiled[["nodes"]][too_many], function(n) n[["path"]], "")
    abort(
      "A choice takes at most ",
      DECISION_MAX_OPTIONS,
      " options, and ",
      paste0("`", paths, "`", collapse = ", "),
      " has more.\n",
      "generate() fills such a field by asking about groups of values first; ",
      "to ask it yourself, split the values into several choice() questions."
    )
  }
  .fill_questions(compiled[["nodes"]])
} # /as_questions


# %% decide ----
#' Ask a Decision Model Typed Questions
#'
#' Sends `questions` about `state` to a decision model. Each [choice()] is answered with the
#' option chosen, a probability per option, and the provider's confidence (how concentrated the
#' probabilities are); each [noul()] with the probability that it is true. More than 64 questions
#' are sent as several calls.
#'
#' @param x `DecisionModel`: From [create_DecisionModel()].
#' @param state Character: The passage the questions are about. The model's `context`, if any, is
#'   placed before it.
#' @param questions Named list of [choice()] and [noul()] questions. The names identify the
#'   answers. [as_questions()] converts a schema into such a list.
#' @param image_path Optional Character: Paths to local images (PNG, JPEG or WebP) judged with
#'   the state.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `Decision` object. Use [probabilities()] for a table of every option's probability.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires a running Ollama server with a decision model
#' \dontrun{
#'   dm <- create_DecisionModel(config_OllamaDecision("clef-flash"))
#'   decide(
#'     dm,
#'     "The app crashes whenever I open the billing page. Please fix it today.",
#'     list(
#'       team = choice("Which team should handle this?", c("billing", "engineering", "sales")),
#'       urgent = noul("Does the customer need an answer today?")
#'     )
#'   )
#' }
decide <- new_generic(
  "decide",
  "x",
  function(x, state, questions, image_path = NULL, verbosity = 1L) {
    S7_dispatch()
  }
)


# %% decide.DecisionModel ----
method(decide, DecisionModel) <- function(
  x,
  state,
  questions,
  image_path = NULL,
  verbosity = 1L
) {
  if (S7_inherits(questions, Question)) {
    abort(
      "`questions` must be a named list of questions.\n",
      "Wrap a single question as `list(name = question)`."
    )
  }
  if (
    !is.list(questions) ||
      length(questions) == 0L ||
      !all(vapply(questions, S7_inherits, logical(1L), class = Question))
  ) {
    abort(
      "`questions` must be a non-empty list of questions made with choice() or noul().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  keys <- names(questions)
  if (is.null(keys) || any(!nzchar(keys)) || anyDuplicated(keys)) {
    abort("Name every question in `questions`, with distinct names.")
  }
  state <- .decision_state(x@context, state)
  images <- .decision_images(image_path)
  wire <- lapply(questions, as_list)
  msg(repr_bracket(x@config@model_name), "deciding...", verbosity = verbosity)
  start <- proc.time()[["elapsed"]]
  asked <- .decision_ask(x@config, state, wire, images, verbosity = verbosity)
  elapsed <- proc.time()[["elapsed"]] - start
  msg(repr_bracket(x@config@model_name), "done.", verbosity = verbosity)
  Decision(
    answers = asked[["answers"]],
    model_name = x@config@model_name,
    metadata = .decision_metadata(
      x@config,
      asked[["calls"]],
      length(wire),
      elapsed
    )
  )
}


# %% generate.DecisionModel ----
#' Generate method for DecisionModel: fill the output schema
#'
#' @return DecisionMessage object
#' @author EDG
#' @noRd
method(generate, DecisionModel) <- function(
  x,
  prompt,
  temperature = NULL,
  top_p = NULL,
  max_tokens = NULL,
  stop = NULL,
  image_path = NULL,
  think = NULL,
  output_schema = NULL,
  verbosity = 1L,
  validate_output = TRUE,
  on_validation_failure = c("warn", "collect", "abort"),
  ...
) {
  sampling <- c(
    temperature = !is.null(temperature),
    top_p = !is.null(top_p),
    max_tokens = !is.null(max_tokens),
    stop = !is.null(stop),
    think = !is.null(think)
  )
  extra <- names(list(...))
  if (any(sampling) || length(extra)) {
    abort(
      "A decision model has no sampling options; drop ",
      paste0("`", c(names(sampling)[sampling], extra), "`", collapse = ", "),
      "."
    )
  }
  on_validation_failure <- match.arg(on_validation_failure)
  output_schema <- output_schema %||% x@output_schema
  if (is.null(output_schema)) {
    abort(
      "generate() on a decision model fills an output schema, and none was given.\n",
      "Pass `output_schema`, set it in create_DecisionModel(), or ask typed ",
      "questions with decide()."
    )
  }
  compiled <- .check_decidable(output_schema)
  validator <- .prepare_output_validation(
    output_schema,
    validate_output,
    on_validation_failure
  )
  state <- .decision_state(x@context, prompt)
  images <- .decision_images(image_path)
  msg(repr_bracket(x@config@model_name), "deciding...", verbosity = verbosity)
  start <- proc.time()[["elapsed"]]
  filled <- .fill_schema(
    x@config,
    compiled,
    state,
    images,
    verbosity = verbosity
  )
  elapsed <- proc.time()[["elapsed"]] - start
  msg(repr_bracket(x@config@model_name), "done.", verbosity = verbosity)
  message <- DecisionMessage(
    name = x@name,
    content = filled[["json"]],
    metadata = .decision_metadata(
      x@config,
      filled[["calls"]],
      filled[["questions"]],
      elapsed
    ),
    model_name = x@config@model_name,
    fields = filled[["fields"]],
    omitted = filled[["omitted"]]
  )
  .validate_generated_message(
    message,
    output_schema,
    validator,
    on_validation_failure,
    verbosity
  )
}


# %% probabilities() ----
#' Extract a Decision Model's Probabilities
#'
#' Tabulates every option's probability from a [decide()] result, a schema filled by
#' [generate()], or a list of either from [dmapply()].
#'
#' @param x `Decision`, `DecisionMessage`, or a list of them (`NULL` elements, left by failed
#'   calls, are skipped).
#'
#' @return `data.table` with one row per option: `index` (the element's position, for a list),
#'   `question` (a `Decision`'s question name) or `path` (a filled field), `option`, `p`, `chosen`
#'   and `unsure`. A noul has two rows, `"true"` and `"false"`. An array field has one row per
#'   value, `p` being the probability that it belongs.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Requires a running Ollama server with a decision model
#' \dontrun{
#'   dm <- create_DecisionModel(config_OllamaDecision("clef-flash"))
#'   d <- decide(dm, "Please refund my last invoice.", list(refund = noul("Is this a refund?")))
#'   probabilities(d)
#' }
probabilities <- function(x) {
  if (S7_inherits(x, Decision)) {
    return(data.table::rbindlist(lapply(names(x@answers), function(k) {
      a <- x@answers[[k]]
      unsure <- .is_unsure(a)
      if (a[["type"]] == "noul") {
        data.table::data.table(
          question = k,
          option = c("true", "false"),
          p = c(a[["p"]], 1 - a[["p"]]),
          chosen = c(a[["p"]] >= 0.5, a[["p"]] < 0.5),
          unsure = unsure
        )
      } else {
        data.table::data.table(
          question = k,
          option = names(a[["probabilities"]]),
          p = unname(a[["probabilities"]]),
          chosen = names(a[["probabilities"]]) == a[["choice"]],
          unsure = unsure
        )
      }
    })))
  }
  if (S7_inherits(x, DecisionMessage)) {
    return(data.table::rbindlist(lapply(x@fields, function(f) {
      chosen <- if (is.na(f[["label"]])) {
        f[["options"]] >= 0.5
      } else {
        names(f[["options"]]) == f[["label"]]
      }
      data.table::data.table(
        path = f[["path"]],
        option = names(f[["options"]]),
        p = unname(f[["options"]]),
        chosen = unname(chosen),
        unsure = f[["unsure"]]
      )
    })))
  }
  if (is.list(x) && !is.object(x)) {
    ok <- vapply(
      x,
      function(e) {
        is.null(e) ||
          S7_inherits(e, Decision) ||
          S7_inherits(e, DecisionMessage)
      },
      logical(1L)
    )
    if (all(ok)) {
      tables <- lapply(seq_along(x), function(i) {
        if (is.null(x[[i]])) {
          return(NULL)
        }
        dt <- probabilities(x[[i]])
        dt[, ("index") := i]
        data.table::setcolorder(dt, "index")
        dt
      })
      return(data.table::rbindlist(tables))
    }
  }
  abort(
    "Could not extract probabilities from `x`.\n",
    "Pass a Decision from decide(), a DecisionMessage from generate() on a ",
    "DecisionModel, or a list of them from dmapply().",
    class = c("rtemis_type_error", "rtemis_input_error")
  )
} # /probabilities
