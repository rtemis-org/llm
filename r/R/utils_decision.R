# utils_decision.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# The `/v1/systemone` wire, and the compiler that turns a Schema into the typed
# questions that fill it. Both follow rtemis-harness (`decision.rs`,
# `structured.rs`, `context.rs`) so a schema filled here and in rtemislive is
# asked the same questions, in the same words, with the same thresholds
# (spec: llm/decision-models#schema-compiler).

# %% Constants ----
# Below this, a choice is unsure. The provider's `confidence` measures how
# concentrated the probabilities are, not how often the pick is right; the
# value is the harness's provisional one.
DECISION_CHOICE_CONFIDENCE <- 0.5
# A noul within this of 0.5 is unsure. Provisional, for the same reason.
DECISION_NOUL_MARGIN <- 0.15


# --- The wire -------------------------------------------------------------------------------------

# %% .decision_state() ----
#' The state every question is judged against
#'
#' @param context Optional Character: Text placed before every prompt.
#' @param prompt Character: The passage.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.decision_state <- function(context, prompt) {
  check_character_scalar(prompt, "prompt")
  if (is.null(context)) prompt else paste0(context, "\n\n", prompt)
}


# %% .decision_images() ----
#' Images for the wire: base64, no data-URL prefix
#'
#' @param image_path Optional Character: Paths to local images.
#'
#' @return Character vector, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.decision_images <- function(image_path) {
  images <- .read_images(image_path)
  vapply(images, function(img) img[["data"]], character(1L))
}


# %% .decision_request_body() ----
#' The `/v1/systemone` request body
#'
#' @param model_name Character: Model name.
#' @param state Character: State.
#' @param questions Named list: Wire questions, from `as_list()` of each Question.
#' @param images Character: Base64 images; sent only when there are any.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.decision_request_body <- function(model_name, state, questions, images) {
  body <- list(model = model_name, state = state, questions = questions)
  if (length(images) > 0L) {
    body[["images"]] <- I(images)
  }
  body
}


# %% .decision_perform() ----
#' Send one call to a decision model
#'
#' @param config DecisionConfig: Configuration.
#' @param body Named list: Request body.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Named list: the parsed response, with `model` filled in when the
#'   provider leaves it out.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.decision_perform <- function(config, body, verbosity = 1L) {
  provider <- .decision_provider_name(config)
  req <- httr2::request(paste0(config@base_url, "/v1/systemone")) |>
    httr2::req_body_json(body, auto_unbox = TRUE, digits = NA) |>
    httr2::req_user_agent("rtemis.llm-r (www.rtemis.org)") |>
    httr2::req_timeout(config@timeout) |>
    httr2::req_error(is_error = function(resp) FALSE)
  req <- .decision_auth(config, req)
  resp <- .req_perform_status(req, verbosity = verbosity)
  .check_http_response(resp, provider)
  res <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  # OpenRouter may leave out `model`; the model asked for is what answered.
  if (is.null(res[["model"]])) {
    res[["model"]] <- config@model_name
  }
  res
}


# %% .decision_parse_answers() ----
#' Read a `/v1/systemone` response's answers
#'
#' @param res Named list: Parsed response.
#' @param questions Named list: The wire questions asked, to hold each answer
#'   to its question's type and options.
#' @param provider Character: Provider name, for errors.
#'
#' @return Named list of answers: `list(type = "choice", choice, probabilities,
#'   confidence)` or `list(type = "noul", p)`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.decision_parse_answers <- function(res, questions, provider) {
  malformed <- function(...) {
    abort(
      provider,
      " returned a decision response that does not read: ",
      ...,
      class = "rtemis_llm_api_error"
    )
  }
  answers <- res[["answers"]]
  if (!is.list(answers)) {
    malformed("it has no `answers`.")
  }
  out <- lapply(names(questions), function(key) {
    q <- questions[[key]]
    a <- answers[[key]]
    if (is.null(a)) {
      malformed("no answer to the question `", key, "`.")
    }
    type <- a[["type"]] %||% ""
    if (!identical(type, q[["type"]])) {
      malformed(
        "`",
        key,
        "`: an answer of type `",
        type,
        "` to a ",
        q[["type"]],
        " question."
      )
    }
    if (type == "noul") {
      p <- a[["noul"]]
      if (!is.numeric(p) || length(p) != 1L) {
        malformed("`", key, "`: a noul answer with no probability.")
      }
      return(list(type = "noul", p = as.numeric(p)))
    }
    choice <- a[["choice"]]
    if (!is.character(choice) || length(choice) != 1L) {
      malformed("`", key, "`: a choice answer with no choice.")
    }
    if (!choice %in% names(q[["criteria"]])) {
      malformed("`", key, "` answered `", choice, "`, which was not an option.")
    }
    probs <- unlist(a[["probabilities"]] %||% list())
    probs <- if (length(probs)) {
      sort(vapply(probs, as.numeric, numeric(1L)), decreasing = TRUE)
    } else {
      stats::setNames(1, choice)
    }
    list(
      type = "choice",
      choice = choice,
      probabilities = probs,
      confidence = as.numeric(a[["confidence"]] %||% 0)
    )
  })
  stats::setNames(out, names(questions))
}


# %% .decision_ask() ----
#' Ask questions, in as many calls as the per-call limit needs
#'
#' @param config DecisionConfig: Configuration.
#' @param state Character: State.
#' @param questions Named list: Wire questions.
#' @param images Character: Base64 images.
#' @param verbosity Integer: Verbosity level.
#'
#' @return List of `answers` (named list) and `calls` (list of each call's
#'   `model` and `usage`).
#'
#' @author EDG
#' @keywords internal
#' @noRd
.decision_ask <- function(config, state, questions, images, verbosity = 1L) {
  provider <- .decision_provider_name(config)
  batches <- split(
    seq_along(questions),
    ceiling(seq_along(questions) / DECISION_MAX_QUESTIONS)
  )
  answers <- list()
  calls <- list()
  for (idx in batches) {
    batch <- questions[idx]
    res <- .decision_perform(
      config,
      .decision_request_body(config@model_name, state, batch, images),
      verbosity = verbosity
    )
    answers <- c(answers, .decision_parse_answers(res, batch, provider))
    calls[[length(calls) + 1L]] <- list(
      model = res[["model"]],
      usage = res[["usage"]]
    )
  }
  list(answers = answers, calls = calls)
}


# %% .decision_metadata() ----
#' Metadata for a decision: provider, calls, questions, models, usage, time
#'
#' @param config DecisionConfig: Configuration.
#' @param calls List: From `.decision_ask()`, one element per call, all rounds.
#' @param questions Integer: Questions asked, all rounds.
#' @param elapsed Numeric: Wall time in seconds.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.decision_metadata <- function(config, calls, questions, elapsed) {
  usage_sum <- function(field) {
    n <- vapply(
      calls,
      function(cl) {
        v <- cl[["usage"]][[field]]
        if (is.null(v)) NA_real_ else as.numeric(v)
      },
      numeric(1L)
    )
    if (all(is.na(n))) NULL else sum(n, na.rm = TRUE)
  }
  usage <- list(
    input_tokens = usage_sum("input_tokens"),
    output_tokens = usage_sum("output_tokens")
  )
  list(
    provider = .decision_provider_name(config),
    models = unique(vapply(calls, function(cl) cl[["model"]], character(1L))),
    calls = length(calls),
    questions = as.integer(questions),
    usage = Filter(Negate(is.null), usage),
    elapsed = elapsed
  )
}


# %% .is_unsure() ----
#' Whether an answer is unsure, by the harness's thresholds
#'
#' @param answer List: A parsed answer.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.is_unsure <- function(answer) {
  if (answer[["type"]] == "noul") {
    abs(answer[["p"]] - 0.5) < DECISION_NOUL_MARGIN
  } else {
    answer[["confidence"]] < DECISION_CHOICE_CONFIDENCE
  }
}


# --- The schema compiler --------------------------------------------------------------------------

# %% .fill_field_words() ----
# The question wording below is `context.rs`'s `fill_*`, word for word.
.fill_field_words <- function(path, about) {
  if (!is.null(about) && nzchar(trimws(about))) {
    paste0("`", path, "` (", trimws(about), ")")
  } else {
    paste0("`", path, "`")
  }
}

.fill_choice <- function(path, about) {
  paste0(
    "Which value of the field ",
    .fill_field_words(path, about),
    " does the passage support?"
  )
}

.fill_noul <- function(path, about) {
  paste0(
    "Is the field ",
    .fill_field_words(path, about),
    " true, according to the passage?"
  )
}

.fill_member <- function(path, about, value) {
  paste0(
    "Does the field ",
    .fill_field_words(path, about),
    " include `",
    value,
    "`, according to the passage?"
  )
}

.fill_present <- function(path, about) {
  paste0(
    "Does the passage give a value for the field ",
    .fill_field_words(path, about),
    "?"
  )
}


# %% .fill_options() ----
#' A closed field's values as wire options
#'
#' A value's label is its own text for a string and its JSON otherwise.
#'
#' @param values Vector: Values, already of the field's type.
#'
#' @return List of `label` (character) and `value` (vector), or a character
#'   reason when two values read alike.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.fill_options <- function(values) {
  labels <- if (is.character(values)) {
    values
  } else {
    vapply(
      values,
      function(v) {
        as.character(jsonlite::toJSON(v, auto_unbox = TRUE, digits = NA))
      },
      character(1L)
    )
  }
  dup <- labels[duplicated(labels)]
  if (length(dup)) {
    return(paste0("two values that read as `", dup[[1L]], "`"))
  }
  list(label = unname(labels), value = unname(values))
}


# %% .fill_compile() ----
#' Compile a Schema into the questions that fill it
#'
#' rtemis.llm's `Schema` is one object of `Field`s, so every node is a field of
#' the root: a choice (an `enum`), a noul (a boolean), members (an array whose
#' `items` is a field with an `enum`), or open.
#'
#' @param schema Schema: Output schema.
#'
#' @return List of `nodes` (one per field, in order) and `open` (data.frame of
#'   `path` and `reason`, in field order).
#'
#' @author EDG
#' @keywords internal
#' @noRd
.fill_compile <- function(schema) {
  n <- 0L
  next_key <- function() {
    n <<- n + 1L
    paste0("q", n)
  }
  open <- list()
  nodes <- lapply(schema@fields, function(f) {
    path <- f@name
    about <- if (!is.null(f@description) && f@description != f@name) {
      f@description
    }
    open_node <- function(reason) {
      open[[length(open) + 1L]] <<- data.frame(
        path = path,
        reason = reason,
        stringsAsFactors = FALSE
      )
      list(kind = "open", path = path)
    }
    node <- if (!is.null(f@enum)) {
      options <- .fill_options(.enum_values(f@enum, f@type))
      if (is.character(options)) {
        open_node(options)
      } else {
        list(
          kind = "choice",
          key = next_key(),
          path = path,
          about = about,
          options = options
        )
      }
    } else {
      switch(
        f@type,
        boolean = list(
          kind = "noul",
          key = next_key(),
          path = path,
          about = about
        ),
        string = open_node("free text"),
        number = open_node("a number"),
        integer = open_node(
          "an integer with no bounds, or more than 100 values"
        ),
        array = {
          items <- f@items
          if (
            S7_inherits(items, Field) &&
              !is.null(items@enum)
          ) {
            options <- .fill_options(.enum_values(items@enum, items@type))
            if (is.list(options)) {
              list(
                kind = "members",
                key = next_key(),
                path = path,
                about = about,
                options = options
              )
            } else {
              open_node(options)
            }
          } else {
            open_node("a list of anything but closed values")
          }
        }
      )
    }
    # The presence key is taken after the field's own, as the harness does.
    node[["presence"]] <- if (!f@required) paste0(next_key(), "?")
    node[["about"]] <- about
    node[["name"]] <- f@name
    node
  })
  list(
    nodes = nodes,
    open = if (length(open)) {
      do.call(rbind, open)
    } else {
      data.frame(path = character(), reason = character())
    }
  )
}


# %% .fill_choice_first() ----
#' A choice's first-round questions
#'
#' One choice when the options fit in one; one per balanced group of 26 or
#' fewer when they do not (a tournament); none when there is one option.
#'
#' @param node List: A choice node.
#'
#' @return Named list of wire questions.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.fill_choice_first <- function(node) {
  labels <- node[["options"]][["label"]]
  n <- length(labels)
  instructions <- .fill_choice(node[["path"]], node[["about"]])
  if (n <= 1L) {
    return(list())
  }
  if (n <= DECISION_MAX_OPTIONS) {
    return(stats::setNames(
      list(as_list(choice(instructions, labels))),
      node[["key"]]
    ))
  }
  groups <- ceiling(n / DECISION_MAX_OPTIONS)
  size <- ceiling(n / groups)
  chunks <- split(labels, ceiling(seq_along(labels) / size))
  stats::setNames(
    lapply(chunks, function(g) as_list(choice(instructions, g))),
    paste0(node[["key"]], "~", seq_along(chunks))
  )
}


# %% .fill_first_round() ----
#' Every first-round question, in field order
#'
#' @param nodes List: Compiled nodes.
#'
#' @return Named list of wire questions.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.fill_first_round <- function(nodes) {
  out <- list()
  for (node in nodes) {
    if (!is.null(node[["presence"]])) {
      out[[node[["presence"]]]] <- as_list(noul(.fill_present(
        node[["path"]],
        node[["about"]]
      )))
    }
    out <- c(
      out,
      switch(
        node[["kind"]],
        choice = .fill_choice_first(node),
        noul = stats::setNames(
          list(as_list(noul(.fill_noul(node[["path"]], node[["about"]])))),
          node[["key"]]
        ),
        members = {
          labels <- node[["options"]][["label"]]
          stats::setNames(
            lapply(labels, function(l) {
              as_list(noul(.fill_member(node[["path"]], node[["about"]], l)))
            }),
            paste0(node[["key"]], ".", seq_along(labels) - 1L)
          )
        },
        list()
      )
    )
  }
  out
}


# %% .fill_finals() ----
#' Every tournament's final, after the first round
#'
#' @param nodes List: Compiled nodes.
#' @param answers Named list: First-round answers.
#'
#' @return Named list of wire questions; empty when no choice was a tournament
#'   with more than one group winner.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.fill_finals <- function(nodes, answers) {
  out <- list()
  for (node in nodes) {
    if (node[["kind"]] != "choice") {
      next
    }
    winners <- .fill_group_winners(node, answers)
    if (length(winners) > 1L) {
      out[[node[["key"]]]] <- as_list(choice(
        .fill_choice(node[["path"]], node[["about"]]),
        winners
      ))
    }
  }
  out
}


# %% .fill_group_winners() ----
.fill_group_winners <- function(node, answers) {
  if (length(node[["options"]][["label"]]) <= DECISION_MAX_OPTIONS) {
    return(character())
  }
  prefix <- paste0(node[["key"]], "~")
  keys <- names(answers)[startsWith(names(answers), prefix)]
  keys <- keys[order(as.integer(substring(keys, nchar(prefix) + 1L)))]
  vapply(
    keys,
    function(k) answers[[k]][["choice"]],
    character(1L),
    USE.NAMES = FALSE
  )
}


# %% .fill_assemble() ----
#' Assemble the filled document from the answers
#'
#' @param nodes List: Compiled nodes.
#' @param answers Named list: Every answer, all rounds.
#'
#' @return List of `value` (named list), `fields` (list per filled field:
#'   `path`, `value`, `p`, `options`, `unsure`, `by`) and `omitted` (character).
#'
#' @author EDG
#' @keywords internal
#' @noRd
.fill_assemble <- function(nodes, answers) {
  value <- list()
  fields <- list()
  omitted <- character()
  noul_p <- function(key) {
    a <- answers[[key]]
    if (is.null(a)) 0 else a[["p"]]
  }
  for (node in nodes) {
    if (!is.null(node[["presence"]]) && noul_p(node[["presence"]]) < 0.5) {
      omitted <- c(omitted, node[["path"]])
      next
    }
    field <- switch(
      node[["kind"]],
      choice = .fill_assemble_choice(node, answers),
      noul = {
        p <- noul_p(node[["key"]])
        v <- p >= 0.5
        list(
          value = v,
          label = if (v) "true" else "false",
          p = if (v) p else 1 - p,
          options = c("true" = p, "false" = 1 - p),
          unsure = abs(p - 0.5) < DECISION_NOUL_MARGIN,
          by = "noul"
        )
      },
      members = {
        labels <- node[["options"]][["label"]]
        p <- vapply(
          seq_along(labels) - 1L,
          function(i) noul_p(paste0(node[["key"]], ".", i)),
          numeric(1L)
        )
        keep <- p >= 0.5
        list(
          value = I(node[["options"]][["value"]][keep]),
          label = NA_character_,
          p = NA_real_,
          options = sort(stats::setNames(p, labels), decreasing = TRUE),
          unsure = any(abs(p - 0.5) < DECISION_NOUL_MARGIN),
          by = "nouls"
        )
      }
    )
    value[[node[["name"]]]] <- field[["value"]]
    fields[[length(fields) + 1L]] <- c(list(path = node[["path"]]), field)
  }
  list(value = value, fields = fields, omitted = omitted)
}


# %% .fill_assemble_choice() ----
.fill_assemble_choice <- function(node, answers) {
  labels <- node[["options"]][["label"]]
  pick <- if (length(labels) == 1L) {
    list(
      choice = labels,
      probabilities = stats::setNames(1, labels),
      confidence = 1,
      by = "elimination"
    )
  } else if (length(labels) <= DECISION_MAX_OPTIONS) {
    c(answers[[node[["key"]]]], by = "choice")
  } else {
    winners <- .fill_group_winners(node, answers)
    if (length(winners) == 1L) {
      list(
        choice = winners,
        probabilities = stats::setNames(1, winners),
        confidence = 1,
        by = "elimination"
      )
    } else {
      c(answers[[node[["key"]]]], by = "tournament")
    }
  }
  idx <- match(pick[["choice"]], labels)
  p <- unname(pick[["probabilities"]][pick[["choice"]]])
  list(
    value = node[["options"]][["value"]][[idx]],
    label = pick[["choice"]],
    p = p,
    options = pick[["probabilities"]],
    unsure = pick[["by"]] != "elimination" &&
      pick[["confidence"]] < DECISION_CHOICE_CONFIDENCE,
    by = pick[["by"]]
  )
}


# %% .fill_schema() ----
#' Fill a compiled schema from a state
#'
#' Two rounds at most: every field's question at once, an optional field's
#' value alongside whether it is there, then the final of any tournament.
#'
#' @param config DecisionConfig: Configuration.
#' @param compiled List: From `.fill_compile()`, with no open fields.
#' @param state Character: State.
#' @param images Character: Base64 images.
#' @param verbosity Integer: Verbosity level.
#'
#' @return List of `json` (character), `fields`, `omitted`, `calls`,
#'   `questions`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.fill_schema <- function(config, compiled, state, images, verbosity = 1L) {
  nodes <- compiled[["nodes"]]
  first <- .fill_first_round(nodes)
  asked <- if (length(first)) {
    .decision_ask(config, state, first, images, verbosity = verbosity)
  } else {
    list(answers = list(), calls = list())
  }
  answers <- asked[["answers"]]
  calls <- asked[["calls"]]
  finals <- .fill_finals(nodes, answers)
  if (length(finals)) {
    more <- .decision_ask(config, state, finals, images, verbosity = verbosity)
    answers <- c(answers, more[["answers"]])
    calls <- c(calls, more[["calls"]])
  }
  filled <- .fill_assemble(nodes, answers)
  json <- if (length(filled[["value"]])) {
    as.character(jsonlite::toJSON(
      filled[["value"]],
      auto_unbox = TRUE,
      digits = NA
    ))
  } else {
    "{}"
  }
  list(
    json = json,
    fields = filled[["fields"]],
    omitted = filled[["omitted"]],
    calls = calls,
    questions = length(first) + length(finals)
  )
}
