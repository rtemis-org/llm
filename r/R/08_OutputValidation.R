# Compiled engines stay local to this process, never in messages or saved reports.
.output_validator_cache <- new.env(parent = emptyenv())
.output_validator_cache[["keys"]] <- character()

#' Structured output validation report
#'
#' @field schema Optional Schema: Requested schema, without provider transformations.
#' @field output Character: Original response texts, with input names preserved.
#' @field status Character: One of valid, invalid, unavailable, or not_validated per input.
#' @field issues Data.frame: Diagnostic index, path, keyword, and message columns.
#' @keywords internal
#' @noRd
OutputValidation <- new_class(
  "OutputValidation",
  properties = list(
    schema = optional(Schema),
    output = class_character,
    status = class_character,
    issues = class_data.frame
  ),
  validator = function(self) {
    if (
      length(self@output) != length(self@status) ||
        anyNA(self@status) ||
        !all(
          self@status %in% c("valid", "invalid", "unavailable", "not_validated")
        )
    ) {
      abort("Supply one valid validation status per response.")
    }
    if (
      !identical(
        names(self@issues),
        c("index", "path", "keyword", "message")
      ) ||
        !is.integer(self@issues[["index"]]) ||
        anyNA(self@issues[["index"]]) ||
        any(!self@issues[["index"]] %in% seq_along(self@output)) ||
        !all(vapply(self@issues[-1L], is.character, logical(1L)))
    ) {
      abort(
        "Supply validation issues with input indices, paths, keywords, and messages."
      )
    }
    NULL
  }
)

#' Display a validation report
#' @param x OutputValidation: Validation report.
#' @param ... Additional print arguments.
#' @return The report, invisibly.
#' @keywords internal
#' @noRd
method(print, OutputValidation) <- function(x, ...) {
  counts <- table(factor(
    x@status,
    levels = c("valid", "invalid", "unavailable", "not_validated")
  ))
  cat("Schema validation:", paste(names(counts), counts, collapse = "; "), "\n")
  if (nrow(x@issues)) {
    print(x@issues, row.names = FALSE)
  }
  invisible(x)
}

#' Prepare a reusable local validator before generation
#' @param schema Optional Schema: Requested schema.
#' @param enabled Logical: Whether validation is enabled.
#' @param policy Character: Validation failure policy.
#' @return A compiled validation function, or NULL when disabled or without a schema.
#' @importFrom jsonvalidate json_validator
#' @keywords internal
#' @noRd
.prepare_output_validation <- new_generic(
  ".prepare_output_validation",
  "schema"
)
method(.prepare_output_validation, optional(Schema)) <- function(
  schema,
  enabled,
  policy
) {
  check_logical_scalar(enabled, "validate_output")
  if (
    length(policy) != 1L ||
      is.na(policy) ||
      !policy %in% c("warn", "collect", "abort")
  ) {
    abort('Set `on_validation_failure` to "warn", "collect", or "abort".')
  }
  if (!enabled || is.null(schema)) {
    return(NULL)
  }
  # Use full-precision numeric enum values and the user's schema contract.
  schema_json <- jsonlite::toJSON(
    as_list(schema),
    auto_unbox = TRUE,
    digits = NA
  )
  key <- digest::digest(schema_json, algo = "sha256")
  if (exists(key, .output_validator_cache, inherits = FALSE)) {
    return(.output_validator_cache[[key]])
  }
  validator <- tryCatch(
    jsonvalidate::json_validator(
      .json_validation_input(as.character(schema_json)),
      engine = "ajv"
    ),
    error = function(e) {
      abort(
        "Could not compile output schema. Check the schema and jsonvalidate installation.",
        class = "llm_validation_engine_error",
        parent = e
      )
    }
  )
  # Bound retained V8 contexts; do not attach these non-serializable functions to outputs.
  keys <- .output_validator_cache[["keys"]]
  if (length(keys) >= 16L) {
    rm(list = keys[[1L]], envir = .output_validator_cache)
    keys <- keys[-1L]
  }
  .output_validator_cache[[key]] <- validator
  .output_validator_cache[["keys"]] <- c(keys, key)
  validator
}

#' Validate response text without coercion or logging
#' @param schema Optional Schema: Requested schema.
#' @param output Character: Response texts.
#' @param validator Optional Function: Prepared local validator.
#' @return An OutputValidation report.
#' @keywords internal
#' @noRd
.validate_output_text <- new_generic(".validate_output_text", "schema")
method(.validate_output_text, optional(Schema)) <- function(
  schema,
  output,
  validator = NULL
) {
  check_inherits(output, "character")
  status <- rep("not_validated", length(output))
  status[is.na(output)] <- "unavailable"
  issues <- list(data.frame(
    index = integer(),
    path = character(),
    keyword = character(),
    message = character()
  ))
  if (!is.null(validator)) {
    for (i in which(!is.na(output))) {
      json <- output[[i]]
      # Ajv's R bridge accepts JavaScript expressions: reject non-JSON first.
      syntax <- jsonlite::validate(json)
      if (!isTRUE(syntax)) {
        status[[i]] <- "invalid"
        issues[[length(issues) + 1L]] <- data.frame(
          index = as.integer(i),
          path = "",
          keyword = "parse",
          message = attr(syntax, "err") %||% "Invalid JSON."
        )
        next
      }
      result <- tryCatch(
        validator(.json_validation_input(json), verbose = TRUE),
        error = function(e) {
          abort(
            "Output validation engine failed. Check the jsonvalidate installation.",
            class = "llm_validation_engine_error",
            parent = e
          )
        }
      )
      status[[i]] <- if (isTRUE(result)) "valid" else "invalid"
      if (!isTRUE(result)) {
        errors <- attr(result, "errors")
        issues[[length(issues) + 1L]] <- data.frame(
          index = rep(as.integer(i), nrow(errors)),
          path = errors[["instancePath"]] %||% errors[["dataPath"]],
          keyword = errors[["keyword"]],
          message = errors[["message"]]
        )
      }
    }
  }
  OutputValidation(
    schema = schema,
    output = output,
    status = status,
    issues = do.call(rbind, issues)
  )
}

#' Validate saved structured responses
#'
#' Checks strict JSON syntax and the requested schema locally, without changing
#' responses or logging validation failures. Does not coerce values or repair JSON.
#' Extra properties are allowed by the current Schema; optional fields may be absent
#' but may not be null. Array and object fields constrain only their outer type.
#'
#' @param x Character, Message, or list: JSON response text(s), a single message,
#'   an LLM batch, or a list of agent conversations. A flat message list is an LLM
#'   batch, as in [responses]; wrap one agent conversation in `list()` to check only
#'   its final assistant answer. A result directly from `generate(agent, ...)` is
#'   recognized as a single conversation.
#' @param schema Schema: Requested output schema created with [schema].
#' @return An S7 validation report with `@output` (original text), `@status`
#'   (valid, invalid, unavailable, or not_validated), `@schema`, and `@issues`
#'   (a data.frame with index, path, keyword, and message). Missing responses are
#'   unavailable; malformed JSON is invalid. Empty input gives an empty report.
#' @export
#' @examples
#' sch <- schema("Count", field("n", type = "integer"))
#' report <- validate_output(c('{"n":10}', '{"n":"10"}', NA_character_), sch)
#' report@status
#' report@issues
validate_output <- new_generic(
  "validate_output",
  c("x", "schema"),
  function(x, schema) S7_dispatch()
)
method(validate_output, list(class_character, Schema)) <- function(x, schema) {
  .validate_output_text(
    schema,
    x,
    .prepare_output_validation(schema, TRUE, "collect")
  )
}
method(validate_output, list(new_S3_class("json"), Schema)) <- function(
  x,
  schema
) {
  validate_output(as.character(x), schema)
}
method(validate_output, list(Message, Schema)) <- function(x, schema) {
  text <- if (x@role == "assistant") x@content else NA_character_
  if (S7_inherits(x, LLMMessage) && length(x@tool_calls)) {
    text <- NA_character_
  }
  validate_output(text, schema)
}
method(validate_output, list(class_list, Schema)) <- function(x, schema) {
  # An attached one-answer report disambiguates an Agent history from an LLM batch.
  if (isTRUE(attr(x, "agent_output"))) {
    x <- list(x)
  }
  if (all(vapply(x, .is_null_or_message, logical(1L)))) {
    text <- vapply(
      x,
      function(m) {
        if (
          is.null(m) ||
            m@role != "assistant" ||
            (S7_inherits(m, LLMMessage) && length(m@tool_calls))
        ) {
          NA_character_
        } else {
          m@content
        }
      },
      character(1L)
    )
  } else if (all(vapply(x, .is_null_or_message_list, logical(1L)))) {
    text <- vapply(
      x,
      function(messages) {
        asst <- Filter(function(m) m@role == "assistant", messages)
        if (!length(asst)) {
          return(NA_character_)
        }
        m <- asst[[length(asst)]]
        if (S7_inherits(m, LLMMessage) && length(m@tool_calls)) {
          NA_character_
        } else {
          m@content
        }
      },
      character(1L)
    )
  } else {
    abort(
      "Pass JSON text, Messages, or a list of agent conversations to validate_output()."
    )
  }
  validate_output(text, schema)
}

#' Retrieve an attached validation report
#' @param x Message, character, or list: A generated message, extracted response(s),
#'   agent history, or batch result.
#' @return An OutputValidation report, or NULL if none is attached. With a supplied
#'   schema and disabled validation, the attached report records not_validated.
#' @export
#' @examples
#' validation_results("unvalidated text")
validation_results <- new_generic("validation_results", "x", function(x) {
  S7_dispatch()
})
method(validation_results, Message) <- function(x) x@metadata[["validation"]]
method(validation_results, class_character | class_list) <- function(x) {
  attr(x, "validation", exact = TRUE)
}

#' Apply the configured validation failure policy
#' @param report OutputValidation: Completed report.
#' @param policy Character: Failure policy.
#' @param verbosity Integer: Logging verbosity.
#' @return The report, invisibly; aborts only for invalid output with policy abort.
#' @keywords internal
#' @noRd
.report_output_validation <- new_generic(".report_output_validation", "report")
method(.report_output_validation, OutputValidation) <- function(
  report,
  policy,
  verbosity
) {
  invalid <- sum(report@status == "invalid")
  if (!invalid || policy == "collect") {
    return(invisible(report))
  }
  if (policy == "abort") {
    abort(
      "Output failed schema validation. Inspect the condition's validation report; ",
      'use `on_validation_failure = "collect"` to retain invalid responses normally.',
      class = "llm_output_validation_error",
      data = list(output = report@output, validation = report)
    )
  }
  warn(
    "Schema validation: ",
    sum(report@status == "valid"),
    "/",
    length(report@status),
    " valid; ",
    invalid,
    " invalid; ",
    sum(report@status == "unavailable"),
    " unavailable. Outputs retained. Use validation_results() for diagnostics.",
    use_warning = FALSE,
    verbosity = verbosity
  )
  invisible(report)
}

#' Annotate a final generated message before returning or committing it
#' @param x LLMMessage: Final answer or unfinished tool-call message.
#' @param schema Optional Schema: Requested schema.
#' @param validator Optional Function: Prepared validator.
#' @param policy Character: Failure policy.
#' @param verbosity Integer: Logging verbosity.
#' @return The message with validation metadata; content is unchanged.
#' @keywords internal
#' @noRd
.validate_generated_message <- new_generic(".validate_generated_message", "x")
method(.validate_generated_message, LLMMessage) <- function(
  x,
  schema,
  validator,
  policy,
  verbosity
) {
  if (is.null(schema)) {
    return(x)
  }
  # Tool calls do not constitute a final structured answer.
  report <- if (length(x@tool_calls)) {
    .validate_output_text(schema, NA_character_)
  } else {
    .validate_output_text(schema, x@content, validator)
  }
  x@metadata[["validation"]] <- report
  .report_output_validation(report, policy, verbosity)
  x
}

#' Combine per-call validation reports without validating a second time
#' @param x List: Completed batch, including NULL slots for generation failures.
#' @param schema Schema: Batch schema.
#' @param failures List: Reports retained from explicit validation aborts.
#' @return An OutputValidation report aligned to the batch.
#' @keywords internal
#' @noRd
.combine_output_validation <- new_generic(".combine_output_validation", "x")
method(.combine_output_validation, class_list) <- function(
  x,
  schema,
  failures = vector("list", length(x))
) {
  out <- rep(NA_character_, length(x))
  names(out) <- names(x)
  status <- rep("unavailable", length(x))
  issues <- list(data.frame(
    index = integer(),
    path = character(),
    keyword = character(),
    message = character()
  ))
  for (i in seq_along(x)) {
    report <- failures[[i]]
    if (is.null(x[[i]]) && is.null(report)) {
      next
    }
    if (is.null(report)) {
      report <- validation_results(x[[i]])
    }
    if (is.null(report)) {
      status[[i]] <- "not_validated"
      next
    }
    out[[i]] <- report@output[[1L]]
    status[[i]] <- report@status[[1L]]
    if (nrow(report@issues)) {
      rows <- report@issues
      rows[["index"]] <- rep(as.integer(i), nrow(rows))
      issues[[length(issues) + 1L]] <- rows
    }
  }
  OutputValidation(
    schema = schema,
    output = out,
    status = status,
    issues = do.call(rbind, issues)
  )
}

#' Preserve validation metadata during response extraction
#' @param x Character: Extracted response text.
#' @param from Message or list: Source result carrying the report.
#' @return Response text with its validation attribute preserved.
#' @keywords internal
#' @noRd
.keep_output_validation <- new_generic(".keep_output_validation", "x")
method(.keep_output_validation, class_character) <- function(x, from) {
  report <- validation_results(from)
  if (!is.null(report)) {
    attr(x, "validation") <- report
  }
  x
}

#' Preserve JSON semantics across the validator's JavaScript bridge
#' @param x Character: A single schema or response JSON string.
#' @return An escaped JSON.parse expression marked as JSON input for jsonvalidate.
#' @keywords internal
#' @noRd
.json_validation_input <- new_generic(".json_validation_input", "x")
method(.json_validation_input, class_character) <- function(x) {
  # jsonvalidate 1.5 passes its input as a JavaScript expression. Parsing an
  # escaped string preserves literal __proto__ keys instead of setting prototypes.
  # The explicit json class also prevents scalar data from being read as a file.
  structure(
    paste0(
      "JSON.parse(",
      jsonlite::toJSON(x, auto_unbox = TRUE),
      ", function(k, v) {",
      "if (v !== null && typeof v === 'object' && !Array.isArray(v)) ",
      "Object.setPrototypeOf(v, null); return v; })"
    ),
    class = "json"
  )
}
