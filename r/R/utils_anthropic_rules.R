# utils_anthropic_rules.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# %% Model request rules ----
# spec: llm/anthropic-adapter#model-table
# The rules follow the claude-api reference (Thinking & Effort table, Structured
# Outputs, and the per-model migration sections) as of 2026-10-15.
ANTHROPIC_FALLBACK_BETA_DEFAULT <- "server-side-fallback-2026-07-01"
ANTHROPIC_FALLBACK_BETA_MODELS <- "server-side-fallback-2026-06-01"
ANTHROPIC_FALLBACK_MAX <- 3L
ANTHROPIC_OUTPUT_SCHEMA_DROP <- c(
  "minimum",
  "maximum",
  "exclusiveMinimum",
  "exclusiveMaximum",
  "multipleOf",
  "minLength",
  "maxLength"
)


# %% .anthropic_profile() ----
#' Define the Request Rules of an Anthropic Model Family
#'
#' @param ids Character: Normalized model ids of the family (see `.anthropic_model_id()`).
#' @param sampling Character \{"free", "exclusive", "default", "none"\}: Sampling parameters the
#' model accepts: all of them; `temperature` or `top_p` but not both; only their default values;
#' none.
#' @param thinking_adaptive Logical: Whether `thinking: {type: "adaptive"}` is accepted.
#' @param thinking_budget Logical: Whether `thinking: {type: "enabled", budget_tokens}` is
#' accepted.
#' @param thinking_default Character \{"on", "off"\}: What a request without `thinking` runs.
#' @param thinking_off Character \{"omit", "disabled", "between_tools", "none"\}: How a request
#' turns thinking off: by omitting `thinking`, with `{type: "disabled"}`, with
#' `{type: "between_tools"}`, or not at all.
#' @param thinking_off_effort_max Optional Character: Highest effort level at which thinking can be
#' turned off; `NA` for no limit.
#' @param effort_levels Character: Accepted `output_config.effort` levels; empty when `effort` is
#' rejected.
#' @param effort_default Optional Character: Effort level of a request without `effort`.
#' @param structured_outputs Logical: Whether `output_config.format` is accepted.
#' @param forced_tool_choice Logical: Whether `tool_choice` `"tool"` and `"any"` are accepted.
#' @param prefill Logical: Whether a trailing assistant message is accepted.
#' @param fallbacks Character \{"none", "default", "any"\}: Server-side fallbacks accepted: none;
#' `"default"` only; `"default"` or a list of models.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_profile <- function(
  ids,
  sampling,
  thinking_adaptive,
  thinking_budget,
  thinking_default,
  thinking_off,
  thinking_off_effort_max = NA_character_,
  effort_levels = ANTHROPIC_EFFORT_LEVELS,
  effort_default = "high",
  structured_outputs = TRUE,
  forced_tool_choice = TRUE,
  prefill = FALSE,
  fallbacks = "none"
) {
  list(
    ids = ids,
    known = TRUE,
    sampling = sampling,
    thinking_adaptive = thinking_adaptive,
    thinking_budget = thinking_budget,
    thinking_default = thinking_default,
    thinking_off = thinking_off,
    thinking_off_effort_max = thinking_off_effort_max,
    effort_levels = effort_levels,
    effort_default = effort_default,
    structured_outputs = structured_outputs,
    forced_tool_choice = forced_tool_choice,
    prefill = prefill,
    fallbacks = fallbacks
  )
}


# %% ANTHROPIC_MODEL_PROFILES ----
# One entry per model family, newest first.
ANTHROPIC_MODEL_PROFILES <- list(
  .anthropic_profile(
    ids = c("claude-fable-5-1", "claude-mythos-5-1"),
    sampling = "none",
    thinking_adaptive = TRUE,
    thinking_budget = FALSE,
    thinking_default = "on",
    thinking_off = "none",
    forced_tool_choice = FALSE,
    fallbacks = "any"
  ),
  .anthropic_profile(
    ids = c("claude-fable-5", "claude-mythos-5"),
    sampling = "none",
    thinking_adaptive = TRUE,
    thinking_budget = FALSE,
    thinking_default = "on",
    thinking_off = "none"
  ),
  .anthropic_profile(
    ids = "claude-opus-5-5",
    sampling = "none",
    thinking_adaptive = TRUE,
    thinking_budget = FALSE,
    thinking_default = "on",
    thinking_off = "none",
    effort_default = "medium",
    forced_tool_choice = FALSE,
    fallbacks = "any"
  ),
  .anthropic_profile(
    ids = "claude-opus-5",
    sampling = "none",
    thinking_adaptive = TRUE,
    thinking_budget = FALSE,
    thinking_default = "on",
    thinking_off = "disabled",
    thinking_off_effort_max = "high",
    fallbacks = "any"
  ),
  .anthropic_profile(
    ids = "claude-opus-4-8",
    sampling = "none",
    thinking_adaptive = TRUE,
    thinking_budget = FALSE,
    thinking_default = "off",
    thinking_off = "omit"
  ),
  .anthropic_profile(
    ids = "claude-opus-4-7",
    sampling = "none",
    thinking_adaptive = TRUE,
    thinking_budget = FALSE,
    thinking_default = "off",
    thinking_off = "omit",
    structured_outputs = FALSE
  ),
  .anthropic_profile(
    ids = c("claude-opus-4-6", "claude-sonnet-4-6"),
    sampling = "exclusive",
    thinking_adaptive = TRUE,
    thinking_budget = TRUE,
    thinking_default = "off",
    thinking_off = "omit",
    effort_levels = c("low", "medium", "high", "max"),
    structured_outputs = FALSE
  ),
  .anthropic_profile(
    ids = "claude-sonnet-5-5",
    sampling = "default",
    thinking_adaptive = TRUE,
    thinking_budget = FALSE,
    thinking_default = "on",
    thinking_off = "between_tools",
    thinking_off_effort_max = "high",
    forced_tool_choice = FALSE,
    fallbacks = "default"
  ),
  .anthropic_profile(
    ids = "claude-sonnet-5",
    sampling = "default",
    thinking_adaptive = TRUE,
    thinking_budget = FALSE,
    thinking_default = "on",
    thinking_off = "disabled"
  ),
  .anthropic_profile(
    ids = "claude-haiku-5-5",
    sampling = "default",
    thinking_adaptive = TRUE,
    thinking_budget = FALSE,
    thinking_default = "on",
    thinking_off = "disabled",
    thinking_off_effort_max = "high",
    effort_default = "medium"
  ),
  .anthropic_profile(
    ids = "claude-opus-4-5",
    sampling = "exclusive",
    thinking_adaptive = FALSE,
    thinking_budget = TRUE,
    thinking_default = "off",
    thinking_off = "omit",
    effort_levels = c("low", "medium", "high"),
    prefill = TRUE
  ),
  .anthropic_profile(
    ids = c("claude-opus-4-1", "claude-haiku-4-5"),
    sampling = "exclusive",
    thinking_adaptive = FALSE,
    thinking_budget = TRUE,
    thinking_default = "off",
    thinking_off = "omit",
    effort_levels = character(),
    effort_default = NA_character_,
    prefill = TRUE
  ),
  .anthropic_profile(
    ids = c(
      "claude-sonnet-4-5",
      "claude-opus-4",
      "claude-opus-4-0",
      "claude-sonnet-4",
      "claude-sonnet-4-0"
    ),
    sampling = "exclusive",
    thinking_adaptive = FALSE,
    thinking_budget = TRUE,
    thinking_default = "off",
    thinking_off = "omit",
    effort_levels = character(),
    effort_default = NA_character_,
    structured_outputs = FALSE,
    prefill = TRUE
  )
)


# %% ANTHROPIC_PASS_THROUGH_PROFILE ----
# Model names absent from the table: every configured field is sent as given,
# structured output uses the forced tool (the form Anthropic-compatible servers
# implement), and no rule aborts before the request.
ANTHROPIC_PASS_THROUGH_PROFILE <- list(
  ids = character(),
  known = FALSE,
  sampling = "free",
  thinking_adaptive = TRUE,
  thinking_budget = TRUE,
  thinking_default = NA_character_,
  thinking_off = "omit",
  thinking_off_effort_max = NA_character_,
  effort_levels = ANTHROPIC_EFFORT_LEVELS,
  effort_default = NA_character_,
  structured_outputs = FALSE,
  forced_tool_choice = TRUE,
  prefill = TRUE,
  fallbacks = "any"
)


# %% .anthropic_model_id() ----
#' Normalize an Anthropic Model Name to Its Family Id
#'
#' Removes the Amazon Bedrock prefix (`anthropic.`, `us.anthropic.`), the Bedrock version suffix
#' (`-v1:0`) and a snapshot date (`-20251001`, `@20251001`).
#'
#' @param x Character: Model name.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_model_id <- function(x) {
  x <- tolower(x)
  x <- sub("^([a-z]+\\.)?anthropic\\.", "", x)
  x <- sub("-v[0-9]+(:[0-9]+)?$", "", x)
  sub("[-@][0-9]{8}$", "", x)
}


# %% .anthropic_model_profile() ----
#' Resolve the Request Rules for an Anthropic Model
#'
#' @param model_name Character: Model name.
#' @param capabilities Optional list: `capabilities` from the Models API, which replace the
#' table's structured-output, thinking-type and effort-level entries.
#'
#' @return Named list: The family's rules from `ANTHROPIC_MODEL_PROFILES`, or
#' `ANTHROPIC_PASS_THROUGH_PROFILE`, with `model` set to `model_name`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_model_profile <- function(model_name, capabilities = NULL) {
  id <- .anthropic_model_id(model_name)
  profile <- ANTHROPIC_PASS_THROUGH_PROFILE
  for (p in ANTHROPIC_MODEL_PROFILES) {
    if (id %in% p[["ids"]]) {
      profile <- p
      break
    }
  }
  profile[["model"]] <- model_name
  if (!is.null(capabilities)) {
    profile <- .anthropic_apply_capabilities(profile, capabilities)
  }
  profile
}


# %% .anthropic_capability_entries() ----
#' Read Request Rules From Models API Capabilities
#'
#' @param capabilities List: `capabilities` object of `GET /models/{id}`.
#'
#' @return Named list with the entries the capabilities report: `structured_outputs`,
#' `thinking_adaptive`, `thinking_budget`, `effort_levels`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_capability_entries <- function(capabilities) {
  supported <- function(...) {
    node <- capabilities
    for (key in c(...)) {
      if (!is.list(node) || is.null(node[[key]])) {
        return(NULL)
      }
      node <- node[[key]]
    }
    value <- if (is.list(node)) node[["supported"]] else NULL
    if (is.logical(value) && length(value) == 1L && !is.na(value)) {
      value
    } else {
      NULL
    }
  }
  out <- list(
    structured_outputs = supported("structured_outputs"),
    thinking_adaptive = supported("thinking", "types", "adaptive"),
    thinking_budget = supported("thinking", "types", "enabled")
  )
  effort <- supported("effort")
  if (isFALSE(effort)) {
    out[["effort_levels"]] <- character()
  } else if (isTRUE(effort)) {
    levels <- Filter(
      function(level) isTRUE(supported("effort", level)),
      ANTHROPIC_EFFORT_LEVELS
    )
    out[["effort_levels"]] <- as.character(levels)
  }
  out[!vapply(out, is.null, logical(1L))]
}


# %% .anthropic_apply_capabilities() ----
#' Overlay Models API Capabilities on a Model Profile
#'
#' @param profile List: Model profile.
#' @param capabilities List: `capabilities` object of `GET /models/{id}`.
#'
#' @return Named list: `profile` with each reported entry replaced, and `capability_differences`
#' naming the entries where a table profile disagreed with the report.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_apply_capabilities <- function(profile, capabilities) {
  entries <- .anthropic_capability_entries(capabilities)
  differences <- character()
  for (name in names(entries)) {
    if (profile[["known"]] && !identical(profile[[name]], entries[[name]])) {
      differences <- c(differences, name)
    }
    profile[[name]] <- entries[[name]]
  }
  profile[["capability_differences"]] <- differences
  profile
}


# %% .abort_anthropic_rule() ----
#' Abort on a Request Field the Model Rejects
#'
#' @param profile List: Model profile.
#' @param parameter Character: Name of the rejected argument.
#' @param ... Message components passed to `abort()`.
#'
#' @return Does not return.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.abort_anthropic_rule <- function(profile, parameter, ...) {
  abort(
    ...,
    class = c("rtemis_llm_unsupported_parameter", "rtemis_input_error"),
    data = list(
      provider = "Anthropic",
      model = profile[["model"]],
      parameter = parameter
    )
  )
}


# %% .anthropic_sampling_fields() ----
#' Resolve Anthropic Sampling Fields
#'
#' @param profile List: Model profile.
#' @param temperature Optional numeric: Sampling temperature.
#' @param top_p Optional numeric: Nucleus sampling cutoff.
#' @param top_k Optional integer: Top-K sampling cutoff.
#'
#' @return Named list of the sampling fields to send.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_sampling_fields <- function(
  profile,
  temperature = NULL,
  top_p = NULL,
  top_k = NULL
) {
  model <- profile[["model"]]
  fields <- list(
    temperature = temperature,
    top_p = top_p,
    top_k = if (!is.null(top_k)) as.integer(top_k)
  )
  fields <- fields[!vapply(fields, is.null, logical(1L))]
  switch(
    profile[["sampling"]],
    none = {
      if (length(fields) > 0L) {
        .abort_anthropic_rule(
          profile,
          names(fields)[1L],
          "Model '",
          model,
          "' does not accept `",
          names(fields)[1L],
          "`.\nLeave `",
          names(fields)[1L],
          "` NULL so that the request omits it."
        )
      }
    },
    default = {
      # Claude Sonnet 5, Claude Sonnet 5.5 and Claude Haiku 5.5 accept a
      # sampling field only at its default value, which equals omitting it.
      if (identical(as.numeric(fields[["temperature"]]), 1)) {
        fields[["temperature"]] <- NULL
      }
      if (length(fields) > 0L) {
        .abort_anthropic_rule(
          profile,
          names(fields)[1L],
          "Model '",
          model,
          "' accepts only the default value of `",
          names(fields)[1L],
          "`.\nLeave `",
          names(fields)[1L],
          "` NULL so that the request omits it."
        )
      }
    },
    exclusive = {
      if (!is.null(fields[["temperature"]]) && !is.null(fields[["top_p"]])) {
        .abort_anthropic_rule(
          profile,
          "top_p",
          "Model '",
          model,
          "' accepts `temperature` or `top_p`, not both.\n",
          "Set one of them to NULL."
        )
      }
    }
  )
  fields
}


# %% .anthropic_effort_index() ----
#' Position of an Effort Level in ANTHROPIC_EFFORT_LEVELS
#'
#' @param x Optional Character: Effort level.
#'
#' @return Integer, or NA for `NULL` or `NA`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_effort_index <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(NA_integer_)
  }
  match(x, ANTHROPIC_EFFORT_LEVELS)
}


# %% .anthropic_thinking_field() ----
#' Resolve the Anthropic `thinking` Field
#'
#' `think = NULL` sends a budget when one is configured and otherwise omits `thinking`, so the
#' model runs its default. `TRUE` sends the configured budget, or adaptive thinking. `FALSE` sends
#' the model's off form.
#'
#' @param profile List: Model profile.
#' @param think Optional logical: Whether thinking is on.
#' @param thinking_budget_tokens Optional integer: Budget for `{type: "enabled"}`.
#' @param effort Optional character: Effort level of the request.
#' @param max_tokens Integer: `max_tokens` of the request.
#'
#' @return Optional named list: The `thinking` field.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_thinking_field <- function(
  profile,
  think = NULL,
  thinking_budget_tokens = NULL,
  effort = NULL,
  max_tokens
) {
  model <- profile[["model"]]
  if (isFALSE(think)) {
    off <- profile[["thinking_off"]]
    if (identical(off, "none")) {
      .abort_anthropic_rule(
        profile,
        "think",
        "Model '",
        model,
        "' always thinks; `think = FALSE` has no request form.\n",
        "Set `think` to NULL, and set `effort = \"low\"` to reduce thinking."
      )
    }
    effort_max <- profile[["thinking_off_effort_max"]]
    effort_in_effect <- effort %||% profile[["effort_default"]]
    if (
      !is.na(effort_max) &&
        isTRUE(
          .anthropic_effort_index(effort_in_effect) >
            .anthropic_effort_index(effort_max)
        )
    ) {
      .abort_anthropic_rule(
        profile,
        "think",
        "Model '",
        model,
        "' turns thinking off only at effort \"",
        effort_max,
        "\" or below; the request's effort is \"",
        effort_in_effect,
        "\".\nLower `effort`, or set `think` to NULL."
      )
    }
    return(switch(
      off,
      omit = NULL,
      disabled = list(type = "disabled"),
      between_tools = list(type = "between_tools")
    ))
  }
  if (!is.null(thinking_budget_tokens)) {
    if (!profile[["thinking_budget"]]) {
      .abort_anthropic_rule(
        profile,
        "thinking_budget_tokens",
        "Model '",
        model,
        "' does not accept `thinking_budget_tokens`: its thinking is adaptive.\n",
        "Leave `thinking_budget_tokens` NULL, and set `effort` to control how much it thinks."
      )
    }
    if (thinking_budget_tokens >= max_tokens) {
      .abort_anthropic_rule(
        profile,
        "thinking_budget_tokens",
        "`thinking_budget_tokens` (",
        thinking_budget_tokens,
        ") must be less than `max_tokens` (",
        max_tokens,
        ").\nRaise `max_tokens` or lower `thinking_budget_tokens`."
      )
    }
    return(list(type = "enabled", budget_tokens = thinking_budget_tokens))
  }
  if (isTRUE(think)) {
    if (!profile[["thinking_adaptive"]]) {
      .abort_anthropic_rule(
        profile,
        "think",
        "`think = TRUE` on model '",
        model,
        "' requires `thinking_budget_tokens` (minimum ",
        ANTHROPIC_THINKING_MIN_BUDGET,
        ").\nSet `thinking_budget_tokens` on config_Anthropic()."
      )
    }
    return(list(type = "adaptive"))
  }
  NULL
}


# %% .anthropic_check_effort() ----
#' Check an Effort Level Against the Model's Levels
#'
#' @param profile List: Model profile.
#' @param effort Optional character: Effort level.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_check_effort <- function(profile, effort = NULL) {
  if (is.null(effort)) {
    return(invisible(NULL))
  }
  levels <- profile[["effort_levels"]]
  if (length(levels) == 0L) {
    .abort_anthropic_rule(
      profile,
      "effort",
      "Model '",
      profile[["model"]],
      "' does not accept `effort`.\nLeave `effort` NULL."
    )
  }
  if (!effort %in% levels) {
    .abort_anthropic_rule(
      profile,
      "effort",
      "Model '",
      profile[["model"]],
      "' accepts `effort` ",
      paste0("\"", levels, "\"", collapse = ", "),
      "; the request sets \"",
      effort,
      "\"."
    )
  }
  invisible(NULL)
}


# %% .anthropic_fallbacks_field() ----
#' Resolve the Anthropic `fallbacks` Field
#'
#' @param profile List: Model profile.
#' @param fallbacks Optional character: `"default"`, or fallback model names.
#'
#' @return `"default"`, a list of `{model}` objects, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_fallbacks_field <- function(profile, fallbacks = NULL) {
  if (is.null(fallbacks)) {
    return(NULL)
  }
  accepted <- profile[["fallbacks"]]
  if (identical(accepted, "none")) {
    .abort_anthropic_rule(
      profile,
      "fallbacks",
      "Model '",
      profile[["model"]],
      "' has no server-side fallback.\nLeave `fallbacks` NULL."
    )
  }
  if (identical(fallbacks, "default")) {
    return("default")
  }
  if (identical(accepted, "default")) {
    .abort_anthropic_rule(
      profile,
      "fallbacks",
      "Model '",
      profile[["model"]],
      "' accepts only `fallbacks = \"default\"`."
    )
  }
  lapply(unname(fallbacks), function(model) list(model = model))
}


# %% .anthropic_fallbacks_beta() ----
#' Beta Header for a `fallbacks` Setting
#'
#' @param fallbacks Optional character: `"default"`, or fallback model names.
#'
#' @return Optional character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_fallbacks_beta <- function(fallbacks = NULL) {
  if (is.null(fallbacks)) {
    return(NULL)
  }
  if (identical(fallbacks, "default")) {
    ANTHROPIC_FALLBACK_BETA_DEFAULT
  } else {
    ANTHROPIC_FALLBACK_BETA_MODELS
  }
}


# %% .anthropic_request_fields() ----
#' Resolve the Model-Dependent Fields of an Anthropic Request
#'
#' Applies the model's rules to sampling, thinking, effort and fallbacks, and aborts on a value
#' the model rejects.
#'
#' @param profile List: Model profile.
#' @param temperature,top_p,top_k Optional numeric: Sampling parameters.
#' @param think Optional logical: Whether thinking is on.
#' @param thinking_budget_tokens Optional integer: Thinking budget.
#' @param effort Optional character: Effort level.
#' @param fallbacks Optional character: `"default"`, or fallback model names.
#' @param max_tokens Integer: `max_tokens` of the request.
#'
#' @return Named list: `sampling` (named list), `thinking`, `thinking_on` (logical, NA when the
#' model's default is unknown), `effort`, `fallbacks`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_request_fields <- function(
  profile,
  temperature = NULL,
  top_p = NULL,
  top_k = NULL,
  think = NULL,
  thinking_budget_tokens = NULL,
  effort = NULL,
  fallbacks = NULL,
  max_tokens
) {
  sampling <- .anthropic_sampling_fields(
    profile,
    temperature = temperature,
    top_p = top_p,
    top_k = top_k
  )
  .anthropic_check_effort(profile, effort)
  thinking <- .anthropic_thinking_field(
    profile,
    think = think,
    thinking_budget_tokens = thinking_budget_tokens,
    effort = effort,
    max_tokens = max_tokens
  )
  thinking_on <- if (is.null(thinking)) {
    if (isFALSE(think)) FALSE else profile[["thinking_default"]] == "on"
  } else {
    thinking[["type"]] %in% c("adaptive", "enabled")
  }
  list(
    sampling = sampling,
    thinking = thinking,
    thinking_on = thinking_on,
    effort = effort,
    fallbacks = .anthropic_fallbacks_field(profile, fallbacks)
  )
}


# %% .anthropic_check_prefill() ----
#' Abort on a Trailing Assistant Message the Model Rejects
#'
#' @param profile List: Model profile.
#' @param messages List: Anthropic-shaped messages.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_check_prefill <- function(profile, messages) {
  n <- length(messages)
  if (
    n > 0L &&
      identical(messages[[n]][["role"]], "assistant") &&
      !profile[["prefill"]]
  ) {
    .abort_anthropic_rule(
      profile,
      "messages",
      "Model '",
      profile[["model"]],
      "' does not accept a trailing assistant message (prefill).\n",
      "End the conversation with a user message; use `output_schema` to constrain the format."
    )
  }
  invisible(NULL)
}


# %% .check_anthropic_effort_arg() ----
#' Validate the `effort` Argument
#'
#' @param effort Optional character: Effort level.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_anthropic_effort_arg <- function(effort) {
  if (is.null(effort)) {
    return(invisible(NULL))
  }
  if (
    !is.character(effort) ||
      length(effort) != 1L ||
      is.na(effort) ||
      !effort %in% ANTHROPIC_EFFORT_LEVELS
  ) {
    abort(
      "`effort` must be one of ",
      paste0("\"", ANTHROPIC_EFFORT_LEVELS, "\"", collapse = ", "),
      ", or NULL.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(NULL)
}


# %% .check_anthropic_fallbacks_arg() ----
#' Validate the `fallbacks` Argument
#'
#' @param fallbacks Optional character: `"default"`, or one to three fallback model names.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_anthropic_fallbacks_arg <- function(fallbacks) {
  if (is.null(fallbacks)) {
    return(invisible(NULL))
  }
  valid <- is.character(fallbacks) &&
    length(fallbacks) >= 1L &&
    length(fallbacks) <= ANTHROPIC_FALLBACK_MAX &&
    !anyNA(fallbacks) &&
    all(nzchar(trimws(fallbacks))) &&
    !anyDuplicated(fallbacks) &&
    (identical(fallbacks, "default") || !"default" %in% fallbacks)
  if (!valid) {
    abort(
      "`fallbacks` must be \"default\", or 1 to ",
      ANTHROPIC_FALLBACK_MAX,
      " distinct model names, or NULL.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(NULL)
}


# %% .warn_anthropic_profile() ----
#' Report Gaps in the Model Table at Configuration Time
#'
#' Warns when a `claude-*` name is absent from the table and no capabilities were read, and when
#' the Models API disagrees with the table.
#'
#' @param profile List: Model profile.
#' @param capabilities Optional list: Models API capabilities.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.warn_anthropic_profile <- function(profile, capabilities = NULL) {
  model <- profile[["model"]]
  if (
    !profile[["known"]] &&
      is.null(capabilities) &&
      startsWith(.anthropic_model_id(model), "claude-")
  ) {
    warn(
      "Model '",
      model,
      "' is not in rtemis.llm's Anthropic model table: requests send every ",
      "configured field as given and use a forced tool for structured output.\n",
      "Set `validate_model = TRUE` to read the model's capabilities from the Models API.",
      use_warning = TRUE
    )
  }
  differences <- profile[["capability_differences"]]
  if (length(differences) > 0L) {
    warn(
      "The Models API capabilities of '",
      model,
      "' differ from rtemis.llm's model table in: ",
      paste(differences, collapse = ", "),
      ". Requests follow the Models API.",
      use_warning = TRUE
    )
  }
  invisible(NULL)
}


# %% .abort_anthropic_refusal() ----
#' Abort on a Refused Anthropic Request
#'
#' A response with `stop_reason: "refusal"` is a failed call. The condition carries the
#' `stop_details` category and explanation, which are informational and can be NULL.
#'
#' @param res List: Parsed response body.
#' @param model Character: Model that produced the response.
#' @param content Character: Text returned before the refusal.
#' @param request_id Optional character: Request id.
#'
#' @return Does not return.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.abort_anthropic_refusal <- function(res, model, content, request_id = NULL) {
  details <- res[["stop_details"]]
  category <- if (is.list(details)) details[["category"]] else NULL
  explanation <- if (is.list(details)) details[["explanation"]] else NULL
  abort(
    "Model '",
    model,
    "' declined the request (stop_reason \"refusal\"",
    if (!is.null(category)) paste0(", category \"", category, "\""),
    ").",
    if (!is.null(explanation)) paste0("\n", explanation),
    if (!is.null(request_id)) paste0("\nRequest id: ", request_id, "."),
    class = "rtemis_llm_refusal",
    data = list(
      provider = "Anthropic",
      model = model,
      request_id = request_id,
      stop_reason = "refusal",
      category = category,
      explanation = explanation,
      stop_details = details,
      content = content
    )
  )
}


# %% .anthropic_fallback_served() ----
#' Whether a Fallback Model Served an Anthropic Response
#'
#' @param res List: Parsed response body.
#'
#' @return Logical: TRUE when `usage.iterations` holds a `fallback_message` entry.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_fallback_served <- function(res) {
  iterations <- res[["usage"]][["iterations"]]
  if (!is.list(iterations)) {
    return(FALSE)
  }
  any(vapply(
    iterations,
    function(entry) identical(entry[["type"]], "fallback_message"),
    logical(1L)
  ))
}


# %% .anthropic_output_format_schema() ----
#' Clean a JSON Schema for `output_config.format`
#'
#' Structured outputs accept a subset of JSON Schema: every object carries
#' `additionalProperties: false`, and numeric and string-length constraints are removed.
#'
#' @param x List: JSON schema.
#'
#' @return List.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_output_format_schema <- function(x) {
  if (!is.list(x)) {
    abort("`output_schema` must be a JSON Schema list.")
  }
  x <- unclass(x)
  x[intersect(names(x), ANTHROPIC_OUTPUT_SCHEMA_DROP)] <- NULL
  if (identical(x[["type"]], "object")) {
    if (is.null(x[["properties"]]) || !is.list(x[["properties"]])) {
      abort(
        "`output_schema` object schemas must define `properties`.\n",
        "Use schema() and field() to create an output schema."
      )
    }
    x[["properties"]] <- lapply(
      x[["properties"]],
      .anthropic_output_format_schema
    )
    x[["required"]] <- I(as.character(x[["required"]]))
    x[["additionalProperties"]] <- FALSE
  } else if (is.list(x[["items"]])) {
    x[["items"]] <- .anthropic_output_format_schema(x[["items"]])
  }
  x
}


# %% .anthropic_model_capabilities() ----
#' Read a Model's Capabilities From the Models API
#'
#' Reads `GET /models/{id}`. When the endpoint answers with an error, the model list
#' (`anthropic_check_model()`) decides availability, as on Anthropic-compatible servers that serve
#' only `/models`.
#'
#' @inheritParams anthropic_check_model
#'
#' @return Optional list: The `capabilities` object, or NULL when the endpoint reports none.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_model_capabilities <- function(
  x,
  base_url = ANTHROPIC_URL_DEFAULT,
  api_key = NULL,
  api_key_env = ANTHROPIC_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  anthropic_version = ANTHROPIC_API_VERSION_DEFAULT
) {
  config <- AnthropicConfig(
    model_name = "models",
    base_url = base_url,
    api_key = api_key,
    api_key_env = api_key_env,
    keychain_service = keychain_service,
    anthropic_version = anthropic_version,
    validate_model = FALSE
  )
  req <- httr2::request(paste0(
    config@base_url,
    "/models/",
    utils::URLencode(x, reserved = TRUE)
  )) |>
    httr2::req_method("GET") |>
    httr2::req_user_agent("rtemis (www.rtemis.org)") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    .add_anthropic_headers(config)
  resp <- httr2::req_perform(req)
  res <- if (httr2::resp_is_error(resp)) {
    NULL
  } else {
    tryCatch(
      httr2::resp_body_json(resp, simplifyVector = FALSE),
      error = function(e) NULL
    )
  }
  if (!is.list(res) || !is.character(res[["id"]])) {
    anthropic_check_model(
      x = x,
      base_url = base_url,
      api_key = api_key,
      api_key_env = api_key_env,
      keychain_service = keychain_service,
      anthropic_version = anthropic_version
    )
    return(NULL)
  }
  capabilities <- res[["capabilities"]]
  if (is.list(capabilities)) capabilities else NULL
}
