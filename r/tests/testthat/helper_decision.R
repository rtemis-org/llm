# helper_decision.R
# ::rtemis.llm::
# 2026- EDG rtemis.org

# A `/v1/systemone` server with scripted answers, keyed by question. A choice
# picks `script[[key]]` (a label) or its first option; a noul answers
# `script[[key]]` (a number) or 0.9. Every request body is kept in `calls`.
mock_systemone <- function(script = list(), model = "clef-flash", env = NULL) {
  function(req) {
    body <- req[["body"]][["data"]]
    if (!is.null(env)) {
      env[["calls"]] <- c(env[["calls"]], list(list(req = req, body = body)))
    }
    answers <- lapply(names(body[["questions"]]), function(key) {
      q <- body[["questions"]][[key]]
      if (q[["type"]] == "noul") {
        return(list(type = "noul", noul = script[[key]] %||% 0.9))
      }
      options <- names(q[["criteria"]])
      pick <- script[[key]] %||% options[[1L]]
      rest <- setdiff(options, pick)
      probs <- c(0.8, rep(0.2 / max(length(rest), 1L), length(rest)))
      list(
        type = "choice",
        choice = pick,
        probabilities = as.list(stats::setNames(probs, c(pick, rest))),
        confidence = 0.7
      )
    })
    names(answers) <- names(body[["questions"]])
    out <- list(
      answers = answers,
      usage = list(input_tokens = 100L, output_tokens = 0L)
    )
    if (!is.null(model)) {
      out[["model"]] <- model
    }
    httr2::response_json(status_code = 200L, body = out)
  }
}

mock_decision_model <- function(output_schema = NULL, context = NULL) {
  create_DecisionModel(
    config_OllamaDecision("clef-flash", validate_model = FALSE),
    context = context,
    output_schema = output_schema
  )
}

skip_if_no_decision_model <- function(
  model_name = "clef-flash",
  base_url = ollama_test_url
) {
  skip_if_ollama_unavailable(base_url = base_url)
  if (
    !model_name %in% sub(":latest$", "", ollama_list_decision_models(base_url))
  ) {
    testthat::skip(paste0(
      "Ollama decision model '",
      model_name,
      "' not available"
    ))
  }
}
