apple_test_url <- "http://127.0.0.1:1977/v1"

# Skip unless a rtemis-afm bridge is answering and reports the model
# available. A CRAN check should not open the connection at all, so the skip
# is declared up front rather than inferred from a refused loopback probe.
skip_if_apple_unavailable <- function(base_url = apple_test_url) {
  testthat::skip_on_cran()
  tryCatch(
    apple_check_available(base_url = base_url),
    error = function(e) {
      testthat::skip(paste0("rtemis-afm bridge not available at ", base_url))
    }
  )
}

# A /health body as rtemis-afm reports it (spec: rtemis-afm/wire#get-health).
apple_health_body <- function(
  availability = "available",
  reason = NULL,
  context_window = 8192L
) {
  model <- list(
    id = "afm",
    name = "AFM 3 Core Advanced",
    availability = availability,
    context_window = context_window
  )
  if (!is.null(reason)) {
    model[["reason"]] <- reason
  }
  list(status = "ok", version = "0.1.1", model = model)
}
