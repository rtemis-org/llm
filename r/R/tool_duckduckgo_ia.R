# %% Constants ----
DUCKDUCKGO_URL <- "https://api.duckduckgo.com/"

#' Query DuckDuckGo Instant Answer API
#'
#' @param query Character: Search query.
#' @param ia Character or NULL: Specific Instant Answer to request. If NULL, general search is performed.
#' @param return_all Logical: If TRUE, return full response as a list. If FALSE, return selected fields.
#' @param output_type Character: "json" or "data.table". This should be "json" when used as an agent
#' tool.
#'
#' @return JSON or data.table with response
#'
#' @author EDG
#'
#' @export
query_duckduckgo_ia <- function(
  query,
  ia = NULL,
  return_all = FALSE,
  output_type = c("json", "data.table")
) {
  output_type <- match.arg(output_type)
  # Validate query
  if (!is.character(query) || length(query) != 1L) {
    stop("query must be a single character string")
  }
  query <- trimws(query)
  if (nchar(query) == 0L) {
    stop("query must be a non-empty string")
  }

  # Query using httr2 from DuckDuckGo Instant Answer API:
  # https://api.duckduckgo.com/?q={query}&format=json
  req <- httr2::request(DUCKDUCKGO_URL)
  req <- if (is.null(ia)) {
    httr2::req_url_query(
      req,
      q = query,
      format = "json",
      no_redirect = 1,
      no_html = 1,
      skip_disambig = 1
    )
  } else {
    httr2::req_url_query(
      req,
      q = query,
      format = "json",
      ia = ia,
      no_redirect = 1,
      no_html = 1,
      skip_disambig = 1
    )
  }
  # Add user agent
  req <- httr2::req_user_agent(req, "rtemis.llm (rtemis.llm.rtemis.org)")
  # Perform request
  res <- tryCatch(
    {
      resp <- httr2::req_perform(req)
      # Check for HTTP errors
      httr2::resp_check_status(resp)
      resp
    },
    error = function(e) {
      # Extract HTTP status code if available
      if (inherits(e, "httr2_http_error")) {
        status_code <- e$status
        return(paste0("tool call returned HTTP error ", status_code))
      }
      # For other errors, return the error message
      paste0("tool call error: ", conditionMessage(e))
    }
  )

  # Check if we got an error string back
  if (is.character(res)) {
    return(res)
  }

  # Parse response (resp_body_json fails here)
  res_json_raw <- httr2::resp_body_string(res)
  # Convert to list
  res_list <- jsonlite::fromJSON(res_json_raw, simplifyVector = TRUE)
  # Build data.table
  if (return_all) {
    return(res_list)
  }
  dat <- data.table(
    Abstract_text = res_list[["AbstractText"]],
    Abstract_URL = res_list[["AbstractURL"]]
  )
  if (nchar(res_list[["Answer"]]) > 0L) {
    dat[, Answer := res_list[["Answer"]]]
    dat[, Answer_type := res_list[["AnswerType"]]]
  }
  # Convert to JSON
  if (output_type == "json") {
    return(jsonlite::toJSON(dat, pretty = FALSE, auto_unbox = TRUE))
  }
  dat
}


# %% tool_duckduckgo_ia ----
#' DuckDuckGo Instant Answer Search Tool
#'
#' Tool definition for DuckDuckGo Instant Answer search
#'
#' @author EDG
#' @export
tool_duckduckgo_ia <- create_tool(
  name = "DuckDuckGo Instant Answer Search",
  function_name = "query_duckduckgo_ia",
  description = paste(
    "Search DuckDuckGo Instant Answer API. ",
    "Use single-word queries for best results.",
    "Some queries may not return any results."
  ),
  parameters = list(
    query = tool_param(
      name = "query",
      type = "string",
      description = "Search query",
      required = TRUE
    )
  )
) # /tool_duckduckgo_ia
