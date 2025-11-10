# tool_duckduckgo.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% Constants ----
DUCKDUCKGO_URL <- "https://api.duckduckgo.com/"

#' Query DuckDuckGo Instant Answer API
#'
#' @param query Character: Search query.
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
  output = c("json", "data.table")
) {
  output <- match.arg(output)
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
  req <- httr2::req_user_agent(req, "Kaimana (kaimana.rtemis.org)")
  # Perform request
  res <- httr2::req_perform(req)
  # Check for HTTP errors
  httr2::resp_check_status(res)
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
  if (output == "json") {
    return(jsonlite::toJSON(dat, pretty = FALSE, auto_unbox = TRUE))
  }
  dat
} # /query_duckduckgo_ia


#' Scrape DuckDuckGo Search Results
#'
#' @param query Character: Search query.
#'
#' @return JSON or data.table with response
#'
#' @author EDG
#'
#' @export
query_duckduckgo <- function(
  query,
  ia = NULL,
  return_all = FALSE,
  output = c("json", "data.table")
) {
  output <- match.arg(output)
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
  req <- httr2::request("https://duckduckgo.com/") |>
    httr2::req_url_query(
      q = query,
      ia = "web"
    ) |>
    # Add user agent
    httr2::req_user_agent("Kaimana (kaimana.rtemis.org)")
  # Perform request
  res <- httr2::req_perform(req)
  # Check for HTTP errors
  httr2::resp_check_status(res)
  # Parse response (resp_body_json fails here)
  res_raw <- httr2::resp_body_string(res)
  # Scrape the results from the HTML using rvest
  scraped <- rvest::read_html(res_raw)
  rvest::html_text2(res_raw)
  # Convert to list
  res_list <- jsonlite::fromJSON(res_raw, simplifyVector = TRUE)
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
  if (output == "json") {
    return(jsonlite::toJSON(dat, pretty = FALSE, auto_unbox = TRUE))
  }
  dat
} # /query_duckduckgo_ia


# %% tool_duckduckgo_ia ----
tool_duckduckgo_ia <- create_tool(
  name = "query_duckduckgo_ia",
  description = paste(
    "Search DuckDuckGo Instant Answer API. ",
    "Use single-word queries for best results.",
    "Some queries may not return any results."
  ),
  parameters = list(
    tool_param(
      name = "query",
      type = "string",
      description = "Search query",
      required = TRUE
    )
  )
) # /tool_duckduckgo_ia
