# tool_wikipedia.R
# ::kaimana::
# 2025 EDG rtemis.org

# References:
# https://www.mediawiki.org/wiki/API:Main_page
# https://api.wikimedia.org/wiki/Core_REST_API/Reference/Search/Search_content

# Note on specific section retrieval:
# The Action API's prop=extracts is not designed to fetch a named section directly.
# To get a specific section, you would typically fetch the full page (section_mode = "all")
# and then use R's string manipulation functions (like str_split) to parse the text.

# %% query_wikipedia() ----
#' Search Wikipedia
#'
#' Searches wikipedia for a given query
#'
#' @param query Character: The search query.
#' @param limit Integer: Maximum number of pages to return.
#' @param section_mode Character: "intro", "all", or "raw_json". "intro" returns only the
#' introduction section, "all" returns the full page text, "raw_json" returns the raw JSON response.
#' @param base_url Character: The base URL for the Wikipedia API.
#' @param output_type Character: "json" or "data.table". This should be "json" when used as an agent
#' tool.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Character with JSON response, list, or data.table.
#'
#' @author EDG
#' @export
query_wikipedia <- function(
  query,
  limit = 2L,
  section_mode = c("intro", "all", "raw_json"),
  base_url = "https://en.wikipedia.org/w/api.php",
  output_type = c("json", "list", "data.table"),
  verbosity = 1L
) {
  # Input Validation
  section_mode <- match.arg(section_mode)
  output_type <- match.arg(output_type)
  limit <- clean_int(limit)

  # --- Get Page Titles ----------------------------------------------------------------------------

  # Create request to search for pages matching the query
  # This request finds the page titles (and page IDs) that match the query.
  search_req <- httr2::request(base_url) |>
    httr2::req_url_query(
      action = "query",
      format = "json",
      list = "search",
      srsearch = query,
      srlimit = as.integer(limit),
      utf8 = "true"
    )

  # Perform the request
  search_resp <- tryCatch(
    {
      resp <- httr2::req_perform(search_req)
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
      return(paste0("tool call error: ", conditionMessage(e)))
    }
  )

  # Check if we got an error string back
  if (is.character(search_resp)) {
    return(search_resp)
  }

  # Convert response to R list
  resp_list <- httr2::resp_body_json(search_resp, simplifyVector = TRUE)

  # Extract titles from the search results
  titles <- resp_list[["query"]][["search"]][["title"]]
  if (verbosity > 0L && length(titles) == 0) {
    msg("No Wikipedia pages found for query: '", query, "'.")
    return(data.table(title = character(), content = character()))
  }

  # Join titles with the pipe '|' for the next API call
  titles_string <- paste(titles, collapse = "|")

  # --- Get Page Content ---------------------------------------------------------------------------

  # Set content retrieval parameters based on section_mode
  exintro_val <- if (section_mode == "intro") TRUE else FALSE

  # Create request to get page content for the found titles
  content_req <- httr2::request(base_url) |>
    httr2::req_url_query(
      action = "query",
      format = "json",
      prop = "extracts",
      titles = titles_string,
      exlimit = as.integer(limit), # Match the limit from the search step
      explaintext = TRUE, # Return plain text instead of HTML
      exintro = exintro_val, # TRUE for intro, FALSE for all
      utf8 = TRUE
    )

  # Perform the request
  content_resp <- tryCatch(
    {
      resp <- httr2::req_perform(content_req)
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
  if (is.character(content_resp) && grepl("^tool call", content_resp)) {
    return(content_resp)
  }

  # Convert response to required output type ("json" or "data.table")
  out <- if (output_type == "json") {
    httr2::resp_body_string(content_resp)
  } else {
    out <- httr2::resp_body_json(content_resp, simplifyVector = TRUE)
    if (output_type == "data.table") {
      out <- data.table::rbindlist(lapply(
        out[["query"]][["pages"]],
        function(page) {
          list(
            pageid = page[["pageid"]],
            title = page[["title"]],
            content = page[["extract"]]
          )
        }
      ))
    }
  }
  out
} # /kaimana::query_wikipedia


# %% tool_wikipedia ----
#' Wikipedia Search Tool
#'
#' Tool definition for Wikipedia search
#'
#' @author EDG
#' @export
tool_wikipedia <- create_tool(
  name = "Wikipedia Search",
  function_name = "query_wikipedia",
  description = "Search Wikipedia for articles and return their introduction content",
  parameters = list(
    tool_param(
      name = "query",
      type = "string",
      description = "The search query.",
      required = TRUE
    ),
    tool_param(
      name = "limit",
      type = "integer",
      description = "Maximum number of pages to return.",
      required = FALSE
    )
  )
) # /kaimana::tool_wikipedia
