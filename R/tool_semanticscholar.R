# References:
# https://www.semanticscholar.org/product/api/tutorial
# https://api.semanticscholar.org/api-docs/#tag/Paper-Data/operation/get_graph_paper_relevance_search

# Docs:
# Set the Query Parameters

# The paper bulk search API documentation lists the following query parameters:

#     query sets the search term
#     token automatically handles pagination
#     fields determines what data the API endpoint will return to you
#     sort allows users to sort the results by the paperId, publicationDate, or citationCount fields
#     publicationTypes filters results by paper publication type (e.g. journal articles)
#     openAccessPdf filters results by whether they contain public PDFs of papers
#     minCitationCount filters results by whether they have at least a given number of citations
#     publicationDateOrYear filters results by a date range
#     year filters results by a year range
#     venue filters results by publication venue
#     fieldsOfStudy filters results by the paper’s field of study

# Only the first query parameter, query, is required in every request. The token query parameter isn’t included in the original request. Instead, it is returned in the response to the original request, then included in subsequent requests to automatically handle pagination.

# In our request, we will include 3 query parameters: query, fields, and year:

#     Use quotation marks in the query to search for the phrase “generative AI”. See the Additional Help section for more examples of using search query syntax.
#     In fields, include the title, url, type of publication, date of publication, and link to the pdf of the paper. Separate field names with commas, without spaces. See the API documentation for all available field names.
#     Filter for papers published during or after the year 2023 by using the “2023–” syntax.

# These query parameters are appended to the end of the URL, so the complete URL looks like this: http://api.semanticscholar.org/graph/v1/paper/search/bulk?query="generative ai"&fields=title,url,publicationTypes,publicationDate,openAccessPdf&year=2023-

# Available fields, based on https://api.semanticscholar.org/api-docs/#tag/Paper-Data/operation/get_graph_paper_relevance_search
# Response Schema, under "data" key
# available_fields <-
#   c(
#     "paperId",
#     "corpusId",
#     "externalIds",
#     "url",
#     "title",
#     "abstract",
#     "venue",
#     "publicationVenue",
#     "year",
#     "referenceCount",
#     "citationCount",
#     "influentialCitationCount",
#     "isOpenAccess",
#     "openAccessPdf",
#     "fieldsOfStudy",
#     "s2FieldsOfStudy",
#     "publicationTypes",
#     "publicationDate",
#     "journal",
#     "citationStyles",
#     "authors"
#   )

# %% query_semanticscholar() ----
#' Query Semantic Scholar API
#'
#' @param query Character: The search query.
#' @param fields Character Vector: The fields to return. See References for available fields.
#' @param year Character: Year filter in the format "YYYY-" or "YYYY-YYYY".
#' @param limit Integer: Maximum number of results to return.
#' @param endpoint_url Character: The Semantic Scholar API endpoint URL.
#' @param output_type Character: "json" or "data.table". This should be "json" when used as an agent
#' tool.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Character with JSON response or data.table
#' @author EDG
#'
#' @details
#'
#' Available fields, based on https://api.semanticscholar.org/api-docs/graph#tag/Paper-Data/operation/get_graph_paper_bulk_search
#' Response Schema, under "data" key
#' \itemize{
#'   \item{paperId}
#'   \item{corpusId}
#'   \item{externalIds}
#'   \item{url}
#'   \item{title}
#'   \item{abstract}
#'   \item{venue}
#'   \item{publicationVenue}
#'   \item{year}
#'   \item{referenceCount}
#'   \item{citationCount}
#'   \item{influentialCitationCount}
#'   \item{isOpenAccess}
#'   \item{openAccessPdf}
#'   \item{fieldsOfStudy}
#'   \item{s2FieldsOfStudy}
#'   \item{publicationTypes}
#'   \item{publicationDate}
#'   \item{journal}
#'   \item{citationStyles}
#'   \item{authors}
#' }
#'
#' @export
query_semanticscholar <- function(
  query,
  fields = c(
    "title",
    "year",
    "abstract",
    "url",
    "publicationTypes",
    "publicationDate",
    "openAccessPdf"
  ),
  year = "2000-",
  limit = 5L,
  endpoint_url = "http://api.semanticscholar.org/graph/v1/paper/search",
  output_type = c("json", "data.table"),
  verbosity = 1L
) {
  output_type <- match.arg(output_type)
  # --- Validate query ---
  if (!is.character(query) || length(query) != 1L) {
    stop("Query must be a single character string.")
  }
  query <- trimws(query)
  if (nchar(query) == 0L) {
    stop("Query must be a non-empty string.")
  }
  # Validate fields
  if (!is.character(fields) || length(fields) < 1L) {
    stop("Fields must be a character vector of field names.")
  }
  # Validate year
  if (!is.character(year) || length(year) != 1L) {
    stop("Year must be a single character string.")
  }
  year <- trimws(year)
  if (nchar(year) == 0L) {
    stop("Year must be a non-empty string.")
  }

  # API key if available
  api_key <- keyring::key_get("semanticscholar_api_key")

  # --- Prepare request using httr2 ---
  req <- httr2::request(endpoint_url) |>
    httr2::req_url_query(
      query = query,
      fields = paste(fields, collapse = ","),
      year = year,
      limit = limit
    ) |>
    httr2::req_user_agent("rtemis.llm-r (rtemis.llm.rtemis.org)")

  if (!is.null(api_key) && nchar(api_key) > 0L) {
    msg("Using Semantic Scholar API key from keyring.", verbosity = verbosity)
    req <- req |>
      httr2::req_headers(
        "x-api-key" = api_key
      )
  }

  # --- Perform request ---
  result <- tryCatch(
    {
      res <- httr2::req_perform(req)
      # Check for HTTP errors
      httr2::resp_check_status(res)
      # Parse response
      out <- httr2::resp_body_string(res)

      if (output_type == "data.table") {
        # Convert to list
        out <- jsonlite::fromJSON(out, simplifyVector = TRUE)
        # Convert to data.table
        out <- data.table::as.data.table(out[["data"]])
      }

      out
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

  result
} # /query_semanticscholar


# %% tool_semanticscholar ----
#' Semantic Scholar Search Tool
#'
#' Tool definition for Semantic Scholar search
#'
#' @author EDG
#' @export
tool_semanticscholar <- create_tool(
  name = "Semantic Scholar Search",
  function_name = "query_semanticscholar",
  description = paste(
    "Search Semantic Scholar for academic papers across science, technology, medicine,",
    "social sciences, and humanities."
  ),
  parameters = list(
    query = tool_param(
      name = "query",
      type = "string",
      description = paste(
        "The search query. Use clear, relevant keywords or phrases.",
        "Use double quotes for exact phrase matching."
      ),
      required = TRUE
    ),
    year = tool_param(
      name = "year",
      type = "string",
      description = "Year filter in the format 'YYYY-' or 'YYYY-YYYY'.",
      required = FALSE
    )
  )
)
