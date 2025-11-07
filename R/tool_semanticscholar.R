# tools_semanticscholar.R
# ::kaimana::
# 2025 EDG rtemis.org

# References:
# https://www.semanticscholar.org/product/api/tutorial

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

# Available fields, based on https://api.semanticscholar.org/api-docs/graph#tag/Paper-Data/operation/get_graph_paper_bulk_search
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
#' @param endpoint_url Character: The Semantic Scholar API endpoint URL.
#' @param output_format Character: "json" or "data.table". The output format.
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
  endpoint_url = "http://api.semanticscholar.org/graph/v1/paper/search/bulk",
  output_format = c("json", "data.table")
) {
  output_format <- match.arg(output_format)
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

  # --- Prepare request using httr2 ---
  req <- httr2::request(endpoint_url)
  req <- httr2::req_url_query(
    req,
    query = query,
    fields = paste(fields, collapse = ","),
    year = year
  )
  # Add user agent
  req <- httr2::req_user_agent(req, "Kaimana (kaimana.rtemis.org)")

  # --- Perform request ---
  res <- httr2::req_perform(req)
  # Check for HTTP errors
  httr2::resp_check_status(res)
  # Parse response
  out <- httr2::resp_body_string(res)

  if (output_format[1L] == "data.table") {
    # Convert to list
    out <- jsonlite::fromJSON(out, simplifyVector = TRUE)
    # Convert to data.table
    out <- data.table::as.data.table(out$data)
  }

  out
} # /query_semanticscholar
