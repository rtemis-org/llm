# tool_drugbank.R
# ::kaimana::
# 2025 EDG rtemis.org

# => Requires API key

# References:
# https://docs.drugbank.com/v1/#introduction
# https://docs.drugbank.com/v1/#token-authentication

#' Query DrugBank API
#'
#' @param query Character: Search query.
#'
#' @return JSON or data.table with response
#'
#' @author EDG
#' @export
query_drugbank <- function(
  query,
  region = "us",
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
  # Query using httr2 from DrugBank API:
  # https://api.drugbank.com/v1/drug_names?q=Tylenol
  # url <- paste0("https://api.drugbank.com/v1/drug_names?q=", URLencode(query))
  # response <- httr2::request(url) %>%
  #   httr2::req_headers(Authorization = paste("Bearer", Sys.getenv("DRUGBANK_API_KEY"))) %>%
  #   httr2::req_perform()
  # if (httr2::resp_status(response) != 200L) {
  #   stop("Failed to query DrugBank API")
  # }
  # if (output == "json") {
  #   return(httr2::resp_body_json(response))
  # }
  # return(data.table::as.data.table(httr2::resp_body_json(response)))
  req <- httr2::request("https://api.drugbank.com/v1/drug_names") |>
    httr2::req_url_query(
      region = region,
      q = query
    )
}
