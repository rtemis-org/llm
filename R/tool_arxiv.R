# tool_arxiv.R
# ::kaimana::
# 2025 EDG rtemis.org

# References:
# https://info.arxiv.org/help/api/index.html
# https://info.arxiv.org/help/api/user-manual.html

# %% query_arxiv() ----
#' Search arXiv for Academic Papers
#'
#' Searches arXiv.org for academic papers based on a query using the arXiv API.
#'
#' @param query Character: The search query.
#' @param max_results Integer: Maximum number of results to return (default: 5).
#' @param sort_by Character: Sort results by "relevance", "lastUpdatedDate", or "submittedDate".
#' @param sort_order Character: Sort order "descending" or "ascending".
#' @param base_url Character: The base URL for the arXiv API.
#' @param output_type Character: "json" or "data.table". This should be "json" when used as an agent
#' tool.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Character with JSON response or data.table.
#'
#' @details
#' The arXiv API returns results in Atom XML format, which is parsed and converted to JSON or data.table.
#' Each result includes:
#' \itemize{
#'   \item{id: arXiv ID}
#'   \item{title: Paper title}
#'   \item{summary: Abstract}
#'   \item{authors: List of authors}
#'   \item{published: Publication date}
#'   \item{updated: Last update date}
#'   \item{link: URL to paper}
#'   \item{pdf_link: URL to PDF}
#'   \item{primary_category: Primary subject category}
#' }
#'
#' @author EDG
#' @export
query_arxiv <- function(
  query,
  max_results = 5L,
  sort_by = c("relevance", "lastUpdatedDate", "submittedDate"),
  sort_order = c("descending", "ascending"),
  base_url = "http://export.arxiv.org/api/query",
  output_type = c("json", "data.table"),
  verbosity = 1L
) {
  # Input Validation
  sort_by <- match.arg(sort_by)
  sort_order <- match.arg(sort_order)
  output_type <- match.arg(output_type)
  max_results <- clean_int(max_results)

  if (!is.character(query) || length(query) != 1L) {
    stop("Query must be a single character string.")
  }
  query <- trimws(query)
  if (nchar(query) == 0L) {
    stop("Query must be a non-empty string.")
  }

  # --- Prepare request using httr2 ---
  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      search_query = paste0("all:", query),
      start = 0,
      max_results = as.integer(max_results),
      sortBy = sort_by,
      sortOrder = sort_order
    ) |>
    httr2::req_user_agent("kaimana-r (kaimana.rtemis.org)")

  # --- Perform request ---
  result <- tryCatch(
    {
      resp <- httr2::req_perform(req)
      # Check for HTTP errors
      httr2::resp_check_status(resp)

      # Parse XML response
      xml_content <- httr2::resp_body_string(resp)
      xml_doc <- xml2::read_xml(xml_content)

      # Extract entries
      ns <- xml2::xml_ns(xml_doc)
      entries <- xml2::xml_find_all(xml_doc, ".//d1:entry", ns)

      if (length(entries) == 0L) {
        if (verbosity > 0L) {
          msg("No arXiv papers found for query: '", query, "'.")
        }
        if (output_type == "json") {
          return(jsonlite::toJSON(list(papers = list()), auto_unbox = TRUE))
        } else {
          return(data.table::data.table(
            id = character(),
            title = character(),
            summary = character(),
            authors = character(),
            published = character(),
            updated = character(),
            link = character(),
            pdf_link = character(),
            primary_category = character()
          ))
        }
      }

      # Parse each entry
      papers <- lapply(entries, function(entry) {
        # Extract ID
        id_full <- xml2::xml_text(xml2::xml_find_first(entry, ".//d1:id", ns))
        id <- sub("http://arxiv.org/abs/", "", id_full)

        # Extract title
        title <- xml2::xml_text(xml2::xml_find_first(entry, ".//d1:title", ns))
        title <- gsub("\\s+", " ", trimws(title)) # Clean up whitespace

        # Extract summary
        summary <- xml2::xml_text(xml2::xml_find_first(
          entry,
          ".//d1:summary",
          ns
        ))
        summary <- gsub("\\s+", " ", trimws(summary)) # Clean up whitespace

        # Extract authors
        author_nodes <- xml2::xml_find_all(entry, ".//d1:author/d1:name", ns)
        authors <- paste(xml2::xml_text(author_nodes), collapse = ", ")

        # Extract dates
        published <- xml2::xml_text(xml2::xml_find_first(
          entry,
          ".//d1:published",
          ns
        ))
        updated <- xml2::xml_text(xml2::xml_find_first(
          entry,
          ".//d1:updated",
          ns
        ))

        # Extract links
        link_nodes <- xml2::xml_find_all(entry, ".//d1:link", ns)
        link <- ""
        pdf_link <- ""
        for (link_node in link_nodes) {
          type <- xml2::xml_attr(link_node, "type")
          href <- xml2::xml_attr(link_node, "href")
          if (!is.na(type) && type == "text/html") {
            link <- href
          } else if (!is.na(type) && type == "application/pdf") {
            pdf_link <- href
          }
        }

        # Extract primary category
        primary_cat_node <- xml2::xml_find_first(
          entry,
          ".//arxiv:primary_category",
          ns
        )
        primary_category <- xml2::xml_attr(primary_cat_node, "term")
        if (is.na(primary_category)) {
          primary_category <- ""
        }

        list(
          id = id,
          title = title,
          summary = summary,
          authors = authors,
          published = published,
          updated = updated,
          link = link,
          pdf_link = pdf_link,
          primary_category = primary_category
        )
      })

      # Convert to appropriate format
      if (output_type == "json") {
        jsonlite::toJSON(list(papers = papers), auto_unbox = TRUE)
      } else {
        data.table::rbindlist(papers)
      }
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
} # /query_arxiv


# %% tool_arxiv ----
#' arXiv Search Tool
#'
#' Tool definition for arXiv search
#'
#' @author EDG
#' @export
tool_arxiv <- create_tool(
  name = "arXiv Search",
  function_name = "query_arxiv",
  description = paste(
    "Search arXiv.org for academic papers in physics, mathematics, computer science,",
    "quantitative biology, quantitative finance, statistics, electrical engineering,",
    "systems science, and economics."
  ),
  parameters = list(
    query = tool_param(
      name = "query",
      type = "string",
      description = paste(
        "The search query. Use clear, relevant keywords or phrases to search across",
        "title, abstract, authors, and comments."
      ),
      required = TRUE
    ),
    max_results = tool_param(
      name = "max_results",
      type = "integer",
      description = "Maximum number of results to return (default: 5).",
      required = FALSE
    ),
    sort_by = tool_param(
      name = "sort_by",
      type = "string",
      description = "Sort results by 'relevance', 'lastUpdatedDate', or 'submittedDate'.",
      required = FALSE
    )
  )
) # /kaimana::tool_arxiv
