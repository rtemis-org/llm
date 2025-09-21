# tool_wikipedia.R
# ::kaimana::
# 2025 EDG rtemis.org

# Depends on: httr2, data.table, rvest
# (Use fully qualified calls to avoid requiring library(...) in dev.)

#' Search Wikipedia
#'
#' @param query Character: Search query.
#' @param limit Integer: Number of results to return.
#' @param language Character: Language edition of Wikipedia, e.g. "en", "de".
#' @param clean_refs Logical: If TRUE, remove numeric bracketed references like `[1]`, `[2]` from
#' text.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Data frame: Search results with title, snippet, and pageid.
wiki_search <- function(query, limit = 3, language = "en", verbosity = 1L) {
  .sanitize_language <- function(lang) {
    lang <- tolower(as.character(lang %||% "en"))
    if (!isTRUE(grepl("^[a-z-]+$", lang))) "en" else lang
  }
  .sanitize_limit <- function(x) {
    x <- as.integer(x %||% 3L)
    if (is.na(x) || x < 1L) 1L else min(x, 50L)
  }
  `%||%` <- function(a, b) if (is.null(a)) b else a

  language <- .sanitize_language(language)
  limit <- .sanitize_limit(limit)
  if (verbosity > 0L) {
    msg(
      "Searching Wikipedia for '",
      query,
      "' (lang=",
      language,
      ", n=",
      limit,
      ")..."
    )
  }

  req <- .wiki_req(language) |>
    httr2::req_url_query(
      action = "query",
      list = "search",
      srsearch = query,
      format = "json",
      srlimit = limit
    )

  resp <- httr2::req_perform(req)
  .ensure_success(resp)

  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  results <- body$query$search
  if (is.null(results) || length(results) == 0L) {
    return(data.table::data.table())
  }
  data.table::as.data.table(results)
}

#' Get sections of a Wikipedia page by pageid
#'
#' @param pageid Integer: Wikipedia page ID.
#' @param language Character: Language edition of Wikipedia, e.g. "en", "de".
#' @param verbosity Integer: Verbosity level.
#'
#' @return Data frame: Sections of the Wikipedia page.
wiki_sections <- function(pageid, language = "en", verbosity = 1L) {
  language <- tolower(language)
  if (verbosity > 0L) {
    msg("Retrieving sections for page ID ", pageid, "...")
  }

  req <- .wiki_req(language) |>
    httr2::req_url_query(
      action = "parse",
      pageid = pageid,
      prop = "sections",
      redirects = "true",
      format = "json"
    )
  resp <- httr2::req_perform(req)
  .ensure_success(resp)

  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  secs <- body$parse$sections
  if (is.null(secs) || length(secs) == 0L) {
    return(data.table::data.table())
  }
  data.table::as.data.table(secs)
}

#' Get text of a specific section of a Wikipedia page
#'
#' @param pageid Integer: Wikipedia page ID.
#' @param section_id Integer: Section index.
#' @param language Character: Language edition of Wikipedia, e.g. "en", "de".
#'
#' @return Character: Text of the specified section.
wiki_section_text <- function(
  pageid,
  section_id,
  language = "en",
  clean_refs = TRUE,
  verbosity = 1L
) {
  language <- tolower(language)
  if (verbosity > 0L) {
    msg(
      "Retrieving text for section ",
      section_id,
      " of page ID ",
      pageid,
      "..."
    )
  }
  req <- .wiki_req(language) |>
    httr2::req_url_query(
      action = "parse",
      pageid = pageid,
      prop = "text",
      section = section_id,
      redirects = "true",
      format = "json"
    )
  resp <- httr2::req_perform(req)
  .ensure_success(resp)

  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  html <- body$parse$text[["*"]]
  if (is.null(html) || !nzchar(html)) {
    return("")
  }
  doc <- rvest::read_html(html)
  # Prefer paragraphs and headings to reduce noise
  nodes <- rvest::html_elements(doc, "p, h2, h3")
  txt <- if (length(nodes)) {
    rvest::html_text2(nodes)
  } else {
    rvest::html_text(doc, trim = TRUE)
  }
  out <- paste(txt, collapse = "\n\n")
  if (isTRUE(clean_refs)) {
    # Remove numeric bracketed references like [1], [12], [1, 2], [3–5]
    # Handles simple numeric forms and comma/space separated lists
    out <- gsub("\\[(?:\\d+)(?:\\s*,\\s*\\d+)*\\]", "", out, perl = TRUE)
    # Also remove residual multiple spaces created by removals
    out <- gsub("[ ]{2,}", " ", out)
  }
  out
}

#' Query Wikipedia
#'
#' @param query Character: Search query.
#' @param limit Integer: Number of search results to return.
#' @param language Character: Language edition of Wikipedia, e.g. "en", "de".
#' @param max_sections_per_page Integer: Maximum number of sections to retrieve per page. If NULL
#' (default), retrieves all sections.
#' @param clean_refs Logical: If TRUE, remove numeric bracketed references like `[1]`, `[2]` from
#' text.
#' @param output Character: One of "data.table" or "JSON" (default "data.table"). When "JSON",
#'   returns a JSON string with one object per row.
#' @param json_pretty Logical: If TRUE and output == "JSON", pretty-print the JSON (default FALSE).
#' @param verbosity Integer: Verbosity level.
#'
#' @return data.table or JSON string: Combined data of search results, sections, text, and page URL.
query_wikipedia <- function(
  query,
  limit = 3L,
  language = "en",
  max_sections_per_page = NULL,
  clean_refs = TRUE,
  output = c("JSON", "data.table"),
  json_pretty = FALSE,
  verbosity = 1L
) {
  output <- match.arg(output)
  if (verbosity > 0L) {
    msg0("Querying Wikipedia for '", query, "' with limit ", limit, "...")
  }
  results <- wiki_search(
    query,
    limit = limit,
    language = language,
    verbosity = verbosity
  )
  if (is.null(results) || NROW(results) == 0L) {
    dt <- data.table::data.table(
      page_id = integer(),
      title = character(),
      section = character(),
      section_id = integer(),
      text = character(),
      url = character()
    )
    if (output == "JSON") {
      return(jsonlite::toJSON(
        dt,
        dataframe = "rows",
        auto_unbox = TRUE,
        na = "null",
        POSIXt = "ISO8601",
        Date = "ISO8601",
        digits = NA,
        pretty = json_pretty
      ))
    }
    return(dt)
  }

  rows <- vector("list", 0L)
  for (i in seq_len(NROW(results))) {
    pageid <- results$pageid[i]
    title <- results$title[i]

    secs <- wiki_sections(
      pageid,
      language = language,
      verbosity = verbosity - 1L
    )
    if (is.null(secs) || NROW(secs) == 0L) {
      next
    }

    if (!is.null(max_sections_per_page)) {
      keep <- seq_len(min(NROW(secs), as.integer(max_sections_per_page)))
      secs <- secs[keep, ]
    }

    # Precompute page URL once per page
    page_url <- sprintf(
      "https://%s.wikipedia.org/?curid=%s",
      tolower(language),
      pageid
    )

    for (j in seq_len(NROW(secs))) {
      sec_index <- secs$index[j]
      txt <- wiki_section_text(
        pageid,
        sec_index,
        language = language,
        clean_refs = clean_refs,
        verbosity = verbosity - 1L
      )
      rows[[length(rows) + 1L]] <- data.table::data.table(
        page_id = pageid,
        title = title,
        section = secs$line[j],
        section_id = sec_index,
        text = txt,
        url = page_url
      )
    }
  }
  if (length(rows) == 0L) {
    dt <- data.table::data.table(
      page_id = integer(),
      title = character(),
      section = character(),
      section_id = integer(),
      text = character(),
      url = character()
    )
  } else {
    dt <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  }
  if (output == "JSON") {
    return(
      jsonlite::toJSON(
        dt,
        dataframe = "rows",
        auto_unbox = TRUE,
        na = "null",
        POSIXt = "ISO8601",
        Date = "ISO8601",
        digits = NA,
        pretty = json_pretty
      )
    )
  }
  dt
} # /kaimana::query_wikipedia

# --- Internal helpers -----------------------------------------------------------------------------

# Build a base httr2 request for Wikipedia API with UA and polite retry
.wiki_req <- function(language = "en") {
  lang <- tolower(language)
  base <- sprintf("https://%s.wikipedia.org/w/api.php", lang)
  httr2::request(base) |>
    httr2::req_user_agent(
      "kaimana-r (https://github.com/rtemis-org/kaimana-r; contact: stathis@rtemis.org)"
    ) |>
    httr2::req_retry(
      max_tries = 4,
      backoff = function(attempt) min(0.75 * attempt, 5),
      is_transient = function(resp) {
        status <- httr2::resp_status(resp)
        status %in% c(408L, 429L, 500L, 502L, 503L, 504L)
      }
    )
}

# Ensure HTTP 2xx; otherwise throw a clear error
.ensure_success <- function(resp) {
  # Prefer httr2's built-in checker; provide clearer message on error
  tryCatch(
    {
      httr2::resp_check_status(resp)
      invisible(resp)
    },
    error = function(e) {
      status <- httr2::resp_status(resp)
      url <- httr2::req_url(httr2::resp_request(resp))
      stop(
        sprintf(
          "Wikipedia API request failed [%s]: %s\n%s",
          status,
          url,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}
