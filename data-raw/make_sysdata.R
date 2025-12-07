# make_sysdata.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% Constants ----
AVAILABLE_TOOLS <- list(
  datetime = "get_current_datetime",
  wikipedia = "query_wikipedia",
  semanticscholar = "query_semanticscholar",
  arxiv = "query_arxiv",
  duckduckgoia = "query_duckduckgo_ia"
)
HASH_ALGO <- "sha256"

# Import your package namespace manually to access the functions
pkg_env <- asNamespace("kaimana")

.hash_function <- getFromNamespace(".hash_function", "kaimana")

# %% Build the tool_DB
tool_DB <- data.frame(
  name = names(AVAILABLE_TOOLS),
  function_name = unlist(AVAILABLE_TOOLS, use.names = FALSE),
  hash = vapply(
    unlist(AVAILABLE_TOOLS),
    function(nm) .hash_function(get(nm, envir = pkg_env)),
    FUN.VALUE = character(1)
  ),
  stringsAsFactors = FALSE
)


# %% Save to sysdata.rda in your R/ directory
usethis::use_data(tool_DB, internal = TRUE, overwrite = TRUE)
