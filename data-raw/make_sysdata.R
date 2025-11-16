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

# # Define hash function (e.g. using serialize for integrity)
# hash_function <- function(f) {
#   f_env_stripped <- f
#   environment(f_env_stripped) <- baseenv()
#   digest(serialize(f_env_stripped, NULL), algo = "sha256")
# }

# # List of allowed tools (can auto-detect from a tagged object or manual list)
# allowed_functions <- c("safe_sum", "extract_pdf_text")

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
