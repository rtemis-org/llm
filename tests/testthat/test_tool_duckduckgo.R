# test_query_ddgs.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% query_duckduckgo_ia() ----
query <- "History of California"
query <- "Capital of California"
query <- "Capital of France"
query <- "Define P/F ratio"
query <- "Define P/F ratio in the context of Medicine"
ddgs <- query_duckduckgo_ia(query)
ddgs

# %% Must use single word queries for best results
query_duckduckgo_ia("Hypertension")
query_duckduckgo_ia("P/F ratio")
query_duckduckgo_ia("Creatinine")

# %%
query_duckduckgo_ia("san francisco", ia = "news", return_all = TRUE) |> View()


query_duckduckgo_ia(
  "all the leaves are brown",
  ia = "music",
  return_all = TRUE
) |>
  View()


query_duckduckgo_ia("athens", return_all = TRUE) |>
  View()

query_duckduckgo_ia("athens", ia = "news", return_all = TRUE) |>
  View()

# %% query_duckduckgo()
query <- "Define P/F ratio in the context of Medicine"
