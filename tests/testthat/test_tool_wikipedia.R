# test_tools_wikipedia.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% query_wikipedia() ----
ws <- query_wikipedia(
  query = "MAPT",
  limit = 3L,
  language = "en",
  verbosity = 1L
)
# print(head(ws[, .(title, section, text)]))
