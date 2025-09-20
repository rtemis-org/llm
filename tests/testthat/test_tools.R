# test-tools.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% wikipedia_search() ----
query <- "tau"
wiki_results <- wikipedia_search(query = query, limit = 5L, lang = "en")
test_that("wikipedia_search() returns expected results", {
  expect_s3_class(wiki_results, "data.frame")
  expect_equal(nrow(wiki_results), 5L)
  expect_true(all(
    c("title", "snippet", "pageid") %in% colnames(wiki_results)
  ))
})

# %% wikipedia_get() ----
article <- wikipedia_get(title = wiki_results[1, "title"])
test_that("wikipedia_get() returns expected results", {
  expect_type(article, "list")
  expect_true(all(c("title", "text") %in% names(article)))
  expect_equal(article[["title"]], wiki_results[1, "title"])
  expect_true(nchar(article[["text"]]) > 0)
})
