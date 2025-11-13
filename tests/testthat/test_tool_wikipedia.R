# test_tools_wikipedia.R
# ::kaimana::
# 2025 EDG rtemis.org

test_that("query_wikipedia returns expected columns (light run)", {
  skip_on_cran()
  skip_if_offline()
  ws <- query_wikipedia(
    query = "MAPT",
    limit = 2L,
    language = "en",
    verbosity = 0L,
    output = "data.table"
  )
  expect_s3_class(ws, "data.frame")
  expect_true(all(
    c("page_id", "title", "content") %in% names(ws)
  ))
})
