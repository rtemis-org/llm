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
    c("page_id", "title", "section", "section_id", "text", "url") %in% names(ws)
  ))
})

test_that("query_wikipedia_relevant sections mode works (light run)", {
  skip_on_cran()
  skip_if_offline()
  wr <- query_wikipedia_relevant(
    query = "MAPT",
    titles_limit = 2L,
    language = "en",
    verbosity = 0L,
    section_filter = c("function", "structure"),
    section_filter_mode = "any",
    mode = "sections",
    max_sections_per_page = 2L,
    output = "data.table"
  )
  expect_s3_class(wr, "data.frame")
  if (NROW(wr) > 0L) {
    expect_true(all(
      c("page_id", "title", "section", "section_id", "text", "url") %in%
        names(wr)
    ))
  }
})

test_that("query_wikipedia_relevant full_page mode works (light run)", {
  skip_on_cran()
  skip_if_offline()
  wr <- query_wikipedia_relevant(
    query = "MAPT",
    titles_limit = 1L,
    language = "en",
    verbosity = 0L,
    mode = "full_page",
    output = "data.table"
  )
  expect_s3_class(wr, "data.frame")
  if (NROW(wr) > 0L) {
    expect_true(all(
      c("page_id", "title", "section", "section_id", "text", "url") %in%
        names(wr)
    ))
    expect_true(all(wr$section == "FULL_PAGE"))
  }
})
