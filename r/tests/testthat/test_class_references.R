# test_class_references.R

test_that("every class reference of the package's classes resolves", {
  expect_true(rtemis.core::check_class_references(asNamespace("rtemis.llm")))
})
