# test-ollama.R
# ::rtemis.llm::
# 2025 EDG rtemis.org

# library(testthat)

model_name <- "qwen3.5:0.8b"

# %% ollama_list_models() ----
test_that("ollama_list_models works", {
  skip_if_ollama_unavailable()
  expect_type(ollama_list_models(), "character")
}) # /ollama_list_models


# %% ollama_get_model_info() ----
test_that("ollama_get_model_info works", {
  skip_if_ollama_unavailable()
  all_models <- ollama_get_model_info()
  expect_s3_class(all_models, "data.table")
}) # /ollama_get_model_info


# %% ollama_check_model() ----
test_that("ollama_check_model works", {
  skip_if_ollama_model_missing(model_name)
  expect_null(ollama_check_model(model_name))
  expect_error(ollama_check_model("non_existent_model_12345"))
}) # /ollama_check_model
