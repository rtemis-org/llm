# test-ollama.R
# ::kaimana::
# 2025 EDG rtemis.org

# library(testthat)

model_name <- "qwen3:8b"

# %% ollama_list_models() ----
test_that("ollama_list_models works", {
  expect_type(ollama_list_models(), "character")
}) # /ollama_list_models


# %% ollama_get_model_info() ----
test_that("ollama_get_model_info works", {
  all_models <- ollama_get_model_info()
  expect_s3_class(all_models, "data.table")
  model_info <- ollama_get_model_info(x = model_name)
  expect_s3_class(model_info, "list")
}) # /ollama_get_model_info


# %% ollama_check_model() ----
test_that("ollama_check_model works", {
  expect_null(ollama_check_model(model_name))
  expect_error(ollama_check_model("non_existent_model_12345"))
}) # /ollama_check_model
