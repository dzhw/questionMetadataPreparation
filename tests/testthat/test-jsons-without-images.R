library(questionMetadataPreperation)
library(testthat)

testthat::context("test json generation without images")

testthat::setup({
  base::file.copy(from = system.file("extdata", "ssy20-without-images",
    package = "questionMetadataPreperation"), to = base::tempdir(),
    recursive = TRUE)
})


test_that("json filenames and content are identical", {
  questionMetadataPreperation::generate_question_jsons(base::tempdir(),
    "ssy20-without-images", has_images = FALSE)
  file_list_generated <- list.files(paste0(base::tempdir(),
    "/ssy20-without-images/questions/out"), recursive = TRUE)
  file_list_expected <- list.files(paste0(base::tempdir(),
    "/ssy20-without-images/questions/out"), recursive = TRUE)
  generated_json_list <- vector("list", length(file_list_generated))
  expected_json_list <- vector("list", length(file_list_expected))
  for (i in seq_along(file_list_expected)) {
    expected_json_list[[i]] <-  jsonlite::read_json(paste0(base::tempdir(),
      "/ssy20-without-images/questions/out/", file_list_expected[i]))
  }
  i <- NULL
  for (i in seq_along(file_list_generated)) {
    generated_json_list[[i]] <-  jsonlite::read_json(paste0(base::tempdir(),
      "/ssy20-without-images/questions/out/", file_list_generated[i]))
  }
  expect_identical(file_list_generated, file_list_expected)
  expect_identical(generated_json_list, expected_json_list)
})

testthat::teardown(
  unlink(paste0(base::tempdir(),"/ssy20-without-images"), recursive = TRUE)
)
