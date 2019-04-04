library(questionMetadataPreperation)
library(testthat)

testthat::context("test json generation with images")

testthat::setup({
  base::file.copy(from = system.file("extdata", "ssy20",
    package = "questionMetadataPreperation"), to = base::tempdir(),
    recursive = TRUE)
  questionMetadataPreperation::generate_question_jsons(base::tempdir(), "ssy20")
  file_list_generated <<- list.files(paste0(base::tempdir(),
    "/ssy20/questions/out"), recursive = TRUE)
  file_list_expected <<- list.files(paste0(base::tempdir(),
    "/ssy20/questions/out"), recursive = TRUE)
  generated_json_list <<- vector("list", length(file_list_generated))
  expected_json_list <<- vector("list", length(file_list_expected))
  for (i in seq_along(file_list_expected)) {
    expected_json_list[[i]] <<-  jsonlite::read_json(paste0(base::tempdir(),
      "/ssy20/questions/out/", file_list_expected[i]))
  }
  for (i in seq_along(file_list_generated)) {
    generated_json_list[[i]] <<-  jsonlite::read_json(paste0(base::tempdir(),
      "/ssy20/questions/out/", file_list_generated[i]))
  }
})


test_that("expect json filenames and content identical",{
  expect_identical(file_list_generated, file_list_expected)
  expect_identical(generated_json_list, expected_json_list)
})

testthat::teardown(
  unlink(paste0(base::tempdir(),"/ssy20"), recursive = TRUE)
)

testthat::teardown(
  expected_json_list <- NULL
)

testthat::teardown(
  generated_json_list <- NULL
)

testthat::teardown(
  file_list_expected <- NULL
)

testthat::teardown(
  file_list_generated <- NULL
)
