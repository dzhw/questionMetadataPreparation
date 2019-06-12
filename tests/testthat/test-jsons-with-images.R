library(questionMetadataPreperation)
library(testthat)

testthat::context("test json generation with images")

testthat::setup({
  base::file.copy(from = system.file("extdata", "ssy20img",
    package = "questionMetadataPreperation"), to = base::tempdir(),
    recursive = TRUE)
})


test_that("json filenames and content are identical", {

  questionMetadataPreperation::generate_question_jsons(base::tempdir(),
    "ssy20img", has_images = TRUE)
  questionMetadataPreperation::sort_images(
    paste0(base::tempdir(), "/ssy20img/questions/images_unsorted"),
    paste0(base::tempdir(), "/ssy20img/questions/out"),
    paste0(base::tempdir(), "/ssy20img/questions/ssy20img.xlsx"))
  file_list_generated <- list.files(paste0(base::tempdir(),
    "/ssy20img/questions/out"), recursive = TRUE)
  file_list_expected <- list.files(paste0(base::tempdir(),
    "/ssy20img/questions/out_exp"), recursive = TRUE)
  json_file_list_expected <- file_list_expected[stringr::str_ends(
    file_list_expected, ".json")]
  json_file_list_generated <- file_list_expected[stringr::str_ends(
    file_list_generated, ".json")]

  generated_json_list <- vector("list", length(json_file_list_generated))
  expected_json_list <- vector("list", length(json_file_list_expected))
  for (i in seq_along(json_file_list_expected)) {
    expected_json_list[[i]] <-  jsonlite::read_json(paste0(base::tempdir(),
      "/ssy20img/questions/out/", json_file_list_expected[i]))
  }
  i <- NULL
  for (i in seq_along(json_file_list_generated)) {
    generated_json_list[[i]] <-  jsonlite::read_json(paste0(base::tempdir(),
      "/ssy20img/questions/out/", json_file_list_generated[i]))
  }
  i <- NULL
  testthat::expect_identical(json_file_list_generated, json_file_list_expected)
  testthat::expect_identical(generated_json_list, expected_json_list)
})

testthat::teardown(
  unlink(paste0(base::tempdir(),"/ssy20img"), recursive = TRUE)
)
