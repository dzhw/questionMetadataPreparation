testthat::context("generate question jsons")
library(questionMetadataPreperation)

testthat::setup({
  base::file.copy(from = system.file("extdata", "ssy20",
    package = "questionMetadataPreperation"), to = base::tempdir(),
    recursive = TRUE)
})



test_that("correct file names",{
  questionMetadataPreperation::generate_question_jsons(base::tempdir(), "ssy20")
  expect_identical(list.files(paste0(base::tempdir(), "/ssy20/questions/out"), recursive = TRUE),
    list.files(paste0(base::tempdir(), "/ssy20/questions/out_expected"), recursive = TRUE))
})

testthat::teardown(
  unlink(paste0(base::tempdir(),"/ssy20"), recursive = TRUE)
)
