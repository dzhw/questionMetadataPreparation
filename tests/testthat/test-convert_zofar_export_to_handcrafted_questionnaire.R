library(questionMetadataPreparation)
library(testthat)

testthat::context("from Zofar to handcrafted")


test_that("conversion from Zofar to handcrafted works", {
  questionMetadataPreparation::convert_zofar_export_to_handcrafted_questionnaire(#nolint
    input_directory = testthat::test_path("from_zofar/questions/ins3"),
    output_directory = paste0(base::tempdir(), "/handcrafted"))
  file_list_generated <- list.files(paste0(base::tempdir(),
    "/handcrafted/"), recursive = TRUE)
  file_list_expected <- c("Bilder/png/ins3/1.1_0.png",
    "Bilder/png/ins3/2.1_0.png", "Bilder/png/ins3/3.1_0.png",
    "Bilder/png/ins3/3.2_0.png", "Bilder/png/ins3/4.1_0.png",
    "Bilder/png/ins3/4.2_0.png", "Bilder/png/ins3/5.1_0.png",
    "questions-ins3.xlsx")

  generated_excel <- readxl::read_xlsx(paste0(base::tempdir(),
    "/handcrafted/questions-ins3.xlsx"))
  expected_excel <- readxl::read_xlsx(
    testthat::test_path("expected_handcrafted/questions-ins3.xlsx"))

  expect_identical(file_list_generated, file_list_expected)
  expect_identical(generated_excel, expected_excel)

 })

testthat::teardown(
  unlink(paste0(base::tempdir(), "/handcrafted"), recursive = TRUE)
)
