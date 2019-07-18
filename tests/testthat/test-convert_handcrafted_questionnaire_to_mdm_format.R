library(questionMetadataPreparation)
library(testthat)

testthat::context("from handcrafted to mdm")


test_that("conversion handcrafted to mdm works", {
  questionMetadataPreparation::convert_zofar_export_to_handcrafted_questionnaire(#nolint
    input_directory = testthat::test_path("from_zofar/questions/ins3"),
    output_directory = paste0(base::tempdir(), "/handcrafted"))

  questionMetadataPreparation::convert_handcrafted_questionnaires_to_mdm_format(
    input_directory = paste0(base::tempdir(), "/handcrafted/"),
    output_directory = paste0(base::tempdir(), "/mdm"),
    images_subdirectory = "/Bilder/png")
  generated_file_list <- list.files(paste0(base::tempdir(),
    "/mdm/"), recursive = TRUE)
  expected_file_list <- c("ins3/1.1.json", "ins3/2.1.json", "ins3/3.1.json",
    "ins3/3.2.json", "ins3/4.1.json", "ins3/4.2.json", "ins3/5.1.json",
    "ins3/images/1.1/1.1_0.json", "ins3/images/1.1/1.1_0.png",
    "ins3/images/2.1/2.1_0.json", "ins3/images/2.1/2.1_0.png",
    "ins3/images/3.1/3.1_0.json", "ins3/images/3.1/3.1_0.png",
    "ins3/images/3.2/3.2_0.json", "ins3/images/3.2/3.2_0.png",
    "ins3/images/4.1/4.1_0.json", "ins3/images/4.1/4.1_0.png",
    "ins3/images/4.2/4.2_0.json", "ins3/images/4.2/4.2_0.png",
    "ins3/images/5.1/5.1_0.json", "ins3/images/5.1/5.1_0.png")
  generated_json1.1 <- jsonlite::read_json(paste0(base::tempdir(),
    "/mdm/ins3/1.1.json"))
  expected_json1.1 <- list(indexInInstrument = 2L,
    instruction = list(en = NULL,
      de = "(Mehrfachnennung möglich, bitte Zutreffendes ankreuzen.)"),
    technicalRespresentation = list(type = "QML",
      source = "<zofar:multipleChoice uid=\"mc\" xmlns:zofar=\"http://www.his.de/zofar/xml/questionnaire\" xmlns:display=\"http://www.dzhw.eu/zofar/xml/display\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n<zofar:header>\n<zofar:question block=\"true\" uid=\"q1\">Welche der folgenden\nTätigkeiten üben Sie derzeit aus?</zofar:question>\n<zofar:instruction block=\"true\" uid=\"ins\">(Mehrfachnennung\nmöglich, bitte Zutreffendes ankreuzen.)</zofar:instruction>\n</zofar:header>\n<zofar:responseDomain itemClasses=\"true\" uid=\"rd1\">\n<zofar:unit uid=\"u1\">\n<zofar:header>\n<zofar:title block=\"true\" uid=\"t1\">#{layout.BOLD_START}Ich\nbin zurzeit ...#{layout.BOLD_END}</zofar:title>\n</zofar:header>\n</zofar:unit>\n<zofar:answerOption label=\"erwerbstätig\" uid=\"ao1\" variable=\"i3zzterw\"/>\n<zofar:answerOption label=\"in kurzfristiger Beschäftigung (Jobben)\" uid=\"ao2\" variable=\"i3zztjob\"/>\n<zofar:answerOption label=\"in einem Volontariat\" uid=\"ao3\" variable=\"i3zztvol\"/>\n<zofar:answerOption label=\"Referendar(in), Inspektorenanwärter(in) (inklusive Anerkennungspraktikum u. Ä.)\" uid=\"ao4\" variable=\"i3zztref\"/>\n<zofar:answerOption label=\"in einem Praktikum\" uid=\"ao5\" variable=\"i3zztprakt\"/>\n<zofar:answerOption label=\"in Berufsausbildung\" uid=\"ao6\" variable=\"i3zztberum\"/>\n<zofar:answerOption label=\"im Studium\" uid=\"ao7\" variable=\"i3zztstudi\"/>\n<zofar:answerOption label=\"Doktorand(in)\" uid=\"ao8\" variable=\"i3zztpromo\"/>\n<zofar:answerOption label=\"Juniorprofessor(in), Habilitand(in)\" uid=\"ao9\" variable=\"i3zzthabil\"/>\n<zofar:answerOption label=\"in akademischer Weiterbildung nach der Promotion („Post-Doc“)\" uid=\"ao10\" variable=\"i3zztpodoc\"/>\n<zofar:answerOption label=\"auf der Suche nach einer (neuen) Erwerbstätigkeit\" uid=\"ao11\" variable=\"i3zztsuche\"/>\n<zofar:answerOption label=\"in Elternzeit\" uid=\"ao12\" variable=\"i3zzterzie\"/>\n<zofar:answerOption label=\"Hausfrau/Hausmann\" uid=\"ao13\" variable=\"i3zzthaus\"/>\n<zofar:answerOption label=\"in einer Umschulung\" uid=\"ao14\" variable=\"i3zztumsch\"/>\n<zofar:answerOption label=\"in einer Fort- bzw. Weiterbildung\" uid=\"ao15\" variable=\"i3zztfortb\"/>\n<zofar:answerOption label=\"arbeitslos\" uid=\"ao16\" variable=\"i3zztablos\"/>\n<zofar:answerOption label=\"anderweitig nicht erwerbstätig\" uid=\"ao17\" variable=\"i3zztnierw\"/>\n<zofar:answerOption label=\"Sonstiges\" uid=\"ao18\" variable=\"i3zztsonst\">\n<zofar:questionOpen size=\"25\" uid=\"ao1\" variable=\"i3zztsonstx\">\n<zofar:prefix>\n<zofar:label uid=\"l1\">und zwar</zofar:label>\n</zofar:prefix>\n</zofar:questionOpen>\n</zofar:answerOption>\n</zofar:responseDomain>\n</zofar:multipleChoice>", #nolint
      language = "XML"), successorNumbers = list("2.1"), type = list(
        en = "Multiple Choice", de = "Mehrfachnennung"), introduction = list(
          en = NULL, de = NULL), questionText = list(en = NULL,
            de = "Welche der folgenden Tätigkeiten üben Sie derzeit aus?"),
    additionalQuestionText = list(en = NULL,
      de = "Welche der folgenden Tätigkeiten üben Sie derzeit aus?,(Mehrfachnennung möglich, bitte Zutreffendes ankreuzen.),Ich bin zurzeit ...,erwerbstätig,in kurzfristiger Beschäftigung (Jobben),in einem Volontariat,Referendar(in), Inspektorenanwärter(in) (inklusive Anerkennungspraktikum u. Ä.),in einem Praktikum,in Berufsausbildung,im Studium,Doktorand(in),Juniorprofessor(in), Habilitand(in),in akademischer Weiterbildung nach der Promotion („Post-Doc“),auf der Suche nach einer (neuen) Erwerbstätigkeit,in Elternzeit,Hausfrau/Hausmann,in einer Umschulung,in einer Fort- bzw. Weiterbildung,arbeitslos,anderweitig nicht erwerbstätig,Sonstiges,und zwar"), #nolint
    topic = list(en = NULL, de = NULL), conceptIds = NULL, annotations = list(
      en = NULL, de = NULL))


  expect_identical(generated_json1.1, expected_json1.1)
  expect_identical(generated_file_list, expected_file_list)
  expect_error(
    questionMetadataPreparation::convert_handcrafted_questionnaires_to_mdm_format( #nolint
    input_directory = testthat::test_dir("erronous_handcrafted_one_column_missing"),
    output_directory = paste0(base::tempdir(), "/mdm_error"),
    images_subdirectory = "/Bilder/png")
  )
  expect_error(
    questionMetadataPreparation::convert_handcrafted_questionnaires_to_mdm_format( #nolint
      input_directory =
        testthat::test_dir("erronous_handcrafted_two_columns_missing"),
      output_directory = paste0(base::tempdir(), "/mdm_error"),
      images_subdirectory = "/Bilder/png")
  )
  expect_error(
    questionMetadataPreparation::convert_handcrafted_questionnaires_to_mdm_format( #nolint
      input_directory =
        testthat::test_dir("from_zofar/questions/ins3"),
      output_directory = paste0(base::tempdir(),
        "/mdm_error_wrong_images_subdirectory"),
      images_subdirectory = "/Bilder")
  )
expect_error(questionMetadataPreparation:::check_missing_columns(
  c("column1", "column2", "column3"),
  dataframe  = data.frame("column1" = c(1, 2, 3), "column2" = c(1, 2, 3),
    "column3" = c(1, 2, 3)),
  sheet_name = "images"), NA
)
  expect_error(questionMetadataPreparation:::check_missing_columns(
    c("column1", "column2", "column3"),
    dataframe  = data.frame("column1" = c(1, 2, 3), "column2" = c(1, 2, 3)),
    sheet_name = "images")
  )
  expect_error(questionMetadataPreparation:::check_missing_columns(
    c("column1", "column2", "column3"),
    dataframe  = data.frame("column1" = c(1, 2, 3)),
    sheet_name = "images")
  )
})
testthat::teardown(
  unlink(paste0(base::tempdir(), "/handcrafted"), recursive = TRUE)
)
