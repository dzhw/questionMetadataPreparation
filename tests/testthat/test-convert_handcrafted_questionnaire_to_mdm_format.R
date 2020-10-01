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
    sheet_name = "images"), NA)
  expect_error(questionMetadataPreparation:::check_missing_columns(
    c("column1", "column2", "column3"),
    dataframe  = data.frame("column1" = c(1, 2, 3), "column2" = c(1, 2, 3)),
    sheet_name = "images"))
  expect_error(questionMetadataPreparation:::check_missing_columns(
    c("column1", "column2", "column3"),
    dataframe  = data.frame("column1" = c(1, 2, 3)),
    sheet_name = "images"))

})

test_that("different delimiters are allowed", {
  excel_multiple_delimiters <- structure(
    list(indexInInstrument = c("2", "3", "4", "5", "6", "7", "8"),
         questionNumber = c("1.1", "2.1", "3.1", "3.2", "4.1","4.2", "5.1"),
         instrumentNumber = c("3", "3", "3", "3", "3", "3", "3"),
         successorNumbers = c("2.1", "3.1",
                              "3.1;4.1,4.1,5.1",
                              "3.1;4.1,4.1,5.1",
                              "4.1;5 1,5.1,6.1",
                              "4.1,5.1,5.1,6.1",
                              "6.1"),
         questionText.de = c("Welche der folgenden Tätigkeiten üben Sie derzeit aus?",  #nolint
                             "Wie würden Sie Ihre derzeitige Tätigkeit bzw. Situation bezeichnen?", #nolint
                             "Waren Sie nach Studienabschluss schon einmal beruflich selbständig?", #nolint
                             "Kein Fragetext vorhanden", "Haben Sie vor, sich beruflich selbständig zu machen?", #nolint
                             "Kein Fragetext vorhanden", "In welcher Form sind Sie als Selbständige(r) tätig bzw. beabsichtigen Sie, tätig zu sein?"), #nolint
         questionText.en = c(NA, NA, NA, "No questiontext available",
                             NA, "No questiontext available", NA),
         instruction.de = c("(Mehrfachnennung möglich, bitte Zutreffendes ankreuzen.)",  #nolint
                            NA, "auch gemeint sind Selbständigkeiten mit Werkvertrag", NA, #nolint
                            NA, NA, NA),
         instruction.en = c(NA_character_, NA_character_, NA_character_,
                            NA_character_, NA_character_, NA_character_,
                            NA_character_),
         introduction.de = c(NA_character_, NA_character_, NA_character_,
                             NA_character_, NA_character_, NA_character_,
                             NA_character_),
         introduction.en = c(NA_character_, NA_character_, NA_character_,
                             NA_character_, NA_character_, NA_character_,
                             NA_character_),
         type.de = c("Mehrfachnennung", "Einfachnennung", "Einfachnennung",
                     "Mehrfachnennung", "Einfachnennung", "Mehrfachnennung",
                     "Einfachnennung"),
         type.en = c("Multiple Choice", "Single Choice", "Single Choice",
                     "Multiple Choice", "Single Choice", "Multiple Choice",
                     "Single Choice"),
         topic.de = c(NA_character_, NA_character_, NA_character_,
                      NA_character_, NA_character_, NA_character_, NA_character_),  #nolint
         topic.en = c(NA_character_, NA_character_, NA_character_,
                      NA_character_, NA_character_, NA_character_, NA_character_),  #nolint
         technicalRepresentation.type = c("QML", "QML", "QML", "QML", "QML", "QML", "QML"),  #nolint
         technicalRepresentation.language = c("XML", "XML", "XML", "XML", "XML", "XML", "XML"), #nolint
         technicalRepresentation.source = c("<zofar:multipleChoice uid=\"mc\" xmlns:zofar=\"http://www.his.de/zofar/xml/questionnaire\" xmlns:display=\"http://www.dzhw.eu/zofar/xml/display\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n<zofar:header>\n<zofar:question block=\"true\" uid=\"q1\">Welche der folgenden\nTätigkeiten üben Sie derzeit aus?</zofar:question>\n<zofar:instruction block=\"true\" uid=\"ins\">(Mehrfachnennung\nmöglich, bitte Zutreffendes ankreuzen.)</zofar:instruction>\n</zofar:header>\n<zofar:responseDomain itemClasses=\"true\" uid=\"rd1\">\n<zofar:unit uid=\"u1\">\n<zofar:header>\n<zofar:title block=\"true\" uid=\"t1\">#{layout.BOLD_START}Ich\nbin zurzeit ...#{layout.BOLD_END}</zofar:title>\n</zofar:header>\n</zofar:unit>\n<zofar:answerOption label=\"erwerbstätig\" uid=\"ao1\" variable=\"i3zzterw\"/>\n<zofar:answerOption label=\"in kurzfristiger Beschäftigung (Jobben)\" uid=\"ao2\" variable=\"i3zztjob\"/>\n<zofar:answerOption label=\"in einem Volontariat\" uid=\"ao3\" variable=\"i3zztvol\"/>\n<zofar:answerOption label=\"Referendar(in), Inspektorenanwärter(in) (inklusive Anerkennungspraktikum u. Ä.)\" uid=\"ao4\" variable=\"i3zztref\"/>\n<zofar:answerOption label=\"in einem Praktikum\" uid=\"ao5\" variable=\"i3zztprakt\"/>\n<zofar:answerOption label=\"in Berufsausbildung\" uid=\"ao6\" variable=\"i3zztberum\"/>\n<zofar:answerOption label=\"im Studium\" uid=\"ao7\" variable=\"i3zztstudi\"/>\n<zofar:answerOption label=\"Doktorand(in)\" uid=\"ao8\" variable=\"i3zztpromo\"/>\n<zofar:answerOption label=\"Juniorprofessor(in), Habilitand(in)\" uid=\"ao9\" variable=\"i3zzthabil\"/>\n<zofar:answerOption label=\"in akademischer Weiterbildung nach der Promotion („Post-Doc“)\" uid=\"ao10\" variable=\"i3zztpodoc\"/>\n<zofar:answerOption label=\"auf der Suche nach einer (neuen) Erwerbstätigkeit\" uid=\"ao11\" variable=\"i3zztsuche\"/>\n<zofar:answerOption label=\"in Elternzeit\" uid=\"ao12\" variable=\"i3zzterzie\"/>\n<zofar:answerOption label=\"Hausfrau/Hausmann\" uid=\"ao13\" variable=\"i3zzthaus\"/>\n<zofar:answerOption label=\"in einer Umschulung\" uid=\"ao14\" variable=\"i3zztumsch\"/>\n<zofar:answerOption label=\"in einer Fort- bzw. Weiterbildung\" uid=\"ao15\" variable=\"i3zztfortb\"/>\n<zofar:answerOption label=\"arbeitslos\" uid=\"ao16\" variable=\"i3zztablos\"/>\n<zofar:answerOption label=\"anderweitig nicht erwerbstätig\" uid=\"ao17\" variable=\"i3zztnierw\"/>\n<zofar:answerOption label=\"Sonstiges\" uid=\"ao18\" variable=\"i3zztsonst\">\n<zofar:questionOpen size=\"25\" uid=\"ao1\" variable=\"i3zztsonstx\">\n<zofar:prefix>\n<zofar:label uid=\"l1\">und zwar</zofar:label>\n</zofar:prefix>\n</zofar:questionOpen>\n</zofar:answerOption>\n</zofar:responseDomain>\n</zofar:multipleChoice>", #nolint
"<zofar:questionSingleChoice uid=\"sc\" xmlns:zofar=\"http://www.his.de/zofar/xml/questionnaire\" xmlns:display=\"http://www.dzhw.eu/zofar/xml/display\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n<zofar:header>\n<zofar:question block=\"true\" uid=\"q1\">Wie würden Sie Ihre\nderzeitige Tätigkeit bzw. Situation bezeichnen?</zofar:question>\n</zofar:header>\n<zofar:responseDomain itemClasses=\"true\" uid=\"rd1\" variable=\"i3tatpersp\">\n<zofar:answerOption label=\"Als kurzfristige Übergangssituation\" uid=\"ao1\" value=\"1\"/>\n<zofar:answerOption label=\"Als Situation, die voraussichtlich mittelfristig Bestand haben wird\" uid=\"ao2\" value=\"2\"/>\n<zofar:answerOption label=\"Als Situation, die vermutlich langfristig stabil sein wird\" uid=\"ao3\" value=\"3\"/>\n</zofar:responseDomain>\n</zofar:questionSingleChoice>", #nolint
"<zofar:questionSingleChoice uid=\"sc\" xmlns:zofar=\"http://www.his.de/zofar/xml/questionnaire\" xmlns:display=\"http://www.dzhw.eu/zofar/xml/display\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n<zofar:header>\n<zofar:question block=\"true\" uid=\"q1\">Waren Sie nach\nStudienabschluss schon einmal beruflich selbständig?</zofar:question>\n<zofar:instruction uid=\"int1\">auch gemeint sind\nSelbständigkeiten mit Werkvertrag</zofar:instruction>\n</zofar:header>\n<zofar:responseDomain itemClasses=\"true\" uid=\"rd1\" variable=\"f3selbwar\">\n<zofar:answerOption label=\"Ja, ich bin zurzeit selbständig\" uid=\"ao1\" value=\"1\"/>\n<zofar:answerOption label=\"Ja, ich war früher einmal selbständig\" uid=\"ao2\" value=\"2\"/>\n<zofar:answerOption label=\"Nein\" uid=\"ao3\" value=\"3\"/>\n</zofar:responseDomain>\n</zofar:questionSingleChoice>", #nolint
"<zofar:multipleChoice uid=\"mc\" xmlns:zofar=\"http://www.his.de/zofar/xml/questionnaire\" xmlns:display=\"http://www.dzhw.eu/zofar/xml/display\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n<zofar:header>\n<zofar:question uid=\"qm\"/>\n</zofar:header>\n<zofar:responseDomain uid=\"rd\">\n<zofar:answerOption label=\"Ja, ich möchte diese Frage überspringen.\" uid=\"ao1\" variable=\"flag1\"/>\n</zofar:responseDomain>\n</zofar:multipleChoice>", #nolint
"<zofar:questionSingleChoice uid=\"sc\" xmlns:zofar=\"http://www.his.de/zofar/xml/questionnaire\" xmlns:display=\"http://www.dzhw.eu/zofar/xml/display\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n<zofar:header>\n<zofar:question block=\"true\" uid=\"q1\">Haben Sie vor, sich\nberuflich selbständig zu machen?</zofar:question>\n</zofar:header>\n<zofar:responseDomain itemClasses=\"true\" uid=\"rd1\" variable=\"f3selbvor\">\n<zofar:answerOption label=\"Ja, ich erwäge es ernsthaft\" uid=\"ao1\" value=\"1\"/>\n<zofar:answerOption label=\"Nein, weil derzeit einiges dagegen spricht\" uid=\"ao2\" value=\"2\"/>\n<zofar:answerOption label=\"Nein, kommt für mich gar nicht in Frage\" uid=\"ao3\" value=\"3\"/>\n</zofar:responseDomain>\n</zofar:questionSingleChoice>", #nolint
"<zofar:multipleChoice uid=\"mc\" xmlns:zofar=\"http://www.his.de/zofar/xml/questionnaire\" xmlns:display=\"http://www.dzhw.eu/zofar/xml/display\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n<zofar:header>\n<zofar:question uid=\"qm\"/>\n</zofar:header>\n<zofar:responseDomain uid=\"rd\">\n<zofar:answerOption label=\"Ja, ich möchte diese Frage überspringen.\" uid=\"ao1\" variable=\"flag2\"/>\n</zofar:responseDomain>\n</zofar:multipleChoice>", #nolint
"<zofar:questionSingleChoice uid=\"sc\" xmlns:zofar=\"http://www.his.de/zofar/xml/questionnaire\" xmlns:display=\"http://www.dzhw.eu/zofar/xml/display\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n<zofar:header>\n<zofar:question block=\"true\" uid=\"q\">In welcher Form sind\nSie als Selbständige(r) tätig bzw. beabsichtigen Sie, tätig zu\nsein?</zofar:question>\n</zofar:header>\n<zofar:responseDomain itemClasses=\"true\" uid=\"rd\" variable=\"f3selbform\">\n<zofar:answerOption label=\"Als Freiberufler(in) durch Übernahme (z. B. einer Praxis) oder Eintritt (z. B. in eine Kanzlei)\" uid=\"ao1\" value=\"1\"/>\n<zofar:answerOption label=\"Als Freiberufler(in) durch Gründung (z. B. einer Praxis)\" uid=\"ao2\" value=\"2\"/>\n<zofar:answerOption label=\"Durch Übernahme einer Firma\" uid=\"ao3\" value=\"3\"/>\n<zofar:answerOption label=\"Durch Gründung einer Firma\" uid=\"ao4\" value=\"4\"/>\n<zofar:answerOption label=\"Als sonstige(r) Selbständige(r) (z. B. auf Basis von Werkverträgen oder Honoraren)\" uid=\"ao5\" value=\"5\"/>\n<zofar:answerOption label=\"Das ist noch unklar\" uid=\"ao6\" value=\"6\"/>\n</zofar:responseDomain>\n</zofar:questionSingleChoice>"), #nolint
additionalQuestionText.de = c("Welche der folgenden Tätigkeiten üben Sie derzeit aus?,(Mehrfachnennung möglich, bitte Zutreffendes ankreuzen.),Ich bin zurzeit ...,erwerbstätig,in kurzfristiger Beschäftigung (Jobben),in einem Volontariat,Referendar(in), Inspektorenanwärter(in) (inklusive Anerkennungspraktikum u. Ä.),in einem Praktikum,in Berufsausbildung,im Studium,Doktorand(in),Juniorprofessor(in), Habilitand(in),in akademischer Weiterbildung nach der Promotion („Post-Doc“),auf der Suche nach einer (neuen) Erwerbstätigkeit,in Elternzeit,Hausfrau/Hausmann,in einer Umschulung,in einer Fort- bzw. Weiterbildung,arbeitslos,anderweitig nicht erwerbstätig,Sonstiges,und zwar",
"Wie würden Sie Ihre derzeitige Tätigkeit bzw. Situation bezeichnen?,Als kurzfristige Übergangssituation,Als Situation, die voraussichtlich mittelfristig Bestand haben wird,Als Situation, die vermutlich langfristig stabil sein wird",
"Waren Sie nach Studienabschluss schon einmal beruflich selbständig?,auch gemeint sind Selbständigkeiten mit Werkvertrag,Ja, ich bin zurzeit selbständig,Ja, ich war früher einmal selbständig,Nein",
"Ja, ich möchte diese Frage überspringen.", "Haben Sie vor, sich beruflich selbständig zu machen?,Ja, ich erwäge es ernsthaft,Nein, weil derzeit einiges dagegen spricht,Nein, kommt für mich gar nicht in Frage",
"Ja, ich möchte diese Frage überspringen.", "In welcher Form sind Sie als Selbständige(r) tätig bzw. beabsichtigen Sie, tätig zu sein?,Als Freiberufler(in) durch Übernahme (z. B. einer Praxis) oder Eintritt (z. B. in eine Kanzlei),Als Freiberufler(in) durch Gründung (z. B. einer Praxis),Durch Übernahme einer Firma,Durch Gründung einer Firma,Als sonstige(r) Selbständige(r) (z. B. auf Basis von Werkverträgen oder Honoraren),Das ist noch unklar"
),
additionalQuestionText.en = c(NA_character_, NA_character_, NA_character_,
                              NA_character_, NA_character_, NA_character_,
                              NA_character_),
annotations.de = c(NA_character_, NA_character_, NA_character_, NA_character_,
                   NA_character_, NA_character_, NA_character_),
annotations.en = c(NA_character_, NA_character_, NA_character_, NA_character_,
                   NA_character_, NA_character_,
NA_character_), conceptIds = c(NA_character_, NA_character_, NA_character_,
                               NA_character_, NA_character_, NA_character_,
                               NA_character_)), row.names = c(NA, 7L),
class = "data.frame")
  successornumbers <- list()
  for(i in 1:7){
    successornumbers[[i]] <- questionMetadataPreparation:::list_attribute(excel_multiple_delimiters[i, "successorNumbers"]) #nolint
  }
  testthat::expect_identical(successornumbers,
    list("2.1",
         "3.1",
         c("3.1", "4.1", "4.1", "5.1"),
         c("3.1", "4.1", "4.1", "5.1"),
         c("4.1", "5", "1", "5.1", "6.1"),
         c("4.1", "5.1", "5.1", "6.1"),
         "6.1"))
})
testthat::teardown(
  unlink(paste0(base::tempdir(), "/handcrafted"), recursive = TRUE)
)
