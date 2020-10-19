#' Convert Zofar Export to Handcrafted Questionnaire Format
#'
#' This script takes the export of zofar and converts it into a format which can
#' be manually edited. The zofar export must have the following structure:
#' \preformatted{
#' |--ins1 (must have the pattern "ins\{number\}")
#'   |--5.json
#'   |--images
#'      |--5
#'         |--5_1.png
#'         |--5_1.json
#'         |--5_2.png
#'         |--5_2.json
#'         |--5_3.png
#'         |--5_3.json
#' }
#' The output directory will be created or overwritten.
#'
#' @param input_directory Input path, e.g. "./pages/ins1", must contain
#' "ins\{number\}"
#' @param output_directory Output directory, e.g. "./handcrafted/pages",
#' will be created if it does not exist or will be overwritten otherwise
#' @examples
#' \dontrun{
#' # All examples do exactly the same. They convert everything under "./ins1"
#' # into the handcrafted format and write the output in "./handcrafted/pages".
#' convert_zofar_export_to_handcrafted_questionnaire(input_directory = "./ins1")
#' convert_zofar_export_to_handcrafted_questionnaire(input_directory = "./ins1", output_directory = "./handcrafted/pages")
#' }
#' @export
convert_zofar_export_to_handcrafted_questionnaire <- function(input_directory,
  output_directory = file.path(".", "handcrafted", "pages")) {
  input_directory <- remove_trailing_directory_delimiter(input_directory)
  output_directory <- remove_trailing_directory_delimiter(output_directory)
  create_empty_directory(output_directory)

  instrument_number <- parse_instrument_number(input_directory)

  copy_image_files(input_directory, output_directory, instrument_number)

  sheets <- list(
    "questions" = create_questions_sheet(input_directory, instrument_number),
    "images" = create_images_sheet(input_directory, instrument_number)
  )

  excel_file <- file.path(output_directory,
    paste0("questions-ins", instrument_number, ".xlsx"))
  message("Write excel: ", excel_file)
  openxlsx::write.xlsx(sheets,
    file = excel_file,
    row.names = FALSE, showNA = FALSE)
}

copy_image_files <- function(input_directory, output_directory,
  instrument_number) {
  images_directory <- file.path(output_directory, "Bilder", "png",
    paste0("ins", instrument_number))
  create_empty_directory(images_directory)
  question_image_pngs <- dir(
    file.path(input_directory, "images"), pattern = "*.png",
    recursive = TRUE, full.names = TRUE)
  for (png in question_image_pngs) {
    message("Copy image '", png, "' to directory '",
      images_directory, "'")
    file.copy(png, images_directory)
  }
}

remove_trailing_directory_delimiter <- function(directory) {
  return(gsub(directory,
    pattern = paste0("\\", .Platform$file.sep, "$"), replacement = ""))
}

create_empty_directory <- function(directory) {
  if (dir.exists(directory)) {
    unlink(directory, recursive = TRUE)
  }
  dir.create(directory, recursive = TRUE)
}

parse_instrument_number <- function(input_directory) {
  return(gsub(input_directory, pattern = "^.*ins", replacement = ""))
}

create_questions_sheet <- function(input_directory, instrument_number) {
  question_jsons <- dir(input_directory, pattern = "*.json")
  col_names <- c("indexInInstrument",
  "questionNumber",
  "instrumentNumber",
  "successorNumbers",
  "questionText.de",
  "questionText.en",
  "instruction.de",
  "instruction.en",
  "introduction.de",
  "introduction.en",
  "type.de",
  "type.en",
  "topic.de",
  "topic.en",
  "technicalRepresentation.type",
  "technicalRepresentation.language",
  "technicalRepresentation.source",
  "additionalQuestionText.de",
  "additionalQuestionText.en",
  "annotations.de",
  "annotations.en",
  "conceptIds")
  excel_sheet <- data.frame(matrix(ncol = length(col_names),
    nrow = length(question_jsons)))
  colnames(excel_sheet) <- col_names

  for (i in seq_along(question_jsons)) {
    message("Read question json: ", question_jsons[[i]])
    json <- jsonlite::fromJSON(
      file.path(input_directory, question_jsons[[i]]))

    excel_sheet$indexInInstrument[i] <- json$indexInInstrument
    excel_sheet$questionNumber[i] <- gsub(question_jsons[[i]],
      pattern = ".json", replacement = "")
    excel_sheet$instrumentNumber[i] <- instrument_number
    excel_sheet$successorNumbers[i] <- ifelse(
      length(json$successorNumbers) == 0,
      NA_character_, paste0(json$successorNumbers, collapse = ","))
    excel_sheet$questionText.de[i] <- ifelse(length(json$questionText$de) == 0,
      NA_character_, json$questionText$de)
    excel_sheet$questionText.en[i] <- ifelse(length(json$questionText$en) == 0,
      NA_character_, json$questionText$en)
    excel_sheet$instruction.de[i] <- ifelse(length(json$instruction$de) == 0,
      NA_character_, json$instruction$de)
    excel_sheet$instruction.en[i] <- ifelse(length(json$instruction$en) == 0,
      NA_character_, json$instruction$en)
    excel_sheet$introduction.de[i] <- ifelse(length(json$introduction$de) == 0,
      NA_character_, json$introduction$de)
    excel_sheet$introduction.en[i] <- ifelse(length(json$introduction$en) == 0,
      NA_character_, json$introduction$en)
    excel_sheet$type.de[i] <- json$type$de
    excel_sheet$type.en[i] <- json$type$en
    excel_sheet$topic.de[i] <- ifelse(length(json$topic$de) == 0,
      NA_character_, json$topic$de)
    excel_sheet$topic.en[i] <- ifelse(length(json$topic$en) == 0,
      NA_character_, json$topic$en)
    excel_sheet$technicalRepresentation.type[i] <-
      json$technicalRepresentation$type
    excel_sheet$technicalRepresentation.language[i] <-
      json$technicalRepresentation$language
    excel_sheet$technicalRepresentation.source[i] <-
      json$technicalRepresentation$source
    excel_sheet$additionalQuestionText.de[i] <- ifelse(
      length(json$additionalQuestionText$de) == 0,
      NA_character_, json$additionalQuestionText$de)
    excel_sheet$additionalQuestionText.en[i] <- ifelse(
      length(json$additionalQuestionText$en) == 0,
      NA_character_, json$additionalQuestionText$en)
    excel_sheet$annotations.de[i] <- ifelse(
      length(json$annotations$de) == 0,
      NA_character_, json$annotations$de)
    excel_sheet$annotations.en[i] <- ifelse(
      length(json$annotations$en) == 0,
      NA_character_, json$annotations$en)
    excel_sheet$conceptIds <- ifelse(
      length(json$conceptIds) == 0,
      NA_character_, json$conceptIds)
  }
  # sort by indexInInstrument
  sorted_excel <- excel_sheet[order(excel_sheet$indexInInstrument), ]

  return(sorted_excel)
}

create_images_sheet <- function(input_directory, instrument_number) {
  question_image_jsons <- dir(
    file.path(input_directory, "images"), pattern = "*.json",
    recursive = TRUE)
  col_names <- c("fileName",
  "questionNumber",
  "instrumentNumber",
  "language",
  "containsAnnotations",
  "indexInQuestion",
  "resolution.widthX",
  "resolution.heightY")
  excel_sheet <- data.frame(matrix(ncol = length(col_names),
    nrow = length(question_image_jsons)))
  colnames(excel_sheet) <- col_names

  for (i in seq_along(question_image_jsons)) {
    message("Read image json: ", question_image_jsons[[i]])
    json <- jsonlite::fromJSON(file.path(input_directory, "images",
      question_image_jsons[[i]]))

    # zofar delivers the field 'file' instead of 'fileName',
    # furthermore there are path delimiters which we need to be stripped of
    # generally this field is not required at all for the MDM
    excel_sheet$fileName[i] <- parse_image_filename(json)

    # generally this field is not required at all for the MDM
    excel_sheet$questionNumber[i] <- parse_question_number(
      question_image_jsons[[i]])

    # generally this field is not required at all for the MDM
    excel_sheet$instrumentNumber[i] <- instrument_number

    excel_sheet$language[i] <- ifelse(length(json$language) == 0,
      NA_character_, json$language)
    excel_sheet$containsAnnotations[i] <- ifelse(
      length(json$containsAnnotations) == 0,
      NA_character_,
      json$containsAnnotations)
    excel_sheet$indexInQuestion[i] <- parse_index_in_question(json,
      question_image_jsons[[i]])
    excel_sheet$resolution.widthX[i] <- parse_resolution_width(json)
    excel_sheet$resolution.heightY[i] <- parse_resolution_height(json)
  }
  # sort by questionNumber
  sorted_excel <- excel_sheet[order(excel_sheet$questionNumber), ]

  return(sorted_excel)
}

parse_resolution_width <- function(image_json) {
  return(ifelse(length(image_json$resolution) == 0,
    NA_character_,
    gsub(image_json$resolution, pattern = "x.*$", replacement = "")))
}

parse_resolution_height <- function(image_json) {
  return(ifelse(length(image_json$resolution) == 0,
    NA_character_,
    gsub(image_json$resolution, pattern = "^.*x", replacement = "")))
}

parse_index_in_question <- function(image_json, json_filename) {
  ifelse(length(image_json$indexInQuestion) == 0,
    gsub(gsub(json_filename, pattern = "^.*_",
      replacement = ""), pattern = ".json", replacement = ""),
    image_json$indexInQuestion)
}

parse_image_filename <- function(image_json) {
  gsub(ifelse(length(image_json$fileName) == 0,
    ifelse(length(image_json$file) == 0, NA_character_, image_json$file),
    image_json$fileName), pattern = "^.*/", replacement = "")
}

parse_question_number <- function(filename) {
  return(gsub(gsub(filename,
    pattern = "_[0-9]*.json", replacement = ""),
    pattern = paste0(.Platform$file.sep, ".*$"), replacement = ""))
}
