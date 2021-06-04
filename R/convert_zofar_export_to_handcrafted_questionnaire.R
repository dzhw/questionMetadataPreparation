#' Convert Zofar Export to Handcrafted Questionnaire Format
#'
#' This script takes the export of zofar and converts it into a format which can
#' be manually edited. The zofar export must have the following structure:
#' \preformatted{
#' 01_zofar-format (= input_directory)
#'   |--ins1 (must have the pattern "ins\{number\}")
#'      |--5.json
#'      |--images
#'         |--5
#'            |--5_1.png
#'            |--5_1.json
#'            |--5_2.png
#'            |--5_2.json
#'   |--ins2
#' }
#' Multiple instruments will be created in the output_directory
#' (=../02_handcrafted-format). Existing instruments in the output_directory
#' will be overwritten if confirmed.
#' @param input_directory Input path, e.g. "01_zofar-format", must contain at
#' least one subfolder "ins\{number\}"
#' @param output_directory Output directory, e.g. "./02_handcrafted-format",
#' existing instruments in this directory will be overwritten if confirmed.
#' @examples
#' \dontrun{
#' # All examples do exactly the same. They convert everything under "."
#' # into the handcrafted format and write the output in "../02_handcrafted-format".
#' convert_zofar_export_to_handcrafted_questionnaire(input_directory = ".")
#' convert_zofar_export_to_handcrafted_questionnaire(input_directory = ".", output_directory = "../02_handcrafted-format")
#' }
#' @export
convert_zofar_export_to_handcrafted_questionnaire <- function(
  input_directory = ".",
  output_directory = file.path("..", "02_handcrafted-format")) {
  input_directory <- remove_trailing_directory_delimiter(input_directory)
  output_directory <- remove_trailing_directory_delimiter(output_directory)
  if (!file.exists(output_directory)) {
    create_empty_directory(output_directory)
  }
  instrument_directories <- dir(input_directory,
      pattern = "ins*", full.names = TRUE)

  invisible(lapply(instrument_directories, function(instrument_directory) {
    message("\nProcessing instrument: ", normalizePath(instrument_directory))
    instrument_number <- parse_instrument_number(instrument_directory)
    sheets <- list(
      "questions" = create_questions_sheet(instrument_directory,
        instrument_number),
      "images" = create_images_sheet(instrument_directory, instrument_number)
    )

    excel_file <- file.path(output_directory,
      paste0("questions-ins", instrument_number, ".xlsx"))
    answer <- ask_for_overwrite(excel_file)
    if (answer == 1) {
      openxlsx::write.xlsx(sheets, file = excel_file, row.names = FALSE,
        showNA = FALSE)
      message("Wrote excel: ", normalizePath(excel_file))
    } else {
      message("Skipping excel: ", normalizePath(excel_file))
    }

    copy_image_files(instrument_directory, output_directory, instrument_number)
  }))

  message("\nSuccessfully created handcrafted format: ",
    normalizePath(output_directory))
}

copy_image_files <- function(instrument_directory, output_directory,
  instrument_number) {
  images_directory <- file.path(output_directory, "images",
    paste0("ins", instrument_number))
  answer <- ask_for_overwrite(images_directory)
  if (answer != 1) {
    message("Skipping directory: ", normalizePath(images_directory))
    return()
  }
  create_empty_directory(images_directory)
  message("Copying images to directory '", normalizePath(images_directory), "'")
  question_image_pngs <- dir(
    file.path(instrument_directory, "images"), pattern = "*.png",
    recursive = TRUE, full.names = TRUE)
  for (png in question_image_pngs) {
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

parse_instrument_number <- function(instrument_directory) {
  return(stringr::str_match(instrument_directory, "(?<=ins)\\d*"))
}

create_questions_sheet <- function(instrument_directory, instrument_number) {
  message("Reading question jsons in '",
    normalizePath(instrument_directory), "'")
  question_jsons <- dir(instrument_directory, pattern = "*.json")
  col_names <- c("indexInInstrument",
  "questionNumber",
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
    json <- jsonlite::fromJSON(
      file.path(instrument_directory, question_jsons[[i]]))

    excel_sheet$indexInInstrument[i] <- json$indexInInstrument
    excel_sheet$questionNumber[i] <- gsub(question_jsons[[i]],
      pattern = ".json", replacement = "")
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

create_images_sheet <- function(instrument_directory, instrument_number) {
  message("Reading image jsons in '",
    normalizePath(file.path(instrument_directory, "images")), "'")
  question_image_jsons <- dir(
    file.path(instrument_directory, "images"), pattern = "*.json",
    recursive = TRUE)
  col_names <- c("fileName",
  "questionNumber",
  "language",
  "containsAnnotations",
  "indexInQuestion",
  "resolution.widthX",
  "resolution.heightY")
  excel_sheet <- data.frame(matrix(ncol = length(col_names),
    nrow = length(question_image_jsons)))
  colnames(excel_sheet) <- col_names

  for (i in seq_along(question_image_jsons)) {
    json <- jsonlite::fromJSON(file.path(instrument_directory, "images",
      question_image_jsons[[i]]))

    excel_sheet$questionNumber[i] <- parse_question_number(
      question_image_jsons[[i]])

    excel_sheet$language[i] <- ifelse(length(json$language) == 0,
      NA_character_, json$language)
    excel_sheet$containsAnnotations[i] <- ifelse(
      length(json$containsAnnotations) == 0,
      NA_character_,
      json$containsAnnotations)
    excel_sheet$indexInQuestion[i] <- parse_index_in_question(json,
      question_image_jsons[[i]])

    excel_sheet$fileName[i] <- paste0(excel_sheet$questionNumber[i],"_",
      excel_sheet$indexInQuestion[i], ".png")

    excel_sheet$resolution.widthX[i] <-
      ifelse(length(json$resolution) == 0, NA_character_, json$resolution$widthX)
    excel_sheet$resolution.heightY[i] <-
      ifelse(length(json$resolution)== 0, NA_character_, json$resolution$heightY)
  }
  # sort by questionNumber
  sorted_excel <- excel_sheet[order(excel_sheet$questionNumber), ]

  return(sorted_excel)
}

parse_index_in_question <- function(image_json, json_filename) {
  ifelse(length(image_json$indexInQuestion) == 0,
    gsub(gsub(json_filename, pattern = "^.*_",
      replacement = ""), pattern = ".json", replacement = ""),
    image_json$indexInQuestion)
}

parse_question_number <- function(filename) {
  return(gsub(gsub(filename,
    pattern = "_[0-9]*.json", replacement = ""),
    pattern = paste0(.Platform$file.sep, ".*$"), replacement = ""))
}

ask_for_overwrite <- function(filename) {
  if (file.exists(filename)) {
    cat(paste0("\nGoing to overwrite '", normalizePath(filename),
      "'.\nMay I proceed? [1=yes, 2=skip, default=exit program]: "))
    stdin <- file("stdin")
    answer <- readLines(stdin,1)
    close(stdin)
    if(answer == 1 || answer == 2) {
      return(answer)
    } else {
      message("Exiting programm...")
      stop_quietly()
    }
  }
  return(1)
}

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
