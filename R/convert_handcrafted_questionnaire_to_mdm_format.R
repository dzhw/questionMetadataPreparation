#' Convert Handcrafted Questionnaires to MDM Format
#'
#' This script converts the handcrafted instruments which consist each of an excel
#' and many images into the format which can be
#' uploaded to the [MDM](https://metadata.fdz.dzhw.eu). The handcrafted
#' instruments must have the following format:
#' \preformatted{
#' |--02_handcrafted-format (=input_directory)
#'   |--questions-ins1.xlsx (two sheets, questions and images)
#'   |--questions-ins2.xlsx (two sheets, questions and images)
#'   |--images
#'      |--ins1
#'         |--5_1.png (must match the filename in the images excel sheet)
#'         |--5_2.png (must match the filename in the images excel sheet)
#'      |--ins2
#'         |--5_1.png (must match the filename in the images excel sheet)
#'         |--5_2.png (must match the filename in the images excel sheet)
#' }
#' The format of the excel sheets is defined [here](https://github.com/dzhw/FDZ_Allgemein/wiki/Fragen-2.0).
#' Multiple instruments are converted at once. Existing instruments in the
#' output_directory will be overwritten if confirmed.
#' @param input_directory Input path, e.g. "02_handcrafted-format"
#' @param output_directory Output directory, e.g. "../03_mdm-format",
#' existing instruments will be overwritten if confirmed
#' @param images_subdirectory Path relative to input_directory containing the
#' images, e.g. "images"
#' @examples
#' \dontrun{
#' # All examples do exactly the same. They convert everything under "."
#' # into the MDM format and write the output in "../03_mdm-format". Images will be
#' # searched for in "./images".
#' convert_handcrafted_questionnaires_to_mdm_format(input_directory = ".")
#' convert_handcrafted_questionnaires_to_mdm_format(input_directory = ".", output_directory = "../03_mdm-format")
#' convert_handcrafted_questionnaires_to_mdm_format(input_directory = ".", output_directory = "../03_mdm-format", images_subdirectory = "images")
#' }
#' @export
convert_handcrafted_questionnaires_to_mdm_format <- function(
  input_directory = file.path("."),
  output_directory = file.path("..", "03_mdm-format"),
  images_subdirectory = file.path("images")) {
  input_directory <- remove_trailing_directory_delimiter(input_directory)
  output_directory <- remove_trailing_directory_delimiter(output_directory)
  if (!file.exists(output_directory)) {
    create_empty_directory(output_directory)
  }
  images_subdirectory <- remove_trailing_directory_delimiter(
    images_subdirectory)

  xlsx_files <- dir(input_directory, pattern = "*.xlsx",
    full.names = TRUE)

  invisible(lapply(xlsx_files, function(xlsx_file) {
    message("\nProcessing file ", normalizePath(xlsx_file))
    instrument_number <- parse_instrument_number(xlsx_file)
    instrument_directory <- file.path(output_directory,
      paste0("ins", instrument_number))

    answer <- ask_for_overwrite(instrument_directory)
    if (answer != 1) {
      message("Skipping directory: ", normalizePath(instrument_directory))
      return()
    }
    create_empty_directory(instrument_directory)

    write_question_jsons(xlsx_file, instrument_directory)

    write_question_images(xlsx_file, input_directory, instrument_directory,
      instrument_number, images_subdirectory)
  }))

  message("\nSuccessfully created mdm format: ",
    normalizePath(output_directory))
}

write_question_jsons <- function(xlsx_file, instrument_directory) {
  message("Read excel file: sheet questions")
  excel <- read_and_trim_excel(xlsx_file, sheet = "questions")
  # trim later
  check_missing_columns(c("instruction.de", "instruction.en",
    "introduction.de", "introduction.en",
    "type.de", "type.en", "topic.de", "topic.en",
    "technicalRepresentation.type", "technicalRepresentation.language",
    "technicalRepresentation.source", "annotations.de", "annotations.en",
    "additionalQuestionText.de", "additionalQuestionText.en", "conceptIds"),
    excel, "questions")
  excel <- trim_list_cols(excel, col1 = "successorNumbers", col2 = "conceptIds")
  message("Writing question jsons: ", normalizePath(instrument_directory))
  # for all questions in excel
  for (i in rownames(excel)) {
    que <- new.env(hash = TRUE, parent = emptyenv())

    que[["indexInInstrument"]] <- jsonlite::unbox(
      as.numeric(excel[i, "indexInInstrument"])
    )
    que[["questionText"]] <- new.i18n_string(
      excel[i, "questionText.de"], excel[i, "questionText.en"]
    )
    que[["instruction"]] <- new.i18n_string(
      excel[i, "instruction.de"], excel[i, "instruction.en"]
    )
    que[["introduction"]] <- new.i18n_string(
      excel[i, "introduction.de"], excel[i, "introduction.en"]
    )
    que[["type"]] <- new.i18n_string(
      excel[i, "type.de"], excel[i, "type.en"]
    )
    que[["additionalQuestionText"]] <- new.i18n_string(
      excel[i, "additionalQuestionText.de"],
      excel[i, "additionalQuestionText.en"]
    )
    que[["topic"]] <- new.i18n_string(
      excel[i, "topic.de"], excel[i, "topic.en"]
    )
    que[["annotations"]] <- new.i18n_string(
      excel[i, "annotations.de"], excel[i, "annotations.en"]
    )
    que[["successorNumbers"]] <- list_attribute(
      excel[i, "successorNumbers"]
    )
    que[["technicalRespresentation"]] <- new.technical_representation(
      excel[i, "technicalRepresentation.type"],
      excel[i, "technicalRepresentation.language"],
      excel[i, "technicalRepresentation.source"]
    )
    que[["conceptIds"]] <- list_attribute(
      excel[i, "conceptIds"]
    )

    # json export
    question_json <- file.path(
      instrument_directory,
      paste0(excel[i, "questionNumber"], ".json")
    )

    con <- file(
      description = question_json,
      open = "w", encoding = "UTF-8"
    )

    write(jsonlite::toJSON(nested_env_as_list(que),
      null = "null", na = "null",
      pretty = TRUE
    ), file = con)
    close(con)
  }
}

write_question_images <- function(xlsx_file, input_directory,
  instrument_directory, instrument_number, images_subdirectory) {
  message("Read excel file: sheet images")
  excel <- read_and_trim_excel(xlsx_file, sheet = "images")

  # for all images in excel
  check_missing_columns(c("fileName", "questionNumber", "language",
    "containsAnnotations", "indexInQuestion",
    "resolution.widthX", "resolution.heightY"), excel, "images")

  for (i in rownames(excel)) {
    images_directory <- file.path(instrument_directory,
      "images", excel[i, "questionNumber"])
    create_empty_directory(images_directory);

    # copy the image file
    question_image <- paste(input_directory, images_subdirectory,
       paste0("ins", instrument_number), excel[i, "fileName"],
        sep = .Platform$file.sep)
    if (!file.exists(question_image)) {
      stop("Could not copy file. Check whether you set the correct
        images_subdirectory")
    }
    file.copy(
      question_image,
      file.path(instrument_directory,
        "images", excel[i, "questionNumber"],
      paste0(
        excel[i, "questionNumber"], "_", excel[i, "indexInQuestion"], ".png")))
    image <- new.env(hash = TRUE, parent = emptyenv())
    image[["language"]] <- jsonlite::unbox(excel[i, "language"])
    image[["containsAnnotations"]] <- jsonlite::unbox(
      as.logical(excel[i, "containsAnnotations"])
    )
    image[["indexInQuestion"]] <- jsonlite::unbox(
      as.numeric(excel[i, "indexInQuestion"])
    )
    image[["resolution"]] <- new.resolution(
      excel[i, "resolution.widthX"],
      excel[i, "resolution.heightY"]
    )
    # json export
    question_image_json <- file.path(instrument_directory,
      "images", excel[i, "questionNumber"],
      paste0(
        excel[i, "questionNumber"], "_", excel[i, "indexInQuestion"], ".json"))
    con <- file(question_image_json, "w", encoding = "UTF-8")
    write(jsonlite::toJSON(nested_env_as_list(image),
      null = "null", na = "null",
      pretty = TRUE
    ), file = con)
    close(con)
  }
  message("Wrote question images: ", normalizePath(
    file.path(instrument_directory, "images")))
}

trim_cols <- function(x) {
  gsub("\\s+", "", x)
}

trim_list_cols <- function(excel, ...) {
  # delete all whitespaces
  cols <- list(...)
  for (i in 1:length(cols)) {
    excel[[cols[[i]]]] <- trim_cols(excel[[cols[[i]]]])
  }
  return(excel)
}

list_attribute <- function(str_attr) {
  attr <- unlist(strsplit(str_attr, "[,;[:space:]]+", fixed = FALSE))
  if (length(attr) > 1 || is.na(attr) == FALSE) {
    return(attr)
  }
}

new.i18n_string <- function(de = NULL, en = NULL) {
  i18n_string <- new.env(hash = TRUE, parent = emptyenv())
  i18n_string[["de"]] <- jsonlite::unbox(ifelse(length(de) != 0, de, NULL))
  i18n_string[["en"]] <- jsonlite::unbox(ifelse(length(en) != 0, en, NULL))
  return(i18n_string)
}

new.resolution <- function(widthX = NULL, heightY = NULL) {
  resolution <- new.env(hash = TRUE, parent = emptyenv())
  resolution[["widthX"]] <-
    jsonlite::unbox(ifelse(length(widthX) != 0, as.numeric(widthX), NULL))
  resolution[["heightY"]] <-
    jsonlite::unbox(ifelse(length(heightY) != 0, as.numeric(heightY), NULL))
  return(resolution)
}

new.technical_representation <- function(technicalRepresentation.type,
  technicalRepresentation.language,
  technicalRepresentation.source) {
  technical_representation <- new.env(hash = TRUE, parent = emptyenv())
  technical_representation[["type"]] <-
    jsonlite::unbox(technicalRepresentation.type)
  technical_representation[["language"]] <-
    jsonlite::unbox(technicalRepresentation.language)
  technical_representation[["source"]] <-
    jsonlite::unbox(technicalRepresentation.source)
  return(technical_representation)
}

nested_env_as_list <- function(env) {
  out <- as.list(env)
  lapply(out, function(x) if (is.environment(x) ||
    is.list(x)) nested_env_as_list(x) else x)
}

check_missing_columns <- function(expected_column_names, dataframe,
  sheet_name) {
  variable_is_in_df <- expected_column_names %in%
    names(dataframe)
  if (!all(variable_is_in_df)) {
    if (sum(!variable_is_in_df) == 1) {
      message(paste("There's no ",
        expected_column_names[!variable_is_in_df],
        " column in the ", sheet_name, " sheet."))
      stop()
      } else {
      if (sum(!variable_is_in_df) > 1) {
        message(paste("In the ", sheet_name,
          " sheet the following columns are missing:"))
        message(paste(expected_column_names[!variable_is_in_df],
          collapse = ", "))
        stop()
      }
    }
  }
}
