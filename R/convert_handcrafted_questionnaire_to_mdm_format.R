#' Convert handcrafted questionnaires to MDM format
#'
#' This script converts the handcrafted questionnaires which contains an excel
#' and many images of several questionnaires into the format which can be
#' uploaded to the [MDM](https://metadata.fdz.dzhw.eu). The handcrafted
#' questionnaire must have the following format:
#' \preformatted{
#' |--questions
#'   |--questions*.xlsx (two sheets, questions and images)
#'   |--Bilder
#'     |--png
#'       |--ins1
#'         |--5_1.png (must match the filename in the images excel sheet)
#'         |--5_2.png (must match the filename in the images excel sheet)
#'       |--ins2
#'         |--5_1.png (must match the filename in the images excel sheet)
#'         |--5_2.png (must match the filename in the images excel sheet)
#' }
#' @param input_directory Input path, e.g. "./questions"
#' @param output_directory Output directory, e.g. "./mdm/questions", will
#' be created if it does not exist or will be overwritten otherwise
#' @param images_subdirectory Path relative to input_directory containing the
#' images, e.g. "Bilder/png"
#' @example convert_handcrafted_questionnaires_to_mdm_format("./questions")
#' @export
convert_handcrafted_questionnaires_to_mdm_format <- function(
  input_directory = file.path(".", "questions"),
  output_directory = file.path(".", "mdm", "questions"),
  images_subdirectory = file.path("Bilder", "png")) {
  input_directory <- remove_trailing_directory_delimiter(input_directory)
  output_directory <- remove_trailing_directory_delimiter(output_directory)
  images_subdirectory <- remove_trailing_directory_delimiter(
    images_subdirectory)
  create_empty_directory(output_directory)

  xlsx_file <- dir(input_directory, pattern = "*.xlsx", full.names = TRUE)[[1]]

  write_question_jsons(xlsx_file, output_directory)

  write_question_images(xlsx_file, input_directory, output_directory,
    images_subdirectory)
}

write_question_jsons <- function(xlsx_file, output_directory) {
  message("Read excel file: sheet questions")
  excel <- read_and_trim_excel(xlsx_file, sheet = "questions")
  # trim later
  col_is_missing_error_message(c("instruction.de", "instruction.en",
    "introduction.de", "introduction.en",
    "type.de", "type.en", "topic.de", "topic.en",
    "technicalRepresentation.type", "technicalRepresentation.language",
    "technicalRepresentation.source", "annotations.de", "annotations.en",
    "additionalQuestionText.de", "additionalQuestionText.en", "conceptIds"),
    "questions", excel)
  excel <- trim_list_cols(excel, col1 = "successorNumbers", col2 = "conceptIds")

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

    instrument_directory <- file.path(output_directory,
      paste0("ins", excel[i, "instrumentNumber"]))
    if (!dir.exists(instrument_directory)) {
      dir.create(instrument_directory, recursive = TRUE)
    }
    # json export
    question_json <- file.path(
      instrument_directory,
      paste0(excel[i, "questionNumber"], ".json")
    )
    message("Write question json:", question_json)
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
  message("Finished writing question jsons\n")
}

write_question_images <- function(xlsx_file, input_directory,
  output_directory, images_subdirectory) {
  message("Read excel file: sheet images\n")
  excel <- read_and_trim_excel(xlsx_file, sheet = "images")

  # for all images in excel
  col_is_missing_error_message(c("fileName", "questionNumber",
    "instrumentNumber", "language", "containsAnnotations", "indexInQuestion",
    "resolution.widthX", "resolution.heightY"), "images", excel)

  for (i in rownames(excel)) {
    images_directory <- file.path(output_directory,
      paste0("ins", excel[i, "instrumentNumber"]),
      "images", excel[i, "questionNumber"])
    if (!dir.exists(images_directory)) {
      dir.create(images_directory, recursive = TRUE)
    }

    # copy the image file
    question_image <- paste(input_directory, images_subdirectory,
       paste0("ins", excel[i, "instrumentNumber"]), excel[i, "fileName"],
        sep = .Platform$file.sep)
    message("Copying image file:", question_image)
    if (!file.exists(question_image)) {
      stop("Could not copy file. Check whether you set the correct
        images_subdirectory")
    }
    file.copy(
      question_image,
      file.path(output_directory,
        paste0("ins", excel[i, "instrumentNumber"]),
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
    question_image_json <- file.path(output_directory,
      paste0("ins", excel[i, "instrumentNumber"]),
      "images", excel[i, "questionNumber"],
      paste0(
        excel[i, "questionNumber"], "_", excel[i, "indexInQuestion"], ".json"))
    message("Write question image json:", question_image_json)
    con <- file(question_image_json, "w", encoding = "UTF-8")
    write(jsonlite::toJSON(nested_env_as_list(image),
      null = "null", na = "null",
      pretty = TRUE
    ), file = con)
    close(con)
  }
  message("Finished writing image files")
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
  attr <- unlist(strsplit(str_attr, ",", fixed = TRUE))
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

col_is_missing_error_message <- function(col_name_in_excel_sheet, sheet_name,
  check_in_this_dataframe) {
  variable_is_in_df <- col_name_in_excel_sheet %in%
    names(check_in_this_dataframe)
  if (!all(variable_is_in_df)) {
    if (sum(!variable_is_in_df) == 1) {
      message(paste("There's no ",
        col_name_in_excel_sheet[!variable_is_in_df],
        " column in the ", sheet_name, " sheet."))
      stop()
      } else {
      if (sum(!variable_is_in_df) > 1) {
        message(paste("In the ", sheet_name,
          " sheet the following columns are missing:"))
        message(paste(col_name_in_excel_sheet[!variable_is_in_df],
          collapse = ", "))
        stop()
      }
    }
  }
}

