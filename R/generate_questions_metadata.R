#' QUESTIONS: EXCEL TO JSON
#' Dieses Skript überführt Angaben aus den Fragetabelle des jeweiligen Projektes
#' in question jsons (eins pro Fragenummer) und
#' image jsons (eins pro Bild) und legt die zum Upload ins
#' Metadatenmanagement-System benötigte Ordnerstruktur an.
#' Eingelesen wird die Exceltabelle "projektID.xlsx" mit den beiden
#' Tabellenblättern "question" und "images".
#' Angepasst werden muss der Pfad, an dem die Exceltabelle liegt und der Pfad an
#' dem die jsons gespeichert werden sollen.
#'
#' @param projects_root the root folder where all your projects reside
#' @param project_name the name of your project
#' @export

generate_questions_metadata <- function(projects_root, project_name) {

  generate_question_jsons <- function(pathXlsxFile, pathJson) {

    # read excel file - sheet questions
    cat("Read excel file: sheet questions\n")

    excel <- read_and_trim_excel(pathXlsxFile, sheet = "questions")
    excel <- trim_list_cols(excel, col1 = "successorNumbers")

    # Export directories
    # -> create new directories for json export if don't exist (e.g. .../json/ins1)
    # -> or delete old jsons from directories if exist
    create_export_directories(pathJson, paste0("ins", unique(excel[["instrumentNumber"]])))
    # and images folder if not existing
    create_export_directories(pathJson, paste0("ins", unique(excel[["instrumentNumber"]]), "/images"))

    # for all questions in excel
    for (i in 1:nrow(excel)) {
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

      # json export
      # nolint start
      con <- file(
        description = paste0(
          pathJson, "/ins",
          excel[i, "instrumentNumber"], "/", excel[i, "questionNumber"], ".json"
        ),
        open = "w", encoding = "UTF-8"
      )

      write(jsonlite::toJSON(nested_env_as_list(que),
        null = "null", na = "null",
        pretty = TRUE
      ), file = con)
      close(con)
      # nolint end
    }
    cat("Finished writing question jsons\n")

    ###
    ### create image jsons (one json per image)

    # read excel file - sheet images
    cat("Read excel file: sheet images\n")
    questionExcel <- read_and_trim_excel(pathXlsxFile, sheet = "images")

    # for all questions in excel
    for (i in 1:nrow(questionExcel)) {
      # Export directories
      # -> create new directories for json export if don't exist (e.g.
      # questions/ins1/images/1.1)
      # -> or delete old jsons from directories if exist
      create_export_directories(
        paste0(
          pathJson, "/ins",
          questionExcel[i, "instrumentNumber"], "/images"
        ),
        questionExcel[i, "questionNumber"]
      )

      que <- new.env(hash = TRUE, parent = emptyenv())

      que[["language"]] <- jsonlite::unbox(questionExcel[i, "language"])
      que[["containsAnnotations"]] <- jsonlite::unbox(
        as.logical(questionExcel[i, "containsAnnotations"])
      )
      que[["indexInQuestion"]] <- jsonlite::unbox(
        as.numeric(questionExcel[i, "indexInQuestion"])
      )

      # json export
      # nolint start
      con <- file(paste0(
        pathJson, "/ins",
        questionExcel[i, "instrumentNumber"], "/images/",
        questionExcel[i, "questionNumber"], "/",
        sub(".png", "", questionExcel[i, "fileName"]), ".json"
      ), "w",
        encoding = "UTF-8"
      )
      write(jsonlite::toJSON(nested_env_as_list(que),
        null = "null", na = "null",
        pretty = TRUE
      ), file = con)
      close(con)
      # nolint end
    }
    cat("Finished writing image jsons\n")
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
    i18n_string[["de"]] <- jsonlite::unbox(de)
    i18n_string[["en"]] <- jsonlite::unbox(en)
    return(i18n_string)
  }




  new.technical_representation <- function(technicalRepresentation.type,
    technicalRepresentation.language,
    technicalRepresentation.source) {
    technical_representation <- new.env(hash = TRUE, parent = emptyenv())
    technical_representation[["type"]] <- jsonlite::unbox(technicalRepresentation.type)
    technical_representation[["language"]] <- jsonlite::unbox(technicalRepresentation.language)
    technical_representation[["source"]] <- jsonlite::unbox(technicalRepresentation.source)
    return(technical_representation)
  }


  nested_env_as_list <- function(env) {
    out <- as.list(env)
    lapply(out, function(x) if (is.environment(x) || is.list(x)) nested_env_as_list(x) else x)
  }



  create_export_directories <- function(path, folders) {
    for (i in folders) {
      if (!dir.exists(file.path(paste0(path, "/", i)))) {
        dir.create(file.path(paste0(path, "/", i)))
      }
      # else {
      #   if (length(list.files(paste0(path,"/",i), pattern = "*.json")) > 0) {
      #     do.call(file.remove,as.list(list.files(paste0(path,"/",i), pattern = "*.json", full.names = TRUE)))   # delete old jsons
      #   }
      # }
    }
  }


  # Import path (path to excel file)
  pathXlsxFile <- paste0(
    projects_root, "/", project_name, "/questions/",
    project_name, ".xlsx"
  )

  # Export path (path to json files)
  pathJson <- paste0(projects_root, "/", project_name, "/questions", "/out")
  generate_question_jsons(pathXlsxFile, pathJson)
}
