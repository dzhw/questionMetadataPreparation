# install packges if not installed already
list.of.packages <- c("jsonlite", "xlsx", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

suppressPackageStartupMessages(suppressWarnings(library(jsonlite)))
suppressPackageStartupMessages(suppressWarnings(library(xlsx)))
suppressPackageStartupMessages(suppressWarnings(library(stringr)))



source("R/utils/question-generation_functions.R")

# read excel file - sheet questions
cat("Read excel file: sheet questions\n")

excel <- read_and_trim_excel(pathXlsxFile, sheet = "questions")
excel <- trim_list_cols(excel, col1 = "successorNumbers")

# Export directories
# -> create new directories for json export if don't exist (z.B. .../json/ins1)
# -> or delete old jsons from directories if exist
create_export_directories(pathJson, paste0("ins",unique(excel[["instrumentNumber"]])))
# and images folder if not existing
create_export_directories(pathJson, paste0("ins",unique(excel[["instrumentNumber"]]),"/images"))

# for all questions in excel
for (i in 1:nrow(excel)) {


  que <- new.env(hash = TRUE, parent = emptyenv())

  que[["indexInInstrument"]] <- unbox(as.numeric(excel[i,"indexInInstrument"]))
  que[["questionText"]] <- new.i18n_string(excel[i,"questionText.de"], excel[i,"questionText.en"])
  que[["instruction"]] <- new.i18n_string(excel[i,"instruction.de"], excel[i,"instruction.en"])
  que[["introduction"]] <- new.i18n_string(excel[i,"introduction.de"], excel[i,"introduction.en"])
  que[["type"]] <- new.i18n_string(excel[i,"type.de"], excel[i,"type.en"])
  que[["additionalQuestionText"]] <- new.i18n_string(excel[i,"additionalQuestionText.de"], excel[i,"additionalQuestionText.en"])
  que[["topic"]] <- new.i18n_string(excel[i,"topic.de"], excel[i,"topic.en"])
  que[["annotations"]] <- new.i18n_string(excel[i,"annotations.de"], excel[i,"annotations.en"])
  que[["successorNumbers"]] <- list_attribute(excel[i,"successorNumbers"])
  que[["technicalRespresentation"]] <- new.technical_representation(excel[i,"technicalRepresentation.type"],
                                                                    excel[i,"technicalRepresentation.language"],
                                                                    excel[i,"technicalRepresentation.source"])

  # json export
  # nolint start
  # browser()
  con <- file(description = paste0(pathJson,
                                   "/ins",
                                   excel[i,"instrumentNumber"],
                                   "/",
                                   excel[i,"questionNumber"],
                                   ".json"),
              open = "w",
              encoding = "UTF-8")
  write(toJSON(nested_env_as_list(que), null = "null", na = "null", pretty = TRUE),
        file = con)
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
  # -> create new directories for json export if don't exist (z.B. questions/ins1/images/1.1)
  # -> or delete old jsons from directories if exist
  create_export_directories(paste0(pathJson,"/ins",questionExcel[i,"instrumentNumber"],"/images"), questionExcel[i,"questionNumber"])

  que <- new.env(hash = TRUE, parent = emptyenv())

  que[["language"]] <- unbox(questionExcel[i,"language"])
  que[["containsAnnotations"]] <- unbox(as.logical(questionExcel[i,"containsAnnotations"]))
  que[["indexInQuestion"]] <- unbox(as.numeric(questionExcel[i,"indexInQuestion"]))

  # json export
  # nolint start
  con <- file(paste0(pathJson,"/ins",questionExcel[i,"instrumentNumber"],"/images/", questionExcel[i,"questionNumber"],"/",
                     sub(".png","",questionExcel[i,"fileName"]),".json"), "w", encoding = "UTF-8")
  write(toJSON(nested_env_as_list(que), null = "null", na = "null", pretty = TRUE),
        file = con)
  close(con)
  # nolint end
}
cat("Finished writing image jsons\n")











