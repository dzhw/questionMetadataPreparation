#' Question-metadata json to excel
#'
#' This script generates a question metadata excel file from question metadata
#' json files.
#'
#' @param in_path Input path, e.g. "./input/questions/ins1"
#' @param out_file Path to the excel file to be generated, e.g.
#'   "./output/projectname.xlsx"
#' @export


question_json_to_excel <- function(in_path, out_file){
files <- dir(in_path, pattern = "*.json")

excel <- data.frame(matrix(ncol = 21, nrow = length(files)))

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
  "annotations.en")

colnames(excel) <- col_names

for (i in 1:length(files)) {
  json <- jsonlite::fromJSON(paste0(in_path, "/", files[[i]]))

  excel$indexInInstrument[i] <- json$indexInInstrument
  excel$questionNumber[i] <- json$number
  excel$instrumentNumber[i] <- json$instrumentNumber
  excel$successorNumbers[i] <- ifelse(length(json$successorNumbers) == 0,
    NA_character_, paste0(json$successorNumbers, collapse = ","))
  excel$questionText.de[i] <- ifelse(length(json$questionText$de) == 0,
    NA_character_, json$questionText$de)
  excel$questionText.en[i] <- ifelse(length(json$questionText$en) == 0,
    NA_character_, json$questionText$en)
  excel$instruction.de[i] <- ifelse(length(json$instruction$de) == 0,
    NA_character_, json$instruction$de)
  excel$instruction.en[i] <- ifelse(length(json$instruction$en) == 0,
    NA_character_, json$instruction$en)
  excel$introduction.de[i] <- ifelse(length(json$introduction$de) == 0,
    NA_character_, json$introduction$de)
  excel$introduction.en[i] <- ifelse(length(json$introduction$en) == 0,
    NA_character_, json$introduction$en)
  excel$type.de[i] <- json$type$de
  excel$type.en[i] <- json$type$en
  excel$topic.de[i] <- ifelse(length(json$topic$de) == 0,
    NA_character_, json$topic$de)
  excel$topic.en[i] <- ifelse(length(json$topic$en) == 0,
    NA_character_, json$topic$en)
  excel$technicalRepresentation.type[i] <- json$technicalRepresentation$type
  excel$technicalRepresentation.language[i] <- json$technicalRepresentation$language #nolint
  excel$technicalRepresentation.source[i] <- json$technicalRepresentation$source
  excel$additionalQuestionText.de[i] <- ifelse(
    length(json$additionalQuestionText$de) == 0,
    NA_character_, json$additionalQuestionText$de)
  excel$additionalQuestionText.en[i] <- ifelse(
    length(json$additionalQuestionText$en) == 0,
    NA_character_, json$additionalQuestionText$en)
  excel$annotations.de[i] <- ifelse(
    length(json$annotations$de) == 0,
    NA_character_, json$annotations$de)
  excel$annotations.en[i] <- ifelse(
    length(json$annotations$en) == 0,
    NA_character_, json$annotations$en)
}

# sort by indexInInstrument
sorted_excel <- excel[order(excel$indexInInstrument),]

openxlsx::write.xlsx(sorted_excel, file = out_file, sheetName = "questions",
  row.names = FALSE, showNA = FALSE)
}
