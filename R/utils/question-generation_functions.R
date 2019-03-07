
read_and_trim_excel <<- function(path, sheet){
  # read excel
  excel <- read.xlsx(path, sheetName = sheet, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = "character")
  # remove multiple, leading and trailing whitespaces
  excel <- data.frame(sapply(excel, trim), stringsAsFactors = FALSE)
  # delete NA rows and columns
  excel <- del_NA_cols_and_rows(excel)
  return(excel)
}

trim <<- function(x) str_replace(gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl = TRUE), "^$", NA_character_)

del_NA_cols_and_rows <<- function(excel){
  NAcol <- str_detect(colnames(excel), "NA.")
  NArow <- apply(excel, 1, function(x){all(is.na(x))})
  return(excel[!NArow,!NAcol])
}

trimCols <<- function(x) gsub("\\s+", "", x)

trim_list_cols <<- function(excel, ...){
  # delete all whitespaces
  cols <- list(...)
  for (i in 1:length(cols)){
    excel[[cols[[i]]]] <-trimCols(excel[[cols[[i]]]])
  }
  return(excel)
}


list_attribute <<- function(str_attr){
  attr <- unlist(strsplit(str_attr,",", fixed = TRUE))
  if (length(attr) > 1 || is.na(attr) == FALSE){
    return(attr)
  }
}

new.i18n_string <<- function(de = NULL, en = NULL) {
  i18n_string <- new.env(hash = TRUE, parent = emptyenv())
  i18n_string[["de"]] <- unbox(de)
  i18n_string[["en"]] <- unbox(en)
  return(i18n_string)
}




new.technical_representation <<- function(technicalRepresentation.type,
                                          technicalRepresentation.language,
                                          technicalRepresentation.source){
  
  technical_representation <- new.env(hash = TRUE, parent = emptyenv())
  technical_representation[["type"]] <- unbox(technicalRepresentation.type)
  technical_representation[["language"]] <- unbox(technicalRepresentation.language)
  technical_representation[["source"]] <- unbox(technicalRepresentation.source)
  return(technical_representation)
}
  

nested_env_as_list <<- function(env) {
  out <- as.list(env)
  lapply(out, function(x) if (is.environment(x) || is.list(x)) nested_env_as_list(x) else x)
}
  


create_export_directories <<- function(path, folders){
  for (i in folders) {
    if (!dir.exists(file.path(paste0(path,"/",i)))) {  
      dir.create(file.path(paste0(path,"/",i)))           
    }
    # else {
    #   if (length(list.files(paste0(path,"/",i), pattern = "*.json")) > 0) {
    #     do.call(file.remove,as.list(list.files(paste0(path,"/",i), pattern = "*.json", full.names = TRUE)))   # delete old jsons
    #   }
    # }
  }
}
  
  
