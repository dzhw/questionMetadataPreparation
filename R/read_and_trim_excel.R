#' Read excel files and get rid of unwanted whitespace
#'
#' @param path the path to the excel file
#' @param sheet the sheet that should be edited
#' @keywords internal
read_and_trim_excel <- function(path, sheet) {
  del_NA_cols_and_rows <- function(excel) {
    NAcol <- stringr::str_detect(colnames(excel), "NA.")
    NArow <- apply(excel, 1, function(x) {
      all(is.na(x))
    })
    return(excel[!NArow, !NAcol])
  }
  excel <- readxl::read_excel(path, sheet = sheet, col_types = "text")
  excel <- data.frame(sapply(excel, trim_multiple_leading_trailing_ws),
    stringsAsFactors = FALSE
  )
  excel <- del_NA_cols_and_rows(excel)
  return(excel)
}
