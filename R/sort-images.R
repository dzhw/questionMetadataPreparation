#' SORT IMAGES IN DIRECTORY STRUCTURE
#'
#' Dieses Skript unterstützt bei der Einsortierung der Fragenbilder (pngs) in die Ordnerstruktur,
#' die für den Upload ins Metadatensystem benötigt wird.
#' Das Skript ist auszuführen, nachdem die json Dateien erzeugt wurden und die Ordnerstruktur angelegt wurde.
#' Angepasst werden muss der Pfad an dem die Bilder liegen, der Pfad an dem die Bilder gespeichert werden
#' sollen und der Pfad an dem die Question Excel Tabelle liegt (+ evtl. Projektname).
#' Das Tabellenblatt "images" der Exceltabelle muss vorher ausgefüllt werden. Dann ist es auch m?glich, wenn mehrere Bilder pro Frage vorliegen,
#' diese richtig einzusortieren.
#' Ordnerstruktur der Bilder vor dem Einsortieren:
#' |--ins1
#'     |--1.1_1.png
#'     |--1.1_2.png
#' |--ins2
#'     |--1.1.png
#'     |--1.2.png
#' Ordnerstruktur nach dem Einsortieren:
#' |--ins1
#'     |--images
#'         |--1.1
#'             |--1.1_1.png
#'             |--1.1_2.png
#'     |--1.1.json
#'     |--1.2.json
#'
#' |--ins2
#'     |--images
#'         |--1.1
#'             |--1.1.png
#'         |--1.2
#'             |--1.2.png
#'     |--1.1.json
#'     |--1.2.json
#'
#' @param pathInImages path containing folders ins1 ins2 etc.
#' @param pathOutImages path to newly generated json files
#' @param pathXlsxFile path to the excel file
#' @export





sort_images <- function(pathInImages, pathOutImages, pathXlsxFile){
  # sort images -------------------------------------------------------------


  # read excel file - sheet images
  cat("Read excel file: sheet images\n")
  excel <- read_and_trim_excel(pathXlsxFile, sheet = "images")


  for (instrument in dir(pathInImages)) {
    pathPng <- paste0(pathInImages,"/",instrument)
    images <- dir(pathPng, ".png")
    dir.create(paste0(pathOutImages, "/", instrument, "/images"),
      recursive = TRUE)
    for (image in images) {
      pngImage <- png::readPNG(paste0(pathPng,"/",image))
      # find image name in excel and get questionNumber
      questionNumber <- excel[excel$instrumentNumber == sub("ins","",
        instrument) & excel$fileName == image,"questionNumber"]
      dir.create(paste0(pathOutImages, "/", instrument, "/images/",
        questionNumber), recursive = TRUE)
      png::writePNG(pngImage, paste0(pathOutImages,"/",instrument,"/images/",
        questionNumber,"/", image))
    }
  }
}
