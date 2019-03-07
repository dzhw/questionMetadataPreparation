#### SORT IMAGES IN DIRECTORY STRUCTURE ####

# Dieses Skript unterstützt bei der Einsortierung der Fragenbilder (pngs) in die Ordnerstruktur, 
# die für den Upload ins Metadatensystem benötigt wird.
# Das Skript ist auszuführen, nachdem die json Dateien erzeugt wurden und die Ordnerstruktur angelegt wurde.
# Angepasst werden muss der Pfad an dem die Bilder liegen, der Pfad an dem die Bilder gespeichert werden
# sollen und der Pfad an dem die Question Excel Tabelle liegt (+ evtl. Projektname).
# Das Tabellenblatt "images" der Exceltabelle muss vorher ausgefüllt werden. Dann ist es auch möglich, wenn mehrere Bilder pro Frage vorliegen,
# diese richtig einzusortieren.


# Ordnerstruktur der Bilder vor dem Einsortieren: 
# |--ins1
#     |--1.1_1.png
#     |--1.1_2.png
# |--ins2
#     |--1.1.png
#     |--1.2.png
#
#
# Ordnerstruktur nach dem Einsortieren:   
# |--ins1
#     |--images
#         |--1.1
#             |--1.1_1.png
#             |--1.1_2.png
#     |--1.1.json
#     |--1.2.json
#
# |--ins2
#     |--images
#         |--1.1
#             |--1.1.png
#         |--1.2
#             |--1.2.png
#     |--1.1.json
#     |--1.2.json


project <- "ssy21"

# path to images (this directory contains the folders "ins1", "ins2",...)
pathInImages <- paste0("../Projekte/", project, "/questions/Bilder/png")

# path to jsons (this directory contains the folders "ins1", "ins2", ...)
pathOutImages <-  paste0("../Projekte/", project, "/questions/out")

# path to excel file
pathXlsxFile <- paste0("../Projekte/",project,"/questions/",project,".xlsx")







# sort images -------------------------------------------------------------

# install packges if not installed already
list.of.packages <- "png"
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

suppressPackageStartupMessages(suppressWarnings(library(png)))


source("R/utils/question-generation_functions.R")

# read excel file - sheet images
cat("Read excel file: sheet images\n")
excel <- read_and_trim_excel(pathXlsxFile, sheet = "images")



for (instrument in dir(pathInImages)) {
  pathPng <- paste0(pathInImages,"/",instrument)
  images <- dir(pathPng, ".png")
  for (image in images) {
    pngImage <- readPNG(paste0(pathPng,"/",image))
    # find image name in excel and get questionNumber
    questionNumber <- excel[excel$instrumentNumber == sub("ins","", instrument) & excel$fileName == image,"questionNumber"]
    
    writePNG(pngImage, paste0(pathOutImages,"/",instrument,"/images/", questionNumber,"/", image))
  }
}