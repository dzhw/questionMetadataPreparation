#####  QUESTIONS: EXCEL TO JSON #####

# Dieses Skript ?berf?hrt Angaben aus den Fragetabelle des jeweiligen Projektes in question jsons (eins pro Fragenummer) und
# image jsons (eins pro Bild) und legt die zum Upload ins Metadatenmanagement-System ben?tigte Ordnerstruktur an.
# Eingelesen wird die Exceltabelle "projektID.xlsx" mit den beiden Tabellenbl?ttern "question" und "images".
# Angepasst werden muss der Pfad, an dem die Exceltabelle liegt und der Pfad an dem die jsons gespeichert
# werden sollen.



project <- "scs2016"


# Import path (path to excel file)
pathXlsxFile <- paste0("../Projekte/",project,"/questions/",project,".xlsx")

#Export path (path to json files)
pathJson <- paste0("../Projekte/",project,"/questions","/out")



source("R/question-generation_main.R")


