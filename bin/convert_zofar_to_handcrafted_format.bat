@echo off

:: run the function in the current directory (should be "01_zofar-format")
Rscript -e "questionMetadataPreparation::convert_zofar_export_to_handcrafted_questionnaire()"

PAUSE
