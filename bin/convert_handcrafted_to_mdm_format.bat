@echo off

:: run the function in the current directory (should be "02_handcrafted-format")
Rscript -e "questionMetadataPreparation::convert_handcrafted_questionnaires_to_mdm_format()"

PAUSE
