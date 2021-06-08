@echo off
:: remember network path to workaround UNC warning
set batch_path=%~dp0
:: replace backslash with slash
set batch_path=%batch_path:\=/%
:: run the function in the current directory (should be "02_handcrafted-format")
Rscript -e "setwd('%batch_path%')" -e "questionMetadataPreparation::convert_handcrafted_questionnaires_to_mdm_format()"
PAUSE
