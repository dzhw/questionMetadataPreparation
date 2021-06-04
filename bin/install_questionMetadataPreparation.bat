@echo off

:: install remotes and the package itself
Rscript -e "install.packages('remotes', dependencies = TRUE)"

Rscript -e "remotes::install_github('dzhw/questionMetadataPreparation', ref = 'master')"

PAUSE
