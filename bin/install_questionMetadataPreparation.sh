#!/usr/bin/env bash
Rscript -e "install.packages('remotes', dependencies = TRUE)"

Rscript -e "remotes::install_github('dzhw/questionMetadataPreparation', ref = 'master')"
