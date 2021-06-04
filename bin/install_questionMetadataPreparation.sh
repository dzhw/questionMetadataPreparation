#!/usr/bin/env bash
Rscript -e "install.packages('remotes', repo = 'https://ftp.gwdg.de/pub/misc/cran/', dependencies = TRUE)"

Rscript -e "remotes::install_github('dzhw/questionMetadataPreparation', ref = 'master')"
