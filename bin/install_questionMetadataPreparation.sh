#!/usr/bin/env bash
R -e "install.packages('remotes', dependencies = TRUE)"

R -e "remotes::install_github('dzhw/questionMetadataPreparation')"
