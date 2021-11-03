
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Build
Status](https://github.com/dzhw/questionMetadataPreparation/workflows/Build%20and%20Deploy/badge.svg)](https://github.com/dzhw/questionMetadataPreparation/actions)
[![Coverage
status](https://codecov.io/github/dzhw/questionMetadataPreparation/branch/master/graph/badge.svg)](https://codecov.io/github/dzhw/questionMetadataPreparation?branch=master)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Documentation](https://img.shields.io/badge/documentation--brightgreen)](https://github.com/dzhw/FDZ_Allgemein/wiki/Fragen-2.0)
<!-- badges: end -->

# [Question Metadata Preparation](https://dzhw.github.io/questionMetadataPreparation/)

This [R](https://www.r-project.org/about.html) package ([Question
Metadata
Preparation](https://dzhw.github.io/questionMetadataPreparation/)) helps
preparing question-metadata for the [MDM](https://metadata.fdz.dzhw.eu)
of the research data center of the DZHW. If you do not work for the
research data center of the DZHW, this package will probably be only
useful for learning purposes, as it is specifically designed to help
with our internal processes.

## Installation of development version

Developers need to setup the R devtools on their machine.

``` r
install.packages("devtools", dependencies = TRUE)
devtools::install_github("dzhw/questionMetadataPreparation")
```

After setting up devtools you can install all required R packages with

``` bash
R -e 'devtools::install_deps(dep = T)'
```

During development you should start an R session in the project root in
order to run:

``` r
devtools::load_all() # loads the project from the current directory
devtools::install() # installs the project from the current directory
devtools::test() # runs the test_that tests
```

You can build the package on your local machine with

``` bash
R CMD build .
```

Before pushing to Github (and thus kicking of CI) you should run

``` bash
R CMD check *tar.gz
```

# Deployment

The scripts under `/bin` must be copied to
`//faust/abt4/FDZ/4_Datenaufnahme/_Organisation/_Vorlage_Ordnerstruktur/DAP-id/vX.X.X/4_Dokumentation/questions`.
New releases can be installed by the users by executing
`/bin/install_questionMetadataPreparation.bat`which installs the latest
commit on `master`.

# Usefull links, further documentation

  - [Further
    Documentation](https://github.com/dzhw/FDZ_Allgemein/wiki/Fragen-(Questions))

## Having trouble?

Please file an issue in our [issue
tracker](https://github.com/dzhw/metadatamanagement/issues)
