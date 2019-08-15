
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/dzhw/questionMetadataPreparation.svg?branch=master)](https://travis-ci.org/dzhw/questionMetadataPreparation)
[![Coverage
status](https://codecov.io/github/dzhw/questionMetadataPreparation/branch/master/graph/badge.svg)](https://codecov.io/github/dzhw/questionMetadataPreparation?branch=master)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Documentation](https://img.shields.io/badge/documentation--brightgreen)](https://dzhw.github.io/questionMetadataPreparation/)
<!-- badges: end -->

# [Question Metadata Preparation](https://dzhw.github.io/questionMetadataPreparation/)

This [R](https://www.r-project.org/about.html) package ([Question
Metadata
Preparation](https://dzhw.github.io/questionMetadataPreparation/)) helps
preparing question-metadata for the [MDM](https://metadata.fdz.dzhw.eu)
of the research data center of the dzhw. If you do not work for the
research data center of the dzhw, this package will probably be only
useful for learning purposes, as it is specifically designed to help
with our internal processes.

## Installation for Users

You can install the released version of questionMetadataPreparation from
[Github](https://github.com/dzhw/questionMetadataPreparation) within
your [R](https://www.r-project.org/about.html) session:

``` r
install.packages("remotes", dependencies = TRUE)
remotes::install_github("dzhw/questionMetadataPreparation")
```

In order to convert a Zofar export into a format which can be manually
edited, you have to run:

``` r
convert_zofar_export_to_handcrafted_questionnaire("./questions/ins1")
```

The output will be written to `"./handcrafted/questions"`.

A set of handcrafted questionnaires can be manually converted into the
MDM format by running

``` r
convert_handcrafted_questionnaires_to_mdm_format("./questions")
```

The output will be written to `"./mdm/questions"`.

## Development

Developers need to setup the R devtools on their machine.

``` r
install.packages("devtools", dependencies = TRUE)
devtools::install_github("dzhw/questionMetadataPreparation")
```

After setting up devtools you can install all required R packages with

``` bash
R -e 'devtools::install_deps(dep = T)'
```

You can build the package on you local machine with

``` bash
R CMD build .
```

Before pushing to Github (and thus kicking of CI) you should run

``` bash
R CMD check *tar.gz
```

## Having trouble?

Please file an issue in our [issue
tracker](https://github.com/dzhw/metadatamanagement/issues)
