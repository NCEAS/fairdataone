
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fairdataone

This repository contains the data and code for our paper:

> Authors, (YYYY). *Title of your paper goes here*. Name of journal/book
> <https://doi.org/xxx/xxx>

Our pre-print is online here:

> Authors, (YYYY). *Title of your paper goes here*. Name of
> journal/book, Accessed 17 Jan 2021. Online at
> <https://doi.org/xxx/xxx>

### How to cite

Please cite this compendium as:

> Authors, (2021). *Compendium of R code and data for Title of your
> paper goes here*. Accessed 17 Jan 2021. Online at
> <https://doi.org/xxx/xxx>

## Contents

  - [:file\_folder: manuscript](/manuscript): R Markdown source document
    for manuscript. Includes code to reproduce the figures and tables
    generated by the analysis. It also has a rendered version,
    `manuscript.pdf`, suitable for reading (the code is replaced by
    figures and tables in this file)
  - [:file\_folder: data](/manuscript/data): Data used in the analysis.
    Most data are retrieved from a data archive, but small static data
    files may also be retrieved from this directory.
  - [:file\_folder: figures](/manuscript/figures): Plots and other
    illustrations.

## How to run run locally

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

After downloading the compendium from GitHub:

  - open the `.Rproj` file in RStudio
  - run `devtools::install()` to ensure you have the packages this
    analysis depends on (also listed in the [DESCRIPTION](/DESCRIPTION)
    file). This also installs the `fairdataone` package, which is
    necessary to Knit the manuscript.
  - finally, open `manuscript/manuscript.Rmd` and knit to produce the
    `manuscript.pdf`, or run
    `rmarkdown::render("manuscript/manuscript.Rmd")` in the R console.

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse
