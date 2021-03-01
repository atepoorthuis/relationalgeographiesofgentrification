
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Changing neighborhoods, shifting connections: mapping relational geographies of gentrification using social media data

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/atepoorthuis/relationalgeographiesofgentrification/master?urlpath=rstudio)
[![DOI](https://zenodo.org/badge/283771059.svg)](https://zenodo.org/badge/latestdoi/283771059)

This repository contains the data and code for our paper “Changing
neighborhoods, shifting connections: mapping relational geographies of
gentrification using social media data”. The structure of this folder
has been created with [rrtools](https://github.com/benmarwick/rrtools).

This repository contains the data and code for our paper:

> Poorthuis, A, T. Shelton and M. Zook, (2021). *Changing neighborhoods,
> shifting connections: mapping relational geographies of gentrification
> using social media data.*. Urban Geography. Online at
> <http://dx.doi.org/10.1080/02723638.2021.1888016>

Our pre-print is online here:

> Poorthuis, A, T. Shelton and M. Zook, (2021). *Changing neighborhoods,
> shifting connections: mapping relational geographies of gentrification
> using social media data.*. Urban Geography, Accessed 01 Mar 2021.
> Online at <https://osf.io/preprints/socarxiv/eq6vm/>

## Contents

This repository contains all the data and code needed to reproduce the
results and figures in our paper.

-   [analysis/01-prepare-twitter-data.Rmd](analysis/01-prepare-twitter-data.md):
    Prepare the Twitter data for subsequent analysis. Because of
    Twitter’s terms and condition, we cannot share the raw Twitter data.
    However, all aggregated data necessary for reproducing the work in
    this paper is included.
-   [analysis/02-figures.Rmd](analysis/02-figures.md): This contains all
    the analytical and visualization steps needed to reproduce each
    figure in our analysis.
-   [figures/](figures/): Contains a high-resolution version of the
    figures in the paper. Unfortunately, the published version in *Urban
    Geography* contains only a down-sampled low-resolution version,
    which can make some of the figures difficult to read.

## How to download or install

You can download the compendium as a zip from from [this
URL](https://github.com/atepoorthuis/relationalgeographiesofgentrification/archive/master.zip)

Or you can install this compendium as an R package,
relationalgeographiesofgentrification, from GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("atepoorthuis/relationalgeographiesofgentrification")
```

## Licenses

Text + figures and data:
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

Code: See the [DESCRIPTION](DESCRIPTION) file
