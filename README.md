<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/pkgmatch/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/pkgmatch/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropensci-review-tools/pkgmatch/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci-review-tools/pkgmatch)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

# pkgmatch

A tool to help find R packages, by matching packages either to a text
description, or to any given package. Can find matching packages either
from rOpenSci’s [suite of packages](https://ropensci.org/packages), or
from all packages currently on [CRAN](https://cran.r-project.org).

## Installation

This package relies on a locally-running instance of
[ollama](https://ollama.com). Procedures for setting that up are
described in a separate vignette. ollama needs to be installed before
this package can be used.

Once ollama is running, the easiest way to install this package is via
the [associated
`r-universe`](https://ropensci-review-tools.r-universe.dev/ui#builds).
As shown there, simply enable the universe with

``` r
options (repos = c (
    ropenscireviewtools = "https://ropensci-review-tools.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
))
```

And then install the usual way with,

``` r
install.packages ("pkgmatch")
```

Alternatively, the package can be installed by first installing either
the [remotes](https://remotes.r-lib.org) or
[pak](https://pak.r-lib.org/) packages and running one of the following
lines:

``` r
remotes::install_github ("ropensci-review-tools/pkgmatch")
pak::pkg_install ("ropensci-review-tools/pkgmatch")
```

The package can then loaded for use with

``` r
library (pkgmatch)
```

The package takes input either from a text description or local path to
an R package, and finds similar packages based on both Language Model
(LM) embeddings, and more traditional text and code matching algorithms.
The LM embeddings require a locally-running instance of
[ollama](https://ollama.com), as described in a separate vignette.

## Using the `pkgmatch` package

The package has two main functions:

- `pkgmatch_similar_pkgs()` to find similar rOpenSci or CRAN packages
  based input as either a local path to an entire package, or as a
  single descriptive text string; and
- `pkgmatch_similar_fns()` to find similar functions from rOpenSci
  packages based on descriptive text input. (Not available for functions
  from CRAN packages.)

The following code demonstrates how these functions work, first matching
general text strings packages from rOpenSci:

``` r
input <- "
Packages for analysing evolutionary trees, with a particular focus
on visualising inter-relationships among distinct trees.
"
pkgmatch_similar_pkgs (input)
```

    ## [1] "lingtypology"   "treedata.table" "treestartr"     "babette"       
    ## [5] "canaper"

Corresponding websites can also be automatically opened, either by
passing `browse = TRUE`, or by specifying a return value and passing
that to the `pkgmatch_browse()` function.

### Matching entire packages

The `input` parameter can also be a local path to an entire package. The
following code finds the most similar packages to this very package by
passing `input = "."`, again by default matching against all rOpenSci
packages:

``` r
pkgmatch_similar_pkgs (".")
```

    ## $text
    ## [1] "osmdata"        "elastic"        "pkgcheck"       "rdataretriever"
    ## [5] "textreuse"     
    ## 
    ## $code
    ## [1] "autotest"    "pkgcheck"    "roreviewapi" "cffr"        "srr"

And the most similar packages in terms of text descriptions include
several general search and retrieval packages, and only [the `pkgcheck`
package](https://github.com/ropensci-review-tools/pkgcheck) from the
`ropensci-review-tools` suite. In contrast, four of the five most
similar packages in terms of code structure are packages from the same
`ropensci-review-tools` suite. Packages from CRAN can be matched by
specifying the `corpus` parameter:

``` r
pkgmatch_similar_pkgs (".", corpus = "cran")
```

    ## $text
    ## [1] "rsyntax"  "searcher" "sos"      "ore"      "Require" 
    ## 
    ## $code
    ## [1] "workflowr" "RInno"     "remotes"   "miniCRAN"  "cffr"

The `input` parameter can also be a local path to compressed `.tar.gz`
binary object directly downloaded from CRAN.

## Finding functions

There is an additional function to find functions within packages which
best match a text description.

``` r
input <- "A function to label a set of geographic coordinates"
pkgmatch_similar_fns (input)
```

    ## [1] "GSODR::nearest_stations"          "refsplitr::plot_addresses_points"
    ## [3] "slopes::elevation_extract"        "quadkeyr::grid_to_polygon"       
    ## [5] "rnoaa::meteo_nearby_stations"

``` r
input <- "Identify genetic sequences matching a given input fragment"
pkgmatch_similar_fns (input)
```

    ## [1] "charlatan::SequenceProvider" "beastier::is_alignment"     
    ## [3] "charlatan::ch_gene_sequence" "beautier::is_phylo"         
    ## [5] "textreuse::align_local"

Setting `browse = TRUE` will then open the documentation pages
corresponding to those best-matching functions.

## Prior Art

- The [`utils::RSiteSearch()`
  function](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/RSiteSearch.html).
- The [`sos` package](https://github.com/sbgraves237/sos) that queries
  the “RSiteSearch” database.
