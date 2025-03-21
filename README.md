<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/pkgmatch/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/pkgmatch/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropensci-review-tools/pkgmatch/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci-review-tools/pkgmatch)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

# pkgmatch

A tool to help find R packages, by matching packages either to a text
description, or to any given package. Can find matching packages either
from rOpenSci’s [suite of packages](https://ropensci.org/packages), or
from all packages currently on [CRAN](https://cran.r-project.org).

## Installation

This package relies on a locally-running instance of
[ollama](https://ollama.com). Procedures for setting that up are
described in a [separate
vignette](https://docs.ropensci.org/pkgmatch/articles/ollama.html)
(`vignette("ollama", package = "pkgmatch")`). ollama needs to be
installed before this package can be used.

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

- [`pkgmatch_similar_pkgs()`](https://docs.ropensci.org/pkgmatch/reference/pkgmatch_similar_pkgs.html)
  to find similar rOpenSci or CRAN packages based input as either a
  local path to an entire package, or as a single descriptive text
  string; and
- [`pkgmatch_similar_fns()`](https://docs.ropensci.org/pkgmatch/reference/pkgmatch_similar_fns.html)
  to find similar functions from rOpenSci packages based on descriptive
  text input. (Not available for functions from CRAN packages.)

The following code demonstrates how these functions work, first matching
general text strings packages from rOpenSci:

``` r
input <- "
Packages for analysing evolutionary trees, with a particular focus
on visualising inter-relationships among distinct trees.
"
pkgmatch_similar_pkgs (input, corpus = "ropensci")
```

    ## [1] "treestartr"     "treedata.table" "canaper"        "phylogram"     
    ## [5] "rotl"

The corpus parameter must be specified as one of “ropensci” or “cran”.
The CRAN corpus is much larger than the rOpenSci corpus, and matching
for `corpus = "cran"` will generally take notably longer.

Websites of packages returned by the
[`pkgmatch_similar_pkgs()`](https://docs.ropensci.org/pkgmatch/reference/pkgmatch_similar_pkgs.html)
function can be automatically opened, either by passing `browse = TRUE`,
or by storing the value of a function as an object and passing that to
the
[`pkgmatch_browse()`](https://docs.ropensci.org/pkgmatch/reference/pkgmatch_browse.html)
function.

### Matching entire packages

The `input` parameter can also be a local path to an entire package. To
demonstrate that, the following code downloads a `.tar.gz` file of the
`httr2` package from CRAN:

``` r
pkg <- "httr2"
p <- available.packages () |>
    data.frame () |>
    dplyr::filter (Package == pkg)
url_base <- "https://cran.r-project.org/src/contrib/"
url <- paste0 (url_base, p$Package, "_", p$Version, ".tar.gz")
path <- fs::path (fs::path_temp (), basename (url))
download.file (url, destfile = path, quiet = TRUE)
```

The path to that package (in this case as a compressed tarball) can then
be passed to the
[`pkgmatch_similar_pkgs()`](https://docs.ropensci.org/pkgmatch/reference/pkgmatch_similar_pkgs.html)
function:

``` r
pkgmatch_similar_pkgs (path, corpus = "ropensci")
```

    ## $text
    ## [1] "elastic"  "vcr"      "cyphr"    "ruODK"    "webmockr"
    ## 
    ## $code
    ## [1] "taxize"    "webmockr"  "rdhs"      "crul"      "babeldown"

Packages from CRAN can also be matched:

``` r
pkgmatch_similar_pkgs (path, corpus = "cran")
```

    ## $text
    ## [1] "httr2"       "httr"        "googleAuthR" "httptest"    "request"    
    ## 
    ## $code
    ## [1] "httr2"    "httr"     "pkgcache" "ellmer"   "webfakes"

The `input` parameter can also be a local path to a full source code
repository.

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

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->

All contributions to this project are gratefully acknowledged using the
[`allcontributors` package](https://github.com/ropensci/allcontributors)
following the [allcontributors](https://allcontributors.org)
specification. Contributions of any kind are welcome!

### Code

<table>
<tr>
<td align="center">
<a href="https://github.com/mpadge">
<img src="https://avatars.githubusercontent.com/u/6697851?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgmatch/commits?author=mpadge">mpadge</a>
</td>
<td align="center">
<a href="https://github.com/Bisaloo">
<img src="https://avatars.githubusercontent.com/u/10783929?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgmatch/commits?author=Bisaloo">Bisaloo</a>
</td>
</tr>
</table>

### Issues

<table>
<tr>
<td align="center">
<a href="https://github.com/MargaretSiple-NOAA">
<img src="https://avatars.githubusercontent.com/u/73858992?u=7ea549d423535a74d69a75ff6303af35496290fb&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgmatch/issues?q=is%3Aissue+author%3AMargaretSiple-NOAA">MargaretSiple-NOAA</a>
</td>
</tr>
</table>
<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->
