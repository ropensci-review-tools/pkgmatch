<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/pkgmatch/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/pkgmatch/actions?query=workflow%3AR-CMD-check)
[![](https://badges.ropensci.org/671_status.svg)](https://github.com/ropensci/software-review/issues/671)
[![codecov](https://codecov.io/gh/ropensci-review-tools/pkgmatch/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci-review-tools/pkgmatch)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

# pkgmatch

A tool that uses language models to help find R packages, by matching
packages either to a text description, or to entire packages. Can find
matching packages either from rOpenSci’s [suite of
packages](https://ropensci.org/packages), or from all packages currently
on [CRAN](https://cran.r-project.org).

## Installation

The easiest way to install this package is via the [associated
`r-universe`](https://ropensci-review-tools.r-universe.dev/). As shown
there, simply enable the universe with

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
remotes::install_git ("https://codeberg.org/ropensci-review-tools/pkgmatch")
remotes::install_git ("https://codefloe.com/ropensci-review-tools/pkgmatch")
```

The package can then loaded for use with

``` r
library (pkgmatch)
```

## Using the `pkgmatch` package

The ‘pkgmatch’ package takes input either from a text description or
local path to an R package, and finds matching packages based on text
and code matching algorithms.

The package has two main functions:

- [`pkgmatch_similar_pkgs()`](https://docs.ropensci.org/pkgmatch/reference/pkgmatch_similar_pkgs.html)
  to find similar [rOpenSci](https://ropensci.org/packages/),
  [CRAN](https://cran.r-project.org), or
  [BioConductor](https://bioconductor.org/packages/release/BiocViews.html#___Software)
  packages based on input as either a local path to an entire package,
  the name of an installed package, or as a single descriptive text
  string; and
- [`pkgmatch_similar_fns()`](https://docs.ropensci.org/pkgmatch/reference/pkgmatch_similar_fns.html)
  to find similar functions from rOpenSci or BioConductor packages based
  on descriptive text input. (Not available for functions from CRAN
  packages.)

The following code demonstrates how these functions work, first matching
general text strings packages from rOpenSci:

``` r
input <- "
Packages for analysing evolutionary trees, with a particular focus
on visualising inter-relationships among distinct trees.
"
pkgmatch_similar_pkgs (input, corpus = "ropensci")
```

    ## [1] "ssarp"   "rotl"    "stats19" "phruta"  "stplanr"

The corpus parameter must be specified as one of “ropensci”, “cran”, or
“bioc” (for [BioConductor](https://bioconductor.org); all
case-insensitive). The CRAN corpus is much larger than the rOpenSci or
BioConductor corpora, and matching for `corpus = "cran"` will generally
take notably longer.

Websites of packages returned by [the `pkgmatch_similar_pkgs()`
function](https://docs.ropensci.org/pkgmatch/reference/pkgmatch_similar_pkgs.html)
can be automatically opened, either by calling the function with
`browse = TRUE`, or by storing the return value of [the
`pkgmatch_similar_pkgs()`
function](https://docs.ropensci.org/pkgmatch/reference/pkgmatch_similar_pkgs.html)
as an object and passing that to [the `pkgmatch_browse()`
function](https://docs.ropensci.org/pkgmatch/reference/pkgmatch_browse.html).

### Matching entire packages

The `input` parameter can also specify an entire package, either as a
local path to a package directory, or the name of an installed package.
To demonstrate that, the following code downloads a `.tar.gz` file of
the `httr2` package from CRAN:

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
pkgmatch_similar_pkgs (path, corpus = "cran")
```

    ## $text
    ## [1] "httr2"     "ellmer"    "geocodebr" "thisplot"  "httr"     
    ## 
    ## $code
    ## [1] "r4ds.tutorials"  "outliers.ts.oga" "ReliaShiny"      "secrfunc"       
    ## [5] "strider"

The result includes the top five matches based from both text and code
of the input package.

Setting `browse = TRUE` will then open the documentation pages
corresponding to those best-matching functions.

## Package vignettes

The `pkgmatch` package includes the following vignettes:

- [A main *pkgmatch*
  vignette](https://docs.ropensci.org/pkgmatch/articles/pkgmatch.html)
  which gives an overview of how to use the package.
- [*Example
  applications*](https://docs.ropensci.org/pkgmatch/articles/A_extended-use-case.html)
  which describes several different example applications of `pkgmatch`,
  and illustrates the ways by which this package provides different kind
  of results to search engines and to general language model interfaces.
- [*How does pkgmatch
  work?*](https://docs.ropensci.org/pkgmatch/articles/C_how-does-it-work.html)
  which provides detailed explanations of the matching algorithms
  implemented in the package.
- [*Data caching and
  updating*](https://docs.ropensci.org/pkgmatch/articles/D_data-caching-and-updating.html)
  which describes how `pkgmatch` caches and updates the language model
  results for the individual corpora.

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

<a href="https://github.com/MargaretSiple-NOAA">
<img src="https://avatars.githubusercontent.com/u/73858992?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgmatch/commits?author=MargaretSiple-NOAA">MargaretSiple-NOAA</a>
</td>

<td align="center">

<a href="https://github.com/Bisaloo">
<img src="https://avatars.githubusercontent.com/u/10783929?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgmatch/commits?author=Bisaloo">Bisaloo</a>
</td>

</tr>

</table>

### Issue Authors

<table>

<tr>

<td align="center">

<a href="https://github.com/maelle">
<img src="https://avatars.githubusercontent.com/u/8360597?u=824f03caa87c92420352e3dd9a05470320a67412&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgmatch/issues?q=is%3Aissue+author%3Amaelle">maelle</a>
</td>

<td align="center">

<a href="https://github.com/Selbosh">
<img src="https://avatars.githubusercontent.com/u/7850509?u=df48c73a83db7c2f1e5101e98f8bbf628ae85505&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgmatch/issues?q=is%3Aissue+author%3ASelbosh">Selbosh</a>
</td>

<td align="center">

<a href="https://github.com/nhejazi">
<img src="https://avatars.githubusercontent.com/u/8527276?u=931dc381c88c91eaa34d7c9a3ce926e1f8a3463b&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgmatch/issues?q=is%3Aissue+author%3Anhejazi">nhejazi</a>
</td>

</tr>

</table>

### Issue Contributors

<table>

<tr>

<td align="center">

<a href="https://github.com/agricolamz">
<img src="https://avatars.githubusercontent.com/u/15956515?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgmatch/issues?q=is%3Aissue+commenter%3Aagricolamz">agricolamz</a>
</td>

</tr>

</table>

<!-- markdownlint-enable -->

<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->
