<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/pkgmatch/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/pkgmatch/actions?query=workflow%3AR-CMD-check)
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

This package relies on a locally-running instance of
[ollama](https://ollama.com). Procedures for setting that up are
described in a [separate
vignette](https://docs.ropensci.org/pkgmatch/articles/B_ollama.html)
(`vignette("ollama", package = "pkgmatch")`). Although some
functionality of this package may be used without ollama, the main
functions require ollama to be installed.

Once ollama is running, the easiest way to install this package is via
the [associated
`r-universe`](https://ropensci-review-tools.r-universe.dev/).
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

The [`ollama_check()`
function](https://docs.ropensci.org/pkgmatch/reference/ollama_check.html)
can then be used to confirm that [ollama](https://ollama.com) is up and
running as expected.

## Using the `pkgmatch` package

The ‘pkgmatch’ package takes input either from a text description or
local path to an R package, and finds matching packages based on both
Language Model (LM) embeddings, and more traditional text and code
matching algorithms.

The package has two main functions:

- [`pkgmatch_similar_pkgs()`](https://docs.ropensci.org/pkgmatch/reference/pkgmatch_similar_pkgs.html)
  to find similar rOpenSci or CRAN packages based on input as either a
  local path to an entire package, the name of an installed package, or
  as a single descriptive text string; and
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

    ## [1] "phylogram"    "phruta"       "rotl"         "taxa"         "lingtypology"

The corpus parameter must be specified as one of “ropensci” or “cran”
(case-insensitive). The CRAN corpus is much larger than the rOpenSci
corpus, and matching for `corpus = "cran"` will generally take notably
longer.

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
    ## [1] "luca"          "httr"          "tapLock"       "scatterplot3d"
    ## [5] "AzureAuth"    
    ## 
    ## $code
    ## [1] "paperplanes" "httr"        "prenoms"     "tapLock"     "AzureAuth"

The result includes the top five matches based from both text and code
of the input package. The input package itself is the second-placed
match in both cases, and not the top match. This happens because
embeddings are “chunked” or randomly permuted, and because matches are
statistical and not deterministic. Nevertheless, the only two packages
which appear in the top five matches on both lists are the package
itself, `httr2`, and the very closely related, `httptest2` package for
testing output of `httr2`. See the [vignette on *Why are the results not
what I
expect?*](https://docs.ropensci.org/pkgmatch/articles/F_why-are-the-results-not-what-i-expect.html)
for more detail on how matches are generated.

## Finding functions

There is an additional function to find functions within packages which
best match a text description.

``` r
input <- "A function to label a set of geographic coordinates"
pkgmatch_similar_fns (input)
```

    ## [1] "GSODR::nearest_stations"          "refsplitr::plot_addresses_points"
    ## [3] "slopes::elevation_extract"        "rnoaa::meteo_nearby_stations"    
    ## [5] "charlatan::CoordinateProvider"

``` r
input <- "Identify genetic sequences matching a given input fragment"
pkgmatch_similar_fns (input)
```

    ## [1] "charlatan::SequenceProvider" "beastier::is_alignment"     
    ## [3] "charlatan::ch_gene_sequence" "beautier::is_phylo"         
    ## [5] "textreuse::align_local"

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
- [*Before you begin: ollama
  installation*](https://docs.ropensci.org/pkgmatch/articles/B_ollama.html)
  which describes how to install and setup the
  [`ollama`](https://ollama.com) software needed to download and run the
  language models.
- [*How does pkgmatch
  work?*](https://docs.ropensci.org/pkgmatch/articles/C_how-does-it-work.html)
  which provides detailed explanations of the matching algorithms
  implemented in the package.
- [*Data caching and
  updating*](https://docs.ropensci.org/pkgmatch/articles/D_data-caching-and-updating.html)
  which describes how `pkgmatch` caches and updates the language model
  results for the individual corpora.
- [*Why local language models
  (LMs)?*](https://docs.ropensci.org/pkgmatch/articles/E_why-local-lms.html)
  which explains why `pkgmatch` uses locally-running language models,
  instead of relying on external APIs.
- [*Why are the results not what I
  expect?*](https://docs.ropensci.org/pkgmatch/articles/F_why-are-the-results-not-what-i-expect.html)
  which explains in detail why matches generated by `pkgmatch` may
  sometimes differ from what you might expect, and includes advice for
  how to improve matches.

## Prior Art

- The [`utils::RSiteSearch()`
  function](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/RSiteSearch.html).
- The [`sos` package](https://github.com/sbgraves237/sos) that queries
  the “RSiteSearch” database.

## Contributors


<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->

All contributions to this project are gratefully acknowledged using the [`allcontributors` package](https://github.com/ropensci/allcontributors) following the [allcontributors](https://allcontributors.org) specification. Contributions of any kind are welcome!

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


### Issue Authors

<table>

<tr>
<td align="center">
<a href="https://github.com/MargaretSiple-NOAA">
<img src="https://avatars.githubusercontent.com/u/73858992?u=7ea549d423535a74d69a75ff6303af35496290fb&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgmatch/issues?q=is%3Aissue+author%3AMargaretSiple-NOAA">MargaretSiple-NOAA</a>
</td>
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
