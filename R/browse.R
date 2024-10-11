#' Open web pages for `pkgmatch` results
#'
#' @param p A `pkgmatch` object returned from either
#' \link{pkgmatch_similar_pkgs} or \link{pkgmatch_similar_fns}.
#' @param n Number of top-matching entries which should be opened. Defaults to
#' the value passed to the main functions.
#' @return (Invisibly) A named vector of integers, with 0 for all pages able to
#' be successfully opened, and 1 otherwise.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' input <- "genomics and transcriptomics sequence data"
#' p <- pkgmatch_similar_pkgs (input)
#' pkgmatch_browse (p) # Open main package pages on rOpenSci
#' p <- pkgmatch_similar_pkgs (input, corpus = "cran")
#' pkgmatch_browse (p) # Open main package pages on CRAN
#' p <- pkgmatch_similar_fns (input)
#' pkgmatch_browse (p) # Open pages for best-matching rOpenSci functions
#' }
pkgmatch_browse <- function (p, n = NULL) {

    if (is.null (n)) {
        n <- attr (p, "n")
    }

    url_ropensci <- "https://docs.ropensci.org/"
    url_cran <- "https://cran.r-project.org/package="

    if ("version" %in% names (p)) { # CRAN
        urls <- paste0 (url_cran, p$package [seq_len (n)])
    } else if (all (grepl ("\\:\\:", p$package))) { # fns
        pkgs <- gsub ("\\:\\:.*$", "", p$package [seq_len (n)])
        fns <- gsub ("^.*\\:\\:", "", p$package [seq_len (n)])
        urls <- paste0 (url_ropensci, pkgs, "/reference/", fns)
    } else { # ropensci pkgs
        pkgs <- p$package [seq_len (n)]
        urls <- paste0 (url_ropensci, pkgs, "/")
    }
    chks <- vapply (urls, utils::browseURL, integer (1L))

    invisible (chks)
}
