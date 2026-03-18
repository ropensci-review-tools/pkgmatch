#' @title Find R packages matching an input of either text or another package
#'
#' @description This function accepts as `input` either a text description, the
#' name of a locally-installed package, or a path to a local directory containing an R package.
#' It ranks all R packages within the specified corpus in terms of how well they
#' match that input. The "corpus" argument can specify either [rOpenSci's
#' package suite](https://ropensci.org/packages),
#' [CRAN](https://cran.r-project.org), or
#' [Bioconductor](https://bioconductor.org).
#'
#' Ranks are obtained from scores derived from ["Best Match 25"
#' (BM25)](https://en.wikipedia.org/wiki/Okapi_BM25) scores based on document
#' token frequencies.
#'
#' Ranks are generally obtained by matching both for full package text from the
#' specified corpus, including all long-form documentation, and by matching
#' package descriptions only. The function returns a single rank derived by
#' combining individual ranks using the [Reciprocal Rank Fusion (RRF)
#' algorithm](https://plg.uwaterloo.ca/~gvcormac/cormacksigir09-rrf.pdf).
#'
#' Finally, all components of this function are locally cached for each call
#' (by the \pkg{memoise} package), so additional calls to this function with
#' the same `input` and `corpus` should be much faster than initial calls.
#'
#' @param input Either a text string, a path to local source code of an R
#' package, or the name of any installed R package.
#' @param corpus Must be specified as one of "ropensci", "cran", or "bioc" (for
#' BioConductor). If `idfs` parameter is not specified, data will be
#' automatically downloaded for the corpus specified by this parameter. The
#' function will then return the most similar package from the specified
#' corpus. Note that calculations will `corpus = "cran"` will generally take
#' longer, because the corpus is much larger.
#' @param idfs Inverse Document Frequency tables for a suite of packages,
#' generated from \link{pkgmatch_bm25}. If not provided, pre-generated IDF
#' tables will be downloaded and stored in a local cache directory.
#' @param n When the result of this function is printed to screen, the top `n`
#' packages will be displayed.
#' @param browse If `TRUE`, automatically open webpages of the top `n` matches
#' in local browser.
#'
#' @return A `data.frame` with a "package" column naming packages, and one or
#' more columns of package ranks in terms of text similarity and, if `input` is
#' an R package, of similarity in code structure.
#'
#' The returned object has a default `print` method which prints the best 5
#' matches directly to the screen, yet returns information on all packages
#' within the specified corpus. This information is in the form of a
#' `data.frame`, with one column for the package name, and one or more
#' additional columns of integer ranks for each package. There is also a `head`
#' method to print the first few entries of these full data (default `n = 5`).
#' To see all data, use `as.data.frame()`. See the example below for how to
#' manipulate these objects.
#'
#' @note The first time this function is run without passing `idfs`, required
#' values will be automatically downloaded and stored in a locally persistent
#' cache directory. Especially for the "cran" corpus, this downloading may take
#' quite some time.
#'
#' @family main
#' @export
#'
#' @examples
#' # The following function simulates remote data in temporary directory, to
#' # enable package usage without downloading. Do not run for normal usage.
#' generate_pkgmatch_example_data ()
#'
#' input <- "curl" # Name of a single installed package
#' p <- pkgmatch_similar_pkgs (input, corpus = "cran")
#' p # Default print method, lists 5 best matching packages
#' head (p) # Shows first 5 rows of full `data.frame` object
pkgmatch_similar_pkgs <- function (input,
                                   corpus = NULL,
                                   idfs = NULL,
                                   n = 5L,
                                   browse = FALSE) {

    m_pkgmatch_similar_pkgs (input, corpus, idfs, n = 5L, browse)
}

pkgmatch_similar_pkgs_internal <- function (input,
                                            corpus = NULL,
                                            idfs = NULL,
                                            n = 5L,
                                            browse = FALSE) {

    # Suppress no visible binding notes:
    bm25 <- NULL

    if (is.null (idfs)) {
        corpus <- check_corpus_param (corpus)
    }

    checkmate::assert_character (input, len = 1L)
    checkmate::assert_integerish (n, len = 1L, lower = 1L)
    checkmate::assert_logical (browse, len = 1L)

    code <- NULL # Suppress no visible binding note

    if (is.null (idfs)) {
        fname <- get_cache_file_name (
            what = "idfs", corpus = corpus, fns = FALSE, raw = FALSE
        )
        send_dl_message (fname)
        idfs <- pkgmatch_load_data (what = "idfs", corpus = corpus)
    }
    assert_idfs (idfs)

    if (input_is_pkg (input)) {

        txt_with_fns <- get_pkg_text (input)
        input <- rm_fns_from_pkg_txt (txt_with_fns) [[1]]
        desc <- get_pkg_desc_from_pkg_txt (txt_with_fns) [[1]]

    } else {

        desc <- input
    }

    res_full <- pkgmatch_bm25 (input, idfs = idfs$full, corpus = corpus) |>
        dplyr::rename (bm25_full = bm25)
    res_descs <- pkgmatch_bm25 (desc, idfs = idfs$descs_only, corpus = corpus) |>
        dplyr::rename (bm25_desc = bm25)
    res <- dplyr::left_join (res_full, res_descs, by = "package") |>
        pkgmatch_rerank ()

    if (identical (corpus, "cran")) {
        res <- res |>
            dplyr::mutate (version = gsub ("^.*\\_", "", res$package), .after = package) |>
            dplyr::mutate (package = gsub ("\\_.*$", "", res$package))
    }

    class (res) <- c ("pkgmatch", class (res))
    attr (res, "n") <- as.integer (n)

    if (browse) {
        pkgmatch_browse (res) # nocov
    }

    return (res)
}
m_pkgmatch_similar_pkgs <- memoise::memoise (pkgmatch_similar_pkgs_internal)

input_mentions_functions <- function (input) {

    stopifnot (length (input) == 1L)

    grepl ("\\bfunction(s)?\\b", input, ignore.case = TRUE)
}
