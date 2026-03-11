#' @title Identify R functions best matching a given input string
#'
#' @description Function matching is only available for functions from the
#' corpus of rOpenSci packages. Function matching is also based on LM output
#' only, and unlike package matching does not combine LM output with BM25
#' word-frequency matching.
#'
#' @inheritParams pkgmatch_similar_pkgs
#' @param input A text string.
#' @param corpus One of "ropensci" or "bioc" (for BioConductor). It is not
#' possible to match functions again CRAN packages.
#' @return A modified `data.frame` object of class "pkgmatch". The `data.frame`
#' has 3 columns:
#' \enumerate{
#' \item "pkg_fn" with the name of the function in the form
#' "<package>::<function>";
#' \item "simil" with a similarity score between 0 and 1; and
#' \item "rank" as an integer index, with the highest rank of 1 as the first row.
#' }
#' The return object has a default `print` method which prints the names only
#' of the first 5 best matching functions; see `?print.pkgmatch` for details.
#'
#' @family main
#' @export
#'
#' @examples
#' \dontrun{
#' input <- "Process raster satellite images"
#' p <- pkgmatch_similar_fns (input)
#' p # Default print method, lists 5 best matching functions
#' head (p) # Shows first 5 rows of full `data.frame` object
#' }
pkgmatch_similar_fns <- function (input,
                                  corpus = "ropensci",
                                  n = 5L,
                                  browse = FALSE) {

    checkmate::assert_character (input, len = 1L)
    checkmate::assert_character (corpus, len = 1L)
    checkmate::assert_integerish (n, len = 1L, lower = 1L)
    checkmate::assert_logical (browse, len = 1L)

    corpus <- match.arg (tolower (corpus), c ("ropensci", "bioc"))

    fname <- get_cache_file_name (
        what = "idfs",
        corpus = corpus,
        fns = TRUE,
        raw = FALSE
    )
    send_dl_message (fname)

    idfs <- pkgmatch_load_data ("idfs", corpus = corpus, fns = TRUE)

    bm25 <- pkgmatch_bm25_from_idf (input, idfs$token_lists, idfs$idfs)
    res <- data.frame (
        pkg_fn = bm25$package,
        rank = order (bm25$bm25, decreasing = TRUE)
    )

    class (res) <- c ("pkgmatch", class (res))
    attr (res, "n") <- as.integer (n)
    attr (res, "corpus") <- corpus

    if (browse) {
        pkgmatch_browse (res) # nocov
    }

    return (res)
}
