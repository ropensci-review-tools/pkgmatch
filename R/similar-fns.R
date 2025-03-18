#' Identify R functions best matching a given input string
#'
#' @description Function matching is only available for functions from the
#' corpus of rOpenSci packages. Function matching is also based on LM output
#' only, and unlike package matching does not combine LM output with BM25
#' word-frequency matching.
#'
#' @inheritParams pkgmatch_similar_pkgs
#' @param input A text string.
#' @return A character vector of function names in the form
#' "<package>::<function>".
#'
#' @family main
#' @export
#'
#' @examples
#' \dontrun{
#' input <- "Process raster satellite images"
#' p <- pkgmatch_similar_fns (input)
#' p # Default print method, lists 5 best matching packages
#' head (p) # Shows first 5 rows of full `data.frame` object
#' }
pkgmatch_similar_fns <- function (input,
                                  embeddings = NULL,
                                  n = 5L,
                                  browse = FALSE) {

    expected_embedding_len <- 768L

    checkmate::assert_character (input, len = 1L)
    checkmate::assert_integerish (n, len = 1L, lower = 1L)
    checkmate::assert_logical (browse, len = 1L)

    if (is.null (embeddings)) {
        embeddings <-
            pkgmatch_load_data ("embeddings", corpus = "ropensci", fns = TRUE)
    }
    checkmate::assert_matrix (
        embeddings,
        nrow = expected_embedding_len,
        any.missing = FALSE
    )
    nms <- colnames (embeddings)
    stopifnot (!is.null (nms))
    stopifnot (all (grepl ("\\:\\:", nms)))

    op <- getOption ("rlib_message_verbosity")
    options (rlib_message_verbosity = "quiet")
    emb <- get_embeddings (input)
    options (rlib_message_verbosity = op)

    res <- cosine_similarity (emb [, 1], embeddings)
    res$rank <- seq_len (nrow (res))

    class (res) <- c ("pkgmatch", class (res))
    attr (res, "n") <- as.integer (n)

    if (browse) {
        pkgmatch_browse (res) # nocov
    }

    return (res)
}
