#' Calculate the "BM25" = "Best Matching 25" ranking function between text
#' input and all R packages within specified corpus.
#'
#' See \url{https://en.wikipedia.org/wiki/Okapi_BM25}.
#'
#' @param input A single character string to match against the second parameter
#' of all input documents.
#' @param txt An optional list of input documents. If not specified, data will
#' be loaded as specified by the `corpus` parameter.
#' @param idfs Optional list of Inverse Document Frequency weightings generated
#' by the internal `bm25_idf` function. If not specified, values for the
#' rOpenSci corpus will be automatically downloaded and used.
#' @param corpus If `txt` is not specified, data for nominated corpus will be
#' downloaded to local cache directory, and BM25 values calculated against
#' those. Must be one of "ropensci", "ropensci-fns".
#'
#' @return A `data.frame` of package names and 'BM25' measures against text
#' from whole packages both with and without function descriptions.
#'
#' @family bm25
#' @export
#'
#' @examples
#' \dontrun{
#' input <- "Download open spatial data from NASA"
#' bm25 <- pkgmatch_bm25 (input)
#' # Or pre-load document-frequency weightings:
#' idfs <- pkgmatch_load_data ("idfs", fns = FALSE)
#' bm25 <- pkgmatch_bm25 (input, idfs = idfs)
#' }
pkgmatch_bm25 <- function (input, txt = NULL,
                           idfs = NULL, corpus = "ropensci") {
    m_pkgmatch_bm25 (input, txt, idfs, corpus)
}

pkgmatch_bm25_internal <- function (input, txt, idfs, corpus) {

    if (is.null (txt)) {
        if (is.null (idfs)) {
            idfs <- pkgmatch_load_data ("idfs", fns = FALSE)
        }
        tokens_idf <- idfs$idfs
        tokens_list <- idfs$token_lists
    } else {
        stopifnot (is.list (txt))
        txt_lens <- vapply (txt, length, integer (1L))
        stopifnot (all (txt_lens == 1L))
        txt_class <- vapply (txt, class, character (1L))
        stopifnot (all (txt_class == "character"))

        tokens_list <- bm25_tokens_list (txt)
        tokens_idf <- bm25_idf (txt)
    }

    bm25_with_fns <- pkgmatch_bm25_from_idf (
        input,
        tokens_list$with_fns,
        tokens_idf$with_fns
    )
    bm25_wo_fns <- pkgmatch_bm25_from_idf (
        input,
        tokens_list$wo_fns,
        tokens_idf$wo_fns
    )
    names (bm25_with_fns) [2] <- "bm25_with_fns"
    names (bm25_wo_fns) [2] <- "bm25_wo_fns"

    dplyr::left_join (bm25_with_fns, bm25_wo_fns, by = "package")
}
m_pkgmatch_bm25 <- memoise::memoise (pkgmatch_bm25_internal)

#' Calculate a "BM25" index from function-call frequencies between a local R
#' package and all packages in specified corpus.
#'
#' @param path Local path to source code of an R package.
#' @param corpus One of "ropensci" or "cran"
#'
#' @family bm25
#' @export
#'
#' @examples
#' \dontrun{
#' u <- "https://cran.r-project.org/src/contrib/odbc_1.5.0.tar.gz"
#' path <- file.path (tempdir (), basename (u))
#' download.file (u, destfile = path)
#' bm25 <- pkgmatch_bm25_fn_calls (path)
#' }
pkgmatch_bm25_fn_calls <- function (path, corpus = "ropensci") {

    m_pkgmatch_bm25_fn_calls (path, corpus)
}

pkgmatch_bm25_fn_calls_internal <- function (path, corpus) {

    tokens_idf <- pkgmatch_load_data (what = "calls", corpus = corpus, raw = FALSE)
    calls <- pkgmatch_load_data (what = "calls", corpus = corpus, raw = TRUE)

    tokens_list <- lapply (calls, function (i) {
        data.frame (
            token = names (i),
            n = as.integer (i)
        )
    })

    input <- pkgmatch_treesitter_fn_tags (path)

    pkgmatch_bm25_from_idf (input, tokens_list, tokens_idf)
}
m_pkgmatch_bm25_fn_calls <- memoise::memoise (pkgmatch_bm25_fn_calls_internal)

pkgmatch_bm25_from_idf <- function (input, tokens_list, tokens_idf) {

    m_pkgmatch_bm25_from_idf (input, tokens_list, tokens_idf)
}

pkgmatch_bm25_from_idf_internal <- function (input, tokens_list, tokens_idf) {

    n <- name <- NULL # suppress no visible binding note

    ntoks_list <- vapply (tokens_list, function (i) sum (i$n), integer (1L))
    ntoks_avg <- mean (ntoks_list)
    tok_list_nms <- basename (names (tokens_list))
    n_tarballs <- length (grep ("\\.tar\\.gz$", tok_list_nms))
    if (n_tarballs / length (tokens_list) > 0.9) {
        # All CRAN pkgs have only one underscore between pkg and version:
        tok_list_nms <- gsub ("\\_.*$", "", tok_list_nms)
    }

    if (is.character (input)) {
        tokens_i <- bm25_tokens_list (input) [[1]]
        tokens_i <- dplyr::rename (tokens_i, np = n)
    } else if (is.data.frame (input)) {
        treesit_nms <- c ("fn", "name", "start", "end", "file")
        if (!identical (names (input), treesit_nms)) {
            cli::cli_abort ("'input' must be from 'pkgmatch_treesitter_fn_tags()'")
        }
        tokens_i <-
            dplyr::summarise (dplyr::group_by (input, name), np = dplyr::n ())
        tokens_i <- dplyr::rename (tokens_i, token = "name")
    } else {
        cli::cli_abort ("Unrecognised 'input' type in 'bm25_from_idf()'")
    }

    bm25 <- rcpp_bm25 (tokens_idf, tokens_list, tokens_i, ntoks_avg)
    index <- order (bm25, decreasing = TRUE)
    bm25 <- data.frame (
        package = tok_list_nms,
        bm25 = bm25
    ) [index, ]
    rownames (bm25) <- NULL

    return (bm25)
}
m_pkgmatch_bm25_from_idf <- memoise::memoise (pkgmatch_bm25_from_idf_internal)

#' Convert input list of text documents into lists of tokens.
#'
#' @inheritParams pkgmatch_bm25
#' @return The input list of text strings converted to tokens.
#' @noRd
bm25_tokens <- function (txt) {

    m_bm25_tokens (txt)
}

bm25_tokens_internal <- function (txt) {

    tokens <- tokenizers::tokenize_words (
        txt,
        lowercase = TRUE,
        strip_punct = TRUE,
        strip_numeric = TRUE
    )

    tokens <- lapply (tokens, function (i) {
        index <- grep ("\\.|\\_|^[0-9]", i)
        if (length (index) > 0) {
            i <- i [-(index)]
        }
        return (i)
    })

    return (tokens)
}

m_bm25_tokens <- memoise::memoise (bm25_tokens_internal)

#' Convert input list of raw tokens to a list of tokens and corresponding
#' frequencies.
#' @inheritParams pkgmatch_bm25
#' @return A list of `data.frame` objects, one for each input item, and each
#' including two columns of "token" and "n" holding frequencies for each token.
#' @noRd
bm25_tokens_list <- function (txt) {

    tokens <- bm25_tokens (txt)

    m_bm25_tokens_list (tokens)
}

bm25_tokens_list_internal <- function (tokens) {

    token <- NULL # suppress no visible binding note

    lapply (tokens, function (i) {
        data.frame (token = i) |>
            dplyr::group_by (token) |>
            dplyr::summarise (n = dplyr::n ())
    })
}

m_bm25_tokens_list <- memoise::memoise (bm25_tokens_list_internal)

#' Calculate inverse document frequencies for all tokens across a list of
#' documents.
#'
#' @inheritParams pkgmatch_bm25
#' @return A list of `data.frame` objects, each containing two columns of
#' "tokens" and "idf" for inverse document frequencies for each token.
#' @noRd
bm25_idf <- function (txt) {

    m_bm25_idf (txt)
}

bm25_idf_internal <- function (txt) {

    token <- n <- NULL # suppress no visible binding note

    n_docs <- length (txt)

    tokens_txt <- bm25_tokens (txt)

    tokens_list <- bm25_tokens_list (txt)
    index <- which (vapply (tokens_list, nrow, integer (1L)) > 0L)

    tokens_idf <- do.call (rbind, lapply (tokens_list [index], function (i) {
        data.frame (token = unique (i$token), n = 1L)
    })) |>
        dplyr::group_by (token) |>
        dplyr::summarise (n = dplyr::n ()) |>
        dplyr::mutate (idf = log ((n_docs - n + 0.5) / (n + 0.5) + 1)) |>
        dplyr::select (-n)

    return (tokens_idf)
}

m_bm25_idf <- memoise::memoise (bm25_idf_internal)
