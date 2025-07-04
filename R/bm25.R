#' @title The "Best Matching 25" (BM25) ranking function.
#'
#' @description BM25 values match single inputs to document corpora by
#' weighting terms by their inverse frequencies, so that relatively rare words
#' contribute more to match scores than common words. For each input, the BM25
#' value is the sum of relative frequencies of each term in the input
#' multiplied by the Inverse Document Frequency (IDF) of that term in the
#' entire corpus. See the Wikipedia page at
#' \url{https://en.wikipedia.org/wiki/Okapi_BM25} for further details.
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
#' those. Must be one of "ropensci", "ropensci-fns", or "cran". Note that the
#' "ropensci-fns" corpus contains entries for every single function of every
#' rOpenSci package, and the resulting BM25 values can be used to determine the
#' best-matching function. The other two corpora are package-based, and the
#' results can be used to find the best-matching package.
#'
#' @return A `data.frame` of package names and 'BM25' measures against text
#' from whole packages both with and without function descriptions.
#'
#' @family bm25
#' @export
#'
#' @examples
#' # The following function simulates remote data in temporary directory, to
#' # enable package usage without downloading. Do not run for normal usage.
#' generate_pkgmatch_example_data ()
#'
#' input <- "curl" # Name of a single installed package
#' pkgmatch_bm25 (input, corpus = "cran")
#' # Or pre-load document-frequency weightings and pass those:
#' idfs <- pkgmatch_load_data ("idfs", corpus = "cran", fns = FALSE)
#' pkgmatch_bm25 (input, corpus = "cran", idfs = idfs)
pkgmatch_bm25 <- function (input, txt = NULL,
                           idfs = NULL, corpus = NULL) {

    checkmate::assert_character (input, len = 1L)
    if (is.null (idfs)) {
        corpus <- check_corpus_param (corpus)
    }

    m_pkgmatch_bm25 (input, txt, idfs, corpus)
}

pkgmatch_bm25_internal <- function (input, txt, idfs, corpus) {

    if (is.null (idfs)) {
        fname <- get_cache_file_name (
            what = "idfs",
            corpus = corpus,
            fns = FALSE,
            raw = FALSE
        )
        send_dl_message (fname)
    }

    if (is.null (txt)) {
        if (is.null (idfs)) {
            idfs <- pkgmatch_load_data ("idfs", corpus = corpus, fns = FALSE)
        }
        checkmate::assert_list (idfs, len = 2L)
        checkmate::assert_names (
            names (idfs),
            identical.to = c ("idfs", "token_lists")
        )
        tokens_idf <- idfs$idfs
        tokens_list <- idfs$token_lists
    } else {
        checkmate::assert_list (txt)
        txt_lens <- vapply (txt, length, integer (1L))
        stopifnot (all (txt_lens == 1L))
        txt_class <- vapply (txt, class, character (1L))
        stopifnot (all (txt_class == "character"))

        txt_wo_fns <- rm_fns_from_pkg_txt (txt)
        tokens_list <- list (
            with_fns = bm25_tokens_list (txt),
            wo_fns = bm25_tokens_list (txt_wo_fns)
        )
        tokens_idf <- list (
            with_fns = bm25_idf (txt),
            wo_fns = bm25_idf (txt_wo_fns)
        )
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

#' @title The "Best Matching 25" (BM25) ranking function for function calls
#'
#' @description See `?pkgmatch_bm25` for details of BM25 ranks. This function
#' calculates "BM25" ranks from function-call frequencies between a local R
#' package and all packages in specified corpus. Values are thus higher for
#' packages with similar patterns of function calls, weighted by inverse
#' frequencies, so functions called infrequently across the entire corpus
#' contribute more than common functions.
#'
#' Note that the results of this function are entirely different from
#' \link{pkgmatch_bm25} with `corpus = "ropensci-fns"`. The latter returns BM25
#' values from text descriptions of all functions in all rOpenSci packages,
#' whereas this function returns BM25 values based on frequencies of function
#' calls within packages.
#'
#' @param path Local path to source code of an R package.
#' @param corpus One of "ropensci" or "cran"
#' @return A `data.frame` of two columns:
#' \itemize{
#' \item "package" Naming the package from the specified corpus;
#' \item bm25 The "BM25" index value for the nominated packages, where high
#' values indicate greater overlap in term frequencies.
#' }
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
pkgmatch_bm25_fn_calls <- function (path, corpus = NULL) {

    chk <- checkmate::check_file_exists (path)
    if (!is.logical (chk)) {
        chk <- checkmate::check_directory_exists (path)
    }
    if (!is.logical (chk)) {
        chk <- input_is_pkg (path)
    }
    if (!chk) {
        cli::cli_abort ("'path' does not appear to be an R package.")
    }

    corpus <- check_corpus_param (corpus, fns = TRUE)

    m_pkgmatch_bm25_fn_calls (path, corpus)
}

pkgmatch_bm25_fn_calls_internal <- function (path, corpus) { # nolint

    calling_fn <- tryCatch (
        as.character (sys.call (sys.parent (n = 2L)) [[1]]),
        error = function (e) ""
    )
    if (calling_fn != "pkgmatch_similar_pkgs") {
        fnames <- c (
            get_cache_file_name (what = "calls", corpus, fns = FALSE, raw = FALSE),
            get_cache_file_name (what = "calls", corpus, fns = FALSE, raw = TRUE)
        )
        send_dl_message (fnames)
    }

    tokens_idf <-
        pkgmatch_load_data (what = "calls", corpus = corpus, raw = FALSE)
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

pkgmatch_bm25_from_idf_internal <- function (input, tokens_list, tokens_idf) { # nolint

    n <- name <- NULL # suppress no visible binding note

    ntoks_list <- vapply (tokens_list, function (i) sum (i$n), integer (1L))
    ntoks_avg <- mean (ntoks_list)
    tok_list_nms <- basename (names (tokens_list))
    n_tarballs <- length (grep ("\\.tar\\.gz$", tok_list_nms))
    if (n_tarballs / length (tokens_list) > 0.9) {
        # All CRAN pkgs have only one underscore between pkg and version:
        # tok_list_nms <- gsub ("\\_.*$", "", tok_list_nms)
    }

    if (is.character (input)) {
        tokens_i <- bm25_tokens_list (input) [[1]]
        tokens_i <- dplyr::rename (tokens_i, np = n)
    } else if (is.data.frame (input)) {
        treesit_nms <- c ("fn", "name", "start", "end", "file")
        if (!identical (names (input), treesit_nms)) {
            cli::cli_abort (
                "'input' must be from 'pkgmatch_treesitter_fn_tags()'"
            )
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
