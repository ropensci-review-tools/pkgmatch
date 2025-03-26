#' @title Generate example data to use with pkgmatch
#'
#' @description This function generates a selection of test data for the "cran"
#' corpus, to allow functions to be run offline, without having to download the
#' large datasets otherwise required for the package to function.
#'
#' Note that these data are randomly generated, and results will be generally
#' meaningless. They are generated solely to demonstrate how the package
#' functions, and are not intended to derive meaningful outputs.
#'
#' @return (Invisibly) The path to the temporary directory containing the
#' package data.
#' @export
generate_pkgmatch_example_data <- function () {

    cli::cli_alert_info ("This function resets the cache directory used by 'pkgmatch'")
    cli::cli_alert_info ("to a temporary path. To restore functionality with full data, ")
    cli::cli_alert_info ("you'll either need to restart your R session, or set an ")
    cli::cli_alert_info ("environment variable named 'PKGMATCH_CACHE_DIR' to the ")
    cli::cli_alert_info ("desired path. Default path is {pkgmatch_cache_path()}")

    ex_dir <- fs::path (fs::path_temp (), "pkgmatch_ex_data")
    if (!fs::dir_exists (ex_dir)) {
        fs::dir_create (ex_dir)
    }

    Sys.setenv ("PKGMATCH_CACHE_DIR" = ex_dir)

    corpus <- "cran"
    fnames <- c ("embeddings", "bm25", "idfs-fn-calls", "fn-calls")
    fnames_full <- fs::path (ex_dir, paste0 (fnames, "-", corpus, ".Rds"))
    index <- which (!fs::file_exists (fnames_full))
    fnames <- data.frame (name = fnames, path = fnames_full) [index, ]

    # Best matching packages against "curl" package for text and code:
    pkg_nms <- c (
        "mRpostman", "crul", "RCurl", "ancerGram", "AmpGram",
        "curl", "httr", "ssh", "httr2", "pkgcache"
    )

    paths <- apply (fnames, 1, function (f) {
        fn <- paste0 ("ex_", f [1])
        do.call (fn, list (pkg_nms = pkg_nms, fname = f [2]))
    })
}

ex_embeddings <- function (pkg_nms, fname) {
    n <- length (pkg_nms)
    dat_nms <- c ("text_with_fns", "text_wo_fns", "code")
    dat <- lapply (seq_along (dat_nms), function (i) {
        m <- matrix (runif (n * expected_embedding_length), ncol = n)
        colnames (m) <- pkg_nms
        return (m)
    })
    names (dat) <- dat_nms

    saveRDS (dat, fname)
    return (fname)
}

ex_bm25 <- function (pkg_nms, fname) {
    words <- ex_words ()
    dat <- lapply (seq_along (pkg_nms), function (p) {
        list (
            with_fns = data.frame (
                token = words,
                idf = 10 - rgamma (length (words), shape = 1)
            ) |> dplyr::arrange (dplyr::desc (idf)),
            wo_fns = data.frame (
                token = words,
                idf = 10 - rgamma (length (words), shape = 1)
            ) |> dplyr::arrange (dplyr::desc (idf))
        )
    })
    names (dat) <- pkg_nms

    saveRDS (dat, fname)
    return (fname)
}

# Grab example vector of words
ex_words <- function () {
    txt <- get_pkg_text ("curl")
    txt <- gsub ("\\n|#+|[[:punct:]]", "", txt)
    words <- strsplit (txt, "\\s+") [[1]]
    words [which (nzchar (words))]
}
