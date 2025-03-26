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
#' @family utils
#' @export
generate_pkgmatch_example_data <- function () {

    cli::cli_inform ("This function resets the cache directory used by 'pkgmatch'")
    cli::cli_inform ("to a temporary path. To restore functionality with full data, ")
    cli::cli_inform ("you'll either need to restart your R session, or set an ")
    cli::cli_inform ("environment variable named 'PKGMATCH_CACHE_DIR' to the ")
    cli::cli_inform ("desired path. Default path is {pkgmatch_cache_path()}")

    ex_dir <- fs::path (fs::path_temp (), "pkgmatch_ex_data")
    if (!fs::dir_exists (ex_dir)) {
        fs::dir_create (ex_dir)
    }

    Sys.setenv ("PKGMATCH_CACHE_DIR" = ex_dir)
    options ("pkgmatch.example_env" = "true")

    corpus <- "cran"
    fnames <- c ("embeddings", "bm25", "idfs-fn-calls", "fn-calls")
    fnames_full <- fs::path (ex_dir, paste0 (fnames, "-", corpus, ".Rds"))
    index <- which (!fs::file_exists (fnames_full))
    fnames <- data.frame (name = fnames, path = fnames_full) [index, ]

    # Best matching packages against "curl" package for text and code:
    pkg_nms <- c (
        "mRpostman", "crul", "RCurl", "CancerGram", "AmpGram",
        "curl", "httr", "ssh", "httr2", "pkgcache"
    )
    pkg_vers <- c (
        "1.1.4", "1.5.0", "1.98", "1.0.0", "1.0",
        "6.2.2", "1.4.7", "0.9.3", "1.1.2", "2.2.3"
    )
    pkg_nms <- paste0 (pkg_nms, "_", pkg_vers)

    if (nrow (fnames) > 0L) {
        paths <- apply (fnames, 1, function (f) {
            fn <- paste0 ("ex_", gsub ("\\-", "_", f [1]))
            do.call (fn, list (pkg_nms = pkg_nms, fname = f [2]))
        })
    }

    invisible (fnames_full)
}

ex_embeddings <- function (pkg_nms, fname) {
    expected_embedding_length <- 768L
    n <- length (pkg_nms)
    dat_nms <- c ("text_with_fns", "text_wo_fns", "code")
    dat <- lapply (seq_along (dat_nms), function (i) {
        m <- matrix (stats::runif (n * expected_embedding_length), ncol = n)
        colnames (m) <- pkg_nms
        return (m)
    })
    names (dat) <- dat_nms

    saveRDS (dat, fname)
    return (fname)
}

ex_bm25 <- function (pkg_nms, fname) {
    words <- ex_words ()
    with_fns <- lapply (seq_along (pkg_nms), function (p) {
        data.frame (
            token = words,
            n = as.integer (ceiling (stats::rgamma (length (words), shape = 1)))
        )
    })
    wo_fns <- lapply (seq_along (pkg_nms), function (p) {
        data.frame (
            token = words,
            n = as.integer (ceiling (stats::rgamma (length (words), shape = 1)))
        )
    })
    names (with_fns) <- names (wo_fns) <- pkg_nms
    token_lists <- list (with_fns = with_fns, wo_fns = wo_fns)

    idfs <- list (
        with_fns = data.frame (
            token = words,
            idf = 10 - stats::rgamma (length (words), shape = 1)
        ) |> dplyr::arrange (dplyr::desc (idf)),
        wo_fns = data.frame (
            token = words,
            idf = 10 - stats::rgamma (length (words), shape = 1)
        ) |> dplyr::arrange (dplyr::desc (idf))
    )

    dat <- list (
        idfs = idfs,
        token_lists = token_lists
    )

    saveRDS (dat, fname)
    return (fname)
}

# Grab example vector of words for bm25 data:
ex_words <- function () {
    txt <- get_pkg_text ("curl")
    txt <- gsub ("\\n|#+|[[:punct:]]", "", txt)
    words <- strsplit (txt, "\\s+") [[1]]
    words [which (nzchar (words))]
}

ex_idfs_fn_calls <- function (pkg_nms, fname) {
    ip <- data.frame (utils::installed.packages ())
    fns <- lapply (seq_len (nrow (ip)), function (i) {
        ns <- tryCatch (
            parseNamespaceFile (ip$Package [i], ip$LibPath [i]),
            error = function (e) NULL
        )
        if (!is.null (ns)) {
            ns <- ns$exports
        }
        res <- NULL
        if (length (ns) > 0) {
            res <- data.frame (
                token = paste0 (ip$Package [i], "::", ns),
                idf = 10 - stats::rgamma (length (ns), shape = 1)
            )
        }
        return (res)
    })
    fns <- do.call (rbind, fns)

    saveRDS (fns, fname)
    return (fname)
}

ex_fn_calls <- function (pkg_nms, fname) {
    ip <- data.frame (utils::installed.packages ())

    tags <- NULL
    while (is.null (tags)) {
        i <- sample (nrow (ip), size = 1L)
        pkg <- ip$Package [i]
        tags <- tryCatch (
            pkgmatch_treesitter_fn_tags (pkg),
            error = function (e) NULL
        )
    }
    fns <- tags$name [which (!grepl (paste0 ("^", pkg), tags$name))]
    fn_tbl <- table (fns)

    fn_calls <- lapply (pkg_nms, function (i) fn_tbl)
    names (fn_calls) <- pkg_nms

    saveRDS (fn_calls, fname)
    return (fname)
}
