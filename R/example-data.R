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
#' @param corpus One of "ropensci" or "cran", where "ropensci" generates
#' additional data on function call frequencies.
#' @return (Invisibly) The path to the temporary directory containing the
#' package data.
#' @family utils
#' @export
#' @examples
#' generate_pkgmatch_example_data ()
#' input <- "curl" # Name of a single installed package
#' pkgmatch_similar_pkgs (input, corpus = "cran")
generate_pkgmatch_example_data <- function (corpus = "cran") {

    if (interactive ()) {
        cli::cli_inform ("This function resets the cache directory used by 'pkgmatch'")
        cli::cli_inform ("to a temporary path. To restore functionality with full data, ")
        cli::cli_inform ("you'll either need to restart your R session, or set an ")
        cli::cli_inform ("environment variable named 'PKGMATCH_CACHE_DIR' to the ")
        cli::cli_inform ("desired path. Default path is {pkgmatch_cache_path()}")
    }

    ex_dir <- fs::path (fs::path_temp (), "pkgmatch_ex_data")
    if (!fs::dir_exists (ex_dir)) {
        fs::dir_create (ex_dir)
    }

    Sys.setenv ("PKGMATCH_CACHE_DIR" = ex_dir)
    options ("pkgmatch.example_env" = "true")

    fname <- paste0 ("bm25-", corpus)
    fnames <- paste0 (c (fname, paste0 (fname, "-fns")), ".Rds")
    fn_names <- c ("bm25", "bm25_fns")
    if (corpus == "ropensci") {
        fnames <- c (fnames, "fn-calls-ropensci.Rds")
        fn_names <- c (fn_names, "fn_calls")
    }
    fnames_full <- fs::path (ex_dir, fnames)
    index <- which (!fs::file_exists (fnames_full))
    fnames <- data.frame (name = fn_names, path = fnames_full) [index, ]

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

ex_bm25 <- function (pkg_nms, fname) {
    # Suppress no visible binding note:
    idf <- NULL

    words <- ex_words ()
    token_lists <- lapply (seq_along (pkg_nms), function (p) {
        data.frame (
            token = words,
            n = as.integer (ceiling (stats::rgamma (length (words), shape = 1)))
        )
    })
    names (token_lists) <- pkg_nms

    idfs <- data.frame (
        token = words,
        idf = 10 - stats::rgamma (length (words), shape = 1)
    ) |> dplyr::arrange (dplyr::desc (idf))

    dat1 <- list (
        idfs = idfs,
        token_lists = token_lists
    )
    dat <- list (full = dat1, descs_only = dat1)

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

ex_bm25_fns <- function (pkg_nms, fname) {

    sample_pkgs <- c (
        "curl",
        "cli",
        "fs",
        "httr",
        "memoise",
        "tokenizers"
    )

    fns <- lapply (sample_pkgs, function (p) {

        lp <- .libPaths ()
        index <- vapply (.libPaths (), function (i) {
            fs::dir_exists (fs::path (i, p))
        }, logical (1L))
        if (!any (index)) {
            return (NULL)
        }
        lp <- lp [which (index)] [1L]
        ns <- tryCatch (
            parseNamespaceFile (p, lp),
            error = function (e) NULL
        )
        if (!is.null (ns)) {
            ns <- ns$exports
        }
        res <- NULL
        if (length (ns) > 0) {
            res <- data.frame (
                token = paste0 (p, "::", ns),
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

    # suppress no visible binding notes:
    token <- n <- NULL

    sample_pkgs <- c (
        "curl",
        "cli",
        "fs",
        "httr",
        "memoise",
        "tokenizers"
    )

    tags <- lapply (sample_pkgs, function (p) {
        tryCatch (
            suppressMessages (
                pkgmatch_treesitter_fn_tags (p)
            ),
            error = function (e) NULL
        )
    })

    fn_calls <- lapply (tags, function (p) {
        out <- table (p$name)
        data.frame (token = names (out), n = as.integer (out))
    })
    names (fn_calls) <- sample_pkgs

    n_docs <- length (sample_pkgs)
    tokens_idf <- do.call (rbind, lapply (tags, function (i) {
        data.frame (token = unique (i$name), n = 1L)
    })) |>
        dplyr::group_by (token) |>
        dplyr::summarise (n = dplyr::n ()) |>
        dplyr::mutate (idf = log ((n_docs - n + 0.5) / (n + 0.5) + 1)) |>
        dplyr::select (-n)

    dat <- list (
        idfs = tokens_idf,
        calls = fn_calls
    )

    saveRDS (dat, fname)

    return (fname)
}
