#' Generate pkgmatch` data for BioConductor packages on GitHub release
#'
#' BioConductor packages are fixed to a particular global BioConductor release,
#' and are not individually updated. Therefore, unlike CRAN and rOpenSci, this
#' is a "generate" function only, and not an "update function". It requires a
#' full local clone of all BioConductor repositories.
#'
#' @param local_mirror_path Optional path to a local directory with full
#' BioConductor mirror. If specified, data will use packages from this local
#' source for updating. Default behaviour if not specified is to download new
#' packages into tempdir, and delete once data have been updated.
#' @param minchar Minimal number of characters; tokens with less than this
#' number are discarded.
#'
#' @noRd

# nocov start
pkgmatch_generate_bioc <- function (local_mirror_path = NULL, minchar = 3L) {

    checkmate::assert_directory_exists (local_mirror_path)

    requireNamespace ("gert", quietly = TRUE)
    requireNamespace ("jsonlite", quietly = TRUE)

    local_mirror_root <- fs::path_dir (local_mirror_path)
    pkgs_json <- fs::dir_ls (
        local_mirror_root,
        type = "file",
        regexp = "packages\\.json$"
    )
    pkgs_json <- grep ("bioc", pkgs_json, value = TRUE)
    pkgs <- jsonlite::read_json (pkgs_json, simplifyVector = TRUE)

    pt0 <- proc.time ()
    op_is_quiet <- opt_is_quiet ()
    op <- getOption ("rlib_message_verbosity")
    options ("rlib_message_verbosity" = "quiet")

    res <- lapply (seq_len (nrow (pkgs)), function (i) {
        pkg_dir <- fs::path (local_mirror_path, pkgs$package [i])
        dat <- fns <- NULL

        if (fs::dir_exists (pkg_dir)) {

            dat <- tryCatch (
                extract_data_from_local_dir (pkg_dir, minchar = minchar),
                error = function (e) NULL
            )
        }

        if (!op_is_quiet) {
            pkgmatch_update_progress_message (
                i, 1, nrow (pkgs), pt0, op_is_quiet
            )
        }

        return (dat)
    })

    index <- which (!vapply (res, is.null, logical (1L)))
    res <- res [index]
    names (res) <- pkgs$package [index]

    cache_path <- fs::path_real (rappdirs::user_cache_dir (c ("R", "pkgmatch")))
    save_one <- function (cache_path, dat, filename) {
        f <- fs::path (cache_path, filename)
        saveRDS (dat, f)
        return (f)
    }

    bm25 <- lapply (res, function (i) i$bm25)
    bm25 <- make_bm25_bioc (bm25)
    flist <- save_one (cache_path, bm25, "bm25-bioc.Rds")

    fn_toks <- lapply (res, function (i) i$bm25_fns)
    token_lists <- lapply (fn_toks, function (i) i$token_lists)
    token_lists <- do.call (c, unname (token_lists))
    # Create idfs:
    toks_all <- lapply (token_lists, function (i) i$token)
    n_docs <- length (token_lists)
    idfs <- tok_lists_to_idfs (toks_all, n_docs)
    bm25_fns <- list (idfs = idfs, token_lists = token_lists)

    flist <- c (flist, save_one (cache_path, bm25_fns, "bm25-bioc-fns.Rds"))

    fn_calls <- lapply (res, function (i) i$fn_calls)
    n_docs <- length (fn_calls)
    toks_all <- lapply (fn_calls, function (i) names (i))
    toks_all <- unlist (unname (toks_all))
    # Reduce only to calls able to be assigned to namespaces:
    toks_all <- grep ("\\:\\:", toks_all, value = TRUE)
    # and also remove any extraneous ticks:
    toks_all <- gsub ("\\`|\\'", "", toks_all)
    toks_all <- toks_all [which (!toks_all == "::")]

    toks_tab <- table (toks_all)
    toks_n <- as.integer (toks_tab)
    idf <- unname (log ((n_docs - toks_n + 0.5) / (toks_n + 0.5) + 1))
    toks_idf <- data.frame (
        token = names (toks_tab),
        idf = idf
    )

    out <- list (idfs = toks_idf, calls = fn_calls)
    flist <- c (
        flist,
        save_one (cache_path, out, "fn-calls-bioc.Rds")
    )

    cli::cli_h2 (
        "Uploading BioConductor data to GitHub release {RELEASE_TAG}"
    )
    for (i in flist) {
        piggyback::pb_upload (
            file = i,
            repo = "ropensci-review-tools/pkgmatch",
            tag = RELEASE_TAG
        )
    }

    options ("rlib_message_verbosity" = op)

    return (TRUE)
}

#' Adapted from `append_data-to_bm25()` from `data-update.R`:
#'
#' @param bm25 The 'bm25' component each package, extracted in main function
#' from `lapply(res, function (i) i$bm25))`
#' @noRd
make_bm25_bioc <- function (bm25) {

    not_null_index <- which (vapply (
        bm25,
        function (i) length (i$token_lists) > 0,
        logical (1L)
    ))
    token_lists <- lapply (bm25 [not_null_index], function (i) i$token_lists [[1]])
    names (token_lists) <- names (bm25) [not_null_index]

    n_docs <- length (token_lists)

    toks_all <- lapply (
        token_lists,
        function (i) rep (i$token, times = i$n)
    )
    toks_all <- unlist (unname (toks_all))
    toks_tab <- table (toks_all)
    toks_n <- as.integer (toks_tab)
    idf <- unname (log ((n_docs - toks_n + 0.5) / (toks_n + 0.5) + 1))

    idfs <- data.frame (
        token = names (toks_tab),
        idf = idf
    )
    list (idfs = idfs, token_lists = token_lists)
}
# nocov end
