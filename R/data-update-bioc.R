#' Generate pkgmatch` data for BioConductor packages on GitHub release
#'
#' BioConductor packages are fixed to a particular global BioConductor release,
#' and are not individually updated. Therefore, unlike CRAN and rOpenSci, this
#' is a "generate" function only, and not an "update function". It requires a
#' full local clone of all BioConductor repositories, and will generate all
#' embeddings.
#'
#' @param local_mirror_path Optional path to a local directory with full
#' BioConductor mirror. If specified, data will use packages from this local
#' source for updating. Default behaviour if not specified is to download new
#' packages into tempdir, and delete once data have been updated.
#' @noRd

# nocov start
pkgmatch_generate_bioc <- function (local_mirror_path = NULL) {

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
                extract_data_from_local_dir (pkg_dir),
                error = function (e) NULL
            )
            fns <- tryCatch (
                pkgmatch_embeddings_from_pkgs (pkg_dir, functions_only = TRUE),
                error = function (e) NULL
            )
        }

        if (!op_is_quiet) {
            pkgmatch_update_progress_message (
                i, 1, nrow (pkgs), pt0, op_is_quiet
            )
        }

        return (list (dat = dat, fns = fns))
    })

    fns <- do.call (cbind, lapply (res, function (i) i$fns))
    res <- lapply (res, function (i) i$dat)
    names (res) <- pkgs$package

    cache_path <- fs::path_real (rappdirs::user_cache_dir (c ("R", "pkgmatch")))
    save_one <- function (cache_path, dat, filename) {
        f <- fs::path (cache_path, filename)
        saveRDS (dat, f)
        return (f)
    }
    flist <- save_one (cache_path, fns, "embeddings-fns-bioc.Rds")

    bm25 <- lapply (res, function (i) i$bm25)
    flist <- c (flist, save_one (cache_path, bm25, "bm25-bioc.Rds"))

    bm25_fns <- lapply (res, function (i) i$bm25_fns)
    flist <- c (flist, save_one (cache_path, bm25_fns, "bm25-bioc-fns.Rds"))

    embeddings <- list (
        text_with_fns = do.call (cbind, lapply (res, function (i) {
            i$embeddings$text_with_fns
        })),
        text_wo_fns = do.call (cbind, lapply (res, function (i) {
            i$embeddings$text_wo_fns
        })),
        code = do.call (cbind, lapply (res, function (i) {
            i$embeddings$code
        }))
    )
    flist <- c (flist, save_one (cache_path, embeddings, "embeddings-bioc.Rds"))

    fn_calls <- lapply (res, function (i) i$fn_calls)
    flist <- c (flist, save_one (cache_path, fn_calls, "fn-calls-bioc.Rds"))

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
    flist <- c (
        flist,
        save_one (cache_path, toks_idf, "idfs-fn-calls-bioc.Rds")
    )

    cli::cli_h2 (
        "Uploading BioConductor embeddings to GitHub release {RELEASE_TAG}"
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
# nocov end
