devtools::load_all (".", export_all = TRUE, helpers = FALSE)
# library (pkgmatch)
cli::cli_inform ("Is ollama up and running?")
ollama_check ()
options ("rlib_message_verbosity" = "verbose")

path <- "/<path>/<to>/<ropensci>/<repos>"
packages <- fs::dir_ls (path, type = "directory")

# ----------------- EMBEDDINGS FOR ROPENSCI -----------------
cli::cli_h1 ("rOpenSci package embeddings")
f <- "embeddings-ropensci.Rds"
if (!fs::file_exists (f)) {
    embeddings <- pkgmatch_embeddings_from_pkgs (packages)
    saveRDS (embeddings, f)
} else {
    cli::cli_inform ("skipping coz already done.")
}

cli::cli_h1 ("rOpenSci function embeddings")
f <- "embeddings-fns.Rds"
if (!fs::file_exists (f)) {
    embeddings_fns <-
        pkgmatch_embeddings_from_pkgs (packages, functions_only = TRUE)
    saveRDS (embeddings_fns, f)
} else {
    cli::cli_inform ("skipping coz already done.")
}

# -------------------- BM25 FOR ROPENSCI --------------------
cli::cli_h1 ("rOpenSci BM25")
f <- c ("bm25-ropensci.Rds", "bm25-ropensci-fns.Rds")
if (!all (fs::file_exists (f))) {
    num_cores <- parallel::detectCores () - 2L
    cl <- parallel::makeCluster (num_cores)
    txt_with_fns <- pbapply::pblapply (
        packages,
        function (p) pkgmatch:::get_pkg_text (p),
        cl = cl
    )
    parallel::stopCluster (cl)

    txt_wo_fns <- rm_fns_from_pkg_txt (txt_with_fns)
    idfs <- list (
        with_fns = bm25_idf (txt_with_fns),
        wo_fns = bm25_idf (txt_wo_fns)
    )
    token_lists <- list (
        with_fns = bm25_tokens_list (txt_with_fns),
        wo_fns = bm25_tokens_list (txt_wo_fns)
    )
    bm25_data <- list (idfs = idfs, token_lists = token_lists)
    saveRDS (bm25_data, f [1])

    txt_fns <- get_all_fn_descs (txt_with_fns)
    fns_idfs <- bm25_idf (txt_fns$desc)
    fns_lists <- bm25_tokens_list (txt_fns$desc)
    index <- which (vapply (fns_lists, nrow, integer (1L)) > 0L)
    fns_lists <- fns_lists [index]
    names (fns_lists) <- txt_fns$fn [index]
    bm25_data <- list (idfs = fns_idfs, token_lists = fns_lists)
    saveRDS (bm25_data, f [2])
} else {
    cli::cli_inform ("skipping coz already done.")
}

# ------------------ FN CALLS FOR ROPENSCI ------------------
cli::cli_h1 ("rOpenSci function BM25s")
f <- c ("fn-calls-ropensci.Rds", "idfs-fn-calls-ropensci.Rds")
if (!all (fs::file_exists (f))) {
    flist <- fs::dir_ls (path, recurse = FALSE)
    num_cores <- parallel::detectCores () - 2L
    cl <- parallel::makeCluster (num_cores)

    calls <- pbapply::pblapply (flist, function (f) {
        res <- tryCatch (
            pkgmatch::pkgmatch_treesitter_fn_tags (f),
            error = function (e) NULL
        )
        if (is.null (res)) {
            res <- data.frame (name = character (0L))
        }
        sort (table (res$name), decreasing = TRUE)
    }, cl = cl)

    parallel::stopCluster (cl)
    names (calls) <- basename (names (calls))
    index <- which (vapply (calls, length, integer (1L)) > 0)
    calls <- calls [index]

    saveRDS (calls, f [1])

    # Then remove self-calls:
    calls <- lapply (seq_along (calls), function (i) {
        this_pkg <- names (calls) [i]
        ptn <- paste0 ("^", this_pkg, "\\:\\:")
        index <- which (!grepl (ptn, names (calls [[i]])))
        names (calls [[i]]) [index]
    })

    # And convert to inverse doc freqs:
    tokens_idf <- data.frame (
        token = unique (unlist (calls)),
        n = 0L
    )
    for (i in seq_along (calls)) {
        index <- match (calls [[i]], tokens_idf$token)
        tokens_idf$n [index] <- tokens_idf$n [index] + 1L
    }
    n_docs <- length (calls)
    tokens_idf$idf <- log ((n_docs - tokens_idf$n + 0.5) / (tokens_idf$n + 0.5) + 1)
    tokens_idf$n <- NULL

    saveRDS (tokens_idf, f [2])
} else {
    cli::cli_inform ("skipping coz already done.")
}


# -------------------- EMBEDDINGS FOR CRAN --------------------
options ("rlib_message_verbosity" = "verbose")
path <- "/<path>/<to>/<cran-mirror>/tarballs"
packages <- fs::dir_ls (path, regexp = "\\.tar\\.gz$")

extract_packages <- function (packages) {
    path <- fs::path_common (packages)
    path_exdir <- fs::path (fs::path_dir (path), "temp")
    if (!fs::dir_exists (path_exdir)) {
        fs::dir_create (path_exdir)
    }

    pkgs_tarballs <- grep ("\\.tar\\.gz$", packages, value = TRUE)
    pkgs_names <- gsub ("\\_.*$", "", basename (pkgs_tarballs))
    pkgs_exdir <- fs::path (path_exdir, pkgs_names)
    pkgs_exdir_ls <- fs::dir_ls (path_exdir, type = "directory")
    index <- which (!pkgs_exdir %in% pkgs_exdir_ls)
    pkgs_todo <- pkgs_exdir [index]
    npkgs <- format (length (pkgs_todo), big.mark = ",")
    if (npkgs > 0L) {
        cli::cli_inform ("Extracting {npkgs} CRAN tarballs ...")
    }

    # Single-thread to avoid any race issues on disk writes:
    pkgs_tmp <- pbapply::pblapply (packages [index], function (f) {
        pkg_name <- basename (gsub ("\\_.*$", "", f))
        exdir <- fs::path (path_exdir, pkg_name)
        if (!fs::dir_exists (exdir)) {
            exdir <- pkgmatch:::extract_tarball (f, path_exdir)
        }
        return (exdir)
    })

    fs::dir_ls (path_exdir, type = "directory")
}

cli::cli_h1 ("CRAN package embeddings")
f <- "embeddings-cran.Rds"
path_exdir <- NULL
if (!fs::file_exists (f)) {

    packages <- extract_packages (packages)

    embeddings <- pkgmatch_embeddings_from_pkgs (packages)

    # Fn to reduce names and remove any duplicate packages (owing to multiple
    # versions in tarball dir):
    rename_cols <- function (e) {
        nms_full <- basename (colnames (e))
        nms <- gsub ("\\_.*$", "", nms_full)
        dups <- nms [which (duplicated (nms))]
        if (length (dups) > 0L) {
            index <- match (dups, nms)
            e <- e [, -index]
            nms_full <- nms_full [-index]
        }
        colnames (e) <- nms_full

        return (e)
    }
    embeddings$text_with_fns <- rename_cols (embeddings$text_with_fns)
    embeddings$text_wo_fns <- rename_cols (embeddings$text_wo_fns)
    embeddings$code <- rename_cols (embeddings$code)

    saveRDS (embeddings, f)
} else {
    cli::cli_inform ("skipping coz already done.")
}

# -------------------- BM25 FOR CRAN --------------------
cli::cli_h1 ("CRAN BM25")
f <- "bm25-cran.Rds"
if (!fs::file_exists (f)) {
    packages <- extract_packages (packages)
    npkgs <- format (length (packages), big.mark = ",")

    cli::cli_inform ("Extract text from {npkgs} CRAN packages ...")
    num_cores <- parallel::detectCores () - 2L
    cl <- parallel::makeCluster (num_cores)

    txt_with_fns <- pbapply::pblapply (
        packages,
        function (p) pkgmatch:::get_pkg_text (p),
        cl = cl
    )
    parallel::stopCluster (cl)

    txt_wo_fns <- rm_fns_from_pkg_txt (txt_with_fns)
    idfs <- list (
        with_fns = bm25_idf (txt_with_fns),
        wo_fns = bm25_idf (txt_wo_fns)
    )
    token_lists <- list (
        with_fns = bm25_tokens_list (txt_with_fns),
        wo_fns = bm25_tokens_list (txt_wo_fns)
    )
    rename_lists <- function (ll) {
        nms_full <- basename (names (ll))
        nms <- gsub ("\\_.*$", "", nms_full)
        dups <- nms [which (duplicated (nms))]
        if (length (dups) > 0L) {
            index <- match (dups, nms)
            ll <- ll [-index]
            nms_full <- nms_full [-index]
        }
        names (ll) <- nms_full

        return (ll)

    }
    token_lists$with_fns <- rename_lists (token_lists$with_fns)
    token_lists$wo_fns <- rename_lists (token_lists$wo_fns)
    bm25_data <- list (idfs = idfs, token_lists = token_lists)
    saveRDS (bm25_data, f)
} else {
    cli::cli_inform ("skipping coz already done.")
}

# ------------------ FN CALLS FOR CRAN ------------------
#
# When run separately, these fail because the untar -> treeistter -> rm steps
# are so quick that they can create race conditions on disk I/O which cause
# process failures. The only safe way is to first extract all in a single
# thread, and then extract the function call tags on extracted directories.
cli::cli_h1 ("CRAN function calls")
f <- c ("fn-calls-cran.Rds", "idfs-fn-calls-cran.Rds")
if (!all (fs::file_exists (f))) {

    packages <- extract_packages (packages)
    npkgs <- format (length (packages), big.mark = ",")

    cli::cli_inform ("Extract function calls from {npkgs} CRAN packages ...")
    num_cores <- parallel::detectCores () - 1L
    cl <- parallel::makeCluster (num_cores)

    calls <- pbapply::pblapply (packages, function (f) {
        # This can fail because it calls RSearch:
        res <- NULL
        ntries <- 0L
        while (is.null (res)) {
            res <- tryCatch (
                pkgmatch::pkgmatch_treesitter_fn_tags (f),
                error = function (e) NULL
            )
            ntries <- ntries + 1L
            if (ntries > 5L) {
                break
            }
        }
        if (is.null (res)) {
            res <- data.frame (name = character (0L))
        }

        sort (table (res$name), decreasing = TRUE)
    }, cl = cl)

    parallel::stopCluster (cl)

    names (calls) <- basename (names (calls))
    index <- which (vapply (calls, length, integer (1L)) > 0)
    calls <- calls [index]

    # Rm any duplicated packages (with different versions)
    nms <- gsub ("\\_.*$", "", names (calls))
    dups <- nms [which (duplicated (nms))]
    if (length (dups) > 0L) {
        index <- match (dups, nms)
        calls <- calls [-index]
    }
    saveRDS (calls, f [1])

    # Then remove self-calls:
    calls <- lapply (seq_along (calls), function (i) {
        this_pkg <- names (calls) [i]
        ptn <- paste0 ("^", this_pkg, "\\:\\:")
        index <- which (!grepl (ptn, names (calls [[i]])))
        names (calls [[i]]) [index]
    })

    # And convert to inverse doc freqs:
    tokens_idf <- data.frame (
        token = unique (unlist (calls)),
        n = 0L
    )
    for (i in seq_along (calls)) {
        index <- match (calls [[i]], tokens_idf$token)
        tokens_idf$n [index] <- tokens_idf$n [index] + 1L
    }
    n_docs <- length (calls)
    tokens_idf$idf <- log ((n_docs - tokens_idf$n + 0.5) / (tokens_idf$n + 0.5) + 1)
    tokens_idf$n <- NULL

    saveRDS (tokens_idf, f [2])
} else {
    cli::cli_inform ("skipping coz already done.")
}

# exidr is not cleaned up, because it can cause core dump
path_exdir <- fs::path_common (packages)
# if (fs::dir_exists (path_exdir) && identical (basename (path_exdir), "temp")) {
#     fs::dir_delete (path_exdir)
# }
