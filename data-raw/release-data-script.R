devtools::load_all (".", export_all = TRUE, helpers = FALSE)
# library (pkgmatch)
options ("rlib_message_verbosity" = "verbose")

path <- "/<path>/<to>/ropensci/"
packages <- fs::dir_ls (path, type = "directory")

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
    idfs <- bm25_idf (txt_wo_fns)
    token_lists <- bm25_tokens_list (txt_wo_fns)
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


# -------------------- BM25 FOR CRAN --------------------
options ("rlib_message_verbosity" = "verbose")
path <- "/<path>/<to>/<cran-mirror>/tarballs"
packages <- fs::dir_ls (path, regexp = "\\.tar\\.gz$")

move_obsolete_pkgs <- function (path) {

    packages <- fs::dir_ls (path, regexp = "\\.tar\\.gz$")
    pkgs_tarballs <- grep ("\\.tar\\.gz$", packages, value = TRUE)
    pkgs_names <- gsub ("\\_.*$", "", basename (pkgs_tarballs))
    # Rm any old pkg versions:
    nms_dup <- pkgs_names [which (duplicated (pkgs_names))]
    if (length (nms_dup) > 0L) {
        dups_to_rm <- unlist (lapply (nms_dup, function (n) {
            tb <- pkgs_tarballs [which (pkgs_names == n)]
            tb [tb < max (tb)]
        }))
        cli::cli_alert_danger ("Moving {length (dups_to_rm)} obsolete tarballs to Archive ...")
        fs::file_move (dups_to_rm, fs::path (path, "Archive"))
    }
    fs::dir_ls (path, regexp = "\\.tar\\.gz$")
}
packages <- move_obsolete_pkgs (path)

extract_packages <- function (packages) {
    path <- fs::path_common (packages)
    path_exdir <- fs::path (fs::path_dir (path), "temp")
    if (!fs::dir_exists (path_exdir)) {
        fs::dir_create (path_exdir)
    }

    pkgs_tarballs <- grep ("\\.tar\\.gz$", packages, value = TRUE)
    pkgs_names <- gsub ("\\_.*$", "", basename (pkgs_tarballs))
    pkgs_versions <- gsub ("^.*\\_|\\.tar\\.gz$", "", basename (pkgs_tarballs))
    pkgs_exdir <- fs::path (path_exdir, pkgs_names)
    pkgs_exdir_ls <- fs::dir_ls (path_exdir, type = "directory")
    index <- which (!pkgs_exdir %in% pkgs_exdir_ls)
    pkgs_todo <- pkgs_exdir [index]
    npkgs <- format (length (pkgs_todo), big.mark = ",")
    if (length (pkgs_todo) > 0L) {
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

    pkgs <- fs::dir_ls (path_exdir, type = "directory")
    index <- which (pkgs_names %in% basename (pkgs))
    data.frame (dir = pkgs, version = pkgs_versions [index], row.names = NULL)
}

cli::cli_h1 ("CRAN BM25")
f <- "bm25-cran.Rds"
if (!fs::file_exists (f)) {
    packages <- extract_packages (packages)
    npkgs <- format (nrow (packages), big.mark = ",")

    cli::cli_inform ("Extract text from {npkgs} CRAN packages ...")
    num_cores <- parallel::detectCores () - 2L
    cl <- parallel::makeCluster (num_cores)

    txt_with_fns <- pbapply::pblapply (
        packages$dir,
        function (p) pkgmatch:::get_pkg_text (p),
        cl = cl
    )
    parallel::stopCluster (cl)

    index <- which (vapply (txt_with_fns, nzchar, logical (1L)))
    packages <- packages [index, ]
    names (txt_with_fns) <- paste0 (basename (packages$dir), "_", packages$version)

    txt_wo_fns <- rm_fns_from_pkg_txt (txt_with_fns)
    idfs <- bm25_idf (txt_wo_fns)
    token_lists <- bm25_tokens_list (txt_wo_fns)

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
packages <- fs::dir_ls (path, regexp = "\\.tar\\.gz$")
f <- c ("fn-calls-cran.Rds", "idfs-fn-calls-cran.Rds")
if (!all (fs::file_exists (f))) {

    packages <- extract_packages (packages)
    npkgs <- format (nrow (packages), big.mark = ",")

    cli::cli_inform ("Extract function calls from {npkgs} CRAN packages ...")
    num_cores <- parallel::detectCores () - 1L
    cl <- parallel::makeCluster (num_cores)

    calls <- pbapply::pblapply (packages$dir, function (f) {
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

    n <- vapply (calls, length, integer (1L))
    index <- which (n > 0L)
    packages <- packages [index, ]
    calls <- calls [index]
    names (calls) <- paste0 (basename (packages$dir), "_", packages$version)

    saveRDS (calls, f [1])

    # Then remove self-calls:
    calls <- lapply (seq_along (calls), function (i) {
        this_pkg <- gsub ("\\_.*", "", names (calls) [i])
        ptn <- paste0 ("^", this_pkg, "\\:\\:")
        index <- which (!grepl (ptn, names (calls [[i]])))
        calls [[i]] [index]
    })

    # And convert to inverse doc freqs:
    tokens_idf <- data.frame (
        token = unique (names (unlist (calls))),
        n = 0L
    )
    for (i in seq_along (calls)) {
        index <- match (names (calls [[i]]), tokens_idf$token)
        tokens_idf$n [index] <- tokens_idf$n [index] + as.integer (calls [[i]])
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
