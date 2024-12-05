#' Generate all data necessary to run "pkgmatch" on a local corpus of R
#' packages.
#'
#' The function generates and stores required data in local cache directory,
#' determined by environment variable "PKGMATCH_CACHE_DIR" if that exists,
#' otherwise default of `file.path(rappdirs::user_cache_dir(), "R",
#' "pkgmatch")`.
#'
#' @param path Path to a local directory containing R packages, each in its own
#' sub-directory.
#' @param corpus Name of corpus. This is used to name resultant data files.
#' @param fns Should embeddings also be generated for individual functions of
#' each package? For large corpora, the resultant files can be prohibitively
#' large, and so this should only be set to `TRUE` for sufficiently small
#' corpora. The rOpenSci corpus is under 400 packages, and function-level
#' embeddings are possible, while CRAN with over 20,000 packages is too large.
#' @param num_cores Number of cores to be used in parallel computation of data.
#' @return A list of 5-6 newly-created and cached data files, each in `.Rds`
#' format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- "<path>/<to>/<several>/<R-pacakges>"
#' pkgmatch_generate_data (path)
#' }
pkgmatch_generate_data <- function (path,
                                    corpus = NULL,
                                    fns = FALSE,
                                    num_cores = parallel::detectCores ()) {

    checkmate::assert_character (corpus, len = 1L)
    checkmate::assert_directory_exists (path)
    packages <- fs::dir_ls (path, type = "directory", recurse = FALSE)
    cache_dir <- pkgmatch_cache_path ()

    f1 <- generate_embeddings (pacakges, corpus) # one file
    if (fns) {
        f2 <- generate_fn_embeddings (packages, corpus) # one file
    }
    f3 <- generate_bm25_data (path, corpus, fns, num_cores) # 1 or 2 files
    f4 <- generate_fn_call_data (path, corpus, num_cores) # 2 files

    return (c (f1, f2, f3, f4))
}

generate_embeddings <- function (pacakges, corpus) {

    cli::cli_alert_info ("Generating package embeddings ...")
    embeddings <- pkgmatch_embeddings_from_pkgs (packages)
    f <- fs::path (cache_dir, paste0 ("embeddings-", corpus, ".Rds"))
    saveRDS (embeddings, f)
    cli::cli_alert_success ("Done")
    cli::cli_inform ("")

    return (f)
}

generate_fn_embeddings <- function (pacakges, corpus) {

    cli::cli_alert_info ("Generating function-level embeddings ...")

    embeddings_fns <-
        pkgmatch_embeddings_from_pkgs (packages, functions_only = TRUE)
    f <- fs::path (cache_dir, paste0 ("embeddings-fns-", corpus, ".Rds"))
    saveRDS (embeddings_fns, f)
    cli::cli_alert_success ("Done")
    cli::cli_inform ("")

    return (f)
}

generate_bm25_data <- function (package, corpus, fns, num_cores) {

    cli::cli_alert_info ("Generating BM25 values ...")
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
    ret <- f <- fs::path (cache_dir, paste0 ("bm25-", corpus, ".Rds"))
    saveRDS (bm25_data, f)

    if (fns) {
        txt_fns <- get_all_fn_descs (txt_with_fns)
        fns_idfs <- bm25_idf (txt_fns$desc)
        fns_lists <- bm25_tokens_list (txt_fns$desc)
        index <- which (vapply (fns_lists, nrow, integer (1L)) > 0L)
        fns_lists <- fns_lists [index]
        names (fns_lists) <- txt_fns$fn [index]
        bm25_data <- list (idfs = fns_idfs, token_lists = fns_lists)
        f <- fs::path (cache_dir, paste0 ("bm25-", corpus, "-fns.Rds"))
        saveRDS (bm25_data, f)
        ret <- c (ret, f)
    }
    cli::cli_alert_success ("Done")
    cli::cli_inform ("")

    return (ret)
}

generate_fn_call_data <- function (package, corpus, num_cores) {

    cli::cli_alert_info ("Generating function call data ...")
    cl <- parallel::makeCluster (num_cores)
    calls <- pbapply::pblapply (packages, function (f) {
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

    f1 <- fs::path (cache_dir, paste0 ("fn-calls-", corpus, ".Rds"))
    saveRDS (calls, f1)
    cli::cli_alert_success ("Done")
    cli::cli_inform ("")

    cli::cli_alert_info ("Generating IDFs for function calls ...")
    # Remove self-calls:
    calls <- lapply (seq_along (calls), function (i) {
        this_pkg <- names (calls) [i]
        ptn <- paste0 ("^", this_pkg, "\\:\\:")
        index <- which (!grepl (ptn, names (calls [[i]])))
        names (calls [[i]]) [index]
    })

    # Convert to inverse doc freqs:
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

    f2 <- fs::path (cache_dir, paste0 ("idfs-fn-calls-", corpus, ".Rds"))
    saveRDS (tokens_idf, f2)
    cli::cli_alert_success ("Done")
    cli::cli_inform ("")

    return (c (f1, f2))
}
