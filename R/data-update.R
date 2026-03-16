# General functions for both CRAN and rOpenSci update workflows

RELEASE_TAG <- "v0.5.2" # nolint

#' @title Update pkgmatch corpus data on GitHub
#'
#' @description This function is intended for internal rOpenSci use only. Usage
#' by any unauthorized users will error and have no effect unless run with
#' `upload = FALSE`, in which case updated data will be created in the
#' sub-directory "pkgmatch-results" of R's current temporary directory. This
#' updating may take a very long time!
#'
#' The function does not update the BioConductor data. Because those are fixed
#' to specific BioConductor releases, they are only updated manually with the
#' internal `pkgmatch_generate_bioc()` function.
#'
#' Note that this function is categorically different from
#' \link{pkgmatch_update_cache}. This function updates the internal data used
#' by the `pkgmatch` package, and should only ever be run by package
#' maintainers. The \link{pkgmatch_update_cache} downloads the latest versions
#' of these data to a local cache for use in this package.
#'
#' @param upload If `TRUE`, upload updated results to GitHub release.
#' @param local_cran_mirror Optional path to a local directory with full CRAN
#' mirror. If specified, data will use packages from this local source for
#' updating. Default behaviour if not specified is to download new packages
#' into tempdir, and delete once data have been updated.
#' @param local_ropensci_mirror Optional path to a local directory with full
#' rOpenSci mirror. If specified, data will use repositories from this local
#' source for updating. Default behaviour if not specified is to clone new
#' repositories into tempdir, and delete once data have been updated.
#' @return Local path to directory containing updated results.
#' @family data
#' @export
#'
#' @examples
#' \dontrun{
#' pkgmatch_update_data (upload = FALSEE)
#' }
# nocov start
pkgmatch_update_data <- function (upload = TRUE,
                                  local_cran_mirror = NULL,
                                  local_ropensci_mirror = NULL) {

    if (is.null (local_cran_mirror) && is.null (local_ropensci_mirror)) {
        results_path <-
            fs::dir_create (fs::path (fs::path_temp (), "pkgmatch-results"))
    } else {
        results_path <- pkgmatch_cache_path ()
    }
    flist <- dl_prev_data (results_path)
    flist_cran <- unname (grep ("cran", flist, value = TRUE))
    flist_ropensci <- flist [which (!flist %in% flist_cran)]

    updated_cran <- pkgmatch_update_cran (
        flist,
        local_mirror_path = local_cran_mirror
    )
    updated_ros <- pkgmatch_update_ropensci (
        flist,
        local_mirror_path = local_ropensci_mirror
    )

    flist <- NULL
    if (updated_cran) {
        flist <- flist_cran
    }
    if (updated_ros) {
        flist <- c (flist, flist_ropensci)
    }

    if (upload && (updated_cran || updated_ros)) {
        for (i in flist) {
            piggyback::pb_upload (
                file = i,
                repo = "ropensci-review-tools/pkgmatch",
                tag = RELEASE_TAG
            )
        }
    }

    invisible (flist)
}
# nocov end

extract_data_from_local_dir <- function (pkg_dir) {

    txt_with_fns <- get_pkg_text (pkg_dir)
    txt <- rm_fns_from_pkg_txt (txt_with_fns)
    bm25_data <- list (
        idfs = bm25_idf (txt),
        token_lists = bm25_tokens_list (txt)
    )

    fn_calls <- pkgmatch_treesitter_fn_tags (pkg_dir)
    calls <- sort (table (fn_calls$name))

    # bm25 values for function calls. These are only used for rOpenSci, but
    # take no time to calculate, so done for all regardless.
    txt_fns <- get_all_fn_descs (txt_with_fns)
    fns_idfs <- bm25_idf (txt_fns$desc)
    fns_lists <- bm25_tokens_list (txt_fns$desc)
    index <- which (vapply (fns_lists, nrow, integer (1L)) > 0L)
    fns_lists <- fns_lists [index]
    names (fns_lists) <- txt_fns$fn [index]
    bm25_fns <- list (idfs = fns_idfs, token_lists = fns_lists)

    list (
        bm25 = bm25_data,
        bm25_fns = bm25_fns,
        fn_calls = calls
    )
}

append_data_to_bm25 <- function (res, flist, cran = TRUE) {

    fname <- ifelse (cran, "bm25-cran.Rds", "bm25-ropensci.Rds")
    fname <- flist [which (basename (flist) == fname)]
    bm25 <- readRDS (fname)

    not_null_index <- which (vapply (
        res,
        function (i) !is.null (i$bm25$token_lists),
        logical (1L)
    ))

    token_lists_new <- lapply (res, function (i) i$bm25$token_lists [[1]])
    names (token_lists_new) <- names (res)
    token_lists_new <- token_lists_new [not_null_index]

    pkgs_new <- gsub ("\\_.*$", "", names (token_lists_new))
    pkgs_old <- gsub ("\\_.*$", "", names (bm25$token_lists))
    index <- which (!pkgs_old %in% pkgs_new)
    bm25$token_lists <- c (bm25$token_lists [index], token_lists_new)

    if (any (duplicated (names (bm25$token_lists)))) {
        index <- which (!duplicated (names (bm25$token_lists)))
        bm25$token_lists <- bm25$token_lists [index]
    }

    toks_all <- lapply (bm25$token_lists, function (i) i$token)
    n_docs <- length (bm25$token_lists)
    bm25$idfs <- tok_lists_to_idfs (toks_all, n_docs)

    saveRDS (bm25, fname)

    # Then update bm25 for function calls for rOpenSci only:
    if (!cran) {
        fname <- flist [which (basename (flist) == "bm25-ropensci-fns.Rds")]
        bm25 <- readRDS (fname)

        # Remove updated packages from token lists:
        updated_pkgs <- names (res)
        ptn <- paste0 (
            "^",
            paste0 (updated_pkgs, collapse = "|"),
            paste0 ("\\:\\:")
        )
        index <- which (!grepl (ptn, names (bm25$token_list)))
        bm25$token_lists <- bm25$token_lists [index]

        # Add updated token lists:
        bm25_fns <- lapply (res, function (i) i$bm25_fns)
        token_lists <- lapply (bm25_fns, function (i) i$token_lists)
        token_lists <- do.call (c, unname (token_lists))
        bm25$token_lists <- c (bm25$token_lists, token_lists)

        # And re-create idfs:
        toks_all <- lapply (bm25$token_lists, function (i) i$token)
        n_docs <- length (bm25$token_lists)
        bm25$idfs <- tok_lists_to_idfs (toks_all, n_docs)

        saveRDS (bm25, fname)
    }
}

# Create main 'idfs' tables:
tok_lists_to_idfs <- function (toks_all, n_docs) {
    toks_all <- unlist (unname (toks_all))
    toks_tab <- table (toks_all)
    toks_n <- as.integer (toks_tab)
    idf <- unname (log ((n_docs - toks_n + 0.5) / (toks_n + 0.5) + 1))
    data.frame (
        token = names (toks_tab),
        idf = idf
    )
}

append_data_to_fn_calls <- function (res, flist, cran = TRUE) {

    not_null_index <- which (vapply (
        res, function (i) !is.null (i$fn_calls), logical (1L)
    ))
    fn_calls_new <- lapply (res, function (i) i$fn_calls) [not_null_index]

    fname <- ifelse (cran, "fn-calls-cran.Rds", "fn-calls-ropensci.Rds")
    fname <- flist [which (basename (flist) == fname)]
    fn_calls <- readRDS (fname)

    pkgs_old <- gsub ("\\_.*", "", names (fn_calls))
    pkgs_new <- gsub ("\\_.*", "", names (fn_calls_new))
    index <- which (!pkgs_old %in% pkgs_new)
    fn_calls <- c (fn_calls [index], fn_calls_new)

    index <- order (names (fn_calls))
    fn_calls <- fn_calls [index]
    saveRDS (fn_calls, fname)

    # Then update main 'idfs' table:
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

    fname <- ifelse (
        cran,
        "idfs-fn-calls-cran.Rds",
        "idfs-fn-calls-ropensci.Rds"
    )
    fname <- flist [which (basename (flist) == fname)]
    saveRDS (toks_idf, fname)
}

# nocov start
dl_prev_data <- function (results_path) {

    flist_remote <- list_remote_files ()
    file_names <- flist_remote$file_name
    file_names_done <-
        file_names [which (file_names %in% list.files (results_path))]

    dl_data <- piggyback::pb_download (
        repo = "ropensci-review-tools/pkgmatch",
        dest = results_path,
        tag = RELEASE_TAG,
        ignore = file_names_done
    )

    return (fs::dir_ls (results_path))
}

#' Issue progress message as long as global package-level option is not set to
#' 'quiet'.
#'
#' "rlib_message_verbosity" is set to "quiet" in several internal calls. The
#' `opt_is_quiet` parameter allows progress messages to be issued as long as
#' that option is not globally set.
#'
#' @noRd
pkgmatch_update_progress_message <- function (index, # nolint
                                              chunk_size,
                                              npkgs,
                                              pt0,
                                              op_is_quiet) {

    requireNamespace ("hms", quietly = TRUE)

    prog <- index * chunk_size / npkgs
    prog_fmt <- format (100 * prog, digits = 2)
    pt1 <- as.integer ((proc.time () - pt0) [3])
    t_per_file <- pt1 / (index * chunk_size)
    t_total <- as.integer (t_per_file * npkgs)
    t_rem <- hms::hms (t_total - pt1)
    pt1 <- hms::hms (pt1)

    ndone <- min (c (npkgs, index * chunk_size))

    if (!op_is_quiet) {
        op <- getOption ("rlib_message_verbosity")
        options ("rlib_message_verbosity" = "verbose")
    }
    cli::cli_inform (paste0 (
        "[{ndone} / {npkgs}]  = {prog_fmt}%; ",
        "(elapsed, remaining) = ({pt1}, {t_rem})"
    ))
    if (!op_is_quiet) {
        options ("rlib_message_verbosity" = op)
    }
}
# nocov end
