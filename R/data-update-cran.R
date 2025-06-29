#' Update pkgmatch` data for CRAN packages on GitHub release
#'
#' This function is intended for internal rOpenSci use only. Usage by any
#' unauthorized users will error and have no effect unless run with `upload =
#' FALSE`, in which case updated data will be created in the sub-directory
#' "pkgmatch-results" of R's current temporary directory.
#'
#' @param flist Paths to local 'pkgmatch' results files to be updated
#' @param local_mirror_path Optional path to a local directory with full CRAN
#' mirror. If specified, data will use packages from this local source for
#' updating. Default behaviour if not specified is to download new packages
#' into tempdir, and delete once data have been updated.
#'
#' @return Local path to directory containing updated results.
#' @family update
#' @noRd

# nocov start
pkgmatch_update_cran <- function (flist, local_mirror_path = NULL) {

    results_path <- fs::path_common (flist)

    new_cran_pkgs <- list_new_cran_updates (flist, latest_only = TRUE)

    npkgs <- length (new_cran_pkgs)

    if (npkgs == 0) {
        return (FALSE)
    }

    msg <- ifelse (
        is.null (local_mirror_path),
        "Downloading and analysing {npkgs} packages.",
        "Analysing {npkgs} new packages."
    )
    cli::cli_inform (msg)

    pt0 <- proc.time ()
    op_is_quiet <- opt_is_quiet ()
    op <- getOption ("rlib_message_verbosity")
    options ("rlib_message_verbosity" = "quiet")

    res <- lapply (seq_along (new_cran_pkgs), function (p) {

        dat <- NULL

        if (is.null (local_mirror_path)) {
            tarball_path <- dl_one_tarball (results_path, new_cran_pkgs [p])
        } else {
            tarball_path <- fs::path (local_mirror_path, new_cran_pkgs [1])
        }
        if (!is.null (tarball_path) && fs::file_exists (tarball_path)) {
            pkg_dir <- extract_tarball (tarball_path)
            dat <- tryCatch (
                extract_data_from_local_dir (pkg_dir),
                error = function (e) NULL
            )
            fs::dir_delete (pkg_dir)
            if (is.null (local_mirror_path)) {
                fs::file_delete (tarball_path)
            }
        }

        # This is necessary because `utils::untar()` can create hanging
        # connections:
        closeAllConnections ()

        if (!op_is_quiet) {
            pkgmatch_update_progress_message (p, 1, npkgs, pt0, op_is_quiet)
        }

        return (dat)
    })
    names (res) <- new_cran_pkgs

    append_data_to_embeddings (res, flist, cran = TRUE)
    append_data_to_bm25 (res, flist, cran = TRUE)
    append_data_to_fn_calls (res, flist, cran = TRUE)

    options ("rlib_message_verbosity" = op)

    return (TRUE)
}
# nocov end

get_cran_db <- memoise::memoise (tools::CRAN_package_db)

# nocov start
dl_one_tarball <- function (results_path, tarball) {

    cran_url <- "https://cran.r-project.org/src/contrib/"
    if (!grepl ("\\.tar\\.gz$", tarball)) {
        tarball <- paste0 (tarball, ".tar.gz")
    }
    url <- paste0 (cran_url, tarball)
    path <- fs::path (results_path, tarball)

    if (fs::file_exists (path)) {
        return (path)
    }

    req <- httr2::request (url) |>
        httr2::req_headers ("Accept" = "application/octet-stream")
    resp <- tryCatch (
        httr2::req_perform (req),
        error = function (e) NULL
    )
    # path <- tryCatch (
    #     curl::curl_download (url, destfile = path),
    #     error = function (e) NULL
    # )

    if (is.null (resp)) {
        return (NULL)
    }
    if (httr2::resp_is_error (resp)) {
        return (NULL)
    }

    writeBin (httr2::resp_body_raw (resp), path)
    return (path)
}

#' List new cran packages since those used to generate results in 'flist'. This
#' function also removes all obsolete package versions from all CRAN results,
#' and re-saves the modified versions to local paths defined by `flist`.
#'
#' @param flist Local paths to downloaded versions of all 'pkgmatch' results.
#' Note that all results are presumed to come from same package lists, so that
#' updated packages only need to examine one item.
#' @return A character vector of tarball names of new CRAN packages since time
#' of previous data generation.
#'
#' @noRd
list_new_cran_updates <- function (flist, latest_only = TRUE) {

    # Arbitrarily choose "embeddings" for original list of CRAN pkgs:
    f <- grep ("embeddings\\-cran\\.Rds", flist, value = TRUE)
    embeddings <- readRDS (f)

    pkgs <- c (colnames (embeddings$text_with_fns), colnames (embeddings$code))
    pkgs <- gsub ("\\.tar\\.gz$", "", sort (unique (pkgs)))
    cran_db <- get_cran_db ()
    cran_tarball <- paste0 (cran_db$Package, "_", cran_db$Version)

    # Only include packages published since last update:
    index <- which (!cran_tarball %in% pkgs)
    if (latest_only) {
        published <- as.Date (cran_db$Published [index])
        flist_remote <- list_remote_files ()
        i <- which (flist_remote$file_name == basename (f))
        embeddings_date <- as.Date (flist_remote$timestamp [i])
        dt <- difftime (embeddings_date, published, units = "days")
        max_days <- 2L # allow published up to this many days before last update
        index <- index [which (dt <= max_days)]
    } # Otherwise update all pkgs regardless of dates ...
    cran_new <- cran_tarball [index]

    # Remove old versions from all data
    cran_new_pkg <- gsub ("\\_.*$", "", cran_new)
    pkgs_pkg <- gsub ("\\_.*$", "", pkgs)
    index <- which (pkgs_pkg %in% cran_new_pkg)
    pkgs_old <- pkgs [index]

    if (length (pkgs_old) > 0L) {

        pkgs_old_targz <- paste0 (pkgs [index], ".tar.gz")

        # ----- rm obsolete pkgs from embeddings:
        index <-
            which (!colnames (embeddings$text_with_fns) %in% pkgs_old_targz)
        embeddings$text_with_fns <- embeddings$text_with_fns [, index]
        index <- which (!colnames (embeddings$text_wo_fns) %in% pkgs_old_targz)
        embeddings$text_wo_fns <- embeddings$text_wo_fns [, index]
        index <- which (!colnames (embeddings$code) %in% pkgs_old_targz)
        embeddings$code <- embeddings$code [, index]
        saveRDS (embeddings, f)

        # ----- rm obsolete pkgs from bm25:
        f <- grep ("bm25\\-cran\\.Rds", flist, value = TRUE)
        bm25 <- readRDS (f)
        index <- which (!names (bm25$token_lists$with_fns) %in% pkgs_old_targz)
        bm25$token_lists$with_fns <- bm25$token_lists$with_fns [index]
        index <- which (!names (bm25$token_lists$wo_fns) %in% pkgs_old_targz)
        bm25$token_lists$wo_fns <- bm25$token_lists$wo_fns [index]
        saveRDS (bm25, f)

        # ----- rm obsolete pkgs from fn-calls:
        f <- flist [which (basename (flist) == "fn-calls-cran.Rds")]
        fn_calls <- readRDS (f)
        index <- which (!names (fn_calls) %in% pkgs_old_targz)
        fn_calls <- fn_calls [index]
        saveRDS (fn_calls, f)
    }

    if (length (cran_new) > 0L) {
        cran_new <- paste0 (cran_new, ".tar.gz")
    }

    return (cran_new)
}
# nocov end
