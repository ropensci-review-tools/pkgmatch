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
#' @param minchar Minimal number of characters; tokens with less than this
#' number are discarded.
#'
#' @return Local path to directory containing updated results.
#' @family update
#' @noRd

# nocov start
pkgmatch_update_cran <- function (flist, local_mirror_path = NULL, minchar = 3L) {

    results_path <- fs::path_common (flist)

    new_cran_pkgs <- list_new_cran_updates (flist, latest_only = TRUE)

    npkgs <- length (new_cran_pkgs)

    if (npkgs == 0) {
        return (FALSE)
    }

    cli::cli_inform ("Downloading and extracting {npkgs} packages...")

    pt0 <- proc.time ()

    # Download and extract packages first:
    exdir <- ifelse (
        is.null (local_mirror_path),
        fs::path_temp (),
        fs::path_dir (local_mirror_path)
    )
    exdir <- fs::path (exdir, "temp")
    if (!fs::dir_exists (exdir)) {
        fs::dir_create (exdir)
    }
    paths <- lapply (new_cran_pkgs, function (p) {
        if (is.null (local_mirror_path)) {
            tarball_path <- dl_one_tarball (exdir, p)
        } else {
            tarball_path <- dl_one_tarball (local_mirror_path, p)
        }
        ret <- NULL
        if (!is.null (tarball_path)) {
            ret <- extract_tarball (tarball_path, exdir)
        }
        if (is.null (local_mirror_path)) {
            fs::file_delete (tarball_path)
        }
        return (ret)
    })
    index <- which (vapply (paths, function (p) !is.null (p), logical (1L)))
    paths <- unlist (paths)
    new_cran_pkgs <- new_cran_pkgs [index]

    npkgs_dl <- length (paths)
    cli::cli_inform ("Analysing {npkgs_dl} packages...")
    op_is_quiet <- opt_is_quiet ()
    op <- getOption ("rlib_message_verbosity")
    options ("rlib_message_verbosity" = "quiet")

    res <- lapply (seq_along (paths), function (p) {

        dat <- tryCatch (
            extract_data_from_local_dir (paths [p], minchar = minchar),
            error = function (e) NULL
        )

        if (!op_is_quiet) {
            pkgmatch_update_progress_message (p, 1, npkgs_dl, pt0, op_is_quiet)
        }

        return (dat)
    })
    index <- which (!vapply (res, is.null, logical (1L)))
    res <- res [index]
    names (res) <- gsub ("\\.tar\\.gz$", "", new_cran_pkgs [index])

    append_data_to_bm25 (res, flist, cran = TRUE)
    append_data_to_fn_calls (res, flist, cran = TRUE)

    options ("rlib_message_verbosity" = op)
    fs::dir_delete (exdir)

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

    if (is.null (resp)) {
        return (NULL)
    }
    if (httr2::resp_is_error (resp)) {
        return (NULL)
    }

    # This can happen:
    if (length (resp$body) == 0L) {
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

    flist <- unlist (flist)

    f_bm25 <- grep ("bm25\\-cran\\.Rds", flist, value = TRUE)
    bm25 <- readRDS (f_bm25)
    pkgs_full <- gsub ("\\.tar\\.gz$", "", names (bm25$full$token_lists))
    pkgs_pkg_full <- gsub ("\\_.*$", "", pkgs_full)
    pkgs_desc <- gsub ("\\.tar\\.gz$", "", names (bm25$descs_only$token_lists))
    pkgs_pkg_desc <- gsub ("\\_.*$", "", pkgs_desc)

    cran_db <- get_cran_db ()
    cran_tarball <- paste0 (cran_db$Package, "_", cran_db$Version)

    # Only include packages published since last update:
    index <- which (!cran_tarball %in% pkgs_full)
    if (latest_only) {
        published <- as.Date (cran_db$Published [index])
        flist_remote <- list_remote_files ()
        i <- which (flist_remote$file_name == basename (f_bm25))
        idfs_date <- as.Date (flist_remote$timestamp [i])
        dt <- difftime (idfs_date, published, units = "days")
        max_days <- 2L # allow published up to this many days before last update
        index <- index [which (dt <= max_days)]
    } # Otherwise update all pkgs regardless of dates ...
    cran_new <- cran_tarball [index]

    # Remove old versions from all data
    cran_new_pkg <- gsub ("\\_.*$", "", cran_new)
    pkgs_rm_full <- cran_new_pkg [which (cran_new_pkg %in% pkgs_pkg_full)]
    pkgs_rm_desc <- cran_new_pkg [which (cran_new_pkg %in% pkgs_pkg_desc)]
    pkgs_rm <- unique (c (pkgs_rm_full, pkgs_rm_desc))

    if (length (pkgs_rm) > 0L) {

        # ----- rm obsolete pkgs from bm25:
        index <- match (pkgs_rm, pkgs_pkg_full)
        bm25$full$token_lists <- bm25$full$token_list [-index]
        index <- match (pkgs_rm, pkgs_pkg_desc)
        bm25$descs_only$token_lists <- bm25$descs_only$token_list [-index]
        saveRDS (bm25, f_bm25)

        # ----- rm obsolete pkgs from fn-calls:
        f <- flist [which (basename (flist) == "fn-calls-cran.Rds")]
        fn_calls <- readRDS (f)
        n0 <- length (fn_calls)
        nms <- gsub ("\\_.*$", "", names (fn_calls))
        index <- match (pkgs_rm, nms)
        index <- index [which (!is.na (index))]
        fn_calls <- fn_calls [-index]
        saveRDS (fn_calls, f)
    }

    return (cran_new)
}
# nocov end
