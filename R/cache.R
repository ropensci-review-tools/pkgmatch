#' @title Load 'pkgmatch' data for specified corpus.
#'
#' @description Load pre-computed data for a specified corpus. Data types are:
#' \itemize{
#' \item "embeddings" for language model embeddings;
#' \item "idfs" for Inverse Document Frequency weightings;
#' \item "functions" for frequency tables for text descriptions of function
#' calls; or
#' \item "calls" for frequency tables for actual function calls.
#' }
#'
#' This function is called within the main
#' \link{pkgmatch_similar_pkgs} and \link{pkgmatch_similar_fns} functions
#' to load required data there, and should not generally need to be explicitly
#' called.
#'
#' @inheritParams pkgmatch_similar_pkgs
#' @param what One of the four data types described above: "embeddings",
#' "idfs", "functions", or "calls".
#' @param fns If `FALSE` (default), load embeddings for all packages; otherwise
#' load (considerably larger dataset of) embeddings for all individual
#' functions.
#' @param raw Only has effect of `what = "calls"`, in which case default of
#' `FALSE` loads single Inverse Document Frequency table to entire corpus;
#' otherwise if `TRUE`, loads raw function call counts for each package in
#' corpus.
#' @return The loaded data.
#'
#' @family utils
#' @export
#'
#' @examples
#' \dontrun{
#' embeddings <- pkgmatch_load_data ("embeddings")
#' embeddings_fns <- pkgmatch_load_data ("embeddings", fns = TRUE)
#' idfs <- pkgmatch_load_data ("idfs")
#' idfs_fns <- pkgmatch_load_data ("idfs", fns = TRUE)
#' }
pkgmatch_load_data <- function (what = "embeddings",
                                corpus = "ropensci",
                                fns = FALSE,
                                raw = FALSE) {

    checkmate::assert_character (what, len = 1L)
    checkmate::assert_character (corpus, len = 1L)
    checkmate::assert_logical (fns, len = 1L)
    checkmate::assert_logical (raw, len = 1L)

    m_load_data_internal (what, corpus, fns, raw)
}

pkgmatch_cache_update_interval <- function () {
    dt <- 30

    op <- getOption ("pkgmatch.update_frequency")
    if (!is.null (op)) {
        op <- tryCatch (as.integer (op), error = function (e) NULL)
    }
    if (!is.null (op)) {
        dt <- op
    }
    return (dt)
}

# nocov start

#' @title Update all locally-cached `pkgmatch` data to latest versions.
#'
#' @description This function forces all locally-cached data to be updated with
#' latest version of remote data provided on the latest release of GitHub
#' repository at
#' \url{https://github.com/ropensci-review-tools/pkgmatch/releases}.
#'
#' Caching strategies are described in the "*Data Caching and Updating*"
#' vignette, accessible either locally via
#' `vignette("data-caching-and-updating", package = "pkgmatch")`, or online at
#' \url{https://docs.ropensci.org/pkgmatch/articles/D_data-caching-and-updating.html}.
#' In short, locally-cached data used by this package are updated
#' by default every 30 days (with the vignette describing how to modify this
#' default behaviour). This function forces all locally-cached data to be
#' updated, regardless of update frequencies.
#'
#' @return (Invisibly) A list of full local paths to all files which were
#' updated.
#'
#' @family utils
#' @export
#'
#' @examples
#' \dontrun{
#' pkgmatch_update_cache ()
#' }
pkgmatch_update_cache <- function () {

    what <- c ("embeddings", "idfs", "functions", "calls")
    corpus <- c ("ropensci", "cran", "bioc")
    fns <- c (FALSE, TRUE)
    raw <- c (FALSE, TRUE)

    vals <- expand.grid (what = what, corpus = corpus, fns = fns, raw = raw)
    vals$fname <- apply (vals, 1, function (i) {
        get_cache_file_name (
            what = i [1], corpus = i [2], fns = i [3], raw = i [4]
        )
    })
    vals <- vals [-which (duplicated (vals$fname)), ]
    # expand.grid creates factors which must be converted back:
    vals$what <- as.character (vals$what)
    vals$corpus <- as.character (vals$corpus)
    vals$fname <- as.character (vals$fname)

    fnames <- fs::path (pkgmatch_cache_path (), vals$fname)

    send_dl_message (fnames)

    vals_list <- split (vals, f = as.factor (vals$fname))
    files <- lapply (vals_list, function (i) {
        msg <- "Downloading {i$what} {i$fns_msg}data for {i$corpus} corpus"
        cli::cli_inform (msg)
        pkgmatch_dl_data (
            what = i$what, corpus = i$corpus, fns = i$fns, raw = i$raw
        )
    })

    invisible (unlist (files))
}
# nocov end

load_data_internal <- function (what, corpus, fns, raw) {
    fname <- get_cache_file_name (what, corpus, fns, raw)

    fname <- fs::path (pkgmatch_cache_path (), fname)
    dl <- !fs::file_exists (fname)
    if (!dl) {
        # Check whether existing file should be updated
        fdate <- as.Date (fs::file_info (fname)$modification_time)
        dt <- difftime (as.Date (Sys.time ()), fdate, units = "days")
        dl <- dt > pkgmatch_cache_update_interval ()
        if (dl) {
            cli::cli_inform (
                "Local data are {dt} days old and will be updated ..."
            )
        }
    }
    if (dl) {
        fns_msg <- ifelse (fns, "functions ", "")
        msg <- "Downloading {what} {fns_msg}data for {corpus} corpus"
        cli::cli_inform (msg)
        fname <- pkgmatch_dl_data (
            what = what, corpus = corpus, fns = fns, raw = raw
        )
    }
    readRDS (fname)
}
m_load_data_internal <- memoise::memoise (load_data_internal)

m_list_remote_files <- function () {

    if (!identical (Sys.getenv ("PKGMATCH_TESTS"), "true")) {
        res <- piggyback::pb_list (
            repo = "ropensci-review-tools/pkgmatch",
            tag = RELEASE_TAG
        )
    } else {
        # dummy values for tests only:
        f <- c (
            "bm25-cran.Rds",
            "bm25-ropensci-fns.Rds",
            "bm25-ropensci.Rds",
            "embeddings-cran.Rds",
            "embeddings-fns.Rds",
            "embeddings-ropensci.Rds",
            "fn-calls-cran.Rds",
            "fn-calls-ropensci.Rds",
            "idfs-fn-calls-cran.Rds",
            "idfs-fn-calls-ropensci.Rds"
        )
        res <- data.frame (
            file_name = f,
            size = as.integer (stats::runif (length (f), 1, 1e6)),
            timestamp = as.POSIXct ("2025-01-01T00:00:00"),
            tag = RELEASE_TAG,
            owner = "ropensci-review-tools",
            repo = "pkgmatch"
        )
    }
    return (res)
}
list_remote_files <- memoise::memoise (m_list_remote_files)

get_cache_file_name <- function (what, corpus, fns, raw) {

    corpus <- match.arg (tolower (corpus), c ("ropensci", "cran", "bioc"))
    what <- match.arg (what, c ("embeddings", "idfs", "functions", "calls"))

    if (corpus == "ropensci") {

        fname <- switch (what,
            "embeddings" = ifelse (
                fns, "embeddings-fns.Rds", "embeddings-ropensci.Rds"
            ),
            "idfs" = ifelse (fns, "bm25-ropensci-fns.Rds", "bm25-ropensci.Rds"),
            "functions" = "fn-calls-ropensci.Rds",
            "calls" = ifelse (
                raw, "fn-calls-ropensci.Rds", "idfs-fn-calls-ropensci.Rds"
            )
        )

    } else if (corpus == "cran") {

        fname <- switch (what,
            "embeddings" = "embeddings-cran.Rds",
            "idfs" = "bm25-cran.Rds",
            "functions" = "fn-calls-cran.Rds",
            "calls" = ifelse (
                raw, "fn-calls-cran.Rds", "idfs-fn-calls-cran.Rds"
            )
        )
    } else if (corpus == "bioc") {

        fname <- switch (what,
            "embeddings" = ifelse (
                fns, "embeddings-fns-bioc.Rds", "embeddings-bioc.Rds"
            ),
            "idfs" = ifelse (fns, "bm25-bioc-fns.Rds", "bm25-bioc.Rds"),
            "functions" = "fn-calls-bioc.Rds",
            "calls" = ifelse (
                raw, "fn-calls-bioc.Rds", "idfs-fn-calls-bioc.Rds"
            )
        )
    }

    return (fname)
}

# nocov start
pkgmatch_dl_data <- function (what = "embeddings", corpus = "ropensci",
                              fns = FALSE, raw = FALSE) {

    fname <- get_cache_file_name (what, corpus, fns, raw)

    url_base <-
        "https://github.com/ropensci-review-tools/pkgmatch/releases/download/"

    # RELEASE_TAG is in 'data-update.R':
    dl_url <- paste0 (url_base, RELEASE_TAG, "/", fname)

    destfile <- fs::path (pkgmatch_cache_path (), fname)
    curl::curl_download (
        url = dl_url,
        destfile = destfile,
        quiet = opt_is_quiet ()
    )

    return (destfile)
}
# nocov end

pkgmatch_cache_path <- function () {

    cache_dir <- Sys.getenv ("PKGMATCH_CACHE_DIR")

    if (cache_dir == "") { # nocov start
        cache_dir <- fs::path_expand (fs::path (
            rappdirs::user_cache_dir (),
            "R",
            "pkgmatch"
        ))
        if (!fs::dir_exists (cache_dir)) {
            fs::dir_create (cache_dir, recurse = TRUE)
        }
    } # nocov end

    # Examples are run in pkgdown with withr which inherits options but not
    # envvars.
    if (getOption ("pkgmatch.example_env", "") == "true") {
        cache_dir <- fs::path (fs::path_temp (), "pkgmatch_ex_data")
    }

    return (cache_dir)
}

send_dl_message <- function (fnames) {

    # Suppress no visible binding note:
    file_name <- NULL

    corpus <- unique (gsub ("^.*\\-|\\.Rds$", "", fnames))
    corpus <- corpus [which (!corpus == "fns")]
    flist <- fs::dir_ls (pkgmatch_cache_path ())
    extant_files <- any (vapply (
        corpus,
        function (i) grepl (i, flist),
        logical (length (flist))
    ))
    cache_dir <- pkgmatch_cache_path ()

    flist <- fs::path (pkgmatch_cache_path (), fnames)
    flist_dl <- flist [which (!fs::file_exists (flist))]
    flist <- flist [which (fs::file_exists (flist))]

    fdates <- as.Date (fs::file_info (flist)$modification_time)
    dt <- difftime (as.Date (Sys.time ()), fdates, units = "days")
    flist <- flist [which (dt > pkgmatch_cache_update_interval ())]

    flist <- c (flist_dl, flist)
    finfo_count <- length (flist)
    if (finfo_count == 0L) {

        cli::cli_inform (
            "Cached data in {cache_dir} are up to date."
        )
        return ()

    } else {

        if (!extant_files) {
            cli::cli_inform ("This function requires data to be downloaded.")
            cli::cli_inform ("Data will be downloaded to {cache_dir}.")
        } else {
            cli::cli_inform (
                "Data already exist in {cache_dir}, and will now be overwritten."
            )
        }
        cli::cli_inform ("This directory may be safely deleted at any time.")
        cli::cli_inform (
            "See the pkgmatch 'Data caching and updating' vignette for details."
        )
    }

    finfo <- list_remote_files () |>
        dplyr::filter (file_name %in% basename (flist))
    finfo_size <- signif (sum (finfo$size) / 1024 / 1024, digits = 2)

    msg <- paste0 (
        "Data for the {corpus} corpus comprises {finfo_count} ",
        "file{?s} totalling around {finfo_size}MB."
    )
    cli::cli_inform (msg)

    if (!cli::has_keypress_support () ||
        identical (Sys.getenv ("PKGMATCH_TESTS"), "true")) {
        msg <- paste0 (
            "Your environment does not support key ",
            "entry, downloading will now proceed."
        )
        cli::cli_alert_warning (msg)
    } else {
        cli::cli_alert_info ("Do you want to proceed (y/n)?")
        k <- tolower (cli::keypress ())
        if (!k %in% c ("y", "n")) {
            cli::cli_abort ("Only 'y' or 'n' are recognised.")
        }
        if (k == "n") {
            cli::cli_abort ("Okay, stopping there")
        }
    }
}
