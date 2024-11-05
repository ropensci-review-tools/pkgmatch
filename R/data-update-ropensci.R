#' Update pkgmatch` data for rOpenSci packages on GitHub release
#'
#' This function is intended for internal rOpenSci use only. Usage by any
#' unauthorized users will error and have no effect unless run with `upload =
#' FALSE`, in which case updated data will be created in the sub-directory
#' "pkgmatch-results" of R's current temporary directory.
#'
#' @param upload If `TRUE`, upload updated results to GitHub release.
#' @return Local path to directory containing updated results.
#' @family update
#' @noRd

# nocov start
pkgmatch_update_ropensci <- function (upload = TRUE) {

    requireNamespace ("gert", quietly = TRUE)
    requireNamespace ("piggyback", quietly = TRUE)

    results_path <- fs::dir_create (fs::path (fs::path_temp (), "pkgmatch-results"))
    flist <- dl_prev_data (results_path)

    flist <- fs::dir_ls (results_path, type = "file", regexp = "ropensci\\.Rds$")
    finfo <- fs::file_info (flist)
    pkgmatch_date <- min (finfo$modification_time)
    reg <- ros_registry ()
    reg_today <- registry_daily_chunk (reg)

    reg_updated <- reg_today [which (reg_today$date_last_commit > pkgmatch_date), ]
    if (nrow (reg_updated) == 0L) {
        return (TRUE)
    }

    pt0 <- proc.time ()
    op <- getOption ("rlib_message_verbosity")
    options ("rlib_message_verbosity" = "quiet")

    res <- lapply (seq_len (nrow (reg_updated)), function (i) {
        url <- reg_updated$github [i]
        pkg_dir <- fs::path (fs::path_temp (), reg_updated$name [i])
        fs::dir_create (pkg_dir)
        gert::git_clone (url = url, path = pkg_dir, verbose = FALSE)
        dat <- extract_data_from_local_dir (pkg_dir)
        fs::dir_delete (pkg_dir)

        pkgmatch_update_progress_message (i, 1, nrow (reg_updated), pt0)

        return (dat)
    })
    names (res) <- reg_updated$name

    append_data_to_embeddings (res, flist, cran = FALSE)
    append_data_to_bm25 (res, flist, cran = FALSE)
    append_data_to_fn_calls (res, flist, cran = FALSE)

    options ("rlib_message_verbosity" = op)
}
# nocov end

ros_registry <- function () {
    u_base <- "https://raw.githubusercontent.com/ropensci/roregistry/"
    u_gh <- paste0 (u_base, "refs/heads/gh-pages/")
    u <- paste0 (u_gh, "registry.json")
    reg <- jsonlite::read_json (u, simplify = TRUE)
    return (reg$packages)
}

registry_daily_chunk <- function (reg) {
    ndays <- days_in_this_month ()
    npkgs <- nrow (reg)
    index <- as.integer (cut (seq_len (npkgs), breaks = ndays))

    today_day_num <- as.integer (format (Sys.Date (), "%d"))
    index <- which (index == today_day_num)
    reg [index, ]
}

days_in_this_month <- function (today = Sys.Date ()) {
    if (is.character (today)) {
        today <- as.Date (today) # Will error if not possible
    }
    this_month <- as.integer (format (today, "%m"))
    this_year <- next_year <- as.integer (format (today, "%y"))
    next_month <- this_month + 1L
    if (next_month > 12L) {
        next_month <- next_month - 12L
        next_year <- next_year + 1L

    }
    this_month <- as.Date (paste0 (this_year, "-", this_month, "-", "1"))
    next_month <- as.Date (paste0 (next_year, "-", next_month, "-", "1"))
    dates <- seq (this_month, next_month, by = "day")
    return (length (dates) - 1L)
}
