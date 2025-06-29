#' Update pkgmatch` data for rOpenSci packages on GitHub release
#'
#' @param flist Paths to local 'pkgmatch' results files to be updated
#' @param local_mirror_path Optional path to a local directory with full
#' rOpenSci mirror. If specified, data will use packages from this local source
#' for updating. Default behaviour if not specified is to download new packages
#' into tempdir, and delete once data have been updated.
#' @noRd

# nocov start
pkgmatch_update_ropensci <- function (flist, local_mirror_path = NULL) {

    requireNamespace ("gert", quietly = TRUE)

    results_path <- fs::path_common (flist)

    pkgmatch_date <- min (list_remote_files ()$timestamp)
    reg <- ros_registry ()
    if (is.null (local_mirror_path)) {
        reg_today <- registry_daily_chunk (reg)
    } else {
        index <- which (reg$date_last_commit >= pkgmatch_date)
        reg_today <- reg [index, ]
    }

    dt <- floor (difftime (
        pkgmatch_date,
        reg_today$date_last_commit,
        units = "days"
    ))
    # Can be up a month between updates, so set to 2 months just to be sure:
    max_days <- 62
    reg_updated <- reg_today [which (dt <= max_days), ]

    if (nrow (reg_updated) == 0L) {
        return (FALSE)
    }

    pt0 <- proc.time ()
    op_is_quiet <- opt_is_quiet ()
    op <- getOption ("rlib_message_verbosity")
    options ("rlib_message_verbosity" = "quiet")

    res <- lapply (seq_len (nrow (reg_updated)), function (i) {
        url <- reg_updated$github [i]
        if (is.null (local_mirror_path)) {
            pkg_dir <- fs::path (fs::path_temp (), reg_updated$name [i])
            fs::dir_create (pkg_dir)
            gert::git_clone (url = url, path = pkg_dir, verbose = FALSE)
        } else {
            pkg_dir <- fs::path (local_mirror_path, reg_updated$name [i])
        }
        dat <- tryCatch (
            extract_data_from_local_dir (pkg_dir),
            error = function (e) NULL
        )
        fns <- tryCatch (
            pkgmatch_embeddings_from_pkgs (pkg_dir, functions_only = TRUE),
            error = function (e) NULL
        )
        if (is.null (local_mirror_path)) {
            fs::dir_delete (pkg_dir)
        }

        if (!op_is_quiet) {
            pkgmatch_update_progress_message (
                i, 1, nrow (reg_updated), pt0, op_is_quiet
            )
        }

        return (list (dat = dat, fns = fns))
    })
    fns <- do.call (cbind, lapply (res, function (i) i$fns))
    res <- lapply (res, function (i) i$dat)
    names (res) <- reg_updated$name

    append_data_to_embeddings (res, flist, cran = FALSE)
    append_data_to_bm25 (res, flist, cran = FALSE)
    append_data_to_fn_calls (res, flist, cran = FALSE)

    append_data_to_fn_embeddings (fns, flist)

    options ("rlib_message_verbosity" = op)

    return (TRUE)
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

append_data_to_fn_embeddings <- function (fns, flist) {

    pkgs <- unique (gsub ("::.*$", "", colnames (fns)))
    f <- grep ("embeddings\\-fns\\.", flist, value = TRUE)
    embeddings_fns <- readRDS (f)
    ptn <- paste0 (paste0 (pkgs, "::"), collapse = "|")
    index <- which (!grepl (ptn, colnames (embeddings_fns)))
    embeddings_fns <- cbind (embeddings_fns [, index], fns)

    index <- order (colnames (embeddings_fns))
    embeddings_fns <- embeddings_fns [, index]
    saveRDS (embeddings_fns, f)
}
