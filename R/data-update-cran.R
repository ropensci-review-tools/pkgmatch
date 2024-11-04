RELEASE_TAG <- "v0.4.0"

extract_tarball <- utils::getFromNamespace ("extract_tarball", "pkgstats")


#' Update pkgmatch` data for CRAN packages on GitHub release
#'
#' This function is intended for internal rOpenSci use only. Usage by any
#' unauthorized users will error and have no effect unless run with `upload =
#' FALSE`, in which case updated data will be created in the sub-directory
#' "pkgmatch-results" of R's current temporary directory.
#'
#' @param upload If `TRUE`, upload updated results to GitHub release.
#' @return Local path to directory containing updated results.
#' @family archive
#' @noRd

# nocov start
pkgmatch_update_cran <- function (upload = TRUE) {

    requireNamespace ("piggyback")

    results_path <- fs::dir_create (fs::path (fs::path_temp (), "pkgmatch-results"))
    flist <- dl_prev_data (results_path)

    new_cran_pkgs <- list_new_cran_updates (flist)

    npkgs <- length (new_cran_pkgs)

    if (npkgs == 0) {
        return (NULL)
    }

    message (
        "Downloading and analysing ", npkgs, " packages."
    )

    pt0 <- proc.time ()
    op <- getOption ("rlib_message_verbosity")
    options ("rlib_message_verbosity" = "quiet")

    res <- lapply (seq_along (new_cran_pkgs), function (p) {

        res <- NULL

        tarball_path <- dl_one_tarball (results_path, new_cran_pkgs [p])
        if (!is.null (tarball_path) && fs::file_exists (tarball_path)) {
            pkg_dir <- extract_tarball (tarball_path)

            embeddings <- pkgmatch_embeddings_from_pkgs (pkg_dir)
            embeddings_fns <-
                pkgmatch_embeddings_from_pkgs (pkg_dir, functions_only = TRUE)

            txt_with_fns <- get_pkg_text (pkg_dir)
            txt_wo_fns <- rm_fns_from_pkg_txt (txt_with_fns)
            bm25_data <- list (
                idfs = list (
                    with_fns = bm25_idf (txt_with_fns),
                    wo_fns = bm25_idf (txt_wo_fns)
                ),
                token_lists = list (
                    with_fns = bm25_tokens_list (txt_with_fns),
                    wo_fns = bm25_tokens_list (txt_wo_fns)
                )
            )

            fn_calls <- pkgmatch_treesitter_fn_tags (pkg_dir)
            calls <- sort (table (fn_calls$name))

            fs::dir_delete (pkg_dir)

            res <- list (
                embeddings = embeddings,
                embeddings_fns = embeddings_fns,
                bm25 = bm25_data,
                fn_calls = calls
            )
        }

        pkgmatch_update_progress_message (p, 1, npkgs, pt0)

        return (res)
    })
    names (res) <- new_cran_pkgs

    embeddings <- append_data_to_embeddings_cran (res, flist)
    bm25 <- append_data_to_bm25_cran (res, flist)

    for (i in flist) {
        piggyback::pb_upload (
            file = i,
            repo = "ropensci-review-tools/pkgstats",
            tag = RELEASE_TAG
        )
    }

    options ("rlib_message_verbosity" = op)
}

append_data_to_embeddings_cran <- function (res, flist) {

    not_null_index <- function (res, what) {
        which (vapply (
            res,
            function (i) !is.null (i$embeddings [[what]]),
            logical (1L)
        ))
    }

    append_cols <- function (res, embeddings, what) {
        what <- match.arg (what, c ("text_with_fns", "text_wo_fns", "code"))
        index <- not_null_index (res, what)
        emb <- do.call (cbind, lapply (res, function (i) i$embeddings [[what]]))
        colnames (emb) <- names (res) [index]

        embeddings [[what]] <- cbind (embeddings [[what]], emb)
        index <- order (colnames (embeddings [[what]]))
        embeddings [[what]] <- embeddings [[what]] [, index]

        return (embeddings)
    }

    fname <- flist [which (basename (flist) == "embeddings-cran.Rds")]
    embeddings <- readRDS (fname)

    embeddings <- append_cols (res, embeddings, "text_with_fns")
    embeddings <- append_cols (res, embeddings, "text_wo_fns")
    embeddings <- append_cols (res, embeddings, "code")

    saveRDS (embeddings, fname)

    return (embeddings)
}

append_data_to_bm25_cran <- function (res, flist) {

    not_null_index <- function (res, what) {
        which (vapply (
            res,
            function (i) !is.null (i$bm25$token_lists [[what]]),
            logical (1L)
        ))
    }

    append_cols <- function (res, bm25, what) {
        what <- match.arg (what, c ("with_fns", "wo_fns"))
        index <- not_null_index (res, what)
        bm25_these <- lapply (res, function (i) i$bm25$token_lists [[what]] [[1]])
        names (bm25_these) <- names (res) [index]

        bm25$token_lists [[what]] <- c (bm25$token_lists [[what]], bm25_these)

        return (bm25)
    }

    fname <- flist [which (basename (flist) == "bm25-cran.Rds")]
    bm25 <- readRDS (fname)

    bm25 <- append_cols (res, bm25, "with_fns")
    bm25 <- append_cols (res, bm25, "wo_fns")

    # Then update main 'idfs' table:
    update_idfs <- function (bm25, what = "with_fns") {

        what <- match.arg (what, c ("with_fns", "wo_fns"))

        toks_all <- lapply (bm25$token_lists [[what]], function (i) i$token)
        toks_all <- unlist (unname (toks_all))
        toks_tab <- table (toks_all)
        toks_n <- as.integer (toks_tab)
        n_docs <- length (bm25$token_lists [[what]])
        idf <- unname (log ((n_docs - toks_n + 0.5) / (toks_n + 0.5) + 1))
        toks_idf <- data.frame (
            token = names (toks_tab),
            idf = idf
        )

        bm25$idfs [[what]] <- toks_idf

        return (bm25)
    }

    saveRDS (bm25, fname)

    return (bm25)
}
# nocov end

dl_prev_data <- function (results_path) {

    files <- piggyback::pb_list (tag = RELEASE_TAG)
    file_names <- files$file_name
    file_names_done <- file_names [which (file_names %in% list.files (results_path))]

    dl_data <- piggyback::pb_download (
        repo = "ropensci-review-tools/pkgmatch",
        dest = results_path,
        tag = RELEASE_TAG,
        ignore = file_names_done
    )

    return (fs::dir_ls (results_path))
}

get_cran_db <- memoise::memoise (tools::CRAN_package_db)

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
    resp <- httr2::req_perform (req)

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
list_new_cran_updates <- function (flist) {

    # Arbitrarily choose "embeddings" for original list of CRAN pkgs:
    f <- grep ("embeddings\\-cran\\.Rds", flist, value = TRUE)
    embeddings <- readRDS (f)

    pkgs <- c (colnames (embeddings$text_with_fns), colnames (embeddings$code))
    pkgs <- gsub ("\\.tar\\.gz$", "", sort (unique (pkgs)))
    cran_db <- get_cran_db ()
    cran_tarball <- paste0 (cran_db$Package, "_", cran_db$Version)
    cran_new <- cran_tarball [which (!cran_tarball %in% pkgs)]

    # Remove old versions from all data
    cran_new_pkg <- gsub ("\\_.*$", "", cran_new)
    pkgs_pkg <- gsub ("\\_.*$", "", pkgs)
    index <- which (pkgs_pkg %in% cran_new_pkg)
    pkgs_old <- pkgs [index]
    pkgs_old_targz <- paste0 (pkgs [index], ".tar.gz")

    # ----- rm obsolete pkgs from embeddings:
    index <- which (!colnames (embeddings$text_with_fns) %in% pkgs_old_targz)
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

    return (paste0 (cran_new, ".tar.gz"))
}

pkgmatch_update_progress_message <- function (index, chunk_size, npkgs, pt0) {

    prog <- index * chunk_size / npkgs
    prog_fmt <- format (100 * prog, digits = 2)
    pt1 <- as.integer ((proc.time () - pt0) [3])
    t_per_file <- pt1 / (index * chunk_size)
    t_total <- t_per_file * npkgs
    t_rem <- hms::hms (t_total - pt1)

    ndone <- min (c (npkgs, index * chunk_size))

    message (
        "[", ndone, " / ", npkgs,
        "]  = ", prog_fmt, "%; (elapsed, remaining) = (",
        pt1, ", ", t_rem, ")"
    )
}
