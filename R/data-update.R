# General functions for both CRAN and rOpenSci update workflows

RELEASE_TAG <- "v0.4.0"

#' Update pkgmatch` data for both CRAN and rOpenSci packages on GitHub release
#'
#' This function is intended for internal rOpenSci use only. Usage by any
#' unauthorized users will error and have no effect unless run with `upload =
#' FALSE`, in which case updated data will be created in the sub-directory
#' "pkgmatch-results" of R's current temporary directory. This updating may
#' take a very long time!
#'
#' @param upload If `TRUE`, upload updated results to GitHub release.
#' @return Local path to directory containing updated results.
#' @family data
#' @export

# nocov start
pkgmatch_update_data <- function (upload = TRUE) {

    requireNamespace ("piggyback", quietly = TRUE)

    results_path <- fs::dir_create (fs::path (fs::path_temp (), "pkgmatch-results"))
    flist <- dl_prev_data (results_path)

    pkgmatch_update_cran ()
    pkgmatch_update_ropensci ()

    if (upload) {
        for (i in flist) {
            piggyback::pb_upload (
                file = i,
                repo = "ropensci-review-tools/pkgmatch",
                tag = RELEASE_TAG
            )
        }
    }
}
# nocov end

extract_data_from_local_dir <- function (pkg_dir) {

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

    list (
        embeddings = embeddings,
        embeddings_fns = embeddings_fns,
        bm25 = bm25_data,
        fn_calls = calls
    )
}

append_data_to_embeddings <- function (res, flist, cran = TRUE) {

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

        index <- which (!colnames (embeddings [[what]]) %in% colnames (emb))
        embeddings [[what]] <- cbind (embeddings [[what]] [, index], emb)
        index <- order (colnames (embeddings [[what]]))
        embeddings [[what]] <- embeddings [[what]] [, index]

        return (embeddings)
    }

    fname <- ifelse (cran, "embeddings-cran.Rds", "embeddings-ropensci.Rds")
    fname <- flist [which (basename (flist) == fname)]
    embeddings <- readRDS (fname)

    embeddings <- append_cols (res, embeddings, "text_with_fns")
    embeddings <- append_cols (res, embeddings, "text_wo_fns")
    embeddings <- append_cols (res, embeddings, "code")

    saveRDS (embeddings, fname)
}

append_data_to_bm25 <- function (res, flist, cran = TRUE) {

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

    fname <- ifelse (cran, "bm25-cran.Rds", "bm25-ropensci.Rds")
    fname <- flist [which (basename (flist) == fname)]
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
    bm25 <- update_idfs (bm25, "with_fns")
    bm25 <- update_idfs (bm25, "wo_fns")

    saveRDS (bm25, fname)
}

append_data_to_fn_calls <- function (res, flist, cran = TRUE) {

    not_null_index <- which (vapply (
        res, function (i) !is.null (i$fn_calls), logical (1L)
    ))
    fn_calls_new <- lapply (res, function (i) i$fn_calls) [not_null_index]

    fname <- ifelse (cran, "fn-calls-cran.Rds", "fn-calls-ropensci.Rds")
    fname <- flist [which (basename (flist) == fname)]
    fn_calls <- readRDS (fname)
    fn_calls <- fn_calls [which (!names (fn_calls) %in% names (res))]

    fn_calls <- c (fn_calls, fn_calls_new)
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

    fname <- ifelse (cran, "idfs-fn-calls-cran.Rds", "idfs-fn-calls-ropensci.Rds")
    fname <- flist [which (basename (flist) == fname)]
    saveRDS (toks_idf, fname)

    return (toks_idf)
}

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
