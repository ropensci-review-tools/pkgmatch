# This helper is only necessary because in the environment(s) created by
# `testthat::test_local()`, httrtest2 does not mock the full result, and
# downloads are still triggered for function call data.
# This does not happen in testthat::test_file("test-similar.R"), nor in
# devtools::test(), but only in `test_local()`.
#
# The helper just creates mock versions of the function names data so those are
# loaded directly, and download is avoided.
mknm <- function (nchar = 12) {
    x <- sample (c (letters, LETTERS), size = nchar)
    paste0 (x, collapse = "")
}

generate_fn_call_data <- function (corpus = "ropensci", npkgs = 10, nfns = 10) {
    fp <- file.path (tempdir (), "pkgmatch")
    if (!dir.exists (fp)) {
        dir.create (fp, recursive = TRUE)
    }
    withr::local_envvar (list ("PKGMATCH_CACHE_DIR" = fp))

    # similate idf data:
    pkgs <- vapply (seq_len (npkgs), function (i) mknm (6), character (1L))
    pkgs <- rep (pkgs, each = nfns)
    fns <- vapply (seq_len (npkgs * nfns), function (i) mknm (10), character (1L))
    fn_calls <- data.frame (
        token = paste0 (pkgs, "::", fns),
        idf = stats::runif (length (fns), max = 5)
    )

    fname <- get_cache_file_name (what = "calls", corpus = corpus, raw = FALSE)
    fname <- fs::path (pkgmatch_cache_path (), fname)
    saveRDS (fn_calls, fname)

    # Then similate fn call frequencies:
    pkgs <- unique (pkgs)
    fn_calls <- lapply (pkgs, function (i) {
        nout <- round (stats::runif (1L, min = 1, max = 20))
        pkgs <- vapply (seq_len (nout), function (i) mknm (6), character (1L))
        fns <- vapply (seq_len (nout), function (i) mknm (10), character (1L))
        n <- ceiling (stats::rgamma (nout, shape = 1))
        names (n) <- paste0 (pkgs, "::", fns)
        return (n)
    })
    names (fn_calls) <- pkgs

    fname <- get_cache_file_name (what = "functions", corpus = corpus, raw = FALSE)
    fname <- fs::path (pkgmatch_cache_path (), fname)
    saveRDS (fn_calls, fname)

    return (fp)
}
