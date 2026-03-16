# Pkg data are mocked because the treesitter fn calls also call
# 'pkg_fns_from_r_search()' to get full namespaces of all dependencies.

test_that ("data update extract from local dir", {

    withr::local_envvar (list (
        "PKGMATCH_TESTS" = "true"
    ))

    path <- pkgmatch_test_skeleton ()
    expect_true (dir.exists (path))
    roxygen2::roxygenise (path) # Generate man files

    set.seed (1L)
    dat <- httptest2::with_mock_dir ("update", {
        extract_data_from_local_dir (path)
    })

    expect_type (dat, "list")
    expect_length (dat, 3L)
    expect_equal (names (dat), c ("bm25", "bm25_fns", "fn_calls"))

    # -------- test bm25 --------
    expect_type (dat$bm25, "list")
    expect_length (dat$bm25, 2L)
    expect_equal (names (dat$bm25), c ("idfs", "token_lists"))

    # -------- test bm25 idfs --------
    expect_s3_class (dat$bm25$idfs, "data.frame")
    expect_equal (ncol (dat$bm25$idfs), 2L)
    expect_named (dat$bm25$idfs, c ("token", "idf"))
    expect_true (nrow (dat$bm25$idfs) > 10L)

    # -------- test bm25 token_lists --------
    expect_type (dat$bm25$token_lists, "list")
    expect_length (dat$bm25$token_lists, 1L)

    expect_s3_class (dat$bm25$token_lists [[1]], "data.frame")
    expect_equal (names (dat$bm25$token_lists [[1]]), c ("token", "n"))
    expect_true (nrow (dat$bm25$token_lists [[1]]) > 10L)

    # -------- test bm25_fns --------
    expect_type (dat$bm25_fns, "list")
    expect_named (dat$bm25_fns)
    expect_length (dat$bm25_fns, 2L)
    expect_equal (names (dat$bm25_fns), c ("idfs", "token_lists"))
    expect_s3_class (dat$bm25_fns$idfs, "data.frame")
    expect_equal (names (dat$bm25_fns$idfs), c ("token", "idf"))
    expect_true (nrow (dat$bm25_fns$idfs) > 1L)
    expect_type (dat$bm25_fns$token_lists, "list")
    expect_named (dat$bm25_fns$token_lists)
    expect_length (dat$bm25_fns$token_lists, 1L)
    expect_s3_class (dat$bm25_fns$token_lists [[1]], "data.frame")
    expect_equal (names (dat$bm25_fns$token_lists [[1]]), c ("token", "n"))
    expect_true (nrow (dat$bm25_fns$token_lists [[1]]) > 1L)

    # -------- test fn_calls --------
    # Test package has only one function call to "message()":
    expect_type (dat$fn_calls, "integer")
    expect_named (dat$fn_calls)
    expect_length (dat$fn_calls, 1L)
    expect_equal (as.integer (dat$fn_calls), 1L)
    expect_equal (names (dat$fn_calls), c ("base::message"))

    # detach is critical here, because httptest2 uses `utils::sessionInfo()`,
    # which checks namespaces and tries to load DESC file from pkg location.
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)
})

test_that ("data update append to bm25", {

    withr::local_envvar (list (
        "PKGMATCH_TESTS" = "true"
    ))

    pkgs <- c ("cli", "checkmate", "rappdirs")
    txt_with_fns <- lapply (pkgs, get_pkg_text)
    txt <- rm_fns_from_pkg_txt (txt_with_fns)
    names (txt) <- pkgs

    bm25_pre <- list (
        idfs = bm25_idf (txt),
        token_lists = bm25_tokens_list (txt)
    )
    f <- fs::path (fs::path_temp (), "bm25-ropensci.Rds")
    saveRDS (bm25_pre, f)

    txt_fns <- get_all_fn_descs (txt_with_fns)
    fns_idfs <- bm25_idf (txt_fns$desc)
    fns_lists <- bm25_tokens_list (txt_fns$desc)
    index <- which (vapply (fns_lists, nrow, integer (1L)) > 0L)
    fns_lists <- fns_lists [index]
    names (fns_lists) <- txt_fns$fn [index]
    bm25_pre_fns <- list (idfs = fns_idfs, token_lists = fns_lists)
    f_fns <- fs::path (fs::path_temp (), "bm25-ropensci-fns.Rds")
    saveRDS (bm25_pre_fns, f_fns)

    # Generate locally updated bm25:
    path <- pkgmatch_test_skeleton ()
    expect_true (dir.exists (path))
    roxygen2::roxygenise (path) # Generate man files

    set.seed (1L)
    dat <- httptest2::with_mock_dir ("update", {
        extract_data_from_local_dir (path)
    })
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)

    dat <- list ("demo" = dat)
    expect_silent (
        append_data_to_bm25 (dat, c (f, f_fns), cran = FALSE)
    )
    bm25_post <- readRDS (f)

    expect_true (nrow (bm25_post$idfs) >
        nrow (bm25_pre$idfs))

    expect_length (bm25_pre$token_lists, 3L)
    expect_equal (
        names (bm25_pre$token_lists),
        c ("cli", "checkmate", "rappdirs")
    )

    expect_length (bm25_post$token_lists, 4L)
    expect_equal (
        names (bm25_post$token_lists),
        c ("cli", "checkmate", "rappdirs", "demo")
    )
})

test_that ("data update append to fn calls", {

    withr::local_envvar (list (
        "PKGMATCH_TESTS" = "true"
    ))

    pkgs <- c ("cli", "checkmate", "rappdirs")
    calls <- lapply (pkgs, function (f) {
        res <- pkgmatch::pkgmatch_treesitter_fn_tags (f)
        sort (table (res$name), decreasing = TRUE)
    })
    names (calls) <- pkgs
    f <- fs::path (fs::path_temp (), "fn-calls-ropensci.Rds")
    saveRDS (calls, f)

    # Generate locally updated fn-calls:
    path <- pkgmatch_test_skeleton ()
    expect_true (dir.exists (path))
    roxygen2::roxygenise (path) # Generate man files

    set.seed (1L)
    dat <- httptest2::with_mock_dir ("update", {
        extract_data_from_local_dir (path)
    })
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)

    dat <- list ("demo" = dat)
    expect_silent (
        append_data_to_fn_calls (dat, f, cran = FALSE)
    )
    calls_post <- readRDS (f)

    expect_length (calls_post, 2L)
    expect_named (calls_post, c ("idfs", "calls"))
    expect_true ("demo" %in% names (calls_post$calls))
    expect_true (length (calls_post$calls$demo) > 0L)
})

test_that ("list remote files", {

    withr::local_envvar (list (
        "PKGMATCH_TESTS" = "true"
    ))

    flist <- list_remote_files ()

    expect_s3_class (flist, "data.frame")
    expect_equal (ncol (flist), 6L)
    expect_named (
        flist,
        c ("file_name", "size", "timestamp", "tag", "owner", "repo")
    )
    expect_true (all (flist$owner == "ropensci-review-tools"))
    expect_true (all (flist$repo == "pkgmatch"))
    # expect_true (all (flist$timestamp == as.POSIXct ("2025-01-01T00:00:00")))
    expect_true (all (grepl ("\\.Rds", flist$file_name)))
})

test_that ("tarball", {

    path <- pkgmatch_test_skeleton ()
    files_in <- fs::dir_ls (path, recurse = TRUE)
    path_gz <- paste0 (path, ".tar.gz")
    files_rel <- fs::path_rel (files_in, fs::path_temp ())
    withr::with_dir (
        fs::path_temp (),
        utils::tar (path_gz, files = files_rel, compression = "gzip", tar = "tar")
    )
    fs::dir_delete (path)
    expect_false (any (fs::file_exists (files_in)))

    path <- extract_tarball (path_gz)
    expect_true (fs::dir_exists (path))
    files_out <- fs::dir_ls (path, recurse = TRUE)
    expect_identical (files_in, files_out)
    expect_true (all (fs::file_exists (files_in)))
})
