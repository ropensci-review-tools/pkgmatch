test_that ("data update extract from local dir", {

    withr::local_envvar (list (
        "PKGMATCH_TESTS" = "true",
        "pkgmatch.ollama.url" = "127.0.0.1:11434"
    ))

    path <- pkgmatch_test_skeleton ()
    expect_true (dir.exists (path))
    roxygen2::roxygenise (path) # Generate man files

    set.seed (1L)
    dat <- httptest2::with_mock_dir ("update", {
        extract_data_from_local_dir (path)
    })

    expect_type (dat, "list")
    expect_length (dat, 5L)
    expect_equal (names (dat), c ("embeddings", "embeddings_fns", "bm25", "bm25_fns", "fn_calls"))

    # -------- test embeddings --------
    expect_type (dat$embeddings, "list")
    expect_length (dat$embeddings, 3L)
    expect_equal (names (dat$embeddings), c ("text_with_fns", "text_wo_fns", "code"))
    classes <- do.call (rbind, lapply (dat$embeddings, class))
    expect_equal (nrow (classes), 3L)
    expect_true (all (classes [, 1] == "matrix"))
    expect_true (all (classes [, 2] == "array"))
    dims <- do.call (rbind, lapply (dat$embeddings, dim))
    expect_equal (nrow (dims), 3L)
    expect_true (all (dims [, 1] == 768L))
    expect_true (all (dims [, 2] == 1L))

    # -------- test embeddings_fns --------
    expect_type (dat$embeddings_fns, "double")
    expect_equal (dim (dat$embeddings_fns), c (768L, 1L))

    # -------- test bm25 --------
    expect_type (dat$bm25, "list")
    expect_length (dat$bm25, 2L)
    expect_equal (names (dat$bm25), c ("idfs", "token_lists"))

    # -------- test bm25 idfs --------
    expect_type (dat$bm25$idfs, "list")
    expect_length (dat$bm25$idfs, 2L)
    expect_equal (names (dat$bm25$idfs), c ("with_fns", "wo_fns"))

    expect_s3_class (dat$bm25$idfs$with_fns, "data.frame")
    expect_equal (names (dat$bm25$idfs$with_fns), c ("token", "idf"))
    expect_true (nrow (dat$bm25$idfs$with_fns) > 10L)

    expect_s3_class (dat$bm25$idfs$wo_fns, "data.frame")
    expect_equal (names (dat$bm25$idfs$wo_fns), c ("token", "idf"))
    expect_true (nrow (dat$bm25$idfs$wo_fns) > 10L)
    expect_true (nrow (dat$bm25$idfs$wo_fns) <
        nrow (dat$bm25$idfs$with_fns))

    # -------- test bm25 token_lists --------
    expect_type (dat$bm25$token_lists, "list")
    expect_length (dat$bm25$token_lists, 2L)
    expect_equal (names (dat$bm25$token_lists), c ("with_fns", "wo_fns"))

    expect_type (dat$bm25$token_lists$with_fns, "list")
    expect_length (dat$bm25$token_lists$with_fns, 1L)
    expect_s3_class (dat$bm25$token_lists$with_fns [[1]], "data.frame")
    expect_equal (names (dat$bm25$token_lists$with_fns [[1]]), c ("token", "n"))
    expect_true (nrow (dat$bm25$token_lists$with_fns [[1]]) > 10L)

    expect_type (dat$bm25$token_lists$wo_fns, "list")
    expect_length (dat$bm25$token_lists$wo_fns, 1L)
    expect_s3_class (dat$bm25$token_lists$wo_fns [[1]], "data.frame")
    expect_equal (names (dat$bm25$token_lists$wo_fns [[1]]), c ("token", "n"))
    expect_true (nrow (dat$bm25$token_lists$wo_fns [[1]]) > 10L)
    expect_true (nrow (dat$bm25$token_lists$wo_fns [[1]]) <
        nrow (dat$bm25$token_lists$with_fns [[1]]))

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

test_that ("data update append to embeddings", {

    withr::local_envvar (list (
        "PKGMATCH_TESTS" = "true",
        "pkgmatch.ollama.url" = "127.0.0.1:11434"
    ))

    # Simulate cached embeddings:
    set.seed (1L)
    packages <- "rappdirs"
    emb <- httptest2::with_mock_dir ("emb_raw", {
        pkgmatch_embeddings_from_pkgs (packages)
    })
    f <- fs::path (fs::path_temp (), "embeddings-ropensci.Rds")
    saveRDS (emb, f)

    # Generate locally updated embeddings:
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
        append_data_to_embeddings (dat, f, cran = FALSE)
    )
    emb2 <- readRDS (f)

    for (what in names (emb)) {
        expect_equal (ncol (emb [[what]]), 1L)
        expect_equal (ncol (emb2 [[what]]), 2L)
        expect_equal (colnames (emb [[what]]), "rappdirs")
        # colnames of expanded data are orderd alphabetically:
        expect_equal (colnames (emb2 [[what]]), c ("demo", "rappdirs"))
    }
})

test_that ("data update append to bm25", {

    withr::local_envvar (list (
        "PKGMATCH_TESTS" = "true",
        "pkgmatch.ollama.url" = "127.0.0.1:11434"
    ))

    pkgs <- c ("cli", "checkmate", "rappdirs")
    txt_with_fns <- lapply (pkgs, get_pkg_text)
    txt_wo_fns <- rm_fns_from_pkg_txt (txt_with_fns)
    code <- lapply (pkgs, get_pkg_code)
    names (txt_with_fns) <- names (txt_wo_fns) <- names (code) <- pkgs

    idfs <- list (
        with_fns = bm25_idf (txt_with_fns),
        wo_fns = bm25_idf (txt_wo_fns)
    )
    token_lists <- list (
        with_fns = bm25_tokens_list (txt_with_fns),
        wo_fns = bm25_tokens_list (txt_wo_fns)
    )
    bm25_pre <- list (idfs = idfs, token_lists = token_lists)
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

    for (what in c ("with_fns", "wo_fns")) {
        expect_true (nrow (bm25_post$idfs [[what]]) >
            nrow (bm25_pre$idfs [[what]]))

        expect_length (bm25_pre$token_lists [[what]], 3L)
        expect_equal (
            names (bm25_pre$token_lists [[what]]),
            c ("cli", "checkmate", "rappdirs")
        )

        expect_length (bm25_post$token_lists [[what]], 4L)
        expect_equal (
            names (bm25_post$token_lists [[what]]),
            c ("cli", "checkmate", "rappdirs", "demo")
        )
    }
})

test_that ("data update append to fn calls", {

    withr::local_envvar (list (
        "PKGMATCH_TESTS" = "true",
        "pkgmatch.ollama.url" = "127.0.0.1:11434"
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
    flist <- c (f, fs::path (fs::path_temp (), "idfs-fn-calls-ropensci.Rds"))
    expect_silent (
        append_data_to_fn_calls (dat, flist, cran = FALSE)
    )
    calls_post <- readRDS (f)

    expect_length (calls_post, 4L)
    expect_true ("demo" %in% names (calls_post))
    expect_true (length (calls_post$demo) > 0L)
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
