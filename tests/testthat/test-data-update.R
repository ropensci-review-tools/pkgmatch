test_that ("data update extract from local dir", {

    withr::local_envvar (list ("PKGMATCH_TESTS" = "true"))

    path <- pkgmatch_test_skeleton ()
    expect_true (dir.exists (path))
    roxygen2::roxygenise (path) # Generate man files

    dat <- with_mock_dir ("update", {
        extract_data_from_local_dir (path)
    })

    expect_type (dat, "list")
    expect_length (dat, 4L)
    expect_equal (names (dat), c ("embeddings", "embeddings_fns", "bm25", "fn_calls"))

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

    withr::local_envvar (list ("PKGMATCH_TESTS" = "true"))

    # Simulate cached embeddings:
    packages <- "rappdirs"
    emb <- with_mock_dir ("emb_raw", {
        pkgmatch_embeddings_from_pkgs (packages)
    })
    f <- fs::path (fs::path_temp (), "embeddings-ropensci.Rds")
    saveRDS (emb, f)

    # Generate locally updated embeddings:
    path <- pkgmatch_test_skeleton ()
    expect_true (dir.exists (path))
    roxygen2::roxygenise (path) # Generate man files

    dat <- with_mock_dir ("update", {
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

    # Generate locally updated bm25:
    path <- pkgmatch_test_skeleton ()
    expect_true (dir.exists (path))
    roxygen2::roxygenise (path) # Generate man files

    dat <- with_mock_dir ("update", {
        extract_data_from_local_dir (path)
    })
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)

    dat <- list ("demo" = dat)
    expect_silent (
        append_data_to_bm25 (dat, f, cran = FALSE)
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
