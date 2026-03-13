test_that ("similar pkgs text input", {

    withr::local_envvar (list (
        "PKGMATCH_TESTS" = "true"
    ))

    input <- "A similar package"
    n <- 5L
    txt <- get_sample_input_text ()
    idfs <- get_test_idfs (txt)

    expect_error (
        pkgmatch_similar_pkgs (input),
        "'corpus' must be specified."
    )

    out <- pkgmatch_similar_pkgs (
        input,
        corpus = "ropensci",
        idfs = idfs,
        n = n
    )
    expect_s3_class (out, "pkgmatch")
    expect_type (out, "list")
    expect_true (all (out$package %in% names (idfs$token_lists)))
    expect_equal (attr (out, "n"), n)

    # print method:
    out_p <- strsplit (capture.output (print (out)), "\\\"\\s") [[1]]
    expect_length (out_p, n)

    # head method:
    out_h <- capture.output (head (out))
    expect_length (out_h, 6L)
    # names:
    out_hdr <- strsplit (out_h [1], "[[:space:]]+") [[1]]
    out_hdr <- out_hdr [which (nzchar (out_hdr))]
    expect_length (out_hdr, 2)
    expect_equal (out_hdr, c ("package", "rank"))
    # other rows:
    out_h <- out_h [-1]
    row1 <- vapply (
        out_h,
        function (i) strsplit (i, "\\s+") [[1]] [1],
        character (1L),
        USE.NAMES = FALSE
    )
    expect_true (all (nchar (row1)) == 1L)
    expect_equal (as.integer (row1), seq_along (row1))
})

test_that ("similar pkgs text input cran", {

    withr::local_envvar (list (
        "PKGMATCH_TESTS" = "true"
    ))

    input <- "A similar package"
    n <- 5L
    txt <- get_sample_input_text ()
    idfs <- get_test_idfs (txt)

    out <- pkgmatch_similar_pkgs (
        input,
        idfs = idfs,
        n = n,
        corpus = "cran"
    )
    expect_s3_class (out, "pkgmatch")
    expect_type (out, "list")
    expect_equal (attr (out, "n"), n)
    expect_true (all (out$package %in% names (idfs$token_lists)))
    pkg_nms <- gsub ("\\_.*$", "", names (idfs$token_lists))
    expect_true (all (out$package %in% pkg_nms))

    expect_equal (ncol (out), 3L)
    expect_identical (names (out), c ("package", "version", "rank"))
})

test_that ("similar pkgs package input", {

    cache_dir <- generate_fn_call_data (corpus = "ropensci") # helper-fn-calls.R
    cache_dir <- generate_fn_call_data (corpus = "cran")

    withr::local_envvar (list (
        "PKGMATCH_TESTS" = "true",
        "PKGMATCH_CACHE_DIR" = cache_dir
    ))

    path <- pkgmatch_test_skeleton (pkg_name = "demo")
    roxygen2::roxygenise (path)

    n <- 5L
    npkgs <- 10L
    txt <- get_sample_input_text ()
    idfs <- get_test_idfs (txt)
    out <- pkgmatch_similar_pkgs (
        path,
        corpus = "ropensci",
        idfs = idfs,
        n = n
    )

    unlink (cache_dir)

    # detach is critical here, because httptest2 uses `utils::sessionInfo()`,
    # which checks namespaces and tries to load DESC file from pkg location.
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)

    expect_s3_class (out, "pkgmatch")
    expect_s3_class (out, "data.frame")
    expect_equal (attr (out, "n"), n)
    expect_equal (ncol (out), 2L)
    expect_identical (names (out), c ("package", "rank"))
    expect_equal (nrow (out), npkgs)

    expect_true (all (out$package %in% names (idfs$token_lists)))

    # print method:
    out_p <- capture.output (print (out))
    lens <- vapply (
        out_p,
        function (n) length (strsplit (n, "\\\"\\s") [[1]]),
        integer (1L),
        USE.NAMES = FALSE
    )
    expect_true (all (lens == n))

    # head method:
    out_h <- capture.output (head (out))
    expect_length (out_h, 6L)
    # names:
    out_hdr <- strsplit (out_h [1], "[[:space:]]+") [[1]]
    out_hdr <- out_hdr [which (nzchar (out_hdr))]
    expect_length (out_hdr, 2)
    expect_equal (out_hdr, c ("package", "rank"))
    # other rows:
    out_h <- out_h [-1]
    row1 <- vapply (
        out_h,
        function (i) strsplit (i, "\\s+") [[1]] [1],
        character (1L),
        USE.NAMES = FALSE
    )
    expect_true (all (nchar (row1)) == 1L)
    expect_equal (as.integer (row1), seq_along (row1))
})

test_that ("similar pkgs package input for cran", {

    withr::local_envvar (list (
        "PKGMATCH_TESTS" = "true"
    ))

    path <- pkgmatch_test_skeleton (pkg_name = "demo")
    roxygen2::roxygenise (path)

    n <- 5L
    npkgs <- 10L

    txt <- get_sample_input_text ()
    idfs <- get_test_idfs (txt)

    out <- pkgmatch_similar_pkgs (
        path,
        idfs = idfs,
        corpus = "cran",
        n = n
    )

    # detach is critical here, because httptest2 uses `utils::sessionInfo()`,
    # which checks namespaces and tries to load DESC file from pkg location.
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)

    expect_s3_class (out, "pkgmatch")
    expect_s3_class (out, "data.frame")
    expect_equal (attr (out, "n"), n)
    expect_equal (ncol (out), 3L)
    expect_identical (names (out), c ("package", "version", "rank"))
    expect_equal (nrow (out), npkgs)

    expect_true (all (out$package %in% names (idfs$token_lists)))
    nms_wo_tar <- gsub ("\\_.*$", "", names (idfs$token_lists))
    expect_true (all (out$package %in% nms_wo_tar))
})
