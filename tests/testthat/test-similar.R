test_that ("similar pkgs text input", {

    withr::local_envvar (list ("PKGMATCH_TESTS" = "true"))

    input <- "A similar package"
    n <- 5L
    embeddings <- get_test_embeddings (
        npkgs = 10,
        nfns = 10,
        embedding_len = 768
    )
    txt <- c (
        "a not so very similar package",
        "a test package",
        "a function to test"
    )
    idfs <- get_test_idfs (txt)
    out <- with_mock_dir ("sim_pkgs_txt", {
        pkgmatch_similar_pkgs (
            input,
            embeddings = embeddings,
            idfs = idfs,
            n = n
        )
    })
    expect_s3_class (out, "pkgmatch")
    expect_type (out, "list")
    expect_true (all (out$package %in% colnames (embeddings$text_with_fns)))
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

test_that ("similar pkgs package input", {

    withr::local_envvar (list ("PKGMATCH_TESTS" = "true"))

    path <- pkgmatch_test_skeleton (pkg_name = "demo")
    roxygen2::roxygenise (path)

    n <- 5L
    npkgs <- 10L
    embeddings <- get_test_embeddings (
        npkgs = npkgs,
        nfns = npkgs,
        embedding_len = 768
    )
    txt <- c (
        "a not so very similar package",
        "a test package",
        "a function to test"
    )
    idfs <- get_test_idfs (txt)
    out <- with_mock_dir ("sim_pkgs_pkg", {
        pkgmatch_similar_pkgs (
            path,
            embeddings = embeddings,
            idfs = idfs,
            n = n
        )
    })

    # detach is critical here, because httptest2 uses `utils::sessionInfo()`,
    # which checks namespaces and tries to load DESC file from pkg location.
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)

    expect_s3_class (out, "pkgmatch")
    expect_s3_class (out, "data.frame")
    expect_equal (attr (out, "n"), n)
    expect_equal (ncol (out), 3L)
    expect_identical (names (out), c ("package", "text_rank", "code_rank"))
    expect_false (identical (out$text, out$code))
    expect_equal (nrow (out), npkgs)

    expect_true (all (out$package %in% colnames (embeddings$text_with_fns)))

    # print method:
    out_p <- capture.output (print (out))
    expect_true (any (grepl ("^\\$text$", out_p)))
    expect_true (any (grepl ("^\\$code$", out_p)))
    nm_lines <- grep ("^\\[", out_p, value = TRUE)
    lens <- vapply (
        nm_lines,
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
    expect_length (out_hdr, 3)
    expect_equal (out_hdr, c ("package", "text_rank", "code_rank"))
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

    withr::local_envvar (list ("PKGMATCH_TESTS" = "true"))

    path <- pkgmatch_test_skeleton (pkg_name = "demo")
    roxygen2::roxygenise (path)

    n <- 5L
    npkgs <- 10L
    embeddings <- get_test_embeddings (
        npkgs = npkgs,
        nfns = npkgs,
        embedding_len = 768,
        cran = TRUE
    )

    txt <- c (
        "a not so very similar package",
        "a test package",
        "a function to test"
    )
    idfs <- get_test_idfs (txt)
    out <- with_mock_dir ("sim_pkgs_cran", {
        pkgmatch_similar_pkgs (
            path,
            embeddings = embeddings,
            idfs = idfs,
            corpus = "cran",
            n = n
        )
    })

    # detach is critical here, because httptest2 uses `utils::sessionInfo()`,
    # which checks namespaces and tries to load DESC file from pkg location.
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)

    expect_s3_class (out, "pkgmatch")
    expect_s3_class (out, "data.frame")
    expect_equal (attr (out, "n"), n)
    expect_equal (ncol (out), 4L)
    expect_identical (names (out), c ("package", "version", "text_rank", "code_rank"))
    expect_false (identical (out$text, out$code))
    expect_equal (nrow (out), npkgs)

    expect_false (all (out$package %in% colnames (embeddings$text_with_fns)))
    nms_wo_tar <- gsub ("\\_.*$", "", colnames (embeddings$text_with_fns))
    expect_true (all (out$package %in% nms_wo_tar))
})

test_that ("similar fns", {

    withr::local_envvar (list ("PKGMATCH_TESTS" = "true"))

    nfns <- 10L
    embeddings_fns <- get_test_embeddings_fns (nfns = nfns, embedding_len = 768)

    input <- "A test function"
    n <- 5L
    out <- with_mock_dir ("sim_fns", {
        pkgmatch_similar_fns (input = input, embeddings = embeddings_fns, n = n)
    })
    expect_s3_class (out, "pkgmatch")
    expect_type (out, "list")
    expect_equal (nrow (out), nfns)
    expect_equal (ncol (out), 3L)
    expect_identical (names (out), c ("package", "simil", "rank"))
    expect_true (all (out$package %in% colnames (embeddings_fns)))
    expect_identical (out$rank, seq_len (nrow (out)))

    # print method:
    out_p <- strsplit (capture.output (print (out)), "\\\"\\s") [[1]]
    expect_length (out_p, n)

    # head method:
    out_h <- capture.output (head (out))
    expect_length (out_h, 6L)
    # names:
    out_hdr <- strsplit (out_h [1], "[[:space:]]+") [[1]]
    out_hdr <- out_hdr [which (nzchar (out_hdr))]
    expect_length (out_hdr, 3)
    expect_equal (out_hdr, c ("package", "simil", "rank"))
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
