test_that ("bm25", {

    pkgs <- c ("cli", "checkmate", "rappdirs")
    txt <- lapply (pkgs, get_pkg_text)
    code <- lapply (pkgs, get_pkg_code)
    names (txt) <- names (code) <- pkgs

    res0 <- bm25_idf_internal (txt)
    expect_s3_class (res0, "data.frame")
    expect_equal (ncol (res0), 2L)
    expect_identical (names (res0), c ("token", "idf"))

    res1 <- bm25_idf_internal (code)
    expect_s3_class (res1, "data.frame")
    expect_equal (ncol (res1), 2L)
    expect_identical (names (res1), c ("token", "idf"))
    expect_true (nrow (res0) != nrow (res1))

    input <- "A package which does some stuff"
    res <- pkgmatch_bm25 (input = input, txt = txt, corpus = "ropensci")
    expect_s3_class (res, "data.frame")
    expect_equal (ncol (res), 3L)
    expect_equal (names (res), c ("package", "bm25_with_fns", "bm25_wo_fns"))
    expect_equal (nrow (res), 3L)
    expect_true (all (pkgs %in% res$package))
})
