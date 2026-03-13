test_that ("bm25", {

    pkgs <- c ("cli", "checkmate", "rappdirs")
    txt <- lapply (pkgs, get_pkg_text)
    names (txt) <- pkgs

    res0 <- bm25_idf_internal (txt)
    expect_s3_class (res0, "data.frame")
    expect_equal (ncol (res0), 2L)
    expect_identical (names (res0), c ("token", "idf"))

    # Fails on gha windows machines at `cli::has_keypress_support()`
    input <- "A package which does some stuff"
    res <- pkgmatch_bm25 (input = input, txt = txt, corpus = "ropensci")
    expect_s3_class (res, "data.frame")
    expect_equal (ncol (res), 2L)
    expect_equal (names (res), c ("package", "bm25"))
    expect_equal (nrow (res), 3L)
    expect_true (all (pkgs %in% res$package))
})

test_that ("bm25 fn calls", {

    path <- generate_pkgmatch_example_data ()
    fn_calls <- pkgmatch_bm25_fn_calls (path = "cli", corpus = "cran")
    expect_s3_class (fn_calls, "data.frame")
    expect_equal (ncol (fn_calls), 2L)
    expect_named (fn_calls, c ("package", "bm25"))
    expect_true (nrow (fn_calls) > 5L)
})
