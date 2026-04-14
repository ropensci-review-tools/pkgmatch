test_that ("bm25 internal structure", {

    pkgs <- c ("cli", "checkmate", "rappdirs")
    txt <- lapply (pkgs, get_pkg_text)
    names (txt) <- pkgs

    res0 <- bm25_idf_internal (txt)
    expect_s3_class (res0, "data.frame")
    expect_equal (ncol (res0), 2L)
    expect_identical (names (res0), c ("token", "idf"))
})

test_that ("bm25 main function", {

    flist <- generate_pkgmatch_example_data ()

    input <- "A package which does some stuff"
    res <- pkgmatch_bm25 (input = input, corpus = "ropensci")
    expect_s3_class (res, "data.frame")
    expect_equal (ncol (res), 2L)
    expect_equal (names (res), c ("package", "bm25"))
    expect_gt (nrow (res), 2L)
    # All packages listed with version numbers:
    expect_true (all (grepl ("\\_[0-9]", res$package)))

})

test_that ("bm25 fn calls", {

    flist <- generate_pkgmatch_example_data ()

    fn_calls <- pkgmatch_bm25_fn_calls (path = "cli", corpus = "ropensci")
    expect_s3_class (fn_calls, "data.frame")
    expect_equal (ncol (fn_calls), 2L)
    expect_named (fn_calls, c ("package", "bm25"))
    expect_true (nrow (fn_calls) > 5L)

    ex_dir <- fs::path_common (flist)
    fs::dir_delete (ex_dir)
})
