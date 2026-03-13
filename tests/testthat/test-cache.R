test_that ("cache update interval", {

    i <- pkgmatch_cache_update_interval ()
    expect_type (i, "double")
    expect_equal (i, 30)

    op <- getOption ("pkgmatch.update_frequenty")
    options ("pkgmatch.update_frequency" = 1.0)
    expect_equal (pkgmatch_cache_update_interval (), 1)
    options ("pkgmatch.update_frequency" = op)
    expect_equal (pkgmatch_cache_update_interval (), 30)
})

test_that ("load data", {

    flist <- generate_pkgmatch_example_data ()
    expect_silent (
        dat <- load_data_internal (what = "idfs", corpus = "cran", fns = FALSE, raw = FALSE)
    )
    expect_type (dat, "list")
    expect_named (dat, c ("idfs", "token_lists"))

    tempdir <- fs::path_common (flist)
    fs::dir_delete (tempdir)
})
