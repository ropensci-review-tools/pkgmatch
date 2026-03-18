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
    expect_named (dat, c ("full", "descs_only"))
    expect_named (dat$full, c ("idfs", "token_lists"))
    expect_named (dat$descs_only, c ("idfs", "token_lists"))

    tempdir <- fs::path_common (flist)
    fs::dir_delete (tempdir)
})

test_that ("dl messages", {

    flist <- generate_pkgmatch_example_data ()
    expect_snapshot (
        withr::with_envvar (
            list ("PKGMATCH_TESTS" = "true"),
            msg <- send_dl_message (flist)
        ),
        transform = function (lines) {
            lines <- gsub (
                "Data\\salready\\sexist\\sin.*and\\swill",
                "Data already exist in 'tempdir' and will",
                lines
            )
            gsub (
                "files\\stotalling\\saround\\s[0-9].*$",
                "files totally around 1MB.",
                lines
            )
        }
    )
})
