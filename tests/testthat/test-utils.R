test_that ("verbose limit option", {

    expect_equal (get_verbose_limit (), 50L)
    expect_type (get_verbose_limit (), "integer")

    vl <- withr::with_options (
        list ("pkgmatch.verbose_limit" = 100L),
        get_verbose_limit ()
    )
    expect_equal (vl, 100L)

    expect_error (
        withr::with_options (
            list ("pkgmatch.verbose_limit" = NULL),
            get_verbose_limit ()
        ),
        "verbose_limit option not defined"
    )
})
