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

test_that ("install path", {
    ip <- pkg_install_path ("curl")
    expect_length (ip, 1L)
    expect_type (ip, "character")
    expect_true (fs::dir_exists (ip))
})
