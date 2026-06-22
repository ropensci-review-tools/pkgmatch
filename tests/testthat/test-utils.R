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

test_that ("corpus param", {
    expect_identical (check_corpus_param ("rtgoia"), "ropensci")
    expect_identical (check_corpus_param ("bbb"), "bioc")
    expect_identical (check_corpus_param ("raltih"), "ropensci")
    expect_error (check_corpus_param ("a"), "Unknown corpus")

    corpus <- "a"
    expect_error (pkgmatch_similar_pkgs ("text", corpus), "Unknown corpus")
    expect_error (pkgmatch_similar_fns ("text", corpus), "Unknown corpus")
})
