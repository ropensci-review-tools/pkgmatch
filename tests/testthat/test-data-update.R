test_that ("data update functions", {

    withr::local_envvar (list ("PKGMATCH_TESTS" = "true"))

    path <- pkgmatch_test_skeleton ()
    expect_true (dir.exists (path))
    roxygen2::roxygenise (path) # Generate man files

    dat <- with_mock_dir ("update", {
        extract_data_from_local_dir (path)
    })

    expect_type (dat, "list")
    expect_length (dat, 4L)
    expect_equal (names (dat), c ("embeddings", "embeddings_fns", "bm25", "fn_calls"))

    expect_type (dat$embeddings, "list")
    expect_length (dat$embeddings, 3L)
    expect_equal (names (dat$embeddings), c ("text_with_fns", "text_wo_fns", "code"))
    classes <- do.call (rbind, lapply (dat$embeddings, class))
    expect_true (all (classes [, 1] == "matrix"))
    expect_true (all (classes [, 2] == "array"))
    dims <- do.call (rbind, lapply (dat$embeddings, dim))
    expect_true (all (dims [, 1] == 768L))
    expect_true (all (dims [, 2] == 1L))

    # detach is critical here, because httptest2 uses `utils::sessionInfo()`,
    # which checks namespaces and tries to load DESC file from pkg location.
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)
})
