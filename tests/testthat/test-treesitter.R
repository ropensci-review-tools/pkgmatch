test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

test_that ("tree-sitter", {

    withr::local_envvar (list ("PKGMATCH_TESTS" = "true"))

    path <- pkgmatch_test_skeleton ()
    roxygen2::roxygenise (path)

    tags <- pkgmatch_treesitter_fn_tags (path)
    expect_s3_class (tags, "data.frame")
    expect_true (nrow (tags) > 0L)
    expect_identical (names (tags), c ("fn", "name", "start", "end", "file"))
    expect_true ("demo::test_fn" %in% tags$fn)
    expect_true ("base::message" %in% tags$name)

    # detach is critical here, because httptest2 uses `utils::sessionInfo()`,
    # which checks namespaces and tries to load DESC file from pkg location.
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)
})


skip_if (!test_all)
test_that ("tree-sitter installed package", {
    pkg <- "rappdirs"
    tags <- pkgmatch_treesitter_fn_tags (pkg)
    expect_s3_class (tags, "data.frame")
    expect_true (nrow (tags) > 0L)
})
