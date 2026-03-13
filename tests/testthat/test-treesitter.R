test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

skip_if (!test_all)

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


test_that ("tree-sitter installed package", {
    pkg <- "rappdirs"
    tags <- pkgmatch_treesitter_fn_tags (pkg)
    expect_s3_class (tags, "data.frame")
    expect_true (nrow (tags) > 0L)
})

test_that ("calls in pkg", {
    pkg <- "rappdirs"
    fn_calls <- tressitter_calls_in_package (pkg, is_installed = TRUE)
    expect_s3_class (fn_calls, "tbl_df")
    expect_equal (ncol (fn_calls), 5L)
    expect_named (fn_calls, c ("fn", "name", "start", "end", "file"))
    expect_type (fn_calls$fn, "character")
    expect_type (fn_calls$name, "character")
    expect_type (fn_calls$file, "character")
    expect_type (fn_calls$start, "double")
    expect_type (fn_calls$end, "double")
    expect_true (nrow (fn_calls) > 100L)
})
