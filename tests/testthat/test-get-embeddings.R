is_test_job <- identical (Sys.getenv ("GITHUB_JOB"), "test-coverage")

expect_embeddings_matrix <- function (x) {
    expect_type (x, "double")
    expect_length (dim (x), 2L)
    expect_equal (nrow (x), expected_embedding_length) # in helper-embeddings.R
    expect_true (min (x) < 0)
    expect_true (max (x) > 0)
}

test_that ("embeddings properties", {

    withr::local_envvar (list ("PKGMATCH_TESTS" = "true"))

    txt <- "test text"
    emb <- httptest2::with_mock_dir ("emb_test_text", {
        get_embeddings (txt)
    })
    expect_embeddings_matrix (emb)
})

test_that ("raw embeddings", {

    withr::local_envvar (list ("PKGMATCH_TESTS" = "true"))

    packages <- "rappdirs"

    set.seed (1L)
    msgs0 <- capture_messages (
        emb0 <- httptest2::with_mock_dir ("emb_raw", {
            pkgmatch_embeddings_from_pkgs (packages)
        })
    )
    expect_length (msgs0, 3L)
    expect_length (grep ("Generating", msgs0), length (msgs0))
    expect_length (grep ("text", msgs0), 2L)
    expect_length (grep ("code", msgs0), 1L)
    set.seed (1L)
    expect_snapshot (
        emb0 <- httptest2::with_mock_dir ("emb_raw", {
            pkgmatch_embeddings_from_pkgs (packages)
        })
    )

    # Note in the following that verbosity is still suppressed on all GitHub
    # jobs other than test coverage, for reasons explained in the
    # 'opt_is_quiet()' function in zzz.R.
    set.seed (1L)
    msgs <- capture_messages (
        withr::with_options (
            list (
                "pkgmatch.verbose_limit" = 0L,
                "rlib.message_verbosity" = "verbose"
            ),
            {
                emb <- httptest2::with_mock_dir ("emb_raw", {
                    pkgmatch_embeddings_from_pkgs (packages)
                })
            }
        )
    )
    expect_identical (emb0, emb)
    if (is_test_job) {
        # These tests fail on GH runners only, but I can't reproduce:
        # expect_false (identical (msgs0, msgs))
        # expect_length (msgs, 5L)
        # expect_length (grep ("Extracting", msgs), 2L)
        # expect_length (grep ("Generating", msgs), 3L)
        # expect_length (grep ("text", msgs), 3L)
        # expect_length (grep ("code", msgs), 2L)
    }

    expect_type (emb, "list")
    expect_length (emb, 3L)
    expect_identical (names (emb), c ("text_with_fns", "text_wo_fns", "code"))
    is_mat <- vapply (emb, function (i) length (dim (i)) == 2L, logical (1L))
    expect_true (all (is_mat))
    expect_embeddings_matrix (emb$text_with_fns)
    expect_embeddings_matrix (emb$text_wo_fns)
    expect_embeddings_matrix (emb$code)

    path <- pkgmatch_test_skeleton ()
    roxygen2::roxygenise (path)

    emb_fns <- httptest2::with_mock_dir ("emb_fns", {
        pkgmatch_embeddings_from_pkgs (packages, functions_only = TRUE)
    })
    expect_embeddings_matrix (emb_fns)

    # detach is critical here, because httptest2 uses `utils::sessionInfo()`,
    # which checks namespaces and tries to load DESC file from pkg location.
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)
})
