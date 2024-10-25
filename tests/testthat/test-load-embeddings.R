test_that ("load embeddings", {

    fp <- file.path (tempdir (), "pkgmatch")
    if (!dir.exists (fp)) {
        dir.create (fp, recursive = TRUE)
    }
    Sys.setenv ("PKGMATCH_CACHE_DIR" = fp)
    expect_equal (pkgmatch_cache_path (), fp)

    embeddings <-
        get_test_embeddings (npkgs = 10, nfns = 10, embedding_len = 768)
    embeddings_fns <- get_test_embeddings_fns (nfns = 10, embedding_len = 768)

    saveRDS (embeddings, file.path (fp, "embeddings-ropensci.Rds"))
    saveRDS (embeddings_fns, file.path (fp, "embeddings-fns.Rds"))

    expect_identical (embeddings, pkgmatch_load_data ("embeddings"))
    expect_identical (
        embeddings_fns,
        pkgmatch_load_data ("embeddings", fns = TRUE)
    )
})
