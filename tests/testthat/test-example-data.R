test_that ("example data", {

    ex_dir <- fs::path (fs::path_temp (), "pkgmatch_ex_data")
    expect_false (fs::dir_exists (ex_dir))

    op <- getOption ("pkgmatch.example_env", "")
    expect_false (nzchar (op))

    generate_pkgmatch_example_data ()

    expect_true (fs::dir_exists (ex_dir))

    flist <- fs::dir_ls (ex_dir)
    expect_length (flist, 4L)

    corpus <- "cran"
    fnames <- c ("embeddings", "bm25", "idfs-fn-calls", "fn-calls")
    fnames <- paste0 (fnames, "-", corpus, ".Rds")
    expect_identical (sort (fnames), sort (basename (flist)))

    op <- getOption ("pkgmatch.example_env", "")
    expect_true (nzchar (op))
    expect_equal (op, "true")

    options ("pkgmatch.example_env" = NULL)
    fs::dir_delete (ex_dir)
})
