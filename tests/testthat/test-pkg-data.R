is_test_job <- (identical (Sys.getenv ("GITHUB_JOB"), "test-coverage") ||
    identical (Sys.getenv ("MPADGE_LOCAL"), "true"))

test_that ("get pkg local text", {
    path <- pkgmatch_test_skeleton ()
    expect_true (dir.exists (path))

    roxygen2::roxygenise (path) # Generate man files

    txt <- get_pkg_text (path)
    expect_type (txt, "character")
    expect_length (txt, 1L)
    ptn0 <- sec_separator ("Functions", regex = FALSE)
    ptn <- sec_separator ("Functions", regex = TRUE)
    expect_identical (ptn0, "## ---- Functions ----")
    expect_true (grepl (ptn0, txt, fixed = TRUE))
    expect_false (grepl (ptn, txt, fixed = FALSE))
    expect_true (grepl ("#\\s*demo", txt))
    expect_true (nchar (txt) < 1000) # small test package

    txt <- strsplit (txt, "\\n") [[1]]
    expect_true (length (grep (ptn0, txt, fixed = TRUE)) == 1L)
    expect_false (length (grepl (ptn, txt, fixed = FALSE)) == 1L)

    # detach is critical here, because httptest2 uses `utils::sessionInfo()`,
    # which checks namespaces and tries to load DESC file from pkg location.
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)
})

test_that ("get pkg tarball text", {
    path <- pkgmatch_test_skeleton ()
    expect_true (dir.exists (path))
    roxygen2::roxygenise (path) # Generate man files

    txt0 <- get_pkg_text (path)

    path_gz <- pkgbuild::build (path)
    detach ("package:demo", unload = TRUE)
    fs::dir_delete (path)
    txt1 <- get_pkg_text (path_gz)

    expect_identical (txt0, txt1)
    # All tests above must then also pass ...

    fs::file_delete (path_gz)
})

test_that ("get pkg installed text", {
    pkg <- "cli"

    txt <- get_pkg_text (pkg)
    expect_type (txt, "character")
    expect_length (txt, 1L)
    expect_true (nchar (txt) > 1000)

    code <- get_pkg_code (pkg)
    expect_type (code, "character")
    expect_length (code, 1L)
    expect_true (nchar (txt) > 1000)

    code_exp_only <- get_pkg_code (pkg, exported_only = TRUE)
    expect_true (nchar (code_exp_only) / nchar (code) < 0.75)
})

test_that ("get fn defs", {

    path <- pkgmatch_test_skeleton ()
    fn_defs <- get_fn_defs_local (path)
    expect_type (fn_defs, "character")
    expect_length (fn_defs, 1L)
})

skip_if (!is_test_job)

test_that ("Extract Rnw", {

    pkg_name <- "parallel"
    ip <- data.frame (utils::installed.packages ()) |>
        dplyr::filter (Package == pkg_name)
    skip_if (length (ip) == 0L)
    pkg_path <- fs::path (ip$LibPath, pkg_name)
    rnw_files <- fs::dir_ls (pkg_path, recurse = TRUE, regexp = "\\.Rnw")
    rnw_files <- rnw_files [which (!duplicated (fs::path_file (rnw_files)))]
    rnws <- unname (unlist (lapply (rnw_files, extract_one_rnw)))
    expect_true (length (rnws) > 100L)

    txt_with_fns <- get_pkg_text (pkg_name)
    txt_sp <- strsplit (txt_with_fns, "\\n") [[1]]
    txt_sp <- gsub ("^\\s*", "", txt_sp)
    n0 <- length (which (rnws %in% txt_sp))
    # Lots of rnw lines identically included in resultant text:
    expect_true (n0 > 50)
})
