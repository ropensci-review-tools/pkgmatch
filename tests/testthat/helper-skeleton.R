# From srr/R/pkg-skeleon.R

make_pkg_path <- function (base_dir = tempdir (), pkg_name = "demo") {

    d <- file.path (base_dir, pkg_name)
    if (!file.exists (d)) {
        dir.create (d, recursive = TRUE)
    }

    return (d)
}

write_desc <- function (d, pkg_name) {

    desc <- c (
        paste0 ("Package: ", pkg_name),
        "Title: What the Package Does (One Line, Title Case)",
        "Version: 0.0.1",
        "Authors@R: ",
        "  person(given = \"First\",",
        "         family = \"Last\",",
        "         role = c(\"aut\", \"cre\"),",
        "         email = \"first.last@example.com\")",
        "Description: What the package does (one paragraph).",
        "Suggests:",
        "    testthat",
        "License: GPL-3",
        "Encoding: UTF-8"
    )

    fp <- file.path (d, "DESCRIPTION")
    if (!file.exists (fp)) {
        writeLines (desc, con = fp)
    }
}

write_r_fn <- function (d, pkg_name) {

    rfile <- c (
        "#' test_fn",
        "#'",
        "#' A test funtion",
        "#'",
        "#' @export",
        "test_fn <- function() {",
        "  message(\"This function does nothing\")",
        "}"
    )
    dr <- file.path (d, "R")
    if (!file.exists (dr)) {
        dir.create (dr)
    }


    fp <- file.path (dr, "test.R")
    if (!file.exists (fp)) {
        writeLines (rfile, con = fp)
    }

    rfile <- c (
        "#' @keywords internal",
        "\"_PACKAGE\"",
        "",
        paste0 (
            "# The following block is used by ",
            "usethis to automatically manage"
        ),
        "# roxygen namespace tags. Modify with care!",
        "## usethis namespace: start",
        "## usethis namespace: end",
        "NULL"
    )
    fp <- file.path (dr, paste0 (pkg_name, "-package.R"))
    if (!file.exists (fp)) {
        writeLines (rfile, con = fp)
    }
}

write_readme <- function (d, pkg_name) {

    # nolint start --- lines > 80 characters
    rfile <- c (
        paste0 ("# ", pkg_name),
        "",
        "<!-- badges: start -->",
        "[![A badge](https://badge.svg)](https://badge-link)",
        "<!-- badges: end -->",
        "",
        "This `README.Rmd` file is here to demonstrate some stuff.",
        "",
        "``` r",
        "  x <- 1",
        "```",
        "",
        "And it includes a HTML table",
        "",
        "<table>",
        "<td>",
        "</td>",
        "</table>",
        "",
        "The end."
    )
    # nolint end

    fp <- file.path (d, "README.md")
    if (!file.exists (fp)) {
        writeLines (rfile, con = fp)
    }
}


pkgmatch_test_skeleton <- function (base_dir = tempdir (), pkg_name = "demo") {

    d <- make_pkg_path (base_dir, pkg_name)

    write_desc (d, pkg_name)
    write_r_fn (d, pkg_name)
    write_readme (d, pkg_name)

    return (d)
}

get_sample_input_text <- function (n = NULL) {

    txt <- c (
        "a not so very similar package",
        "a test package",
        "a function to test",
        "yet another something",
        "not really anything here",
        "keep going",
        "a bit more",
        "we need at least ten values here",
        "so here are some more",
        "and this is the tenth one right here"
    )
    if (is.null (n)) {
        n <- length (txt)
    }
    if (n < length (txt)) {
        txt <- txt [seq_len (n)]
    }

    return (txt)
}
