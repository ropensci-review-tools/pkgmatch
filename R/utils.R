opt_is_quiet <- function () {
    options ("rlib_message_verbosity") == "quiet"
}

#' Estimate whether input text string is code or English prose text.
#'
#' This is only approximate, and there are even software packages which can
#' give false negatives and be identified as prose (like rOpenSci's "geonames"
#' package), and prose which may be wrongly identified as code.
#'
#' @param txt Single input text string
#' @return Logical value indicating whether or not `txt` was identified as
#' code.
#'
#' @family utils
#' @export
#'
#' @examples
#' txt <- "Some text without any code"
#' text_is_code (txt)
#' txt <- "this_is_code <- function (x) { x }"
#' text_is_code (txt)
text_is_code <- function (txt) {
    checkmate::assert_character (txt, len = 1L)

    token_threshold <- 0.98

    n0 <- length (strsplit (txt, "[[:space:]]+") [[1]])
    nw <- tokenizers::count_words (txt)
    nw / n0 < token_threshold
}

#' Check whether 'input' parameter is a file or directory path
#'
#' This is necessary because `fs::dir_exists()` errors if the string passed is
#' too long.
#' @noRd
input_is_path <- function (input) {

    chk <- tryCatch (
        fs::file_exists (input),
        error = function (e) NULL
    )
    ifelse (is.null (chk), FALSE, chk)
}

input_is_pkg <- function (input) {
    if (input_is_path (input) || grepl ("\\s|\\-", input)) {
        return (TRUE)
    }

    pkg_is_installed (input)
}

pkg_is_installed <- function (pkg_name) {
    ip <- data.frame (utils::installed.packages ())
    pkg_name %in% ip$Package
}

#' Embeddings functions only return columns for input items that have > 0
#' characters. This reduces `nms` to the appropriate length before applying as
#' column names.
#' @param obj Object for which column names are to be added.
#' @param src Source of column names, generally a named character vector.
#' @param nms Vector of names to be applied.
#' @noRd
apply_col_names <- function (obj, src, nms) {
    index <- which (nzchar (src))
    colnames (obj) <- nms [index]

    return (obj)
}

#' Get names of exported functions for a given package from the
#' search.r-project website
#' @noRd
pkg_fns_from_r_search <- function (pkg_name) {
    m_pkg_fns_from_r_search (pkg_name)
}

pkg_fns_from_r_search_internal <- function (pkg_name) {
    base_url <- "https://search.r-project.org/CRAN/refmans/"
    url <- paste0 (base_url, pkg_name, "/html/00Index.html")
    fns <- rvest::html_table (rvest::read_html (url))
    do.call (rbind, fns)$X1
}

m_pkg_fns_from_r_search <- memoise::memoise (pkg_fns_from_r_search_internal)

pkg_name_from_path <- function (path) {

    desc_path <- fs::path (path, "DESCRIPTION")
    if (!fs::file_exists (desc_path)) {
        return (NULL)
    }

    desc <- data.frame (read.dcf (desc_path))
    desc$Package
}

make_cran_version_column <- function (x) {
    package <- NULL

    x |>
        dplyr::mutate (
            version = gsub ("(^.*\\_|\\.tar\\.gz$)", "", package),
            .after = "package"
        ) |>
        dplyr::mutate (package = gsub ("\\_.*$", "", package))
}

# Function to estimate the `token_threshold` above of 0.98, from running over
# all rOpenSci packages.
# get_threshold <- function (paths) {
#     txt <- vapply (paths, get_pkg_text, character (1L))
#     n0 <- vapply (
#         txt,
#         function (i) {
#             length (strsplit (i, "[[:space:]]+") [[1]])
#         },
#         integer (1L),
#         USE.NAMES = FALSE
#     )
#     n1 <- vapply (
#         txt,
#         tokenizers::count_words,
#         integer (1L),
#         USE.NAMES = FALSE
#     )
#     tok2word1 <- n1 / n0
#
#     code <- vapply (paths, get_pkg_code, character (1L))
#     n2 <- vapply (
#         code,
#         function (i) length (strsplit (i, "[[:space:]]+") [[1]]),
#         integer (1L),
#         USE.NAMES = FALSE
#     )
#     n3 <- vapply (
#         code,
#         tokenizers::count_words,
#         integer (1L),
#         USE.NAMES = FALSE
#     )
#     tok2word2 <- n3 / n2
#
#     prop_correct <- function (threshold, tok2word1, tok2word2) {
#         n_correct <- length (which (tok2word1 > threshold)) +
#             length (which (tok2word2 < threshold))
#         n <- 2 * length (tok2word1)
#         1 - n_correct / n
#     }
#     op <- optimize (prop_correct, c (0, 1), tok2word1, tok2word2)
#     op$minimum
# }
