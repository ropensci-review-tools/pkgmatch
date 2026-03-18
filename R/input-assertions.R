#' @title Estimate whether input text is code or English prose text.
#'
#' @description This function is used as part of the input of many functions,
#' to determine whether the input is text of whether it is code. All such
#' functions use it via an input parameter named `input_is_code`, which is set
#' by default to the value returned from this function. That value can always
#' be over-ridden by specifying a fixed value of either `TRUE` or `FALSE` for
#' `input_is_code`.
#'
#' Values from this function are only approximate, and there are even software
#' packages which can give false negatives and be identified as prose (like
#' rOpenSci's "geonames" package), and prose which may be wrongly identified as
#' code.
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
    if (input_is_path (input) || !grepl ("\\s|\\-", input)) {
        return (TRUE)
    }

    pkg_is_installed (input)
}

assert_idfs <- function (idfs) {

    checkmate::assert_list (idfs, len = 2L)
    checkmate::assert_names (
        names (idfs),
        identical.to = c ("full", "descs_only")
    )
    checkmate::assert_names (
        names (idfs$full),
        identical.to = c ("idfs", "token_lists")
    )
    checkmate::assert_names (
        names (idfs$descs_only),
        identical.to = c ("idfs", "token_lists")
    )
}
