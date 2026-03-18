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
