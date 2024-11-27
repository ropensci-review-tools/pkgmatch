# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    op <- options ()

    op.pkgmatch <- list ( # nolint
        pkgmatch.ollama.url = "127.0.0.1:11434"
    )

    toset <- !(names (op.pkgmatch) %in% names (op))
    if (any (toset)) {
        options (op.pkgmatch [toset])
    }
    invisible ()
}
# nocov end

#' Get the URL for local ollama API
#'
#' Return the URL of the specified ollama API. Default is
#' "127.0.0.1:11434"
#'
#' @return The ollama API URL
#'
#' @seealso `set_ollama_url`
#'
#' @family ollama
#' @export
get_ollama_url <- function () {

    op <- options ()
    if (!"pkgmatch.ollama.url" %in% names (op)) {
        stop ("ollama can not be retrieved")
    }
    options ()$pkgmatch.ollama.url
}


#' Set the URL for local ollama API
#'
#' @param ollama_url The desired ollama API URL
#'
#' @return The ollama API URL
#'
#' @seealso [get_ollama_url()]
#'
#' @family ollama
#' @export
set_ollama_url <- function (ollama_url) {

    op <- options ()
    op.pkgmatch <- list (pkgmatch.ollama.url = ollama_url)
    options (op.pkgmatch)

    invisible (ollama_url)
}
