# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    op <- options ()

    op.pkgmatch <- list ( # nolint
        pkgmatch.ollama.url = Sys.getenv ("OLLAMA_HOST", "127.0.0.1:11434"),
        pkgmatch.verbose_limit = 50L
    )

    uf <- Sys.getenv ("PKGMATCH_UPDATE_FREQUENCY", "")
    uf <- ifelse (nzchar (uf), as.integer (uf), NA_integer_)
    if (!is.na (uf)) {
        op.pkgmatch$pkgmatch.update_frequency <- uf
    }

    toset <- !(names (op.pkgmatch) %in% names (op))
    if (any (toset)) {
        options (op.pkgmatch [toset])
    }

    invisible ()
}

.onAttach <- function (libname, pkgname) {

    chk <- tryCatch (
        ollama_check (),
        error = function (e) e
    )
    if (inherits (chk, "error")) {
        packageStartupMessage (chk$message)
    }
}
# nocov end

#' Get the limit for verbose progress bar output
#'
#' Calls to any main functions with more than this number of inputs (as package
#' paths, or lists of text input) will use verbose progress bars.
#' @noRd
get_verbose_limit <- function () {
    op <- options ()
    if (!"pkgmatch.verbose_limit" %in% names (op)) {
        stop ("verbose_limit option not defined")
    }
    getOption ("pkgmatch.verbose_limit")
}

# Includes line to suppress all output on GHA, via default envvars listed in
# https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/store-information-in-variables#default-environment-variables # nolint
# Generating terminal output on GHA generates segfault errors like
# https://github.com/ropensci-review-tools/pkgmatch/actions/runs/12848889636/job/35826799668
opt_is_quiet <- function () {
    getOption ("rlib_message_verbosity", "notset") == "quiet" ||
        (Sys.getenv ("GITHUB_ACTIONS") == "true" &&
            Sys.getenv ("GITHUB_JOB") != "test-coverage")
}

#' @title Get the URL for local ollama API
#'
#' @description Return the URL of the specified ollama API. Default is
#' "127.0.0.1:11434"
#'
#' @return The ollama API URL
#'
#' @seealso `set_ollama_url`
#'
#' @family ollama
#' @export
get_ollama_url <- function () {

    # Need to return fixed value in tests, because tests create
    # sub-environments which do not inherit withr::local_env:
    if (identical (Sys.getenv ("PKGMATCH_TESTS"), "true")) {
        return ("127.0.0.1:11434")
    }

    op <- options ()
    if (!"pkgmatch.ollama.url" %in% names (op)) {
        stop ("ollama URL can not be retrieved")
    }
    getOption ("pkgmatch.ollama.url")
}


#' @title Set the URL for local ollama API
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

    check_ollama_url (ollama_url)

    op <- options ()
    op.pkgmatch <- list (pkgmatch.ollama.url = ollama_url)
    options (op.pkgmatch)

    invisible (ollama_url)
}

check_ollama_url <- function (ollama_url) {

    ollama_url <- ifelse (
        grepl ("^http", ollama_url),
        ollama_url,
        paste0 ("https://", ollama_url)
    )
    check <- tryCatch (
        curl::curl_parse_url (ollama_url),
        error = function (e) e
    )
    if (methods::is (check, "error")) {
        cli::cli_abort (check$message)
    }
}
