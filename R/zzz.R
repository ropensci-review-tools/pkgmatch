# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    op <- options ()

    op.pkgmatch <- list ( # nolint
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
            Sys.getenv ("GITHUB_JOB") != "test-coverage.yaml")
}
