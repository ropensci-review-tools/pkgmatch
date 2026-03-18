pkg_is_installed <- function (pkg_name) {
    ip <- data.frame (utils::installed.packages ())
    pkg_name %in% ip$Package
}

pkg_install_path <- function (pkg_name) {
    ip <- data.frame (utils::installed.packages ())
    i <- match (pkg_name, ip$Package)
    if (length (i) != length (pkg_name)) {
        return (NULL)
    }
    fs::path (ip$LibPath [i], pkg_name)
}

#' Get names of exported functions for a given package from the
#' search.r-project website
#' @noRd
pkg_fns_from_r_search <- function (pkg_name) {
    m_pkg_fns_from_r_search (pkg_name)
}

# No 'pkgcheck' here, so copied from:
# url_exists <- utils::getFromNamespace ("url_exists", "pkgcheck")

#' Bob Rudis's URL checker function, updated for httr2
#'
#' @param x a single URL
#' @param non_2xx_return_value what to do if the site exists but the HTTP status
#' code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value`
#' condition arises a warning message will be displayed. Default is `FALSE`.
#' @param ... other params (`timeout()` would be a good one) passed directly to
#' \pkg{httr2} functions.
#'
#' @note
#' https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
#' @noRd
url_exists <- function (x, non_2xx_return_value = FALSE, quiet = TRUE, ...) {

    req <- httr2::request (x)
    resp <- tryCatch (
        httr2::req_perform (req),
        error = function (e) {
            e
        },
        interrupt = function (e) {
            stop ("Terminated by user", call. = FALSE)
        }
    )

    if (!inherits (resp, "httr2_error")) {
        status <- httr2::resp_status (resp)
    } else {
        status <- resp$resp$status_code
    }
    if (is.null (status)) {
        return (FALSE)
    }

    if (status / 200 > 1) {
        if (!quiet) {
            warning (paste0 (
                "Requests for [",
                x,
                "] responded with HTTP status ",
                status
            ))
        }
        return (non_2xx_return_value)
    }

    return (TRUE)
}

pkg_fns_from_r_search_internal <- function (pkg_name) {
    base_url <- "https://search.r-project.org/CRAN/refmans/"
    url <- paste0 (base_url, pkg_name, "/html/00Index.html")
    fns <- list ()
    if (url_exists (url)) {
        con <- curl::curl (url)
        fns <- rvest::html_table (rvest::read_html (con))
        chk <- tryCatch (close (con), error = function (e) NULL)
    }
    do.call (rbind, fns)$X1
}

m_pkg_fns_from_r_search <- memoise::memoise (pkg_fns_from_r_search_internal)

pkg_name_from_path <- function (path) {

    ret <- NULL

    desc_path <- fs::path (path, "DESCRIPTION")
    if (fs::file_exists (desc_path)) {
        desc <- data.frame (read.dcf (desc_path))
        ret <- desc$Package
    } else if (pkg_is_installed (path)) {
        ret <- path
    }

    return (ret)
}

check_corpus_param <- function (corpus, fns = FALSE) {

    corpora <- c ("ropensci", "cran", "bioc")
    if (fns) {
        corpora <- c (corpora, "ropensci-fns", "bioc-fns")
    }
    prfxs <- substring (gsub ("^.*\\-", "", corpora), 1, 1)

    if (is.null (corpus)) {
        if (!interactive () ||
            Sys.getenv ("PKGMATCH_TESTS", "nope") == "true" ||
            !cli::has_keypress_support ()) {

            cli::cli_abort ("'corpus' must be specified.")

        } else {

            cli::cli_alert_info ("Which corpus would you like to use?")
            msg <- vapply (seq_along (corpora), function (i) {
                paste0 ("'", prfxs [i], "' for '", corpora [i], "'")
            }, character (1L))
            msg <- ifelse (
                length (msg) == 2L,
                paste0 (msg, collapse = ", and "),
                paste0 (msg [1], ", ", msg [2], ", and ", msg [3])
            )
            cli::cli_alert_info (paste0 ("(", msg, ")"))
            corpus <- tolower (cli::keypress ())
            if (!corpus %in% prfxs) {
                cli::cli_abort ("Corpus must be one of {prfxs}")
            }
            corpus <- corpora [match (corpus, prfxs)]
            cli::cli_alert_info ("Using {corpus} corpus")
        }
    } else {
        checkmate::assert_character (corpus, len = 1L)
        corpus <- match.arg (corpus, c ("ropensci", "cran", "ropensci-fns", "bioc", "bioc-fns"))
    }
    return (corpus)
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
