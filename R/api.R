#' serve plumber API to interface with ollama server
#'
#' @param port Port for API to be exposed on
#' @return Nothing; calling this starts a blocking process.
#'
#' @family api
#' @export
serve_api <- function (port = 8000L) {

    ip <- data.frame (utils::installed.packages ())

    f <- fs::path (
        ip$LibPath [ip$Package == "pkgmatch"],
        "pkgmatch", "plumber.R"
    )

    cache_dir <- fs::path (rappdirs::user_cache_dir (), "R", "pkgcheck")
    if (!fs::file_exists (cache_dir)) {
        cli::cli_abort ("Cache directory does not exist.")
    }

    # ----------log_dir set up----------
    log_dir <- fs::path (cache_dir, "logs")
    if (!fs::dir_exists (log_dir)) {
        fs::dir_create (log_dir, recurse = TRUE)
    }

    log_file <- fs::file_temp ("pkgmatch_", tmp_dir = log_dir, ext = ".log")
    logger::log_appender (logger::appender_tee (log_file))

    convert_empty <- function (string) {
        ifelse (string == "", "-", string)
    }

    # ----------local static dir set up----------
    static_dir <- fs::path (cache_dir, "static")
    if (!fs::file_exists (static_dir)) {
        fs::dir_create (static_dir, recurse = TRUE)
    }

    # ----------plumber process set up----------
    pr <- plumber::pr (f)

    pr <- plumber::pr_static (pr, "/assets", static_dir)

    # pr$registerHooks(
    pr <- plumber::pr_hooks (
        pr,
        list (
            preroute = function () {
                # Start timer for log info
                tictoc::tic ()
            },
            postroute = function (req, res) {
                end <- tictoc::toc (quiet = TRUE) # nolint
                # Log details about the request and the response
                # TODO: Sanitize log details - perhaps in convert_empty
                logger::log_info ('{convert_empty(req$REMOTE_ADDR)} "{convert_empty(req$HTTP_USER_AGENT)}" {convert_empty(req$HTTP_HOST)} {convert_empty(req$REQUEST_METHOD)} {convert_empty(req$PATH_INFO)} {convert_empty(req$QUERY_STRING)} {convert_empty(res$status)} {round(end$toc - end$tic, digits = getOption("digits", 5))}') # nolint
            }
        )
    )

    plumber::pr_run (pr,
        host = "0.0.0.0",
        port = as.integer (port)
    )
}
