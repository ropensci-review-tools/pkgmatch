#' @title Print method for 'pkgmatch' objects
#'
#' @description The main `pkgmatch` functions, \link{pkgmatch_similar_pkgs} and
#' \link{pkgmatch_similar_fns}, return `data.frame` objects of class
#' "pkgmatch". This class exists primarily to enable this print method, which
#' summarises by default the top 5 matching packages or functions. Objects can
#' be converted to standard `data.frame`s with `as.data.frame()`.
#'
#' @param x Object to be printed
#' @param ... Additional parameters passed to default 'print' method.
#' @return The result of printing `x`, in form of either a single character
#' vector, or a named list of character vectors.
#'
#' @family utils
#' @export
#'
#' @examples
#' \dontrun{
#' input <- "Download open spatial data from NASA"
#' p <- pkgmatch_similar_pkgs (input)
#' p # Default print method, lists 5 best matching packages
#' head (p) # Shows first 5 rows of full `data.frame` object
#' }
print.pkgmatch <- function (x, ...) {

    n <- attr (x, "n")

    if ("rank" %in% names (x)) {
        nms <- c ("function", "package")
        nm <- nms [which (nms %in% names (x))]
        xout <- x [[nm]] [seq_len (n)]
    } else {
        xout <- list (
            "text" = x$package [order (x$text_rank)] [seq_len (n)],
            "code" = x$package [order (x$code_rank)] [seq_len (n)]
        )
    }
    print (xout, ...)
}

#' Head method for 'pkgmatch' objects
#'
#' @param x Object for which head is to be printed
#' @param n Number of rows of full `pkgmatch` object to be displayed
#' @param ... Not used
#' @return A (usually) smaller version of `x`, with all columns displayed.
#'
#' @family utils
#' @export
#'
#' @examples
#' \dontrun{
#' input <- "Download open spatial data from NASA"
#' p <- pkgmatch_similar_pkgs (input)
#' p # Default print method, lists 5 best matching packages
#' head (p) # Shows first 5 rows of full `data.frame` object
#' }
head.pkgmatch <- function (x, n = 5L, ...) {
    head (as.data.frame (x), n = n)
}
