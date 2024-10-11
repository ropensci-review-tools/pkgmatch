#' Print method for 'pkgmatch' objects
#'
#' @param x Object to be printed
#' @param ... Not used
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
        xout <- x$package [seq_len (n)]
    } else {
        xout <- list (
            "text" = x$package [x$text_rank] [seq_len (n)],
            "code" = x$package [x$code_rank] [seq_len (n)]
        )
    }
    print (xout)
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
