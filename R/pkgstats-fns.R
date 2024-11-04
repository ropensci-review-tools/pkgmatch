#' Extract tarball of a package into temp directory and return path to extracted
#' package. Copied directly from 'pkgstats' to avoid importing that fn here.
#'
#' @param tarball Full path to local tarball of an R package.
#' @return Path to extracted version of package (in `tempdir()`).
#' @noRd
#' @examples
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' path <- extract_tarball (f)
extract_tarball <- function (tarball) {

    if (!fs::file_exists (tarball)) {
        stop ("file [", tarball, "] does not exist")
    }
    if (!checkmate::testCharacter (tarball,
        len = 1,
        pattern = "\\.tar\\.gz$"
    )) {
        stop (paste0 (
            "tarball must be a single character ",
            "specifying path to .tar.gz file"
        ))
    }

    flist <- utils::untar (tarball,
        exdir = fs::path_temp (),
        list = TRUE, tar = "internal"
    )
    if (utils::untar (tarball, exdir = fs::path_temp (), tar = "internal") != 0) {
        stop ("Unable to extract tarball to 'tempdir'")
    }

    fdir <- vapply (flist, function (i) {
        strsplit (i, .Platform$file.sep) [[1]] [1]
    },
    character (1),
    USE.NAMES = FALSE
    )
    fdir <- names (table (fdir)) [1]
    path <- fs::path_real (fs::path (fs::path_temp (), fdir))

    chk <- rename_files_in_r (path)
    if (!chk) {
        warning ("Files in .R directory unable to be re-named")
    }

    return (path)
}
