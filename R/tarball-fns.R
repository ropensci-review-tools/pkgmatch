#' Extract tarball of a package into temp directory and return path to extracted
#' package. Copied directly from 'pkgstats' to avoid importing that fn here.
#'
#' @param tarball Full path to local tarball of an R package.
#' @return Path to extracted version of package (in `tempdir()`).
#' @param exdir Directory in which tarball is to be extracted.
#' @noRd
#' @examples
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' path <- extract_tarball (f)
extract_tarball <- function (tarball, exdir = fs::path_temp ()) {

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

    flist <- utils::untar (
        tarball,
        list = TRUE,
        tar = "internal"
    )

    chk <- withr::with_dir (
        exdir,
        utils::untar (tarball)
    )
    if (chk != 0) {
        stop ("Unable to extract tarball to '{exdir}'")
    }

    fdir <- fs::path_common (flist)
    path <- fs::path (exdir, fdir)

    chk <- rename_files_in_r (path)
    if (!chk) {
        warning ("Files in .R directory unable to be re-named")
    }

    return (path)
}

#' files in './R' directory may have suffixes of .q, .r, or .s
#' https://cran.r-project.org/doc/manuals/R-exts.html#Package-subdirectories
#' But parsers + cloc only recognise .R/.r, as does gtags, so rename any others
#' Discovered via BDR's
#' https://cran.r-project.org/web/packages/boot/index.html
#' The `rms` package also has loads of '.s' code, which cloc's dictionary
#' identifies as assembly
#' @noRd
rename_files_in_r <- function (path) {

    path <- fs::path_real (path)
    path_r <- fs::path (path, "R")
    if (!fs::dir_exists (path_r)) {
        return (FALSE)
    }
    f_sq <- fs::dir_ls (path_r, regexp = "\\.(s|S|q)$")

    if (length (f_sq) > 0) {

        f_r <- fs::path_ext_set (f_sq, "R")
        fs::file_move (f_sq, f_r)
    }

    return (TRUE)
}
