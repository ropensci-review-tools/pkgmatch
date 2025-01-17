# Functions to attach namespaces to function calls, primarily from calls
# derived from treesitter output.

# base pkgs from R-devel/src/library:
base_pkgs <- c (
    "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tcltk",
    "tools", "utils"
)

# Recommended from R-devel/src/library/Recommended, but these may not be
# installed on all systems, so reduce only to those that are installed.
rcmd_pkgs <- function () {
    p <- c (
        "KernSmooth", "MASS", "Matrix", "boot", "class", "cluster",
        "codetools", "foreign", "lattice", "mgcv", "nlme", "nnet", "rpart",
        "spatial", "survival"
    )
    ip <- utils::installed.packages ()
    p [which (p %in% ip [, "Package"])]
}

#' List all exported functions based on path to local package (either full
#' source repository, or extracted tarball).
#' @noRd
get_pkg_exported_fns <- function (path) {

    rd_path <- fs::path (path, "man")
    if (!fs::dir_exists (rd_path)) {
        return (NULL)
    }
    rd_files <- fs::dir_ls (rd_path, regexp = "\\.Rd$")
    fn_names <- lapply (rd_files, function (i) {
        suppressWarnings (
            rd <- tools::parse_Rd (i)
        )
        tags <- vapply (rd, function (j) {
            gsub ("^\\\\", "", attr (j, "Rd_tag"))
        }, character (1L))
        if (any (tags == "docType")) {
            docType <- as.character (rd [[which (tags == "docType")]] [[1]]) # nolint
            if (identical (docType, "package")) {
                return ("")
            }
        }

        index <- which (tags == "alias")
        if (length (index) == 0) {
            return ("")
        }
        gsub ("\\n$", "", unlist (rd [index]))
    })
    unlist (unname (fn_names))
}

#' Attach namespace of focal package to treesitter 'calls' output.
#' @noRd
attach_this_pkg_namespace <- function (path, calls) {

    pkg_name <- pkg_name_from_path (path)

    if (pkg_is_installed (path)) {
        e <- new.env ()
        pkg_rdb <- paste0 (path, ".rdb")
        f <- system.file ("help", pkg_rdb, package = path)
        f <- fs::path_ext_remove (f)
        lazyLoad (f, e)
        fn_names <- ls (e)
        rm (e)
    } else {
        fn_names <- get_pkg_exported_fns (path)
    }

    index_in <- which (calls$fn %in% fn_names)
    index_out <- which (!calls$fn %in% fn_names)
    fns_exp <- calls$fn [index_in]
    fns_non_exp <- calls$fn [index_out]
    calls$fn [index_in] <- paste0 (pkg_name, "::", calls$fn [index_in])
    calls$fn [index_out] <- paste0 (pkg_name, ":::", calls$fn [index_out])

    name_in <- which (calls$name %in% fns_exp)
    name_out <- which (calls$name %in% fns_non_exp)
    calls$name [name_in] <- paste0 (pkg_name, "::", calls$name [name_in])
    calls$name [name_out] <- paste0 (pkg_name, ":::", calls$name [name_out])

    return (calls)
}

#' Attach namespaces of base and recommended packages to treesitter 'calls'
#' output.
#' @noRd
attach_base_rcmd_ns <- function (calls) {

    # This requires namespaces to be loaded:
    fn_names_base <- function (pkg_name) {
        as.character (unclass (
            utils::lsf.str (envir = asNamespace (pkg_name), all = FALSE)
        ))
    }

    # Read namespace file without requiring it to be loaded: Note that results
    # differ from the above, but it is the best approach without loading the
    # namespace.
    fn_names_rcmd <- function (pkg_name) {
        ip <- utils::installed.packages ()
        i <- which (ip [, "Package"] == pkg_name)
        lp <- ip [i, "LibPath"]
        ns <- parseNamespaceFile (pkg_name, package.lib = lp)
        unique (c (ns$exports, ns$exportMethods))
    }

    attach_ns <- function (calls, pkg_name, base = TRUE) {

        if (base) {
            # This can warn or error for  "tcltk" on some systems when no X11
            # driver loaded:
            suppressWarnings ({
                fn_names <- tryCatch (
                    fn_names_base (pkg_name),
                    error = function (e) NULL
                )
            })
        } else {
            fn_names <- fn_names_rcmd (pkg_name)
        }

        index_no_ns <- which (!grepl ("\\:\\:", calls$name))
        index <- which (calls$name [index_no_ns] %in% fn_names)
        calls$name [index_no_ns] [index] <- paste0 (
            pkg_name,
            "::",
            calls$name [index_no_ns] [index]
        )
        return (calls)
    }

    calls <- attach_ns (calls, "base", base = TRUE)
    # From some reason, `.Call` is not listed in base:
    calls$name [calls$name == ".Call"] <- "base::.Call"

    for (p in base_pkgs) calls <- attach_ns (calls, p, base = TRUE)
    for (p in rcmd_pkgs ()) calls <- attach_ns (calls, p, base = FALSE)

    return (calls)
}

get_fn_defs_namespace <- function (pkg_name, exported_only) {
    # `lsf.str` is for functions only:
    fn_names <- unclass (
        utils::lsf.str (envir = asNamespace (pkg_name), all = TRUE)
    )
    if (exported_only) {
        suppressPackageStartupMessages (
            require (pkg_name, character.only = TRUE)
        )
        fns_exp <- ls (paste0 ("package:", pkg_name))
        fn_names <- fn_names [which (fn_names %in% fns_exp)]
    }
    fn_defs <- lapply (fn_names, function (f) {
        utils::getFromNamespace (f, pkg_name)
    })
    names (fn_defs) <- fn_names

    return (fn_defs)
}

#' List all dependencies of a local package, so namespaces can be appropriately
#' designated.
#' @noRd
get_local_pkg_deps <- function (path) {

    desc_path <- fs::path (path, "DESCRIPTION")
    if (!fs::file_exists (desc_path)) {
        return (NULL)
    }

    desc <- data.frame (read.dcf (desc_path))
    what <- c ("Depends", "Imports", "Suggests", "LinkingTo")
    what <- what [which (what %in% names (desc))]
    pkgs <- unlist (lapply (what, function (i) {
        i_sp <- gsub ("\\n", "", strsplit (desc [[i]], ",") [[1]])
        gsub ("[[:space:]].*$", "", i_sp)
    }))
    out <- c ("R", base_pkgs, rcmd_pkgs ())
    pkgs [which (!pkgs %in% out)]
}

#' Iterate over all local pacakge dependencies and get all function names from
#' the search.r-project site.
#' @noRd
get_local_pkg_dep_fns <- function (path) {
    deps <- get_local_pkg_deps (path)
    fns <- lapply (deps, function (d) {
        fns <- tryCatch (
            pkg_fns_from_r_search (d),
            error = function (e) NULL
        )
        if (is.null (fns)) {
            return (NULL)
        }
        data.frame (
            package = d,
            fn_name = pkg_fns_from_r_search (d)
        )
    })
    do.call (rbind, fns)
}

#' Use the preceding "local_pkg_dep" functions to attach namespaces to all
#' calls to functions from dependent packages.
#' @noRd
attach_local_dep_namespaces <- function (path, calls) {
    fns <- get_local_pkg_dep_fns (path)

    index <- which (!grepl ("\\:", calls$name))
    index_to_fns <- match (calls$name [index], fns$fn_name)
    dep_pkgs <- fns$package [index_to_fns]
    index_pkgs <- which (!is.na (dep_pkgs))
    calls$name [index] [index_pkgs] <-
        paste0 (dep_pkgs [index_pkgs], "::", calls$name [index] [index_pkgs])

    return (calls)
}
