# Get all R function calls from tree-sitter
#
# Adapated from code provided by Davis Vaughan in
# https://github.com/ropensci-review-tools/pkgstats/pull/62#issuecomment-2359834446

#' Extract names of all function called from a given treesitter node
#'
#' This first identifies all functions, then recursively identifies all calls
#' within the bodies of those functions (by calling the `get_calls()`
#' function).
#' @noRd
get_calls_in_functions <- function (node) {

    QUERY_FUNCTIONS <- r"(
    (binary_operator
        lhs: (identifier) @name
        operator: "<-"
        rhs: (function_definition) @fn
    )
    )"
    QUERY_FUNCTIONS <- treesitter::query (treesitter.r::language (), QUERY_FUNCTIONS)

    QUERY_FUNCTIONS_EQ <- r"(
    (binary_operator
        lhs: (identifier) @name
        operator: "="
        rhs: (function_definition) @fn
    )
    )"
    QUERY_FUNCTIONS_EQ <- treesitter::query (treesitter.r::language (), QUERY_FUNCTIONS_EQ)

    functions <- treesitter::query_captures (QUERY_FUNCTIONS, node)
    names <- functions$node [functions$name == "name"]
    bodies <- functions$node [functions$name == "fn"]

    functions_eq <- treesitter::query_captures (QUERY_FUNCTIONS_EQ, node)
    names <- c (names, functions_eq$node [functions_eq$name == "name"])
    bodies <- c (bodies, functions_eq$node [functions_eq$name == "fn"])

    tibble::new_tibble (list (
        fn = vapply (names, treesitter::node_text, character (1)),
        info = lapply (bodies, get_calls)
    ))
}

#' Extract all function calls from a given treesitter node.
#'
#' This is called from the preceding `get_calls_in_functions()`, and is applied
#' to each individual function body, to identify all calls make within that
#' function.
#' @noRd
get_calls <- function (node) {

    QUERY_CALLS <- r"(
    (call
        function: [
        (identifier) @name
        (namespace_operator) @name
        ]
    )
    )"
    QUERY_CALLS <- treesitter::query (treesitter.r::language (), QUERY_CALLS)

    captures <- treesitter::query_captures (QUERY_CALLS, node)

    name <- vapply (
        captures$node,
        function (n) treesitter::node_text (n),
        character (1)
    )

    start <- vapply (
        captures$node,
        function (n) {
            treesitter::point_row (treesitter::node_start_point (n))
        },
        double (1)
    )

    end <- vapply (
        captures$node,
        function (n) {
            treesitter::point_row (treesitter::node_end_point (n))
        },
        double (1)
    )

    tibble::new_tibble (list (
        name = name,
        start = start,
        end = end
    ))
}

#' The main function applied to a package `path`, and used to identify and
#' return all function calls made within the package.
#'
#' @noRd
tressitter_calls_in_package <- function (path, is_installed = FALSE) {

    # supreess 'no visible binding' notes:
    info <- NULL

    parser <- treesitter::parser (treesitter.r::language ())

    # Default return if not results:
    df0 <- data.frame (fn = character (0L), name = character (0L))

    if (is_installed) {
        paths <- lapply (path, get_pkg_code)
    } else {

        path_r <- fs::path (path, "R")
        if (!fs::dir_exists (path_r)) {
            return (df0)
        }
        paths <- fs::dir_ls (path_r, regexp = "\\.(r|R)$")
        paths <- as.character (paths)
    }

    out <- vector ("list", length = length (paths))

    for (i in seq_along (out)) {
        path <- paths [[i]]
        if (is_installed) {
            text <- paths [[i]]
        } else {
            text <- tryCatch (
                brio::read_file (path),
                error = function (e) NULL
            )
        }
        if (length (text) == 0) {
            text <- NULL
        }

        if (!is.null (text)) {

            tree <- treesitter::parser_parse (parser, text)
            node <- treesitter::tree_root_node (tree)
            elt <- get_calls_in_functions (node)
            elt [["file"]] <- as.character (path)
            out [[i]] <- elt
        }
    }

    out <- vctrs::list_unchop (out)
    if (is.null (out)) {
        out <- df0
    } else {
        out <- tidyr::unnest (out, info)
    }
    out
}

#' @title Identify all function calls make within a package.
#'
#' @description This function uses "treesitter"
#' (\url{https://github.com/tree-sitter/tree-sitter}) to tag all function calls
#' made within a local package, and to associate those calls with package
#' namespaces.
#'
#' This is used as input to the \link{pkgmatch_bm25_fn_calls} function, to
#' enable function calls within a local package to be inversely weighted by
#' frequencies within all packages within a corpus. The results of applying
#' this function to the full corpora used in this package are contained within
#' the data listed on
#' \url{https://github.com/ropensci-review-tools/pkgmatch/releases/tag/v0.4.0},
#' as "fn-calls-ropensci.Rds" and "fn-calls-cran.Rds".
#'
#' @param path Path to local package, or `.tar.gz` file of package source.
#' @return A `data.frame` of all function calls made within the package, with
#' the following columns:
#' \itemize{
#' \item 'fn' Name of the package function within which call is made, including
#' namespace identifiers of "::" for exported functions and ":::" for
#' non-exported functions.
#' \item name Name of function being called, including namespace.
#' \item start Byte number within file corresponding to start of definition
#' \item end Byte number within file corresponding to end of definition
#' \item file Name of file in which fn call is defined.
#' }
#'
#' @family treesitter
#' @export
#'
#' @examples
#' # Get function calls made within locally-installed packages:
#' fn_tags <- pkgmatch_treesitter_fn_tags ("curl") # Name of installed package
#' fn_tags <- pkgmatch_treesitter_fn_tags ("cli") # Name of installed package
#'
#' # Or get calls from full source code:
#' u <- "https://cran.r-project.org/src/contrib/odbc_1.5.0.tar.gz"
#' path <- file.path (tempdir (), basename (u))
#' \dontrun{
#' download.file (u, destfile = path)
#' fn_tags <- pkgmatch_treesitter_fn_tags (path)
#' }
pkgmatch_treesitter_fn_tags <- function (path) {

    checkmate::assert_character (path, len = 1L)

    is_installed_pkg <- FALSE
    chk <- checkmate::check_file_exists (path)
    if (!is.logical (chk)) {
        chk <- checkmate::check_directory_exists (path)
    }
    if (!is.logical (chk)) {
        chk <- is_installed_pkg <- input_is_pkg (path)
    }
    if (!chk) {
        cli::cli_abort ("'path' does not appear to be an R package.")
    }

    if (!is_installed_pkg) {

        path <- fs::path_norm (path)

        is_tarball <- fs::path_ext (path) == "gz"
        if (is_tarball) {
            path <- tarball_to_path (path)
            on.exit ({
                fs::dir_delete (path)
            })
        }

        stopifnot (fs::dir_exists (path))
    }

    calls <- tressitter_calls_in_package (path, is_installed_pkg)
    if (nrow (calls) == 0L) {
        calls <- data.frame (name = character (0L))
    } else {
        calls <- attach_this_pkg_namespace (path, calls)
        calls <- attach_base_rcmd_ns (calls)
        calls <- attach_local_dep_namespaces (path, calls)
    }

    return (calls)
}
