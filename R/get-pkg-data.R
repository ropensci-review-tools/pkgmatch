# Auxilliary fns for 'get-pkg-text.R' to extract specific parts of packages.

get_pkg_readme <- function (path) {

    readme_file <- fs::path (path, "README.md")
    if (!fs::file_exists (readme_file)) {
        return (NULL)
    }
    extract_one_md (readme_file)
}

extract_one_md <- function (md_file) {

    md <- brio::read_lines (md_file)

    yaml_front <- grep ("^\\-\\-\\-$", md)
    if (length (yaml_front) > 1L) {
        yaml_index <- seq (yaml_front [1], yaml_front [2])
        md <- md [-yaml_index]
    }

    header_end <- grep ("end\\s*\\-+>\\s*$", md)
    if (length (header_end) > 0L) {
        header_end_index <- which (header_end < floor (length (md) / 2))
        if (length (header_end_index) > 0L) {
            header_end <- max (header_end [header_end_index])
            md <- md [-(seq_len (header_end))]
        }
    }
    # Then rm any image links, including badges. These may extend over multiple
    # lines.
    md <- paste (md, collapse = "\n")
    ptn <- "\\[\\!\\[[^\\[]*\\]\\([^\\(]*\\)"
    matches <- regmatches (md, gregexpr (ptn, md)) [[1]]
    if (length (matches) > 1L) {
        for (m in matches) {
            md <- gsub (m, "", md, fixed = TRUE)
        }
    }
    md <- strsplit (md, "\\n") [[1]]

    # Rm code chunk contents:
    chunks <- grep ("^```", md)
    if (length (chunks) > 0L) {
        index <- seq_len (length (chunks) / 2) * 2 - 1
        index <- cbind (chunks [index], chunks [index + 1])
        index <- unlist (apply (index, 1, function (i) seq (i [1], i [2])))
        md <- md [-index]
    }
    # Chunk output is always spaces followed by "#":"
    chunk_out <- grep ("^\\s+#", md)
    if (length (chunk_out) > 0L) {
        md <- md [-chunk_out]
    }

    # Rm any HTML tables, which also includes 'allcontributors' output
    table_start <- grep ("^<table>", md)
    table_end <- grep ("^<\\/table>", md)
    if (length (table_start) > 0L && length (table_end) > 0L &&
        length (table_start) == length (table_end)) {
        index <- cbind (table_start, table_end)
        index <- unname (unlist (
            apply (index, 1, function (i) seq (i [1], i [2]))
        ))
        md <- md [-index]
    }

    # Finally, condense any sequences of empty lines:
    index <- which (!nzchar (md))
    index <- index [which (c (0, diff (index)) == 1)]
    if (length (index) > 0) md <- md [-(index)]

    return (md)
}

extract_one_rnw <- function (rnw_file) {

    rnw <- brio::read_lines (rnw_file)

    section_start <- grep ("^\\\\section", rnw)
    if (length (section_start) < 1L) {
        return (NULL)
    }
    rnw <- rnw [seq (min (section_start), length (rnw))]

    # Rm code chunk contents:
    chunk_starts <- grep ("^<<.*>>\\=", rnw)
    chunk_ends <- grep ("^@", rnw)
    if (length (chunk_starts) != length (chunk_ends)) {
        return (NULL)
    }
    chunks <- apply (cbind (chunk_starts, chunk_ends), 1, function (i) {
        seq (i [1], i [2])
    }, simplify = FALSE)
    if (length (chunks) > 0L) {
        chunks <- sort (do.call (c, chunks))
        rnw <- rnw [-chunks]
    }

    # Rm any LaTex "\begin{...}" and "\end{...}" lines
    index <- grep ("^\\\\(begin|end)\\{", rnw)
    if (length (index) > 0L) {
        rnw <- rnw [-index]
    }
    rnw <- gsub ("^\\\\item", "", rnw)

    # Remove any direct "\...{" commands:
    rnw <- gsub ("\\\\[^\\\\]*\\{", "", rnw, perl = TRUE)
    # And the terminal "}" after the command:
    rnw <- gsub ("(?<=[[:alpha:]])\\}", "", rnw, perl = TRUE)

    return (rnw)
}

get_pkg_code <- function (pkg_name = NULL, exported_only = FALSE) {

    stopifnot (length (pkg_name) == 1L)

    if (pkg_is_installed (pkg_name)) {
        fns <- get_fn_defs_namespace (pkg_name, exported_only = exported_only)
        fns <- vapply (seq_along (fns), function (i) {
            fi <- fns [[i]] |>
                deparse (width.cutoff = 500L) |>
                paste0 (collapse = "\n")
            paste0 (names (fns) [i], " <- ", fi)
        }, character (1L))

        fns <- fns [order (stats::runif (length (fns)))]

        fns <- paste0 (fns, collapse = "\n")
    } else {
        fns <- get_fn_defs_local (pkg_name)
    }

    return (fns)
}

get_fn_defs_local <- function (path) {

    path <- fs::path_norm (path)

    is_tarball <- fs::path_ext (path) == "gz"
    if (is_tarball) {
        path <- extract_tarball (path)
        on.exit ({
            fs::dir_delete (path)
        })
    }

    stopifnot (fs::dir_exists (path))
    path_r <- fs::path (path, "R")
    if (!fs::dir_exists (path_r)) {
        return ("")
    }

    files_r <- fs::dir_ls (path_r, regexp = "\\.(r|R)$")
    fns_r <- unlist (lapply (
        files_r,
        tryCatch (parse, error = function (e) NULL)
    ))

    fns_txt <- lapply (
        fns_r,
        function (f) tryCatch (deparse (f), error = function (e) NULL)
    )

    fns_txt <- fns_txt [which (nzchar (fns_txt))]
    fns_txt <- gsub ("[[:space:]]+", " ", fns_txt)
    # Permute for chunked inputs:
    fns_txt <- fns_txt [order (stats::runif (length (fns_txt)))]

    paste0 (fns_txt, collapse = "\n ")
}
