get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools") # nolint

get_pkg_text <- function (pkg_name) {

    m_get_pkg_text (pkg_name)
}

get_pkg_text_internal <- function (pkg_name) {

    if (pkg_is_installed (pkg_name) && !fs::dir_exists (pkg_name)) {
        txt <- get_pkg_text_namespace (pkg_name)
    } else {
        txt <- get_pkg_text_local (pkg_name)
    }

    return (txt)
}
m_get_pkg_text <- memoise::memoise (get_pkg_text_internal)

get_pkg_text_namespace <- function (pkg_name) {

    # Suppress no visible binding notes:
    Package <- NULL

    stopifnot (length (pkg_name) == 1L)

    desc <- utils::packageDescription (pkg = pkg_name, fields = "Description")
    desc <- gsub ("\\n", " ", desc)
    desc <- gsub ("\\s+", " ", desc)
    desc <- c (
        desc_template (pkg_name, desc),
        "## Functions",
        ""
    )

    fns <- get_fn_descs_from_ns (pkg_name)
    fns <- lapply (seq_len (nrow (fns)), function (i) {
        c (
            paste0 ("### ", gsub ("\\.Rd$", "", fns$rd_name [i])),
            "",
            fns$desc [i],
            ""
        )
    })

    ip <- data.frame (utils::installed.packages ()) |>
        dplyr::filter (Package == pkg_name)
    rmds <- NULL
    if (nrow (ip) > 0L) {
        pkg_path <- fs::path (ip$LibPath, pkg_name)
        rmd_files <- fs::dir_ls (pkg_path, recurse = TRUE, regexp = "\\.(R)?md")
        rmd_files <- rmd_files [which (!duplicated (fs::path_file (rmd_files)))]
        rmds <- unname (unlist (lapply (rmd_files, extract_one_md)))
    }

    paste0 (c (
        desc,
        "",
        "## Vignettes",
        rmds,
        unlist (fns)
    ), collapse = "\n ")
}

get_fn_descs_from_ns <- function (pkg_name) {

    rd <- tools::Rd_db (package = pkg_name)
    descs <- vapply (rd, function (i) {
        get_Rd_metadata (i, "description")
    }, character (1L))
    descs <- gsub ("\\\\n", " ", descs)
    descs <- gsub ("\\n", " ", descs)
    descs <- gsub ("\\", "", descs, fixed = TRUE)
    descs <- gsub ("\\", "", descs, fixed = TRUE)
    descs <- gsub ("\\s+", " ", descs)

    index <- which (!is.na (descs))
    data.frame (
        desc = unname (descs),
        rd_name = names (descs)
    ) [index, ]
}

desc_template <- function (pkg_name, desc) {
    c (
        paste0 ("# ", pkg_name, "\n"),
        "",
        "## Description",
        "",
        desc,
        ""
    )
}

get_pkg_text_local <- function (path) {

    stopifnot (length (path) == 1L)

    path <- fs::path_norm (path)

    is_tarball <- fs::path_ext (path) == "gz"
    if (is_tarball) {
        path <- tarball_to_path (path)
        on.exit ({
            fs::dir_delete (path)
        })
    }

    stopifnot (fs::dir_exists (path))

    desc_file <- fs::path (path, "DESCRIPTION")
    if (!fs::file_exists (desc_file)) {
        return ("")
    }
    desc <- data.frame (read.dcf (desc_file))$Description

    readme <- get_pkg_readme (path)
    rmd_files <- fs::dir_ls (path, regexp = "\\.Rmd$", recurse = TRUE)
    rmd_files <- rmd_files [which (!duplicated (fs::path_file (rmd_files)))]
    vignettes <- lapply (rmd_files, extract_one_md)

    rd_path <- fs::path (path, "man")
    if (!fs::file_exists (rd_path)) {
        return ("")
    }
    rd_files <- fs::dir_ls (rd_path, regex = "\\.Rd")
    rd <- lapply (rd_files, function (i) {
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

        index <- which (tags == "description")
        if (length (index) == 0) {
            return ("")
        }
        rd_desc <- gsub ("\\n$", "", unlist (rd [[index]]))
        paste (rd_desc, collapse = "")
    })
    rd <- rd [vapply (rd, nzchar, logical (1L))]

    rd <- rd [order (stats::runif (length (rd)))]

    fns <- gsub ("\\.Rd$", "", basename (names (rd)))
    rd <- unname (unlist (rd))
    fn_txt <- lapply (seq_len (length (rd)), function (i) {
        c (
            paste0 ("### ", fns [i]),
            "",
            rd [i],
            ""
        )
    })

    docs_list <- c (list (readme), vignettes)
    docs_list <- docs_list [order (stats::runif (length (docs_list)))]

    out <- c (
        desc_template (basename (path), desc),
        readme,
        "",
        docs_list,
        "",
        "## Functions",
        "",
        unlist (fn_txt)
    )

    paste0 (out, collapse = "\n ")
}

get_pkg_readme <- function (path) {

    readme_file <- fs::path (path, "README.md")
    if (!fs::file_exists (readme_file)) {
        return (NULL)
    }
    extract_one_md (readme_file)
}

extract_one_md <- function (md_file) {

    md <- brio::read_lines (md_file)

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
        path <- tarball_to_path (path)
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
    # Some files, ex. glinvci_1.2.4.tar.gz, parse okay but then fail on
    # subsequent calls like `unlist` or `deparse`. It is necessary to catch
    # errors on every step.
    fns_r <- lapply (
        fns_r,
        function (f) tryCatch (eval (f), error = function (e) NULL)
    )
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

tarball_to_path <- function (path) {

    stopifnot (fs::path_ext (path) == "gz")

    tempdir <- fs::path (fs::path_temp (), "tarballs")
    if (!fs::dir_exists (tempdir)) {
        fs::dir_create (tempdir, recurse = TRUE)
    }
    path2 <- fs::path (tempdir, basename (path))
    fs::file_copy (path, path2)
    utils::untar (path2, exdir = tempdir)
    fs::file_delete (path2)

    fs::dir_ls (tempdir)
}
