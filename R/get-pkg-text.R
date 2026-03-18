get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools") # nolint

#' Grep'able separator for different sections of output doc.
#' @noRd
sec_separator <- function (what = "Functions", regex = FALSE) {

    pre <- ifelse (regex, "^(\\s?)##\\s\\-{4}\\s", "## ---- ")
    post <- ifelse (regex, "\\s\\-{4}$", " ----")
    paste0 (pre, what, post)
}

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

get_pkg_text_namespace <- function (pkg_name, include_news = FALSE) {

    # Suppress no visible binding notes:
    Package <- NULL

    stopifnot (length (pkg_name) == 1L)

    desc <- utils::packageDescription (pkg = pkg_name, fields = "Description")
    desc <- desc_template (pkg_name, desc)

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
    rmds <- rnws <- news <- NULL
    if (nrow (ip) > 0L) {
        pkg_path <- fs::path (ip$LibPath, pkg_name)
        long_docs <- get_pkg_text_md (pkg_path, include_news = include_news)
    }

    paste0 (c (
        desc,
        "",
        sec_separator ("Vignettes", regex = FALSE),
        long_docs,
        sec_separator ("Functions", regex = FALSE),
        "",
        unlist (fns)
    ), collapse = "\n ")
}

#' Get long-form vignettes and other '.md' or '.Rnw'-type documents
#' @noRd
get_pkg_text_md <- function (path, include_news) {

    news <- NULL

    md_files <- fs::dir_ls (path, recurse = TRUE, regexp = "\\.(R)?md")
    md_files <- md_files [which (!duplicated (fs::path_file (md_files)))]
    excl <- which (vapply (fs::path_split (md_files), function (f) {
        any (f %in% "tests")
    }, logical (1L)))
    excl <- c (
        excl,
        which (grepl ("license", basename (md_files), ignore.case = TRUE))
    )
    if (length (excl) > 0L) {
        md_files <- md_files [-(excl)]
    }

    fnms <- basename (fs::path_ext_remove (md_files))
    n <- grep ("news", fnms, ignore.case = TRUE)
    if (length (n) > 0) {
        if (include_news) {
            news <- brio::read_lines (md_files [n])
            news <- c (sec_seperator ("NEWS", regex = FALSE), "", news, "")
        }
        md_files <- md_files [-n]
    }
    rmds <- unname (unlist (lapply (md_files, extract_one_md)))

    rnw_files <- fs::dir_ls (path, recurse = TRUE, regexp = "\\.Rnw")
    rnw_files <- rnw_files [which (!duplicated (fs::path_file (rnw_files)))]
    rnws <- unname (unlist (lapply (rnw_files, extract_one_rnw)))

    c (rmds, rnws, news)
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

    desc <- gsub ("\\n", " ", desc)
    desc <- gsub ("\\s+", " ", desc)

    c (
        paste0 ("# ", pkg_name),
        "",
        "## Description",
        "",
        desc,
        ""
    )
}

get_pkg_text_local <- function (path, include_news = FALSE) {

    stopifnot (length (path) == 1L)

    path <- fs::path_norm (path)

    is_tarball <- fs::path_ext (path) == "gz"
    if (is_tarball) {
        path <- extract_tarball (path)
        on.exit ({
            fs::dir_delete (path)
        })
    }

    stopifnot (fs::dir_exists (path))

    desc_file <- fs::path (path, "DESCRIPTION")
    if (!fs::file_exists (desc_file)) {
        return ("")
    }
    desc <- read.dcf (desc_file)
    pkg_name <- unname (desc [1, "Package"])
    desc <- unname (desc [1, "Description"])

    long_docs <- get_pkg_text_md (path, include_news = include_news)
    fn_docs <- pkg_text_from_local_rds (path)

    paste0 (c (
        desc_template (pkg_name, desc),
        "",
        sec_separator ("Vignettes", regex = FALSE),
        long_docs,
        sec_separator ("Functions", regex = FALSE),
        "",
        unlist (fn_docs)
    ), collapse = "\n ")
}

pkg_text_from_local_rds <- function (path) {

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

    return (fn_txt)
}

convert_paths_to_pkgs <- function (packages) {

    is_installed <- pkg_is_installed (packages)
    if (any (is_installed) && !all (is_installed)) {
        stop (
            "packages must either name installed packages, ",
            "or supply paths to local source packages, but ",
            "not both."
        )
    }
    if (!any (is_installed)) {
        packages <- basename (packages)
    }
    return (packages)
}

rm_fns_from_pkg_txt <- function (txt) {

    lapply (txt, function (i) {
        is_list <- is.list (i)
        if (!is_list) {
            i <- list (i)
        }
        res <- lapply (i, function (j) {
            j_vec <- strsplit (j, "\\n") [[1]]
            index <- grep (sec_separator ("Functions", regex = TRUE), j_vec)
            if (length (index) > 0L) {
                index <- seq (max (index), length (j_vec))
                j_vec <- j_vec [-(index)]
            }
            paste0 (j_vec, collapse = "\n")
        })
        if (!is_list) {
            res <- unlist (res)
        }
        return (res)
    })
}

get_all_fn_descs <- function (txt) {

    fn_txt <- lapply (txt, function (i) {
        i_sp <- strsplit (i, "\\n") [[1]]
        ptn <- "^[[:space:]]*#[[:space:]]"
        pkg_name <- grep (ptn, i_sp)
        if (length (pkg_name) > 0L) {
            pkg_name <- gsub (ptn, "", i_sp [pkg_name [1]])
            pkg_name <- gsub ("[[:space:]]*", "", pkg_name)
        } else {
            pkg_name <- "pkg_has_no_name"
        }

        pos <- grep (sec_separator ("Functions", regex = TRUE), i_sp)
        if (length (pos) == 0) {
            fn_nms <- fn_descs <- character (0L)
        } else {
            # Fn defs are always added at end, so pos has to be last value:
            pos <- utils::tail (pos, n = 1L)

            fns <- i_sp [-seq_len (pos)]
            index <- which (!nzchar (fns) | grepl ("^[[:space:]]+$", fns))
            if (length (index) > 0L) {
                fns <- fns [-index]
            }
            # Construct list of (name, description) for each fn:
            index1 <- grep ("^[[:space:]]*###", fns)
            index2 <- c (index1 [-1] - 1L, length (fns))
            index <- apply (
                cbind (index1, index2), 1,
                function (i) seq (i [1], i [2]),
                simplify = FALSE
            )
            fns <- lapply (index, function (i) fns [i])
            fn_nms <- vapply (fns, function (i) {
                gsub ("###|[[:space:]]*", "", i [1])
            }, character (1L))
            fn_nms <- paste0 (pkg_name, "::", fn_nms)
            fn_descs <- vapply (fns, function (i) {
                gsub ("^[[:space:]]*", "", paste0 (i [-1], collapse = " "))
            }, character (1L))
        }
        data.frame (fn = fn_nms, desc = fn_descs)
    })
    res <- do.call (rbind, fn_txt)
    rownames (res) <- NULL
    return (res)
}
