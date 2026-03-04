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
            index <- grep ("^\\s*##\\s+Functions", j_vec)
            if (length (index) > 0L) {
                index <- seq (max (index), length (j_vec))
                j_vec <- j_vec [-(index)]
            }
            paste0 (j_vec, collapse = "\\n")
        })
        if (!is_list) {
            res <- unlist (res)
        }
        return (res)
    })
}

get_all_fn_descs <- function (txt) {

    # txt is a list of (packages, chunks). First reduce down to the chunk with
    # the longest function description desction.
    txt_red <- lapply (txt, function (i) {
        lens <- vapply (i, function (j) {
            j_sp <- strsplit (j, "\\n") [[1]]
            pos <- grep ("##\\s+Functions$", j_sp)
            pos <- ifelse (length (pos) == 0L, length (j_sp), pos)
            length (j_sp) - pos + 1L
        }, integer (1L))
        return (i [[which.max (lens)]])
    })

    fn_txt <- lapply (txt_red, function (i) {
        i_sp <- strsplit (i, "\\n") [[1]]
        ptn <- "^[[:space:]]*#[[:space:]]"
        pkg_name <- grep (ptn, i_sp)
        if (length (pkg_name) > 0L) {
            pkg_name <- gsub (ptn, "", i_sp [pkg_name [1]])
            pkg_name <- gsub ("[[:space:]]*", "", pkg_name)
        } else {
            pkg_name <- "pkg_has_no_name"
        }

        pos <- grep ("##\\s+Functions$", i_sp)
        if (length (pos) == 0) {
            fn_nms <- fn_descs <- character (0L)
        } else {
            # Fn defs are always added at end, so pos has to be last value:
            pos <- utils::tail (pos, n = 1L)

            fns <- i_sp [seq (pos + 1, length (i_sp))]
            index <- which (!nzchar (fns) | grepl ("^[[:space:]]+$", fns))
            if (length (index) > 0L) fns <- fns [-index]
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
    do.call (rbind, fn_txt)
}
