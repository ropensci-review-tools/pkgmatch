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

#' @title Return raw embeddings from package text and function definitions.
#'
#' @description This function accepts a vector of either names of installed
#' packages, or paths to local source code directories, and calculates language
#' model (LM) embeddings for both text descriptions within the package
#' (documentation, including of functions), and for the entire code base.
#' Embeddings may also be calculating separately for all function descriptions.
#'
#' The embeddings are currently retrieved from a local 'ollama' server
#' (\url{https://ollama.com}) running Jina AI embeddings
#' (\url{https://ollama.com/jina/jina-embeddings-v2-base-en} for text, and
#' \url{https://ollama.com/ordis/jina-embeddings-v2-base-code} for code).
#'
#' @param packages A vector of either names of installed packages, or local
#' paths to directories containing R packages.
#' @param functions_only If `TRUE`, calculate embeddings for function
#' descriptions only. This is intended to generate a separate set of embeddings
#' which can then be used to match plain-text queries of functions, rather than
#' entire packages.
#' @return If `!functions_only`, a list of two matrices of embeddings: one for
#' the text descriptions of the specified packages, including individual
#' descriptions of all package functions, and one for the entire code base. For
#' `functions_only`, a single matrix of embeddings for all function
#' descriptions.
#'
#' @family embeddings
#' @export
#'
#' @examples
#' \dontrun{
#' packages <- c ("cli", "fs")
#' emb_fns <- pkgmatch_embeddings_from_pkgs (packages, functions_only = TRUE)
#' colnames (emb_fns) # All functions of the two packages
#' emb_pkg <- pkgmatch_embeddings_from_pkgs (packages, functions_only = FALSE)
#' names (emb_pkg) # text_with_fns, text_wo_fns, code
#' colnames (emb_pkg$text_with_fns) # cli, fs
#' }
pkgmatch_embeddings_from_pkgs <- function (packages = NULL,
                                           functions_only = FALSE) {

    checkmate::assert_logical (functions_only, len = 1L)
    checkmate::assert_character (packages)
    if (all (grepl ("\\.tar\\.gz$", packages))) {
        checkmate::assert_file_exists (packages)
    } else {
        if (!all (pkg_is_installed (packages))) {
            checkmate::assert_directory_exists (packages)
        }
    }

    pkgs_full <- packages
    packages <- convert_paths_to_pkgs (pkgs_full)
    if (all (grepl ("\\.tar\\.gz$", packages))) {
        packages <- gsub ("\\.tar\\.gz$", "", basename (packages))
    }

    if (!opt_is_quiet () && length (packages) > get_verbose_limit ()) {
        cli::cli_inform ("Extracting package text ...")
        txt_with_fns <-
            pbapply::pblapply (pkgs_full, function (p) get_pkg_text (p))
    } else {
        txt_with_fns <- lapply (pkgs_full, function (p) get_pkg_text (p))
    }

    txt_wo_fns <- rm_fns_from_pkg_txt (txt_with_fns)

    if (!functions_only) {

        cli::cli_inform ("Generating text embeddings [1 / 2] ...")
        embeddings_text_with_fns <- get_embeddings (txt_with_fns, code = FALSE)

        cli::cli_inform ("Generating text embeddings [2 / 2] ...")
        embeddings_text_wo_fns <- get_embeddings (txt_wo_fns, code = FALSE)

        embeddings_text_with_fns <-
            apply_col_names (embeddings_text_with_fns, txt_with_fns, packages)
        embeddings_text_wo_fns <-
            apply_col_names (embeddings_text_wo_fns, txt_wo_fns, packages)

        if (!opt_is_quiet () && length (packages) > get_verbose_limit ()) {
            cli::cli_inform ("Extracting package code ...")
            code <- pbapply::pblapply (pkgs_full, function (p) get_pkg_code (p))
            code <- unlist (code)
        } else {
            code <- vapply (
                pkgs_full,
                function (p) get_pkg_code (p),
                character (1L)
            )
        }
        cli::cli_inform ("Generating code embeddings ...")
        embeddings_code <- get_embeddings (code, code = TRUE)

        embeddings_code <- apply_col_names (embeddings_code, code, packages)

        ret <- list (
            text_with_fns = embeddings_text_with_fns,
            text_wo_fns = embeddings_text_wo_fns,
            code = embeddings_code
        )

    } else {

        cli::cli_inform (
            "Generating text embeddings for function descriptions ..."
        )
        txt_fns <- get_all_fn_descs (txt_with_fns)
        ret <- get_embeddings (txt_fns$desc, code = FALSE)
        colnames (ret) <- txt_fns$fn
    }

    return (ret)
}

#' @title Return raw embeddings from a vector of text strings.
#'
#' @description This function accepts a vector of character strings,
#' packages, or paths to local source code directories, and calculates language
#' model (LM) embeddings for each string within the vector.
#'
#' The embeddings are currently retrieved from a local 'ollama' server
#' (\url{https://ollama.com}) running Jina AI text embeddings
#' (\url{https://ollama.com/jina/jina-embeddings-v2-base-en}).
#'
#' @param input A vector of one or more text strings for which embeddings are
#' to be extracted.
#' @return A matrix of embeddings, one column for each `input` item, and a
#' fixed number of rows defined by the embedding length of the language models.
#'
#' @family embeddings
#' @export
#'
#' @examples
#' \dontrun{
#' input <- "Download open spatial data from NASA"
#' emb <- pkgmatch_embeddings_from_text (input = input)
#' }
pkgmatch_embeddings_from_text <- function (input = NULL) {

    checkmate::assert_character (input)

    get_embeddings (input, code = FALSE)
}

rm_fns_from_pkg_txt <- function (txt) {

    lapply (txt, function (i) {
        i_vec <- strsplit (i, "\\n") [[1]]
        index <- grep ("^\\s*##\\s+Functions", i_vec)
        if (length (index) > 0L) {
            index <- seq (max (index), length (i_vec))
            i_vec <- i_vec [-(index)]
        }
        paste0 (i_vec, collapse = "\\n")
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

get_embeddings <- function (txt, code = FALSE) {
    m_get_embeddings_intern (txt, code)
}

get_embeddings_intern <- function (txt, code = FALSE) {

    ollama_check ()

    # Then remove line breaks to pass to embeddings:
    if (!code) {
        txt <- lapply (
            txt,
            function (i) gsub ("\\s+", " ", gsub ("\\n", " ", i))
        )
    }

    if (!opt_is_quiet () && length (txt) > get_verbose_limit ()) {
        embeddings <- pbapply::pblapply (
            txt,
            function (i) get_embeddings_from_ollama (i, code = code)
        )
    } else {
        embeddings <- lapply (
            txt,
            function (i) get_embeddings_from_ollama (i, code = code)
        )
    }

    do.call (cbind, embeddings)
}

m_get_embeddings_intern <- memoise::memoise (get_embeddings_intern)

get_embeddings_from_ollama <- function (input, code = FALSE) {

    stopifnot (length (input) == 1L)

    u <- paste0 (get_ollama_url (), "/api/embeddings")

    model <- ifelse (
        code,
        "ordis/jina-embeddings-v2-base-code",
        "jina/jina-embeddings-v2-base-en"
    )
    data <- list (model = model, prompt = input)

    req <- httr2::request (u) |>
        httr2::req_headers ("Content-Type" = "application/json") |>
        httr2::req_body_json (data = data)

    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    embeddings <- httr2::resp_body_json (resp, simplifyVector = FALSE)
    unlist (embeddings$embedding)
}
