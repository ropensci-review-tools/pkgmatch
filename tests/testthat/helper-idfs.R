mknm <- function (nchar = 12) {
    x <- sample (c (letters, LETTERS), size = nchar)
    paste0 (x, collapse = "")
}

npkgs <- nfns <- 10L

get_test_idfs <- function (txt, seed = 1L) {

    set.seed (seed)
    pkg_nms <- vapply (seq_along (txt), function (i) mknm (), character (1L))

    token_lists <- list (
        with_fns = bm25_tokens_list (txt),
        wo_fns = bm25_tokens_list (txt)
    )
    names (token_lists$with_fns) <- names (token_lists$wo_fns) <- pkg_nms
    idf_data <- list (with_fns = bm25_idf (txt), wo_fns = bm25_idf (txt))
    list (idfs = idf_data, token_lists = token_lists)
}
