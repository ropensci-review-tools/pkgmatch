mknm <- function (nchar = 12) {
    x <- sample (c (letters, LETTERS), size = nchar)
    paste0 (x, collapse = "")
}

npkgs <- nfns <- 10L

get_test_idfs1 <- function (txt) {

    pkg_nms <- vapply (seq_along (txt), function (i) mknm (), character (1L))

    token_lists <- bm25_tokens_list (txt)
    names (token_lists) <- pkg_nms
    list (idfs = bm25_idf (txt), token_lists = token_lists)
}

get_test_idfs <- function (txt, seed = 1L) {

    set.seed (seed)
    i <- get_test_idfs1 (txt)
    list (full = i, descs_only = i)
}
