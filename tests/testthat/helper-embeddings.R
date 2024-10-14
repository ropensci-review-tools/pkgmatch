mknm <- function (nchar = 12) {
    x <- sample (c (letters, LETTERS), size = nchar)
    paste0 (x, collapse = "")
}

npkgs <- nfns <- 10L
expected_embedding_length <- 768
n <- npkgs * expected_embedding_length

get_test_embeddings <- function (npkgs, nfns, embedding_len, seed = 1L, cran = FALSE) {

    set.seed (seed)

    n <- npkgs * embedding_len

    pkg_nms <- vapply (seq_len (npkgs), function (i) mknm (), character (1L))
    if (cran) {
        # Add version and tarball suffices to pkg names:
        rand_vers <- function (n) {
            paste0 (round (runif (n, 0, 9)), ".", round (runif (n, 0, 9)))
        }
        pkg_nms <- paste0 (pkg_nms, "_", rand_vers (npkgs), ".tar.gz")
    }

    emb_code <- matrix (runif (n), nrow = embedding_len, ncol = npkgs)
    emb_txt2 <- matrix (runif (n), nrow = embedding_len, ncol = npkgs)
    emb_txt1 <- matrix (runif (n), nrow = embedding_len, ncol = npkgs)
    colnames (emb_txt1) <- colnames (emb_txt2) <- colnames (emb_code) <- pkg_nms

    list (text_with_fns = emb_txt1, text_wo_fns = emb_txt2, code = emb_code)
}

get_test_embeddings_fns <- function (nfns, embedding_len, seed = 1L) {

    set.seed (seed)

    n <- nfns * embedding_len
    pkg_nms <- vapply (seq_len (nfns), function (i) mknm (6), character (1L))
    fn_nms <- vapply (seq_len (nfns), function (i) mknm (6), character (1L))
    fn_nms <- paste0 (pkg_nms, "::", fn_nms)

    out <- matrix (runif (n), nrow = embedding_len, ncol = nfns)
    colnames (out) <- fn_nms

    return (out)
}

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
