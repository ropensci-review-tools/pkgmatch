similarity_embeddings <- function (input, embeddings, input_is_code) {

    this_emb <- get_embeddings (input, code = input_is_code)

    if (is.list (embeddings)) {

        dat_with_fns <- cosine_similarity (this_emb, embeddings$text_with_fns)
        dat_wo_fns <- cosine_similarity (this_emb, embeddings$text_wo_fns)

        names (dat_wo_fns) [2] <- "simil_wo_fns"
        names (dat_with_fns) [2] <- "simil_with_fns"

        dat <- dplyr::left_join (dat_with_fns, dat_wo_fns, by = "package")

    } else {

        dat <- cosine_similarity (this_emb, embeddings)
    }

    return (dat)
}

#' cosine similarity between one input vector and an input matrix with column
#' names.
#' @noRd
cosine_similarity <- function (this_vec, this_mat, fns = FALSE) {

    nrow <- length (this_vec)
    ncol <- ncol (this_mat)
    emb_mat <- matrix (this_vec, nrow = nrow, ncol = ncol)

    cs_num <- colSums (emb_mat * this_mat)
    cs_denom <- sqrt (colSums (emb_mat^2) * colSums (this_mat^2))
    cs <- cs_num / cs_denom

    index <- order (cs, decreasing = TRUE)
    res <- data.frame (names (cs), unname (cs)) [index, ]
    names (res) <- c (ifelse (fns, "function", "package"), "simil")
    rownames (res) <- NULL

    return (res)
}
