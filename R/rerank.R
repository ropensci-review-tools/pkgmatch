#' Re-randk an input `data.frame` of packages with several columns of scores.
#'
#' @param rm_fn_data If `TRUE` (default), only generate combined ranks from
#' data excluding function descriptions.
#' @noRd
pkgmatch_rerank <- function (s, rm_fn_data = TRUE, llm_proportion = 0.5) {

    cols <- names (s) [-which (names (s) %in% c ("package", "version"))]
    new_cols <- paste0 (cols, "_rank")
    for (i in seq_along (cols)) {
        # The order of values provides the index that has to be filled with
        # 1..N values.
        decr <- grepl ("^(bm25|simil)", cols [i])
        o <- order (s [[cols [i]]], decreasing = decr)
        index <- rep (NA_integer_, length (o))
        index [o] <- seq_along (o)
        s [[new_cols [i]]] <- index
    }

    # For this fixed value of `k`, see:
    # https://plg.uwaterloo.ca/~gvcormac/cormacksigir09-rrf.pdf
    k <- 60

    rank_matrix <- as.matrix (s [, new_cols])
    rank_matrix <- 1 / (k + rank_matrix)

    if (rm_fn_data) {
        cols_with <- grep ("\\_with\\_", colnames (rank_matrix))
        rank_matrix <- rank_matrix [, -(cols_with)]
    }

    code_cols <- grep ("code", colnames (rank_matrix))
    text_cols <- seq_len (ncol (rank_matrix))
    if (length (code_cols) > 0L) {
        text_cols <- text_cols [-(code_cols)]
    }

    text_rank <- rank_matrix [, text_cols]
    text_rank <- modify_by_llm_prop (text_rank, llm_proportion)
    text_index <- order (rowSums (text_rank), decreasing = TRUE)

    out <- data.frame (
        package = s$package [text_index],
        text_rank = seq_along (text_index)
    )
    if ("version" %in% names (s)) {
        version <- s$version [text_index]
        out <- dplyr::mutate (out, version = version, .after = "package")
    }

    if (length (code_cols) > 0L) {
        code_rank <- rank_matrix [, code_cols]
        code_rank <- modify_by_llm_prop (code_rank, llm_proportion)
        code_index <- order (rowSums (code_rank), decreasing = TRUE)
        out_code <- data.frame (
            package = s$package [code_index],
            code_rank = seq_along (code_index)
        )
        out <- dplyr::left_join (out, out_code, by = "package")
    } else {
        out <- dplyr::rename (out, rank = "text_rank") |>
            dplyr::arrange (rank)
    }

    return (out)
}

modify_by_llm_prop <- function (ranks, llm_proportion) {
    bm25_cols <- grep ("^bm25", colnames (ranks))
    llm_cols <- seq_len (ncol (ranks)) [-bm25_cols]

    bm25_wt <- (1 - llm_proportion) / length (bm25_cols)
    llm_wt <- llm_proportion / length (llm_cols)

    rank_wts <- ranks
    rank_wts [, bm25_cols] <- bm25_wt
    rank_wts [, llm_cols] <- llm_wt

    ranks * rank_wts
}
