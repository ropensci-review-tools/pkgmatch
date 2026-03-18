#' Re-rank an input `data.frame` of packages with several columns of scores.
#'
#' @noRd
pkgmatch_rerank <- function (s) {

    cols <- names (s) [-which (names (s) %in% c ("package", "version"))]
    new_cols <- paste0 (cols, "_rank")
    decr <- TRUE # BM25 scores are always higher = better
    for (i in seq_along (cols)) {
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
    rank_index <- order (rowSums (rank_matrix), decreasing = TRUE)

    out <- data.frame (
        package = s$package [rank_index],
        rank = seq_along (rank_index)
    )
    if ("version" %in% names (s)) {
        version <- s$version [rank_index]
        out <- dplyr::mutate (out, version = version, .after = "package")
    }

    return (out)
}
