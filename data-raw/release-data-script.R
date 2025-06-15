devtools::load_all (".", export_all = TRUE, helpers = FALSE)
# library (pkgmatch)
ollama_check ()
options ("rlib_message_verbosity" = "verbose")

path <- "/<path>/<to>/<ropensci>/<repos>"
packages <- fs::dir_ls (path, type = "directory")

# ----------------- EMBEDDINGS FOR ROPENSCI -----------------
embeddings <- pkgmatch_embeddings_from_pkgs (packages)

saveRDS (embeddings, "embeddings-ropensci.Rds")
embeddings_fns <-
    pkgmatch_embeddings_from_pkgs (packages, functions_only = TRUE)
saveRDS (embeddings_fns, "embeddings-fns.Rds")

# -------------------- BM25 FOR ROPENSCI --------------------
num_cores <- parallel::detectCores () - 2L
cl <- parallel::makeCluster (num_cores)
txt_with_fns <- pbapply::pblapply (
    packages,
    function (p) pkgmatch:::get_pkg_text (p),
    cl = cl
)
parallel::stopCluster (cl)

txt_wo_fns <- rm_fns_from_pkg_txt (txt_with_fns)
idfs <- list (
    with_fns = bm25_idf (txt_with_fns),
    wo_fns = bm25_idf (txt_wo_fns)
)
token_lists <- list (
    with_fns = bm25_tokens_list (txt_with_fns),
    wo_fns = bm25_tokens_list (txt_wo_fns)
)
bm25_data <- list (idfs = idfs, token_lists = token_lists)
saveRDS (bm25_data, "bm25-ropensci.Rds")

txt_fns <- get_all_fn_descs (txt_with_fns)
fns_idfs <- bm25_idf (txt_fns$desc)
fns_lists <- bm25_tokens_list (txt_fns$desc)
index <- which (vapply (fns_lists, nrow, integer (1L)) > 0L)
fns_lists <- fns_lists [index]
names (fns_lists) <- txt_fns$fn [index]
bm25_data <- list (idfs = fns_idfs, token_lists = fns_lists)
saveRDS (bm25_data, "bm25-ropensci-fns.Rds")

# ------------------ FN CALLS FOR ROPENSCI ------------------
flist <- fs::dir_ls (path, recurse = FALSE)
num_cores <- parallel::detectCores () - 2L
cl <- parallel::makeCluster (num_cores)

calls <- pbapply::pblapply (flist, function (f) {
    res <- tryCatch (
        pkgmatch::pkgmatch_treesitter_fn_tags (f),
        error = function (e) NULL
    )
    if (is.null (res)) {
        res <- data.frame (name = character (0L))
    }
    sort (table (res$name), decreasing = TRUE)
}, cl = cl)

parallel::stopCluster (cl)
names (calls) <- basename (names (calls))
index <- which (vapply (calls, length, integer (1L)) > 0)
calls <- calls [index]

saveRDS (calls, "fn-calls-ropensci.Rds")

# Then remove self-calls:
calls <- lapply (seq_along (calls), function (i) {
    this_pkg <- names (calls) [i]
    ptn <- paste0 ("^", this_pkg, "\\:\\:")
    index <- which (!grepl (ptn, names (calls [[i]])))
    names (calls [[i]]) [index]
})

# And convert to inverse doc freqs:
tokens_idf <- data.frame (
    token = unique (unlist (calls)),
    n = 0L
)
for (i in seq_along (calls)) {
    index <- match (calls [[i]], tokens_idf$token)
    tokens_idf$n [index] <- tokens_idf$n [index] + 1L
}
n_docs <- length (calls)
tokens_idf$idf <- log ((n_docs - tokens_idf$n + 0.5) / (tokens_idf$n + 0.5) + 1)
tokens_idf$n <- NULL

saveRDS (tokens_idf, "idfs-fn-calls-ropensci.Rds")


# -------------------- EMBEDDINGS FOR CRAN --------------------
options ("rlib_message_verbosity" = "verbose")
path <- "/<path>/<to>/<cran-mirror>/tarballs"
packages <- fs::dir_ls (path, regexpr = "\\.tar\\.gz$")
embeddings <- pkgmatch_embeddings_from_pkgs (packages)

# Fn to reduce names and remove any duplicate packages (owing to multiple
# versions in tarball dir):
rename_cols <- function (e) {
    nms_full <- basename (colnames (e))
    nms <- gsub ("\\_.*$", "", nms_full)
    dups <- nms [which (duplicated (nms))]
    if (length (dups) > 0L) {
        index <- match (dups, nms)
        e <- e [, -index]
        nms_full <- nms_full [-index]
    }
    colnames (e) <- nms_full

    return (e)
}
embeddings$text_with_fns <- rename_cols (embeddings$text_with_fns)
embeddings$text_wo_fns <- rename_cols (embeddings$text_wo_fns)
embeddings$code <- rename_cols (embeddings$code)

saveRDS (embeddings, "embeddings-cran.Rds")

# -------------------- BM25 FOR CRAN --------------------
cli::cli_inform ("Extract text from all CRAN packages ...")
num_cores <- parallel::detectCores () - 2L
cl <- parallel::makeCluster (num_cores)

txt_with_fns <- pbapply::pblapply (
    packages,
    function (p) pkgmatch:::get_pkg_text (p),
    cl = cl
)
parallel::stopCluster (cl)

txt_wo_fns <- rm_fns_from_pkg_txt (txt_with_fns)
idfs <- list (
    with_fns = bm25_idf (txt_with_fns),
    wo_fns = bm25_idf (txt_wo_fns)
)
token_lists <- list (
    with_fns = bm25_tokens_list (txt_with_fns),
    wo_fns = bm25_tokens_list (txt_wo_fns)
)
rename_lists <- function (ll) {
    nms_full <- basename (names (ll))
    nms <- gsub ("\\_.*$", "", nms_full)
    dups <- nms [which (duplicated (nms))]
    if (length (dups) > 0L) {
        index <- match (dups, nms)
        ll <- ll [-index]
        nms_full <- nms_full [-index]
    }
    names (ll) <- nms_full

    return (ll)

}
token_lists$with_fns <- rename_lists (token_lists$with_fns)
token_lists$wo_fns <- rename_lists (token_lists$wo_fns)
bm25_data <- list (idfs = idfs, token_lists = token_lists)
saveRDS (bm25_data, "bm25-cran.Rds")

# ------------------ FN CALLS FOR CRAN ------------------
cli::cli_inform ("Extract function calls from all CRAN packages ...")
num_cores <- parallel::detectCores () - 2L
cl <- parallel::makeCluster (num_cores)

calls <- pbapply::pblapply (packages, function (f) {
    res <- tryCatch (
        pkgmatch::pkgmatch_treesitter_fn_tags (f),
        error = function (e) NULL
    )
    if (is.null (res)) {
        res <- data.frame (name = character (0L))
    }
    sort (table (res$name), decreasing = TRUE)
}, cl = cl)

parallel::stopCluster (cl)

names (calls) <- basename (names (calls))
index <- which (vapply (calls, length, integer (1L)) > 0)
calls <- calls [index]

# Rm any duplicated packages (with different versions)
nms <- gsub ("\\_.*$", "", names (calls))
dups <- nms [which (duplicated (nms))]
if (length (dups) > 0L) {
    index <- match (dups, nms)
    calls <- calls [-index]
}
saveRDS (calls, "fn-calls-cran.Rds")

# Then remove self-calls:
calls <- lapply (seq_along (calls), function (i) {
    this_pkg <- names (calls) [i]
    ptn <- paste0 ("^", this_pkg, "\\:\\:")
    index <- which (!grepl (ptn, names (calls [[i]])))
    names (calls [[i]]) [index]
})

# And convert to inverse doc freqs:
tokens_idf <- data.frame (
    token = unique (unlist (calls)),
    n = 0L
)
for (i in seq_along (calls)) {
    index <- match (calls [[i]], tokens_idf$token)
    tokens_idf$n [index] <- tokens_idf$n [index] + 1L
}
n_docs <- length (calls)
tokens_idf$idf <- log ((n_docs - tokens_idf$n + 0.5) / (tokens_idf$n + 0.5) + 1)
tokens_idf$n <- NULL

saveRDS (tokens_idf, "idfs-fn-calls-cran.Rds")
