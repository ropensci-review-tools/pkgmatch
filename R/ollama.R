# Functions to check ollama status

is_windows <- function () {
    grepl ("windows", Sys.info () ["sysname"], ignore.case = TRUE)
}

is_docker_sudo <- function () {
    if (is_windows ()) {
        return (FALSE)
    }
    cmd <- "ps aux | grep -v grep | grep dockerd"
    suppressWarnings (
        out <- tryCatch (
            system (cmd, intern = TRUE, ignore.stderr = TRUE),
            error = function (e) NULL
        )
    )
    chk <- any (grepl ("root\\s", out))
    if (chk) {
        # Retain 'TRUE' only if current user in not in "docker" group:
        cmd <- "grep /etc/group -e 'docker'"
        suppressWarnings (
            out <- tryCatch (
                system (cmd, intern = TRUE, ignore.stderr = TRUE),
                error = function (e) NULL
            )
        )
        if (!is.null (out)) {
            user_docker <- gsub ("^.*\\:", "", out)
            suppressWarnings (
                user_current <- tryCatch (
                    system ("echo $USER", intern = TRUE, ignore.stderr = TRUE),
                    error = function (e) NULL
                )
            )
            if (user_current == user_docker) {
                chk <- FALSE
            }
        }
    }
    return (chk)
}

has_ollama <- function (sudo = is_docker_sudo ()) {
    has_ollama_local () || has_ollama_docker (sudo = sudo)
}

has_ollama_local <- function () {
    lib_name <- "ollama"
    cmd <- ifelse (is_windows (), "where", "which")
    result <- system (
        paste (cmd, lib_name),
        ignore.stdout = TRUE,
        ignore.stderr = TRUE
    )

    return (result == 0)
}

has_ollama_docker <- function (sudo = is_docker_sudo ()) {
    cmd <- "docker ps -a"
    if (sudo) {
        cmd <- paste ("sudo", cmd)
    }
    suppressWarnings (
        out <- tryCatch (
            system (cmd, intern = TRUE, ignore.stderr = TRUE),
            error = function (e) NULL
        )
    )
    chk <- !is.null (out)
    if (chk) {
        chk <- any (grepl ("ollama-models", out))
    }
    return (chk)
}

ollama_models <- function () {
    stopifnot (ollama_is_running ())

    out <- system ("ollama list", intern = TRUE)
    out <- lapply (out, function (i) {
        line <- strsplit (i, "\\t") [[1]]
        index <- which (!grepl ("days", line))
        line [index] <- gsub ("[[:space:]]*", "", line [index])
        return (line)
    })
    nms <- tolower (out [[1]])
    out <- data.frame (do.call (rbind, out [-1]))
    names (out) <- nms

    v <- regmatches (out$name, regexpr ("\\:.*$", out$name))
    out$version <- gsub ("^\\:", "", v)
    out$name <- gsub ("\\:.*$", "", out$name)

    return (out)
}

jina_required_models <- c ("base", "code")

jina_model <- function (what = "base") {
    what <- match.arg (what, jina_required_models)
    switch (what,
        "base" = "jina/jina-embeddings-v2-base-en",
        "code" = "ordis/jina-embeddings-v2-base-code",
    )
}
ollama_has_jina_model <- function (what = "base") {
    stopifnot (ollama_is_running ())
    what <- match.arg (what, jina_required_models)
    jina_model (what) %in% ollama_models ()$name
}

ollama_dl_jina_model <- function (what = "base") {
    stopifnot (ollama_is_running ())
    what <- match.arg (what, jina_required_models)
    if (ollama_has_jina_model (what)) {
        return (TRUE)
    }
    out <- system (paste ("ollama pull", jina_model (what), intern = FALSE))
    return (out == 0)
}

ollama_is_running <- function () {
    suppressWarnings (
        chk <- system ("ollama ps", ignore.stdout = TRUE, ignore.stderr = TRUE)
    )
    chk <- (chk != 127L)
    if (!chk) {
        res <- tryCatch (
            curl::curl ("127.0.0.1:11434"),
            error = function (e) NULL
        )
        if (!is.null (res)) {
            suppressWarnings (
                res <- readLines (res)
            )
            chk <- grepl ("Ollama is running", res, fixed = TRUE)
        }
    }
    return (chk)
}

#' Check that ollama is installed with required models, and download if not.
#'
#' @param sudo Set to `TRUE` if ollama is running in docker with sudo
#' privileges.
#' @return TRUE if everything works okay, otherwise the function will error
#' before returning.
#'
#' @examples
#' \dontrun{
#' chk <- ollama_check ()
#' }
#'
#' @family ollama
#' @export
ollama_check <- function (sudo = is_docker_sudo ()) {
    if (identical (Sys.getenv ("PKGMATCH_TESTS"), "true")) {
        return (TRUE)
    }
    if (!has_ollama (sudo = sudo)) {
        cli::cli_abort (paste0 (
            "ollama is not installed. Please follow ",
            "installation instructions at https://ollama.com."
        ))
    }
    if (!ollama_is_running ()) {
        cli::cli_abort (paste0 (
            "ollama is installed but not running. Please run `ollama serve` ",
            "from a separate console (not from within R)."
        ))
    }

    if (has_ollama_local ()) {
        for (mod in jina_required_models) {
            if (!ollama_has_jina_model (mod)) {
                cli::cli_warn (paste0 (
                    "ollama model [",
                    jina_model (mod),
                    "] is not installed."
                ))
                yn <- readline ("Would you like to download it now (y/n) ? ")
                if (substring (tolower (yn), 1, 1) == "y") {
                    mod_name <- jina_model (mod) # nolint
                    cli::cli_inform ("Okay, downloading [{mod_name}] ...")
                    res <- ollama_dl_jina_model (mod)
                    if (res != 0) {
                        cli::cli_abort (paste0 (
                            "ollama model failed to download. ",
                            "Maybe use 'ollama pull' directly?"
                        ))
                    }
                }
            }
        }
    }

    return (TRUE)
}
