test_that ("ollama endpoints", {

    expect_equal (get_ollama_url (), "127.0.0.1:11434")
    expect_silent (check_ollama_url (get_ollama_url ()))

    old_url <- get_ollama_url ()
    new_url <- "httpserror"
    expect_snapshot (
        error = TRUE,
        set_ollama_url (new_url)
    )
    expect_equal (get_ollama_url (), old_url)

    new_url <- "a" # valid URL
    expect_equal (set_ollama_url (new_url), new_url)
    expect_equal (get_ollama_url (), new_url)

    options ("pkgmatch.ollama.url" = NULL)
    expect_error (
        get_ollama_url (),
        "ollama URL can not be retrieved"
    )
})
