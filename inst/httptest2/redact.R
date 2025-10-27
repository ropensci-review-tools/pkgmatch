function (resp) {

    resp <- httptest2::gsub_response (
        resp,
        "https://search.r-project.org/CRAN/refmans/testthat/html",
        "search.r-project/",
        fixed = TRUE
    )

    return (resp)
}
