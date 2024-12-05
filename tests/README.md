# Testing this package

Note that all tests here are mocked, and the entire test suite can be run
without even installing `ollama`, which is otherwise required to actually use
the package itself.

Tests can be run using any common testing functions, such as:

- `devtools::testl()`
- `testthat::test_local()`
- `covr::package_coverage()`

Note that test coverage reports currently [exclude the following
functions](https://github.com/ropensci-review-tools/pkgmatch/blob/52eaf4e841f627c315619e73300cb8cd175af929/.github/workflows/test-coverage.yaml#L41-L440o):

- [`R/ollama.R`](https://github.com/ropensci-review-tools/pkgmatch/blob/main/R/ollama.R),
    which contains functions to ascertain status of locally-running instance of
    [`ollama`](https://ollama.com).
- [`R/browse.R`](https://github.com/ropensci-review-tools/pkgmatch/blob/main/R/browse.R)
    which contains a single function used to open URLs of results from this package
    in a local browser.

Running `covr::package_coverage()` locally without excluding these two files
will thus yield lower package coverage than the values reported from
[`codecov`](https://app.codecov.io/gh/ropensci-review-tools/pkgmatch), yet
still over 75%.
