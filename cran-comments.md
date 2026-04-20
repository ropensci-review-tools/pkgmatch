# CRAN notes for pkgmatch_0.5.4 submission

This is a re-submission of an initially rejected first submission. As requested, the following updates have been implemented:

- Single quotes in description now only used for names of software or R packages (leading to NOTE on potential mis-spelling described below).
- Reference in DESCRIPTION with doi now includes author names and year
- All examples now run, except for:
  - two single-line parts of examples which remain wrapped in `dontrun{}`. These both trigger calls to download large data sets from external services.
  - A "browse" function which uses `utils::browseURL` which fails if run or wrapped in `donttest`
- All uses of `installed.packages()` removed and replaced with `find.package()`.

## Test environments

The package has been checked on all environments listed below, and generates two notes:

- identifying the package as a new submission.
- Potentially mis-spelled "rOpenSci", but that is correct, and has been removed from single quotes as requested.

GitHub actions:
* Linux: R-release, R-devel
* OSX: R-release
* Windows: R-release

CRAN win-builder:
* R-oldrelease, R-release, R-devel
