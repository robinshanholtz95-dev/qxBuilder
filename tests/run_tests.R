## Lightweight, dependency-free test runner ------------------------------
## No testthat dependency (kept this package's install footprint small);
## just a handful of tiny assertion helpers and a set of test files that
## exercise every function NOT requiring a live network call to
## Qualtrics (i.e. everything except qx_api_request() and the functions
## built on it, which are instead exercised with mocks in
## test-run-template-integration.R).
##
## Works two ways:
##  - Under `R CMD check`, the package is already installed into a temp
##    library on the search path, so library(qxBuilder) finds it.
##  - For local dev (Rscript tests/run_tests.R from the package source,
##    without installing), falls back to pkgload::load_all() on the
##    package root, located by walking up from the working directory
##    until a DESCRIPTION file turns up.

failures <- 0L
checks <- 0L

expect_true <- function(x, label) {
  checks <<- checks + 1L
  if (!isTRUE(x)) {
    failures <<- failures + 1L
    cat("FAIL:", label, "\n")
  }
}

expect_equal <- function(actual, expected, label) {
  checks <<- checks + 1L
  if (!isTRUE(all.equal(actual, expected))) {
    failures <<- failures + 1L
    cat("FAIL:", label, "\n")
    cat("  expected:", paste(utils::capture.output(print(expected)), collapse = " "), "\n")
    cat("  actual  :", paste(utils::capture.output(print(actual)), collapse = " "), "\n")
  }
}

find_pkg_root <- function(start = getwd()) {
  d <- normalizePath(start)
  repeat {
    if (file.exists(file.path(d, "DESCRIPTION"))) return(d)
    parent <- dirname(d)
    if (parent == d) stop("Could not find a package root (DESCRIPTION file) above ", start, call. = FALSE)
    d <- parent
  }
}

loaded <- tryCatch({ library("qxBuilder", character.only = TRUE); TRUE }, error = function(e) FALSE)

if (isTRUE(loaded)) {
  # Under R CMD check the package is already installed (found via
  # library() on the temp check library), and this script is run with
  # its working directory set to the tests/ directory itself -- so the
  # case files are right here, under cases/.
  tests_dir <- getwd()
} else {
  # Local dev: package isn't installed, so locate the source root (by
  # walking up from cwd until a DESCRIPTION turns up) and load it
  # in-place instead.
  pkg_root <- find_pkg_root()
  pkgload::load_all(pkg_root, quiet = TRUE)
  tests_dir <- file.path(pkg_root, "tests")
}

# Individual test files live in tests/cases/ rather than directly in
# tests/ -- R CMD check runs every .R file directly under tests/ as
# its own independent script (with no package loaded), so putting them
# one level down keeps this file as the only thing check auto-runs.
test_files <- sort(list.files(file.path(tests_dir, "cases"), pattern = "^test-.*\\.R$", full.names = TRUE))
for (f in test_files) {
  cat("--- Running", basename(f), "---\n")
  source(f, local = TRUE)
}

cat("\n", checks, "checks run,", failures, "failed.\n")
if (failures > 0L) quit(status = 1L)
