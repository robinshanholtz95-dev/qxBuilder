## Internal package state -----------------------------------------------
## Holds the API key/base URL for the current session so every other
## function doesn't need them passed explicitly. Set once with
## qx_set_credentials().

.qx_env <- new.env(parent = emptyenv())

#' Set Qualtrics API credentials for this R session
#'
#' Thin wrapper that stores the API key and base URL (e.g.
#' \code{"yul1.qualtrics.com"}) for use by every other \code{qx_*}
#' function in this package, and -- optionally -- also registers them
#' with the \pkg{qualtRics} package via
#' \code{qualtRics::qualtrics_api_credentials()} if that package is
#' installed, so functions from both packages share one set of
#' credentials.
#'
#' @param api_key Character. Your Qualtrics API token.
#' @param base_url Character. Your Qualtrics data center host, e.g.
#'   \code{"yul1.qualtrics.com"} (no \code{https://} prefix, no path).
#' @param also_set_qualtrics_pkg Logical. If \code{TRUE} (the default)
#'   and the \pkg{qualtRics} package is installed, also call
#'   \code{qualtRics::qualtrics_api_credentials()} with these values.
#' @param install Logical, passed through to
#'   \code{qualtRics::qualtrics_api_credentials()} when applicable.
#'
#' @return Invisibly, \code{TRUE}.
#' @export
#'
#' @examples
#' \dontrun{
#' qx_set_credentials("YOUR_API_KEY", "yul1.qualtrics.com")
#' }
qx_set_credentials <- function(api_key,
                                base_url,
                                also_set_qualtrics_pkg = TRUE,
                                install = TRUE) {
  stopifnot(is.character(api_key), nchar(api_key) > 0)
  stopifnot(is.character(base_url), nchar(base_url) > 0)

  base_url <- sub("^https?://", "", base_url)
  base_url <- sub("/+$", "", base_url)

  assign("api_key", api_key, envir = .qx_env)
  assign("base_url", base_url, envir = .qx_env)

  if (also_set_qualtrics_pkg && requireNamespace("qualtRics", quietly = TRUE)) {
    qualtRics::qualtrics_api_credentials(
      api_key   = api_key,
      base_url  = base_url,
      install   = install,
      overwrite = TRUE
    )
  }

  invisible(TRUE)
}

#' @keywords internal
qx_credentials <- function() {
  if (!exists("api_key", envir = .qx_env) || !exists("base_url", envir = .qx_env)) {
    stop(
      "Qualtrics credentials are not set. Call qx_set_credentials(api_key, base_url) first.",
      call. = FALSE
    )
  }
  list(
    api_key  = get("api_key", envir = .qx_env),
    base_url = get("base_url", envir = .qx_env)
  )
}
