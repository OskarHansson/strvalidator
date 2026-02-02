#' Display a startup message when the package is attached
#'
#' @keywords internal
#'
#' @description
#' The `.onAttach()` function is called automatically when the package is
#' attached to the search path using `library(strvalidator)` or
#' `require(strvalidator)`.
#'
#' It displays a short startup message showing the package name and version.
#' The message is generated using [base::packageStartupMessage()], which allows
#' users to suppress it via [base::suppressPackageStartupMessages()].
#'
#' This function does not modify package options or state and has no side
#' effects beyond printing the startup message.
#'
#' @param libname Character string giving the path to the package library.
#' @param pkgname Character string giving the name of the package.
#'
#' @seealso
#' [base::.onAttach()],
#' [base::packageStartupMessage()],
#' [base::suppressPackageStartupMessages()]
#'
#' @return Called for its side effects; returns `invisible()`.
#'
#' @noRd
.onAttach <- function(libname, pkgname) {
  # Display a startup message when the package is attached.
  packageStartupMessage(
    paste("STR-validator", utils::packageVersion("strvalidator"), "loaded.")
  )
}

#' Initialize package options when the namespace is loaded
#'
#' @keywords internal
#'
#' @description
#' The .onLoad function is called automatically when the package namespace
#' is loaded (but before it is attached).
#'
#' This function initializes the package-level options used by **strvalidator**.
#' In particular, it ensures that the option `strvalidator.language` exists and
#' is set to a default value (`"en"`) if not already defined by the user.
#'
#' Users can control the package language persistently via:
#'
#' ```r
#' options(strvalidator.language = "sv")
#' ```
#'
#' or temporarily within a session. To get the current setting:
#'
#' ```r
#' getOption("strvalidator.language")
#' ```
#'
#' The function does not produce visible output or side effects beyond
#' initializing these options.
#'
#' @param libname Character string giving the path to the package library.
#' @param pkgname Character string giving the name of the package.
#'
#' @seealso [base::.onLoad()], [base::options()]
#'
#' @return Called for its side effects; returns `invisible()`.
#' @noRd
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.strvalidator <- list(
    strvalidator.language = "en"  # default language
  )
  toset <- !(names(op.strvalidator) %in% names(op))
  if (any(toset)) options(op.strvalidator[toset])
  invisible()
}