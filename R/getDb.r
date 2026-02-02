################################################################################
# CHANGE LOG (last 20 changes)
# 10.11.2025: Added fallback for locating the kit definition file (for testing).
# 29.08.2015: Added importFrom.
# 15.12.2014: Changed parameter names to format: lower.case
# 30.09.2014: Check if package is loaded to avoid error in path.package.
# 01.10.2013: First version.

#' @title Get Allele Frequency Database
#'
#' @description
#' Gives access to allele frequency databases.
#'
#' @details
#' The function provides access to allele frequency databases stored in
#' the file database.txt in the package directory.
#' It returns the specified allele frequency database.
#'
#' @param db.name.or.index string or integer specifying the database.
#' If NULL a vector of available databases is returned.
#' @param debug logical indicating printing debug information.
#'
#' @return data.frame with allele frequency database information.
#' If no matching database or database index is found NA is returned.
#' If the database was not found NULL is returned.
#'
#' @keywords internal
#'
#' @export
#'
#' @importFrom utils read.delim
#' @importFrom rprojroot find_root has_file
#'
#' @examples
#' # Show available allele frequency databases.
#' getDb()
getDb <- function(db.name.or.index = NULL, debug = FALSE) {
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  .separator <- .Platform$file.sep # Platform dependent path separator.

  # LOAD DATABASE INFO  #######################################################

  # Get package path (works after installation)
  packagePath <- path.package("strvalidator", quiet = FALSE)
  subFolder <- "extdata"
  fileName <- "database.txt"
  filePath <- paste(packagePath, subFolder, fileName, sep = .separator)

  # Fallback: when running tests from source tree, system.file() may fail
  if (!file.exists(filePath)) {
    # Try to locate inst/extdata in the current or parent directories
    sourcePath <- normalizePath(file.path(getwd(), "inst", "extdata", fileName), mustWork = FALSE)
    if (file.exists(sourcePath)) {
      filePath <- sourcePath
    } else {
      # Last resort: find the project root from the package DESCRIPTION
      descPath <- tryCatch(
        suppressWarnings(rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))),
        error = function(e) NA
      )
      if (!is.na(descPath)) {
        altPath <- file.path(descPath, "inst", "extdata", fileName)
        if (file.exists(altPath)) filePath <- altPath
      }
    }
  }
  
  if (!file.exists(filePath)) {
    stop(sprintf("Cannot locate '%s' in package or source directory.", fileName))
  }

  .db <- read.delim(
    file = filePath, header = TRUE, sep = "\t", quote = "\"",
    dec = ".", fill = TRUE, stringsAsFactors = FALSE
  )

  # Available databases. Must match else if construct.
  databases <- unique(.db$Database)

  # Check if NULL
  if (is.null(db.name.or.index)) {
    db <- databases

    # String provided.
  } else {
    # Check if number or string.
    if (is.numeric(db.name.or.index)) {
      # Set index to number.
      index <- db.name.or.index
    } else {
      # Find matching database index (case insensitive)
      index <- match(toupper(db.name.or.index), toupper(databases))
    }

    # No matching database.
    if (is.na(index)) {
      db <- NA

      # Assign matching database information.
    } else {
      db <- .db[.db$Database == databases[index], ]
    }
  }

  return(db)
}
