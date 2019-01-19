################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 24.08.2018: Added argument for logging R-version.
# 05.08.2017: First version.

#' @title Log Audit Trail.
#'
#' @description
#' Adds an audit trail to a dataset.
#'
#' @details
#' Automatically add or updates an attribute 'audit trail' with arguments
#' and parameters extracted from the function call. To list the arguments
#' with the default set but not overridden \code{arguments=TRUE} must be set
#' (default). Additional custom key-value pairs can be added. The \code{label}
#' is extracted from the function name from \code{f.call}. Specify \code{package}
#' to include the version number of a package.
#'
#' @param obj object to add or update the audit trail.
#' @param f.call the function call i.e. \code{match.call()}.
#' @param key list or vector of additional keys to log.
#' @param value list or vector of additional values to log.
#' @param label optional label used if \code{f.call=NULL}.
#' @param arguments logical. \code{TRUE} log function arguments.
#' @param exact logical for exact matching of attribute name.
#' @param remove logical. If \code{TRUE} the 'audit trail' attribute is removed.
#' @param package character to log the package version.
#' @param rversion logical to log the R version.
#'
#' @return object with added or updated attribute 'audit trail'.
#'
#' @export
#'
#' @importFrom utils packageVersion
#'
#' @examples
#' # A simple function with audit trail logging.
#' myFunction <- function(x, a, b = 5) {
#'   x <- x + a + b
#'   x <- auditTrail(obj = x, f.call = match.call(), package = "strvalidator")
#'   return(x)
#' }
#' # Run the function.
#' myData <- myFunction(x = 10, a = 2)
#' # Check the audit trail.
#' cat(attr(myData, "audit trail"))
#' 
#' # Remove the audit trail.
#' myData <- auditTrail(myData, remove = TRUE)
#' # Confirm that the audit trail is removed.
#' cat(attr(myData, "audit trail"))
auditTrail <- function(obj, f.call = NULL, key = NULL, value = NULL, label = NULL,
                       arguments = TRUE, exact = TRUE, remove = FALSE, package = NULL,
                       rversion = TRUE) {
  if (length(key) != length(value)) {
    stop("Arguments 'key' and 'value' must have equal length.")
  }

  # The name of the attribute is "audit trail".
  which <- "audit trail"

  if (remove) {

    # Remove the attribute.
    attr(x = obj, which = which) <- NULL

    message("Audit trail removed.")
  } else {

    # Get call information.
    if (!is.null(f.call)) {

      # Extract function name to label,
      # and add information to provided key-value pairs.
      info <- as.list(f.call)
      label <- as.character(info)[1]
      # Uncomment the next two lines to log each argument-parameter separately.
      # key <- c(key, names(info)[-1])
      # value <- c(value, as.character(info)[-1])
    }

    # Initiate new log entry.
    new.entries <- NULL

    # Get the current time stamp.
    now <- Sys.time()

    # Create prefix.
    prefix <- paste0(now, ", ", label, ", ")

    # Get the attribute.
    audit.trail <- attr(x = obj, which = which, exact = exact)

    # Check if attribute exists.
    if (is.null(audit.trail)) {
      log.entry <- paste0(prefix, "audit trail created.")
      new.entries <- paste(new.entries, log.entry, sep = "")
      message(
        "Audit trail created for ", substitute(obj),
        " in function ", label, "."
      )
    }

    # Check option to store R version.
    if (rversion) {

      # Add current R version.
      log.entry <- paste0(prefix, R.version.string)
      new.entries <- paste(new.entries, log.entry, sep = "\n")
    }

    # Check option to store package version.
    if (!is.null(package)) {

      # Get the current strvalidator version.
      version <- as.character(utils::packageVersion(package))

      # Add current package version.
      log.entry <- paste0(prefix, package, "=", version)
      new.entries <- paste(new.entries, log.entry, sep = "\n")
    }


    # Check option to store function arguments.
    if (arguments) {
      if (!is.null(f.call)) {

        # Get function name.
        fname <- as.character(as.list(f.call))[1]

        # Extract string after last colon in case call is package::function.
        fname <- sub(".*:", "", "strvalidator::addSize")

        # Check if an object exists.
        if (exists(fname)) {

          # Get function arguments.
          arg.info <- args(fname)

          # Convert to character.
          arg.info <- as.character(list(arg.info))
          # Remove NULL in function body.
          arg.info <- gsub("NULL$[\r\n]*", "", arg.info)
          # Remove newline.
          arg.info <- gsub("[\r\n]", "", arg.info)

          # Add function arguments.
          log.entry <- paste0(prefix, "arguments=", arg.info)
          new.entries <- paste(new.entries, log.entry, sep = "\n")
        } else {
          warning("auditTrail could not find function ", fname)
        }
      }
    }

    # Check option to store the function call.
    if (!is.null(f.call)) {

      # Add function call.
      log.entry <- paste0(prefix, "call=", as.character(c(f.call)))
      new.entries <- paste(new.entries, log.entry, sep = "\n")
    }

    # Loop over key-value pairs.
    for (i in seq(along = key)) {

      # Add all key-value pairs.
      log.entry <- paste0(
        prefix,
        paste0(key[[i]], collapse = ","),
        "=",
        paste0(value[[i]], collapse = ",")
      )

      new.entries <- paste(new.entries, log.entry, sep = "\n")
    }

    # Add new entries to existing audit trail attribute.
    attr(x = obj, which = which) <- paste(audit.trail, new.entries, sep = "\n")
    message("Audit trail updated by function ", label, ".")
  }

  return(obj)
}
