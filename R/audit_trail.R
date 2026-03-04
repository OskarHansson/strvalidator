# ---------------------------------------------------------------------------
#' @title Audit Trail Logging
#'
#' @description
#' Adds or updates an audit trail attribute on an R object.
#'
#' @details
#' An audit trail entry is appended each time the function is called,
#' logging timestamp, function label, optional function arguments,
#' package versions, and custom key-value pairs.
#'
#' The audit trail is stored in the object attribute \code{"audit trail"}.
#'
#' @param obj Object to modify.
#' @param f_call The function call (typically from \code{match.call()}).
#' @param key Character vector of additional metadata keys.
#' @param value Character vector of values corresponding to \code{key}.
#' @param label Optional label. If omitted, the function name is extracted
#'   from \code{f_call}.
#' @param arguments Logical. If TRUE, the function's formal arguments
#'   are logged.
#' @param exact Logical, passed to \code{attr(exact=)}.
#' @param remove Logical. If TRUE, the audit trail attribute is removed.
#' @param package Character string of a package name whose version will
#'   be logged.
#' @param r_version Logical. If TRUE, adds the current R version.
#' @param timestamp Logical. If TRUE, adds a timestamp attribute.
#'
#' @return
#' The modified object with updated audit trail attributes.
#'
#' @seealso
#' \\code{\\link{auditTrail}} - deprecated wrapper for backwards compatibility.
#'
#' @export
#' 
#' @importFrom utils capture.output
# ---------------------------------------------------------------------------

audit_trail <- function(
    obj,
    f_call    = NULL,
    key       = NULL,
    value     = NULL,
    label     = NULL,
    arguments = TRUE,
    exact     = TRUE,
    remove    = FALSE,
    package   = NULL,
    r_version = TRUE,
    timestamp = TRUE
) {
  
  # Validate key/value lengths
  if (length(key) != length(value)) {
    stop("Arguments 'key' and 'value' must have equal length.", call. = FALSE)
  }
  
  attribute_name <- "audit trail"
  
  # -------------------------------------------------------------------------
  # REMOVE MODE
  # -------------------------------------------------------------------------
  if (remove) {
    attr(obj, attribute_name) <- NULL
    return(obj)
  }
  
  # -------------------------------------------------------------------------
  # FUNCTION LABEL
  # -------------------------------------------------------------------------
  if (!is.null(f_call)) {
    call_list <- as.list(f_call)
    label <- as.character(call_list)[1]
  }
  
  # -------------------------------------------------------------------------
  # PREFIX (timestamp + label)
  # -------------------------------------------------------------------------
  now <- Sys.time()
  prefix <- paste0(format(now), ", ", label, ", ")
  
  # Existing audit trail
  previous <- attr(obj, attribute_name, exact = exact)
  log_entries <- character(0)
  
  if (is.null(previous)) {
    log_entries <- c(log_entries, paste0(prefix, "audit trail created."))
  }
  
  # -------------------------------------------------------------------------
  # R version
  # -------------------------------------------------------------------------
  if (r_version) {
    log_entries <- c(log_entries, paste0(prefix, R.version.string))
  }
  
  # -------------------------------------------------------------------------
  # Package version
  # -------------------------------------------------------------------------
  if (!is.null(package)) {
    pkg_version <- as.character(utils::packageVersion(package))
    log_entries <- c(log_entries, paste0(prefix, package, "=", pkg_version))
  }
  
  # -------------------------------------------------------------------------
  # FUNCTION ARGUMENT LOGGING
  # -------------------------------------------------------------------------
  if (arguments && !is.null(f_call)) {
    
    fname <- as.character(as.list(f_call))[1]
    fname <- sub(".*:", "", fname)  # strip pkg:: prefix
    
    fn <- tryCatch(match.fun(fname), error = function(e) NULL)
    
    if (!is.null(fn)) {
      arg_info <- capture.output(args(fn))
      arg_info <- paste(arg_info, collapse = " ")
      arg_info <- gsub("\\s+", " ", arg_info)
      log_entries <- c(log_entries, paste0(prefix, "arguments=", arg_info))
    } else {
      warning("audit_trail could not find function '", fname, "'", call. = FALSE)
    }
  }
  
  # -------------------------------------------------------------------------
  # FUNCTION CALL LOGGING
  # -------------------------------------------------------------------------
  if (!is.null(f_call)) {
    call_str <- paste(deparse(f_call), collapse = "")
    log_entries <- c(log_entries, paste0(prefix, "call=", call_str))
  }
  
  # -------------------------------------------------------------------------
  # CUSTOM KEY-VALUE ENTRIES
  # -------------------------------------------------------------------------
  if (length(key) > 0) {
    for (i in seq_along(key)) {
      entry <- paste0(prefix,
                      paste0(key[[i]], collapse = ","),
                      "=",
                      paste0(value[[i]], collapse = ","))
      log_entries <- c(log_entries, entry)
    }
  }
  
  # -------------------------------------------------------------------------
  # UPDATE ATTRIBUTE
  # -------------------------------------------------------------------------
  new_log <- paste(c(previous, log_entries), collapse = "\n")
  attr(obj, attribute_name) <- new_log
  
  # Timestamp attribute
  if (timestamp) {
    attr(obj, "timestamp") <- format(now)
  }
  
  obj
}

