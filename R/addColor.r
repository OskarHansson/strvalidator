################################################################################
# TODO LIST
#
# CHANGE LOG (last 20 changes)
# 22.09.2025: Refactored and support for 8 dyes.
# 24.08.2018: Removed unused variables.
# 06.08.2017: Added audit trail.
# 06.08.2017: Fixed warning "if (!is.na(need)):the condition has length > 1".
# 18.09.2016: Fixed attribute saved dataset, and kit now saved correctly.
# 09.01.2016: Added attributes to result.
# 28.08.2015: Added importFrom
# 17.12.2014: Fixed error NA Dye for e.g. Yfiler Plus (added 'toupper' in 'match' calls).
# 11.05.2014: Added 'orange' and 'purple'.
# 27.04.2014: Added option to ignore case in marker names.
# 15.12.2013: Fixed check for 'have' and 'need' when converting vector.
# 27.11.2013: Added option 'overwrite'.
# 04.10.2013: Added some debug information.
# 18.09.2013: Added support for vector conversion.
# 17.09.2013: First version.
#
#' @title Add Color Information.
#'
#' @description
#' Add color information 'Color', 'Dye' or 'R Color' to either a vector
#' or a data frame.  The function supports up to eight fluorescent dye
#' channels corresponding to modern STR kits and will gracefully handle
#' older kits with fewer dyes.
#'
#' @details
#' Primers in forensic STR typing kits are labelled with fluorescent dyes.
#' The dyes are represented with single letters (Dye) in exported result files,
#' with strings (Color) in 'panels' files and with R colour names for plotting
#' (R.Color).  This function can add missing colour columns to a data frame
#' based on another colour column, convert between schemes, or derive colours
#' from a kit definition via `getKit()`.  Conversions are case insensitive and
#' missing or unrecognised values will be mapped to NA.
#'
#' @param data data frame or vector.
#' @param kit string representing the forensic STR kit used.  Default is NA,
#'   in which case 'have' must contain a valid column name.
#' @param have character string specifying which colour scheme is present
#'   in the data.  Acceptable values are 'Color', 'Dye' or 'R.Color' (case
#'   insensitive).  If data is a vector 'have' must be supplied.
#' @param need character string or vector specifying which colour schemes
#'   should be added.  Acceptable values are 'Color', 'Dye' and 'R.Color'.
#'   Default NA means that all missing colour columns will be added.
#'   If data is a vector 'need' must be a single value.
#' @param overwrite logical.  If TRUE and the column to be created already
#'   exists in the data frame it will be removed before adding new values.
#' @param ignore.case logical.  If TRUE marker names will be matched ignoring
#'   case when deriving colours from a kit.
#' @param debug logical indicating whether to print debug information.
#'
#' @return For data frames, returns a data frame with additional columns
#'   corresponding to the requested colour schemes.  For vectors, returns a
#'   vector converted to the requested scheme.
#'
#' @export
#'
#' @importFrom utils str
addColor <- function(data, kit = NA, have = NA, need = NA,
                     overwrite = FALSE, ignore.case = FALSE, debug = FALSE) {
  # Debug print of the call.
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }
  
  ## ---------------------------------------------------------------------------
  ## Lookup table describing the mapping between colour names (Color),
  ## dye letters (Dye) and the colour names used by R for plotting (R.Color).
  ## The order of rows corresponds to the order along the fluorescent spectrum
  ## with the internal lane standard first (black/X) followed by blue, green,
  ## cyan, yellow, red, orange and purple.  Kits with fewer dyes will not
  ## utilise all rows but the ordering remains consistent to allow proper
  ## sorting of markers.  To add support for additional dye channels, extend
  ## this table.
  ## ---------------------------------------------------------------------------
  map <- data.frame(
    Color   = c("black", "blue", "green", "cyan", "yellow",
                "red", "orange", "purple"),
    Dye     = c("X",     "B",    "G",    "C",    "Y",
                "R",     "O",    "P"),
    R.Color = c("black", "blue", "green3", "cyan", "yellow",
                "red", "orange", "purple"),
    stringsAsFactors = FALSE
  )
  # Valid scheme names (upper case) for convenience.
  colorSchemes <- toupper(colnames(map))
  
  # Helper: convert a vector from one scheme to another using the map.
  convertVector <- function(x, fromScheme, toScheme) {
    fromVals <- map[[fromScheme]]
    toVals   <- map[[toScheme]]
    idx <- match(toupper(x), toupper(fromVals))
    return(toVals[idx])
  }
  
  # When overwrite is requested remove existing colour columns from a data frame.
  if (overwrite && is.data.frame(data)) {
    nms <- toupper(names(data))
    if ("R.COLOR" %in% nms) {
      message("Column 'R.Color' will be overwritten!")
      data$R.Color <- NULL
    }
    if ("COLOR" %in% nms) {
      message("Column 'Color' will be overwritten!")
      data$Color <- NULL
    }
    if ("DYE" %in% nms) {
      message("Column 'Dye' will be overwritten!")
      data$Dye <- NULL
    }
  }
  
  # Vector conversion mode.  For factors is.vector returns FALSE but dim is NULL.
  if (is.vector(data) || is.null(dim(data))) {
    if (debug) {
      print("data is vector OR dim is NULL")
    }
    # Validate arguments.
    if (any(is.na(have)) || any(is.na(need))) {
      warning("For vector conversion 'have' and 'need' must be provided!")
    } else {
      fromScheme <- toupper(have[1])
      toScheme   <- toupper(need[1])
      if (!(fromScheme %in% colorSchemes)) {
        warning(sprintf("'have' scheme '%s' is not supported!", fromScheme))
      } else if (!(toScheme %in% colorSchemes)) {
        warning(sprintf("'need' scheme '%s' is not supported!", toScheme))
      } else {
        data <- convertVector(data, fromScheme, toScheme)
      }
    }
  } else if (is.data.frame(data)) {
    if (debug) {
      print("data is data.frame")
    }
    # If 'kit' is supplied and no 'have' scheme is specified, derive colours
    # from the kit definition and populate a new 'Color' column.  Colour
    # assignment is based on marker names.  Case sensitivity can be controlled
    # via ignore.case.
    if (is.na(have) && !is.na(kit)) {
      # Only add 'Color' if it doesn't already exist.
      if (!"COLOR" %in% toupper(names(data))) {
        kitInfo <- getKit(kit, what = "Color")
        marker  <- kitInfo$Marker
        # Colour strings provided by getKit may vary in case; normalise to lower.
        mColor  <- tolower(kitInfo$Color)
        if (debug) {
          print("marker"); print(str(marker))
          print("mColor"); print(str(mColor))
        }
        if (ignore.case) {
          # Assign colours ignoring case.
          for (m in seq_along(marker)) {
            data$Color[toupper(data$Marker) == toupper(marker[m])] <- mColor[m]
          }
        } else {
          for (m in seq_along(marker)) {
            data$Color[data$Marker == marker[m]] <- mColor[m]
          }
        }
      }
      # Use 'Color' as the scheme we have for further conversions.
      have <- "Color"
    }
    
    # Determine which scheme(s) we have.
    if (is.na(have)) {
      existing <- toupper(names(data))
      have <- existing[existing %in% colorSchemes]
    } else {
      have <- toupper(have)
    }
    
    # Determine which scheme(s) we need to add.
    if (!is.na(need[1])) {
      need <- toupper(need)
    } else {
      need <- colorSchemes
    }
    # Warn if any requested scheme is not supported.
    if (!any(need %in% colorSchemes)) {
      warning(paste(paste(need, collapse = ","), "not supported!"))
    }
    
    # Sequentially add missing schemes.
    for (scheme in need) {
      # Skip if scheme already exists.
      if (scheme %in% toupper(names(data))) {
        message(sprintf("A column '%s' already exists in data frame!",
                        switch(scheme,
                               "COLOR"   = "Color",
                               "DYE"     = "Dye",
                               "R.COLOR" = "R.Color")))
        next
      }
      # Determine which existing scheme can be used for conversion.
      if (length(have) == 0) {
        warning(sprintf("No existing colour scheme available to derive '%s'!", scheme))
        next
      }
      # Prefer the first available scheme.
      fromScheme <- have[1]
      # Prepare names to match R's column names (case sensitive):
      colName <- switch(scheme,
                        "COLOR"   = "Color",
                        "DYE"     = "Dye",
                        "R.COLOR" = "R.Color")
      fromCol <- switch(fromScheme,
                        "COLOR"   = "Color",
                        "DYE"     = "Dye",
                        "R.COLOR" = "R.Color")
      # Perform conversion.
      if (fromCol %in% names(data)) {
        data[[colName]] <- convertVector(data[[fromCol]], fromScheme, scheme)
      } else {
        warning(sprintf("Can't find column '%s'!\n'%s' was not added!", fromCol, colName))
      }
      # Update 'have' to include the newly created scheme for subsequent conversions.
      have <- unique(c(have, scheme))
    }
  } else {
    warning("Unsupported data type!\n No colour was added!")
  }
  
  # Attach the kit attribute to the result for downstream functions.
  attr(data, which = "kit") <- kit
  
  # Update audit trail if available.
  data <- auditTrail(obj = data, f.call = match.call(), package = "strvalidator")
  
  if (debug) {
    print("Return"); print(str(data))
  }
  
  return(data)
}
