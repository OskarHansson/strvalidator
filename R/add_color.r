#' @title Add Color Information.
#'
#' @description
#' Add color information ('Color', 'Dye' or 'R.Color') to either a vector
#' or a data frame. The function includes mappings for common multi-colour STR
#' kits (up to eight dye channels in current definitions) and will gracefully
#' handle older kits with fewer dyes.
#'
#' @details
#' Primers in forensic STR typing kits are labelled with fluorescent dyes.
#' The dyes are represented with single letters (`Dye`) in exported result files,
#' with strings (`Color`) in panel or kit definition files, and with R color
#' names (`R.Color`) for plotting.
#'
#' This function can add missing color columns to a data frame based on another
#' color column, convert between schemes, or derive colors from a kit
#' definition via `get_kit()`. Conversions are case insensitive and missing or
#' unrecognized values will be mapped to `NA`.
#'
#' A small set of common aliases is accepted for the `Color` scheme (e.g.,
#' `"aqua"` as `"cyan"`, `"violet"` as `"purple"`, and `"magenta"`/`"pink"` as
#' `"brown"`). The `Dye` scheme is treated
#' as canonical single-letter channel codes and is not aliased.
#'
#' Supported `Dye` codes are: `B`, `G`, `C`, `Y`, `R`, `O`, `P`, and `N`.
#' Values that cannot be mapped by the internal lookup table are returned as `NA`.
#'
#' @note
#' Kit definition files contain only color information. This function therefore
#' always calls `get_kit(kit, what = "Color")` when a kit is provided, even if
#' `need = "Dye"`. The `Dye` and `R.Color` columns are derived internally from
#' the `Color` values according to a fixed lookup table.
#'
#' @param data data frame or vector.
#' @param kit String representing the forensic STR kit used. The kit definition
#'   must contain a `"Color"` column; `"Dye"` and `"R.Color"` are computed
#'   internally.
#' @param have character string specifying which color scheme is present
#'   in the data. Acceptable values are `"Color"`, `"Dye"` or `"R.Color"`
#'   (case insensitive). If `data` is a vector, `have` must be supplied.
#' @param need character string or vector specifying which color schemes
#'   should be added. Acceptable values are `"Color"`, `"Dye"` and `"R.Color"`.
#'   Default `NA` means that all missing color columns will be added.
#'   If `data` is a vector, `need` must be a single value.
#' @param overwrite logical. If `TRUE`, existing color columns (`Color`, `Dye`,
#'   `R.Color`) will be replaced with values re-derived from the kit definition
#'   or from other color schemes.
#' @param ignore_case logical. If `TRUE`, marker names will be matched ignoring
#'   case when deriving colors from a kit.
#' @param debug logical indicating whether to print debug information.
#'
#' @return
#' For data frames, returns a data frame with additional columns corresponding
#' to the requested color schemes. For vectors, returns a vector converted
#' to the requested scheme.
#'
#' @export
#' @importFrom utils str
add_color <- function(data, kit = NA, have = NA, need = NA,
                     overwrite = FALSE, ignore_case = FALSE, debug = FALSE) {
  # Debug print of the call.
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }
  
  ## The row order follows the typical electrophoresis display order used by
  ## current forensic STR analysis software. Some display colour names are treated
  ## as aliases for the same dye/channel where software does not distinguish them
  ## as separate channels.
  ##
  ## The table reflects conventions in common use and can be extended if future
  ## platforms expose additional or distinct dye channels.
  map <- data.frame(
    
    # Canonical Color names supported in data / panels.
    # Duplicate rows are accepted aliases.
    Color = c(
      "blue",
      "green",
      "cyan", "aqua",
      "yellow",
      "red",
      "orange",
      "purple", "violet",
      "brown", "magenta", "pink"
    ),
    
    # Canonical single-letter dye codes
    Dye = c(
      "B",
      "G",
      "C", "C",
      "Y",
      "R",
      "O",
      "P", "P",
      "N", "N", "N"
    ),
    
    # R color names used for plotting
    R.Color = c(
      "blue",
      "green3",
      "cyan", "cyan",
      "gold",
      "red",
      "orange",
      "purple", "purple",
      "brown", "magenta3", "magenta3"
    ),
    
    stringsAsFactors = FALSE
  )
  
  # Valid scheme names (upper case) for convenience.
  color_schemes <- toupper(colnames(map))
  
  # Helper: convert a vector from one scheme to another using the map.
  convert_vector <- function(x, from_scheme, to_scheme) {
    from_scheme <- switch(from_scheme,
                          "COLOR"   = "Color",
                          "DYE"     = "Dye",
                          "R.COLOR" = "R.Color",
                          from_scheme)
    to_scheme <- switch(to_scheme,
                        "COLOR"   = "Color",
                        "DYE"     = "Dye",
                        "R.COLOR" = "R.Color",
                        to_scheme)
    from_vals <- map[[from_scheme]]
    to_vals   <- map[[to_scheme]]
    idx <- match(toupper(x), toupper(from_vals))
    return(to_vals[idx])
  }
  
  
  # When overwrite is requested remove existing colour columns from a data frame.
  if (overwrite && is.data.frame(data)) {
    nms <- toupper(names(data))
    if ("R.COLOR" %in% nms) {
      message("Overwriting column 'R.Color'...")
      data$R.Color <- NULL
    }
    if ("COLOR" %in% nms) {
      message("Overwriting column 'Color'...")
      data$Color <- NULL
    }
    if ("DYE" %in% nms) {
      message("Overwriting column 'Dye'...")
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
      from_scheme <- toupper(have[1])
      to_scheme   <- toupper(need[1])
      if (!(from_scheme %in% color_schemes)) {
        warning(sprintf("'have' scheme '%s' is not supported!", from_scheme))
      } else if (!(to_scheme %in% color_schemes)) {
        warning(sprintf("'need' scheme '%s' is not supported!", to_scheme))
      } else {
        data <- convert_vector(data, from_scheme, to_scheme)
      }
    }
  } else if (is.data.frame(data)) {
    if (debug) {
      print("data is data.frame")
    }
    # If 'kit' is supplied and no 'have' scheme is specified, derive colours
    # from the kit definition and populate a new 'Color' column.  Colour
    # assignment is based on marker names.  Case sensitivity can be controlled
    # via ignore_case.
    if (is.na(have) && !is.na(kit)) {
      # Always derive Color from kit if overwrite = TRUE or Color is missing
      if (!"COLOR" %in% toupper(names(data)) || overwrite) {
        kit_info <- get_kit(kit, what = "Color")
        marker  <- kit_info$Marker
        m_color  <- tolower(kit_info$Color)
        if (debug) {
          print("marker"); print(str(marker))
          print("m_color"); print(str(m_color))
        }
        if (ignore_case) {
          for (m in seq_along(marker)) {
            data$Color[toupper(data$Marker) == toupper(marker[m])] <- m_color[m]
          }
        } else {
          for (m in seq_along(marker)) {
            data$Color[data$Marker == marker[m]] <- m_color[m]
          }
        }
      }
      # Treat Color as the scheme we now have
      have <- "Color"
    }
    
    
    # Determine which scheme(s) we have.
    if (is.na(have)) {
      existing <- toupper(names(data))
      have <- existing[existing %in% color_schemes]
    } else {
      have <- toupper(have)
    }
    
    # Determine which scheme(s) we need to add.
    if (!is.na(need[1])) {
      need <- toupper(need)
    } else {
      need <- color_schemes
    }
    # Warn if any requested scheme is not supported.
    if (!any(need %in% color_schemes)) {
      warning(paste(paste(need, collapse = ","), "not supported!"))
    }
    
    if (debug) {
      print("have"); print(have)
      print("need"); print(need)
    }

    # Sequentially add missing schemes.
    for (scheme in need) {
      
      if (debug) {
        print("scheme"); print(scheme)
      }

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
      from_scheme <- have[1]
      # Prepare names to match R's column names (case sensitive):
      col_name <- switch(scheme,
                        "COLOR"   = "Color",
                        "DYE"     = "Dye",
                        "R.COLOR" = "R.Color")
      from_col <- switch(from_scheme,
                        "COLOR"   = "Color",
                        "DYE"     = "Dye",
                        "R.COLOR" = "R.Color")
      
      if (debug) {
        print("col_name"); print(col_name)
        print("from_col"); print(from_col)
      }

      # Perform conversion.
      if (from_col %in% names(data)) {
        data[[col_name]] <- convert_vector(data[[from_col]], from_scheme, scheme)
      } else {
        warning(sprintf("Can't find column '%s'!\n'%s' was not added!", from_col, col_name))
      }
      # Update 'have' to include the newly created scheme for subsequent conversions.
      have <- unique(c(have, scheme))
      
      if (debug) {
        print("have"); print(have)
      }
      
    }
  } else {
    warning("Unsupported data type!\n No colour was added!")
  }
  
  if (is.data.frame(data)) {
    
    # Attach the kit attribute to the result for downstream functions.
    attr(data, which = "kit") <- kit
    
    # Update audit trail if available.
    data <- audit_trail(obj = data, f_call = match.call(), package = "strvalidator")

  }
  
  if (debug) {
    print("Return"); print(str(data))
  }
  
  return(data)
}

