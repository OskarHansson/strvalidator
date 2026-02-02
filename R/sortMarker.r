################################################################################
#' @title Sort Marker and Dye Levels According to Kit Definition
#'
#' @description
#' Sort factor levels for `Marker` and `Dye` so they follow the natural
#' electropherogram (EPG) order defined by the provided kit. This function
#' replaces `sortMarker()` and includes improved validation, an `ignore_case`
#' option, better mismatch reporting, and predictable factor behavior.
#'
#' @details
#' Sorting markers and dyes to match the kit definition is essential for
#' correct ordering in EPG plots (e.g., facet order, x-axis order).
#'
#' The function:
#' * Normalizes marker names when `ignore_case = TRUE`
#' * Ensures `Marker` and `Dye` levels match the kit specification
#' * Optionally adds missing factor levels (for consistency in multi-sample data)
#' * Provides detailed debugging messages when `debug = TRUE`
#'
#' @param data data.frame  
#'   Must contain column `Marker`. If present, column `Dye` will also be sorted.
#'
#' @param kit character or integer  
#'   Kit name as used by `getKit()`.
#'
#' @param add_missing_levels logical  
#'   If `TRUE`, missing kit markers/dyes are added as unused factor levels.
#'
#' @param ignore_case logical  
#'   If `TRUE`, marker names are matched case-insensitively by normalizing to
#'   upper-case for comparison and factor-level generation.
#'
#' @param strict logical  
#'   If `TRUE`, unknown markers or dyes cause an error.  
#'   If `FALSE`, a warning is issued and unmatched values are dropped or left as-is.
#'
#' @param debug logical  
#'   Print detailed debugging information.
#'
#' @return
#' A modified `data.frame` with sorted factor levels for `Marker` and `Dye`.
#'
#' @export
#'
#' @seealso [generate_epg()], [add_color()], [getKit()]
#'
################################################################################

sort_markers <- function(data,
                         kit,
                         add_missing_levels = FALSE,
                         ignore_case = TRUE,
                         strict = TRUE,
                         debug = FALSE) {
  
  debug_msg <- function(...) if (debug) message(...)
  
  # ---------------------------------------------------------------------------
  # VALIDATION
  # ---------------------------------------------------------------------------
  if (!is.data.frame(data))
    stop("'data' must be a data.frame")
  
  if (!"Marker" %in% names(data))
    stop("'data' must contain a 'Marker' column")
  
  # Confirm kit exists
  if (!toupper(kit) %in% toupper(getKit()))
    stop("Unknown kit: ", kit,
         "\nAvailable kits: ", paste(getKit(), collapse = ", "))
  
  # ---------------------------------------------------------------------------
  # FETCH KIT TABLES
  # ---------------------------------------------------------------------------
  kit_markers <- getKit(kit, what = "Marker")
  kit_colors  <- getKit(kit, what = "Color")  # includes dye information
  
  # Normalize case if needed
  if (ignore_case) {
    debug_msg("Normalizing case for Marker names (toupper)...")
    data$Marker       <- toupper(data$Marker)
    kit_markers       <- toupper(kit_markers)
    kit_colors$Marker <- toupper(kit_colors$Marker)
  }
  
  # Unique dye levels as defined by kit
  kit_dye_levels <- unique(kit_colors$Dye)
  
  # ---------------------------------------------------------------------------
  # MARKER ORDERING
  # ---------------------------------------------------------------------------
  current_markers <- unique(as.character(data$Marker))
  
  missing_markers <- setdiff(kit_markers, current_markers)
  extra_markers   <- setdiff(current_markers, kit_markers)
  
  if (debug) {
    debug_msg("Current markers: ", paste(current_markers, collapse = ", "))
    debug_msg("Kit markers    : ", paste(kit_markers, collapse = ", "))
    debug_msg("Missing markers: ", paste(missing_markers, collapse = ", "))
    debug_msg("Extra markers  : ", paste(extra_markers,   collapse = ", "))
  }
  
  if (length(extra_markers) > 0) {
    msg <- paste0("Markers not found in kit: ", paste(extra_markers, collapse = ", "))
    if (strict) stop(msg) else warning(msg, call. = FALSE)
  }
  
  # Add missing markers as unused factor levels
  if (add_missing_levels && length(missing_markers) > 0) {
    debug_msg("Adding missing marker levels: ", paste(missing_markers, collapse = ", "))
    kit_markers <- unique(c(kit_markers, missing_markers))
  }
  
  data$Marker <- factor(data$Marker, levels = kit_markers)
  
  # ---------------------------------------------------------------------------
  # DYE ORDERING
  # ---------------------------------------------------------------------------
  if ("Dye" %in% names(data)) {
    
    # Derive kit dye levels (e.g. B/G/Y/R) from kit Color table.
    # Color table typically has columns like Marker, Color (e.g. "blue").
    if ("Dye" %in% names(kit_colors)) {
      kit_dye_levels <- unique(kit_colors$Dye)
    } else if ("Color" %in% names(kit_colors)) {
      dye_info <- add_color(
        data = kit_colors,
        kit = kit,
        need = "Dye",
        ignore_case = ignore_case
      )
      kit_dye_levels <- unique(dye_info$Dye)
    } else {
      stop("Kit 'Color' table does not contain 'Color' or 'Dye' columns; 
           cannot determine dye levels.")
    }
    
    current_dyes <- unique(as.character(data$Dye))
    
    missing_dyes <- setdiff(kit_dye_levels, current_dyes)
    extra_dyes   <- setdiff(current_dyes, kit_dye_levels)
    
    if (debug) {
      debug_msg("Current dyes: ", paste(current_dyes, collapse = ", "))
      debug_msg("Kit dyes    : ", paste(kit_dye_levels, collapse = ", "))
      debug_msg("Missing dyes: ", paste(missing_dyes, collapse = ", "))
      debug_msg("Extra dyes  : ", paste(extra_dyes,   collapse = ", "))
    }
    
    if (length(extra_dyes) > 0) {
      msg <- paste0("Dye levels not found in kit: ", paste(extra_dyes, collapse = ", "))
      if (strict) stop(msg) else warning(msg, call. = FALSE)
    }
    
    if (add_missing_levels && length(missing_dyes) > 0) {
      debug_msg("Adding missing dye levels: ", paste(missing_dyes, collapse = ", "))
      kit_dye_levels <- unique(c(kit_dye_levels, missing_dyes))
    }
    
    data$Dye <- factor(data$Dye, levels = kit_dye_levels)
  }
  
  # ---------------------------------------------------------------------------
  # AUDIT TRAIL
  # ---------------------------------------------------------------------------
  attr(data, "kit") <- kit
  data <- audit_trail(
    obj     = data,
    f_call  = match.call(),
    package = "strvalidator"
  )
  
  return(data)
}

################################################################################
#' @rdname sort_markers
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [sort_markers()] instead.
################################################################################

sortMarker <- function(data,
                       kit,
                       add.missing.levels = FALSE,
                       debug = FALSE,
                       ...) {
  
  .Deprecated("sort_markers", package = "strvalidator")
  
  sort_markers(
    data                = data,
    kit                 = kit,
    add_missing_levels  = add.missing.levels,
    debug               = debug,
    ...
  )
}