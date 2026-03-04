#' @title Add Size Information
#'
#' @description
#' Assigns base-pair size to alleles using kit definitions.
#'
#' @details
#' The function follows this priority:
#'
#' **1. Exact match in kit**  
#' For each `Marker` + `Allele` combination, if the kit contains a row with
#' the same values, the corresponding `Size` from the kit is used.
#'
#' **2. Numeric alleles missing from kit**  
#' If an allele is numeric (e.g. `16`, `16.3`, `22`) and not present in the
#' kit:
#' \itemize{
#'   \item If two numeric neighbors exist in the kit, the size is
#'         \strong{interpolated} between them.
#'   \item If only one neighbor exists, the marker `Repeat` unit from the kit 
#'         definition, is used to \strong{extrapolated} from the neighbor.
#'   \item If no neighbors exist but `Offset` and `Repeat` are available,
#'         an \strong{offset-based} estimate is used.
#' }
#' A message is printed whenever interpolation, extrapolation, or
#' offset-based fallback is used.
#'
#' **3. Non-numeric alleles missing from kit**  
#' Non-numeric alleles (e.g. OL) that are not present in the kit and
#' have no size information cannot be placed and are removed from the
#' result. A message is printed.
#'
#' @param data data.frame containing at least columns `Marker` and `Allele`.
#' @param kit data.frame containing at least:
#'   \itemize{
#'     \item `Marker`
#'     \item `Allele`
#'     \item `Size`
#'   }
#'   and optionally:
#'   \itemize{
#'     \item `Offset`
#'     \item `Repeat`
#'   }
#'   which are used for extrapolation and offset-based fallback when needed.
#' @param ignore_case logical. If TRUE, marker names are matched
#'   case-insensitive.
#' @param debug logical. If TRUE, debug information is printed.
#'
#' @return A data.frame with a numeric `Size` column added and rows with
#'   unresolved alleles removed.
#'
#' @aliases addSize
#' @export
#'
#' @importFrom utils str head
#'

add_size <- function(data,
                     kit,
                     ignore_case = FALSE,
                     debug = FALSE) {
  
  if (debug) {
    print("===== add_size() DEBUG START =====")
    print("data:")
    print(str(data))
    print("kit:")
    print(str(kit))
  }
  
  # ---------------------------------------------------------------------------
  # CHECK INPUT
  # ---------------------------------------------------------------------------
  if (!"Marker" %in% names(data)) {
    stop("'data' must contain column 'Marker'")
  }
  if (!"Allele" %in% names(data)) {
    stop("'data' must contain column 'Allele'")
  }
  
  req_cols <- c("Marker", "Allele", "Size")
  if (!all(req_cols %in% names(kit))) {
    stop("'kit' must contain columns: 'Marker', 'Allele', and 'Size'")
  }
  
  # Optional columns for fallback.
  has_offset <- "Offset" %in% names(kit)
  has_repeat <- "Repeat" %in% names(kit)
  
  # Ensure Allele is character.
  if (!is.character(data$Allele)) {
    message("'Allele' must be character. Converting 'data$Allele'.")
    data$Allele <- as.character(data$Allele)
  }
  
  # Case handling for markers.
  if (ignore_case) {
    data$Marker <- toupper(data$Marker)
    kit$Marker  <- toupper(kit$Marker)
  }
  
  # Prepare Size column.
  data$Size <- NA_real_
  
  # Split kit by marker for faster lookup.
  kit_by_marker <- split(kit, kit$Marker)
  
  # Helper: find numeric neighbors safely -------------------------------------
  
  find_numeric_neighbors <- function(allele_num, known_numeric) {
    lower_candidates <- known_numeric[known_numeric < allele_num]
    upper_candidates <- known_numeric[known_numeric > allele_num]
    
    has_lower <- length(lower_candidates) > 0
    has_upper <- length(upper_candidates) > 0
    
    lower_val <- if (has_lower) max(lower_candidates) else NA_real_
    upper_val <- if (has_upper) min(upper_candidates) else NA_real_
    
    list(
      has_lower = has_lower,
      has_upper = has_upper,
      lower     = lower_val,
      upper     = upper_val
    )
  }
  
  # ---------------------------------------------------------------------------
  # MAIN LOOP OVER MARKERS
  # ---------------------------------------------------------------------------
  
  for (m in unique(data$Marker)) {
    rows_m <- which(data$Marker == m)
    
    if (!m %in% names(kit_by_marker)) {
      warning("Marker '", m, "' not found in kit. Cannot assign size for these rows.")
      next
    }
    
    km <- kit_by_marker[[m]]
    
    # Repeat and offset may be missing; handle gracefully.
    repeat_len <- NA_real_
    offset_val <- NA_real_
    
    if (has_repeat && any(!is.na(km$Repeat))) {
      repeat_len <- unique(km$Repeat[!is.na(km$Repeat)])[1L]
    }
    if (has_offset && any(!is.na(km$Offset))) {
      offset_val <- unique(km$Offset[!is.na(km$Offset)])[1L]
    }
    
    known_alleles <- km$Allele
    known_sizes   <- km$Size
    
    # Numeric alleles in kit (for interpolation/extrapolation).
    known_numeric <- suppressWarnings(as.numeric(known_alleles))
    valid_numeric_idx    <- which(!is.na(known_numeric))
    known_numeric_vals   <- known_numeric[valid_numeric_idx]
    known_numeric_sizes  <- known_sizes[valid_numeric_idx]
    
    # Loop over rows for this marker.
    for (idx in rows_m) {
      allele <- data$Allele[idx]
      
      # 1. Exact match in kit -------------------------------------------------
      kit_match <- km[km$Allele == allele, , drop = FALSE]
      if (nrow(kit_match) > 0) {
        data$Size[idx] <- kit_match$Size[1L]
        next
      }
      
      # 2. Non-numeric alleles not in kit ------------------------------------
      allele_num <- suppressWarnings(as.numeric(allele))
      if (is.na(allele_num)) {
        message("Non-numeric allele '", allele, "' for marker '", m,
                "' not found in kit and has no size. Removing.")
        data$Size[idx] <- NA_real_
        next
      }
      
      # 3. Interpolation / extrapolation for numeric alleles -----------------
      if (length(known_numeric_vals) > 0) {
        neigh <- find_numeric_neighbors(allele_num, known_numeric_vals)
        
        # 3A. Interpolate between neighbors (no need for repeat_len).
        if (neigh$has_lower && neigh$has_upper) {
          size_low  <- known_numeric_sizes[known_numeric_vals == neigh$lower][1L]
          size_high <- known_numeric_sizes[known_numeric_vals == neigh$upper][1L]
          
          fraction <- (allele_num - neigh$lower) / (neigh$upper - neigh$lower)
          est <- size_low + fraction * (size_high - size_low)
          
          data$Size[idx] <- est
          message("Interpolated size for marker '", m, "', allele ", allele,
                  " between ", neigh$lower, " and ", neigh$upper, ".")
          next
        }
        
        # 3B. Extrapolate upwards from the largest known numeric allele.
        if (neigh$has_lower && !neigh$has_upper && !is.na(repeat_len)) {
          size_low <- known_numeric_sizes[known_numeric_vals == neigh$lower][1L]
          est <- size_low + (allele_num - neigh$lower) * repeat_len
          
          data$Size[idx] <- est
          message("Extrapolated upward for marker '", m, "', allele ", allele,
                  " from nearest known allele ", neigh$lower,
                  " using repeat length ", repeat_len, ".")
          next
        }
        
        # 3C. Extrapolate downwards from the smallest known numeric allele.
        if (!neigh$has_lower && neigh$has_upper && !is.na(repeat_len)) {
          size_high <- known_numeric_sizes[known_numeric_vals == neigh$upper][1L]
          est <- size_high - (neigh$upper - allele_num) * repeat_len
          
          data$Size[idx] <- est
          message("Extrapolated downward for marker '", m, "', allele ", allele,
                  " from nearest known allele ", neigh$upper,
                  " using repeat length ", repeat_len, ".")
          next
        }
      }
      
      # 4. Final fallback: Offset + repeat (if available) --------------------
      if (!is.na(offset_val) && !is.na(repeat_len)) {
        est <- offset_val + allele_num * repeat_len
        data$Size[idx] <- est
        message("Offset-based size estimation used for marker '", m,
                "', allele ", allele, " (no neighbors available).")
      } else {
        message("No neighbors and no offset/repeat available for marker '",
                m, "', allele ", allele, ". Removing.")
        data$Size[idx] <- NA_real_
      }
    }
  }
  
  # Remove rows with NA Size (unresolved or invalid alleles).
  na_rows <- which(is.na(data$Size))
  if (length(na_rows)) {
    message("Removed ", length(na_rows),
            " alleles with no resolvable size information.")
    data <- data[-na_rows, , drop = FALSE]
  }
  
  if (debug) {
    print("===== add_size() DEBUG END =====")
    print(str(data))
    print(head(data))
  }
  
  attr(data, "kit") <- kit
  data <- audit_trail(obj = data, f_call = match.call(), package = "strvalidator")
  
  return(data)
}
