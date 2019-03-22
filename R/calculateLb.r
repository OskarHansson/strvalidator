# NOTE: Column names used for calculations with data.table is declared
# in globals.R to avoid NOTES in R CMD CHECK.

################################################################################
# CHANGE LOG (last 20 changes)
# 20.03.2019: Added new definition of balance (Issue:#14).
# 24.08.2018: Removed unused variables.
# 06.08.2017: Added audit trail.
# 06.09.2016: Fixed implementation of filterProfile function.
# 29.08.2016: Implemented updated filterProfile function.
# 13.07.2016: Fixed 'data' save as attribute.
# 28.06.2016: Added option to remove quality sensor.
# 27.06.2016: Fixed problem with replacing NAs.
# 19.02.2016: Add fix broken dye after removing OL alleles.
# 18.02.2016: Add missing markers after removing OL alleles.
# 09.01.2016: Added more attributes to result.
# 22.12.2015: First version.

#' @title Calculate Inter-locus Balance
#'
#' @description
#' Calculates the inter-locus balance.
#'
#' @details The inter-locus balance (Lb), or profile balance, can be calculated
#' as a proportion of the whole, normalized, or as centred quantities (as in
#' the reference but using the mean total marker peak height instead of H).
#' Lb can be calculated globally across the complete profile or within each dye
#' channel. All markers must be present in each sample. Data can be unfiltered
#' or filtered since the sum of peak heights by marker is used. A reference
#' dataset is required to filter the dataset, which also adds any missing
#' markers. A kit should be provided for filtering of known profile, sex
#' markers, or quality sensors. If not automatic detection will be attempted.
#' If missing, dye will be added according to kit. Off-ladder alleles and
#' quality sensors are by default removed from the dataset.
#' Sex markers are optionally removed.
#' Some columns in the result may vary:
#' TPH: Total (marker) Peak Height.
#' TPPH: Total Profile Peak Height.
#' MTPH: Maximum (sample) Total Peak Height.
#' MPH: Mean (marker) Peak Height.
#'
#' @param data data.frame containing at least
#'  'Sample.Name', 'Marker', and 'Height'.
#' @param ref data.frame containing at least 'Sample.Name', 'Marker', 'Allele'.
#' If provided alleles matching 'ref' will be extracted from 'data'
#' (see \code{\link{filterProfile}}).
#' @param option character: 'prop' for proportional Lb, 'norm' for normalized
#' LB, and 'cent' for centred Lb.
#' @param by.dye logical. Default is FALSE for global Lb, if TRUE Lb is calculated
#' within each dye channel.
#' @param ol.rm logical. Default is TRUE indicating that off-ladder 'OL' alleles
#' will be removed.
#' @param sex.rm logical. Default is FALSE indicating that all markers will be
#' considered. If TRUE sex markers will be removed.
#' @param qs.rm logical. Default is TRUE indicating that all quality sensors
#' will be removed.
#' @param na numeric. Numeric to replace NA values e.g. locus dropout can be
#' given a peak height equal to the limit of detection threshold, or zero.
#' Default is NULL indicating that NA will be treated as missing values.
#' @param kit character providing the kit name. Attempt to auto detect if NULL.
#' @param ignore.case logical indicating if sample matching should ignore case.
#' Only used if 'ref' is provided and 'data' is filtered.
#' @param word logical indicating if word boundaries should be added before sample matching.
#' Only used if 'ref' is provided and 'data' is filtered.
#' @param exact logical indicating if exact sample matching should be used.
#' Only used if 'ref' is provided and 'data' is filtered.
#' @param debug logical indicating printing debug information.
#'
#' @return data.frame with at least columns 'Sample.Name', 'Marker', 'TPH', 'Peaks', and 'Lb'.
#' See description for additional columns.
#'
#' @export
#'
#' @references
#' Torben Tvedebrink et.al.,
#'  Performance of two 17 locus forensic identification STR kits-Applied
#'  Biosystems's AmpFlSTR NGMSElect and Promega's PowerPlex ESI17 kits,
#'  Forensic Science International: Genetics, Volume 6, Issue 5, September 2012,
#'  Pages 523-531, ISSN 1872-4973, 10.1016/j.fsigen.2011.12.006.
#' \url{http://www.sciencedirect.com/science/article/pii/S1872497311002365}
#'
#' @importFrom utils str
#' @importFrom data.table data.table
#'
#' @examples
#' # Load data.
#' data(set2)
#' 
#' # Calculate inter-locus balance.
#' res <- calculateLb(data = set2)
#' print(res)
calculateLb <- function(data, ref = NULL, option = "prop", by.dye = FALSE,
                        ol.rm = TRUE, sex.rm = FALSE, qs.rm = FALSE,
                        na = NULL, kit = NULL, ignore.case = TRUE,
                        word = FALSE, exact = FALSE, debug = FALSE) {
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
    print("Parameters:")
    print("data")
    print(str(data))
    print("ref")
    print(str(ref))
    print("option")
    print(option)
    print("by.dye")
    print(by.dye)
    print("ol.rm")
    print(ol.rm)
    print("sex.rm")
    print(sex.rm)
    print("qs.rm")
    print(qs.rm)
    print("na")
    print(na)
    print("kit")
    print(kit)
    print("ignore.case")
    print(ignore.case)
    print("word")
    print(word)
    print("exact")
    print(exact)
  }

  # Check data ----------------------------------------------------------------

  if (is.null(data$Sample.Name)) {
    stop("'Sample.Name' does not exist!")
  }

  if (is.null(data$Marker)) {
    stop("'Marker' does not exist!")
  }

  if (!any(grepl("Height", names(data)))) {
    stop("'Height' does not exist!")
  }

  # Check if slim format.
  if (sum(grepl("Height", names(data))) > 1) {
    stop("'data' must be in 'slim' format",
      call. = TRUE
    )
  }

  if (!is.null(ref)) {
    if (is.null(ref$Sample.Name)) {
      stop("'Sample.Name' does not exist in ref!")
    }

    if (is.null(ref$Marker)) {
      stop("'Marker' does not exist in ref!")
    }

    if (!any(grepl("Allele", names(ref)))) {
      stop("'Allele' does not exist in ref!")
    }

    # Check if slim format.
    if (sum(grepl("Allele", names(ref))) > 1) {
      stop("'ref' must be in 'slim' format",
        call. = TRUE
      )
    }
  }

  if (!is.logical(by.dye)) {
    stop("'by.dye' must be logical!")
  }

  if (!is.logical(ol.rm)) {
    stop("'ol.rm' must be logical!")
  }

  if (!is.logical(sex.rm)) {
    stop("'sex.rm' must be logical!")
  }

  if (!is.logical(qs.rm)) {
    stop("'qs.rm' must be logical!")
  }

  if (!is.null(na) & !is.numeric(na)) {
    stop("'na' must be numeric or NULL!")
  }

  if (!is.logical(ignore.case)) {
    stop("'ignore.case' must be logical!")
  }

  if (!is.logical(word)) {
    stop("'word' must be logical!")
  }

  if (!is.logical(exact)) {
    stop("'exact' must be logical!")
  }

  # Prepare -------------------------------------------------------------------

  message("Preparing to calculate inter-locus balance.")

  # NB! The kit must be known for some operations so it must come first.
  if (is.null(kit)) {
    message("'kit' not provided. Attempting automatic detection.")

    # Detect kit if not provided.
    kit <- detectKit(data = data, index = FALSE, debug = debug)[1]

    message(kit, " detected.")
  }

  # Check if calculation by dye and add dye if not available.
  if (by.dye) {
    if (is.null(data$Dye)) {
      message("Dye is required to calculate by dye. Adding dye according to 'kit'.")

      data <- addColor(
        data = data, kit = kit, need = "Dye",
        ignore.case = ignore.case, overwrite = TRUE, debug = debug
      )
    }
  }

  # Remove off-ladder alleles.
  if (ol.rm) {
    tmp1 <- nrow(data)

    # Remove off-ladder alleles.
    data <- data[data$Allele != "OL" | is.na(data$Allele), ]

    tmp2 <- nrow(data)

    message("Removed ", tmp1 - tmp2, " off-ladder alleles.")

    # Check that each sample have all markers.
    DT <- data.table::data.table(data)
    tmp <- DT[, list(Marker = length(unique(Marker))), by = list(Sample.Name)]
    if (length(unique(tmp$Marker)) != 1) {
      message("Missing markers detected.")

      # Get kit markers.
      marker <- getKit(kit = kit, what = "Marker")

      # Add missing markers.
      data <- addMarker(data = data, marker = marker, ignore.case = ignore.case, debug = debug)

      # Check and fix dye.
      if (!is.null((data$Dye))) {
        if (any(is.na(data$Dye))) {

          # Fix broken dye.
          data <- addColor(
            data = data, kit = kit, need = "Dye",
            ignore.case = ignore.case, overwrite = TRUE, debug = debug
          )
        }
      }
    }
  }

  # Filter data.
  if (!is.null(ref)) {
    message("Extracting known profiles and adding missing loci.")

    # Filter dataset.
    data <- filterProfile(
      data = data, ref = ref,
      add.missing.loci = TRUE, keep.na = TRUE, invert = FALSE,
      ignore.case = ignore.case, exact = exact, word = word,
      sex.rm = sex.rm, qs.rm = qs.rm, kit = kit, debug = debug
    )

    # Check if Dye exist.
    if (!is.null((data$Dye))) {

      # Check and fix dye.
      if (any(is.na(data$Dye))) {

        # Fix broken dye.
        data <- addColor(
          data = data, kit = kit, need = "Dye",
          ignore.case = ignore.case, overwrite = TRUE, debug = debug
        )
      }
    }
  } else {
    message("Reference dataset not provided.")

    # Filter dataset.
    data <- filterProfile(
      data = data, ref = NULL, add.missing.loci = FALSE,
      keep.na = TRUE, invert = FALSE,
      ignore.case = ignore.case, exact = exact, word = word,
      sex.rm = sex.rm, qs.rm = qs.rm, kit = kit,
      filter.allele = FALSE, debug = debug
    )
  }

  # Convert to numeric.
  if (!is.numeric((data$Height))) {
    data$Height <- as.numeric(data$Height)

    message("'Height' converted to numeric.")
  }

  # Replace missing values.
  if (!is.null(na)) {
    nas <- sum(is.na(data$Height))

    if (nas > 0) {

      # Replace missing values with specified value.
      data[is.na(data$Height), ]$Height <- na
    }

    message("Replaced ", nas, " Height = NA with ", na, ".")
  }

  # Check that each sample have all markers.
  DT <- data.table::data.table(data)
  tmp <- DT[, list(Marker = length(unique(Marker))), by = list(Sample.Name)]
  if (length(unique(tmp$Marker)) != 1) {
    message("Missing markers detected. Each samples must contain all markers.")
    message("The following samples are incomplete:")
    print(tmp[tmp$Marker != max(tmp$Marker), ])
    stop("Missing markers detected!")
  }

  # Analyse -------------------------------------------------------------------

  # Convert to data.table for calculations.
  DT <- data.table::data.table(data)

  # Check method and calculate accordingly.
  if (option == "prop") {

    message("Calculating total peak height by sample and marker.")
    res <- DT[, list(TPH = sum(Height), Peaks = .N, Dye = unique(Dye)),
              by = list(Sample.Name, Marker)]
    
    if (by.dye) {

      message("Calculating total profile peak height by sample and dye.")
      res[, TPPH := sum(TPH), by = list(Sample.Name, Dye)]

      message("Calculating locus proportions of total profile peak height.")
      res[, Lb := TPH / TPPH, by = list(Sample.Name, Dye, Marker)]
    } else {

      message("Calculating total profile peak height by sample.")
      res[, TPPH := sum(TPH), by = list(Sample.Name)]

      message("Calculating locus proportions of total profile peak height.")
      res[, Lb := TPH / TPPH, by = list(Sample.Name, Marker)]
    }
  } else if (option == "norm") {
    
    message("Calculating total peak height by sample and marker.")
    res <- DT[, list(TPH = sum(Height), Peaks = .N, Dye = unique(Dye)),
              by = list(Sample.Name, Marker)]
    
    if (by.dye) {

      message("Calculating maximum total peak height by sample and dye.")
      res[, MTPH := max(TPH), by = list(Sample.Name, Dye)]

      message("Calculating normalized locus proportions.")
      res[, Lb := TPH / MTPH, by = list(Sample.Name, Dye, Marker)]
    } else {

      message("Calculating maximum total peak height by sample.")
      res[, MTPH := max(TPH), by = list(Sample.Name)]

      message("Calculating normalized locus proportions.")
      res[, Lb := TPH / MTPH, by = list(Sample.Name, Marker)]
    }
  } else if (option == "cent") {
    
    message("Calculating total peak height by sample and marker.")
    res <- DT[, list(TPH = sum(Height), Peaks = .N, Dye = unique(Dye)),
              by = list(Sample.Name, Marker)]
    
    if (by.dye) {

      message("Calculating mean total peak height by sample and dye.")
      res[, MPH := mean(TPH), by = list(Sample.Name, Dye)]

      message("Calculating centred locus quantities.")
      res[, Lb := (TPH - MPH) / sqrt(MPH), by = list(Sample.Name, Dye, Marker)]
    } else {

      message("Calculating mean total peak height by sample.")
      res[, MPH := mean(TPH), by = list(Sample.Name)]

      message("Calculating centred locus quantities.")
      res[, Lb := (TPH - MPH) / sqrt(MPH), by = list(Sample.Name, Marker)]
    }
  } else if (option == "peak") {
    if (by.dye) {

      message("Calculating minimum and maximum peak height by sample and dye.")
      res <- DT[, list(Min = min(Height), Max = max(Height), Peaks = .N),
                by = list(Sample.Name, Dye)]
      
      message("Calculating peak ratio by sample and dye.")
      res[, Lb := Min / Max, by = list(Sample.Name, Dye)]
    } else {

      message("Calculating minimum and maximum peak height by sample.")
      res <- DT[, list(Min = min(Height), Max = max(Height), Peaks = .N),
                by = list(Sample.Name)]
      
      message("Calculating peak ratio by sample.")
      res[, Lb := Min / Max, by = list(Sample.Name)]
    }
  } else {
    stop("option = ", option, "not implemented!")
  }

  # Convert to data.frame.
  res <- as.data.frame(res)

  # Add attributes to result.
  attr(res, which = "kit") <- kit

  # Update audit trail.
  res <- auditTrail(obj = res, f.call = match.call(), package = "strvalidator")

  if (debug) {
    print(paste("EXIT:", match.call()[[1]]))
  }

  # Return result.
  return(res)
}
