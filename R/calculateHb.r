# NOTE: Column names used for calculations with data.table is declared
# in globals.R to avoid NOTES in R CMD CHECK.

################################################################################
# CHANGE LOG (last 20 changes)
# 24.08.2018: Removed unused variables.
# 06.08.2017: Added audit trail.
# 18.09.2016: Fixed dataset saved to attributes.
# 28.08.2016: First version.

#' @title Calculate Heterozygote Balance
#'
#' @description
#' Calculates the heterozygote (intra-locus) peak balance.
#'
#' @details
#' Calculates the heterozygote (intra-locus) peak balance for a dataset.
#' Known allele peaks will be extracted using the reference prior to analysis.
#' Calculates the heterozygote balance (Hb), size difference between
#' heterozygous alleles (Delta), and mean peak height (MPH).
#' NB! 'X' and 'Y' will be handled as '1' and '2' respectively.
#'
#' @param data a data frame containing at least
#'  'Sample.Name', 'Marker', 'Height', and 'Allele'.
#' @param ref a data frame containing at least
#'  'Sample.Name', 'Marker', 'Allele'.
#' @param hb numerical, definition of heterozygote balance. Default is hb=1.
#'  hb=1: HMW/LMW, hb=2: LMW/HMW, hb=3; min(Ph)/max(Ph).
#' @param kit character defining the kit used. If NULL automatic detection is attempted.
#' @param ignore.case logical indicating if sample matching should ignore case.
#' @param word logical indicating if word boundaries should be added before sample matching.
#' @param exact logical indicating if exact sample matching should be used.
#' @param sex.rm logical TRUE removes sex markers defined by 'kit'.
#' @param qs.rm logical TRUE removes quality sensors defined by 'kit'.
#' @param debug logical indicating printing debug information.
#'
#' @return data.frame with with columns 'Sample.Name', 'Marker', 'Delta', 'Hb', 'MPH'.
#'
#' @export
#'
#' @importFrom utils str
#' @importFrom data.table data.table :=
#'
#' @examples
#' data(ref2)
#' data(set2)
#' # Calculate average balances.
#' calculateHb(data = set2, ref = ref2)
calculateHb <- function(data, ref, hb = 1, kit = NULL, sex.rm = FALSE, qs.rm = FALSE,
                        ignore.case = TRUE, exact = FALSE, word = FALSE,
                        debug = FALSE) {
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
    print("Parameters:")
    print("data")
    print(str(data))
    print("ref")
    print(str(ref))
    print("hb")
    print(hb)
    print("ignore.case")
    print(ignore.case)
    print("word")
    print(word)
  }

  # Check data ----------------------------------------------------------------

  if (!"Sample.Name" %in% names(data)) {
    stop("'data' must contain a column 'Sample.Name'.", call. = TRUE)
  }

  if (!"Marker" %in% names(data)) {
    stop("'data' must contain a column 'Marker'.", call. = TRUE)
  }

  if (!any(grepl("Allele", names(data)))) {
    stop("'data' must contain a column 'Allele'.", call. = TRUE)
  }

  if (!any(grepl("Height", names(data)))) {
    stop("'data' must contain a column 'Height'.", call. = TRUE)
  }

  # Check if slim format.
  if (sum(grepl("Allele", names(data))) > 1) {
    stop("'data' must be in 'slim' format.", call. = TRUE)
  }

  if (sum(grepl("Height", names(data))) > 1) {
    stop("'data' must be in 'slim' format.", call. = TRUE)
  }

  if (!"Sample.Name" %in% names(ref)) {
    stop("'ref' must contain a column 'Sample.Name'.", call. = TRUE)
  }

  if (!"Marker" %in% names(ref)) {
    stop("'ref' must contain a column 'Marker'.", call. = TRUE)
  }

  if (!any(grepl("Allele", names(ref)))) {
    stop("'ref' must contain a column 'Allele'.", call. = TRUE)
  }

  # Check if slim format.
  if (sum(grepl("Allele", names(ref))) > 1) {
    stop("'ref' must be in 'slim' format", call. = TRUE)
  }

  # Prepare -------------------------------------------------------------------

  # Check if 'kit' is provided.
  if (is.null(kit)) {
    message("'kit' not provided. Attempting auto detection.")
    kit <- detectKit(data = data, debug = debug)
  }

  # Check data type of Height.
  if (typeof(data$Height) != "integer" & typeof(data$Height) != "double") {
    message("'Height' not numeric. Converting to numeric.")
    # Convert to numeric.
    data$Height <- suppressWarnings(as.numeric(data$Height))
  }

  # Filter dataset.
  message("Extracting known alleles from dataset...")
  data <- filterProfile(
    data = data, ref = ref,
    add.missing.loci = FALSE, keep.na = FALSE, invert = FALSE,
    ignore.case = ignore.case, exact = exact, word = word,
    sex.rm = sex.rm, qs.rm = qs.rm, kit = kit, debug = debug
  )

  # Remove sex markers and quality sensors from reference dataset.
  if (sex.rm || qs.rm) {
    message("Removing gender markers and/or quality sensors from reference dataset...")
    ref <- filterProfile(
      data = ref, filter.allele = FALSE,
      sex.rm = sex.rm, qs.rm = qs.rm, kit = kit,
      debug = debug
    )
  }

  # Add Size
  if (!"Size" %in% names(data)) {
    if (hb != 3) {
      message("Estimating size of alleles...")

      # Get repeat size and offset.
      kitSize <- getKit(kit = kit, what = "Repeat")

      # Add estimated size to data.
      data <- addSize(
        data = data, kit = kitSize, bins = FALSE,
        ignore.case = ignore.case, debug = debug
      )
    } else {
      # Size not needed.
    }
  }

  # Analyse -------------------------------------------------------------------

  # Convert to data.table for performance.
  DT <- data.table(data)

  # Add number of peaks.
  DT[!is.na(Height), Peaks := .N, list(Sample.Name, Marker)]

  # Handle gender markers by replacing X->1 and Y->2 (case insensitive).
  message("Replacing gender markers: X->1, Y->2")
  DT[, Allele := toupper(Allele)]
  DT[Allele == "X", Allele := "1"]
  DT[Allele == "Y", Allele := "2"]

  # Calculate repeat difference.
  DT[, Allele := as.numeric(Allele)]
  message("Calculating allele distance: Delta=max(Allele) - min(Allele)...")
  DT[!is.na(Height), Delta := Allele[Allele == max(Allele)] - Allele[Allele == min(Allele)], list(Sample.Name, Marker)]

  if (hb == 1) {
    # High molecular weight (long) allele / Low molecular weight (short) allele

    # New dataset with only heterozygotes.
    message("Extracting heterozygotes...")
    DT2 <- DT[
      Peaks == 2,
      list(
        HMW = Height[Size == max(Size)],
        LMW = Height[Size == min(Size)]
      ),
      list(Sample.Name, Marker, Dye, Delta)
    ]

    # Calculate mean peak height and heterozygote balance.
    message("Calculating mean peak height: MPH=(HMW+LMW)/2...")
    DT2[, MPH := (HMW + LMW) / 2, by = list(Sample.Name, Marker)]
    message("Calculating heterozygote balance: Hb=HMW/LMW...")
    DT2[, Hb := HMW / LMW, by = list(Sample.Name, Marker)]
  } else if (hb == 2) {
    # Low molecular weight (short) allele / High molecular weight (long) allele

    # New dataset with only heterozygotes.
    message("Extracting heterozygotes...")
    DT2 <- DT[
      Peaks == 2,
      list(
        LMW = Height[Size == min(Size)],
        HMW = Height[Size == max(Size)]
      ),
      list(Sample.Name, Marker, Dye, Delta)
    ]

    # Calculate heterozygote balance and mean peak height.
    message("Calculating mean peak height: MPH=(LMW+HMW)/2...")
    DT2[, MPH := (LMW + HMW) / 2, by = list(Sample.Name, Marker)]
    message("Calculating heterozygote balance: Hb=LMW/HMW...")
    DT2[, Hb := LMW / HMW, by = list(Sample.Name, Marker)]
  } else if (hb == 3) {
    # Small peak (low) / Large peak (high)

    # New dataset with only heterozygotes.
    message("Extracting heterozygotes...")
    DT2 <- DT[Peaks == 2,
      list(Small = min(Height), Large = max(Height)),
      by = list(Sample.Name, Marker, Dye, Delta)
    ]

    # Calculate mean peak height and heterozygote balance.
    message("Calculating mean peak height: MPH=(Small+Large)/2...")
    DT2[, MPH := (Small + Large) / 2, by = list(Sample.Name, Marker)]
    message("Calculating heterozygote balance: Hb=Small/Large...")
    DT2[, Hb := Small / Large, by = list(Sample.Name, Marker)]
  } else {
    stop("Illegal method choice.", call. = TRUE)
  }

  # Convert back to data.frame to avoid any backward compatibility issues.
  res <- data.frame(DT2)

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
