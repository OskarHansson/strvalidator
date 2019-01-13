################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 24.08.2018: Removed unused variables.
# 06.08.2017: Added audit trail.
# 18.09.2016: Implemented data.table and added attributes, and factors to result.
# 28.08.2015: Added importFrom.
# 01.06.2015: Changed column name 'File' to 'File.Name'.
# 15.01.2014: Added message to show progress .
# 12.01.2014: Replaced 'subset' with native code.
# 11.01.2014: First version.

#' @title Calculate Peaks
#'
#' @description
#' Calculates the number of peaks in samples.
#'
#' @details
#' Count the number of peaks in a sample profile based on values in the
#' 'Height' column. Each sample is labeled according to custom labels
#' defined by the number of peaks. Peaks can be counted by sample or by
#' marker within a sample.
#' There is an option to discard off-ladder peaks ('OL').
#' The default purpose for this function is to categorize contamination in
#' negative controls, but it can be used to simply calculating the number of
#' peaks in any sample.
#' NB! A column 'Peaks' for the number of peaks will be created.
#'  If present it will be overwritten.
#' NB! A column 'Group' for the sample group will be created.
#'  If present it will be overwritten.
#' NB! A column 'Id' will be created by combining the content in the
#'  'Sample.Name' and 'File' column (if available).
#'  The unique entries in the 'Id' column will be the definition of a unique sample.
#'  If 'File' is present this allows for identical sample names in different
#'  batches (files) to be identified as unique samples.
#'  If 'Id' is present it will be overwritten.
#'
#' @param data data frame containing at least the columns
#'  'Sample.Name' and 'Height'.
#' @param labels character vector defining the group labels.
#' Length must be equal to number of bins + one label for anything above the
#' final cut-off.
#' @param bins numeric vector containing the cut-off points defined as
#' maximum number of peaks for all but the last label, which is anything
#' above final cut-off. Must be sorted in ascending order.
#' @param ol.rm logical if TRUE, off-ladder alleles 'OL' peaks will be discarded.
#' if FALSE, all peaks will be included in the calculations.
#' @param by.marker logical if TRUE, peaks will counted per marker.
#' if FALSE, peaks will counted per sample.
#' @param debug logical indicating printing debug information.
#'
#' @return data.frame with with additional columns 'Peaks', 'Group', and 'Id'.
#'
#' @export
#'
#' @importFrom utils str
#'

calculatePeaks <- function(data, bins = c(0, 2, 3), labels = NULL,
                           ol.rm = FALSE, by.marker = FALSE, debug = FALSE) {
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
    print("data:")
    print(str(data))
    print("bins:")
    print(bins)
    print("labels:")
    print(labels)
    print("ol.rm:")
    print(ol.rm)
    print("by.marker:")
    print(by.marker)
  }

  # Check parameters ----------------------------------------------------------

  if (length(bins) != length(labels) - 1) {
    stop("'bins' must be a vector of length 1 less than 'labels'!")
  }

  if (!is.logical(ol.rm)) {
    stop("'ol.rm' must be logical!")
  }

  if (!is.logical(by.marker)) {
    stop("'by.marker' must be logical!")
  }

  # Check data ----------------------------------------------------------------

  if (!"Sample.Name" %in% names(data)) {
    stop("'data' must contain a column 'Sample.Name'.")
  }

  if (!"Height" %in% names(data)) {
    stop("'data' must contain a column 'Height'.")
  }

  if (by.marker) {
    if (!"Marker" %in% names(data)) {
      stop("'data' must contain a column 'Marker'.")
    }
  }

  if (!is.vector(labels)) {
    stop("'labels' must be a character vector.")
  }

  if (!is.vector(bins)) {
    stop("'bins' must be a numeric vector.")
  }


  # Prepare -------------------------------------------------------------------

  if (ol.rm) {
    # Discard off-ladder peaks but keep NA's.
    data <- data[data$Allele != "OL" | is.na(data$Allele), ]
  }

  if (!is.numeric(bins)) {
    message("'bins' not numeric. Converting to numeric.")
    bins <- as.numeric(bins)
  }

  if (!is.character(labels)) {
    message("'labels' not character. Converting to character.")
    labels <- as.character(labels)
  }

  if (!is.numeric(data$Height)) {
    message("'Height' not numeric. Converting to numeric.")
    data$Height <- as.numeric(data$Height)
  }

  if ("Peaks" %in% names(data)) {
    message("A column 'Peaks' already exist. It will be overwritten.")
    data$Peaks <- NULL
  }

  if ("Group" %in% names(data)) {
    message("A column 'Group' already exist. It will be overwritten.")
    data$Group <- NULL
  }

  if ("Id" %in% names(data)) {
    message("A column 'Id' already exist. It will be overwritten.")
    data$Id <- NULL
  }

  # Add columns:
  data$Peaks <- as.integer(NA)
  data$Group <- as.character(NA)

  # Create Id by combining the sample and file name.
  data$Id <- paste(data$Sample.Name, data$File.Name, sep = "_")


  # Analyse -------------------------------------------------------------------

  # Convert to data.table.
  DT <- data.table::data.table(data)

  if (by.marker) {
    message("Counting number of peaks by marker...")

    DT[, Peaks := sum(!is.na(Height), na.rm = TRUE), by = list(Id, Marker)]
  } else {
    message("Counting number of peaks by sample...")

    DT[, Peaks := sum(!is.na(Height), na.rm = TRUE), by = list(Id)]
  }

  # Add group.
  DT$Group <- as.character(NA)

  # Check if at least one bin.
  if (length(labels) >= 1) {
    message("Adding first group label...")

    DT[Peaks <= bins[1]]$Group <- labels[1]
  }

  # Check if at least two bins.
  if (length(labels) >= 2) {
    message("Adding last group label...")

    DT[Peaks >= bins[length(bins)]]$Group <- labels[length(labels)]
  }

  # Check if more than two bins.
  if (length(labels) > 2) {
    message("Adding other group labels...")

    for (g in seq(from = 2, to = length(bins))) {

      # Add group label.
      DT[Peaks > bins[g - 1] & Peaks <= bins[g]]$Group <- labels[g]
    }
  }

  # Convert to data.frame.
  data <- as.data.frame((DT))

  # Add factors to plot in correct order.
  data$Group <- factor(data$Group, levels = labels)

  # Update audit trail.
  data <- auditTrail(obj = data, f.call = match.call(), package = "strvalidator")

  if (debug) {
    print("data:")
    print(str(data))
    print(paste("EXIT:", match.call()[[1]]))
  }

  # Return result.
  return(data)
}
