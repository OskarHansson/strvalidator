################################################################################
# TODO LIST
# TODO: Allow to use Data.Point

################################################################################
# CHANGE LOG (last 20 changes)
# 24.08.2018: Removed unused variables.
# 07.08.2017: Added audit trail.
# 27.06.2016: Added: @importFrom stats dist
# 19.05.2016: Implemented more accurat method and parameter 'quick'.
# 18.05.2016: Now removes false positives (multiple peaks in same dye).
# 12.10.2015: First version.

#' @title Detect Spike
#'
#' @description
#' Detect samples with possible spikes in the DNA profile.
#'
#' @details Creates a list of possible spikes by searching for peaks aligned
#' vertically (i.e. nearly identical size). There are two methods to search.
#' The default method (quick=FALSE) method that calculates the distance between
#' each peak in a sample, and the quick and dirty method (quick=TRUE) that
#' rounds the size and then group peaks with identical size. The rounding
#' method is faster because it uses the data.table package. The accurate method
#' is slower because it uses nested loops - the first through each sample to
#' calculate the distance between all peaks, and the second loops through the
#' distance matrix to identify which peaks lies within the tolerance.
#' NB! The quick method may not catch all spikes since two peaks can be
#' separated by rounding e.g. 200.5 and 200.6 becomes 200 and 201 respectively.
#'
#' @param data data.frame with including columns 'Sample.Name', 'Marker', 'Size'.
#' @param threshold numeric number of peaks of similar size in different dye
#' channels to pass as a possible spike (NULL = number of dye channels
#' minus one to allow for one unlabeled peak).
#' @param tolerance numeric tolerance for Size. For the quick and dirty
#' rounding method e.g. 1.5 rounds Size to +/- 0.75 bp. For the slower but
#' more accurate method the value is the maximum allowed difference between
#' peaks in a spike.
#' @param kit string or numeric for the STR-kit used (NULL = auto detect).
#' @param quick logical TRUE for the quick and dirty method. Default is FALSE
#' which use a slower but more accurate method.
#' @param debug logical indicating printing debug information.
#'
#' @export
#'
#' @importFrom data.table data.table := .N
#' @importFrom stats dist
#'
#' @return data.frame
#'
#' @seealso \code{\link{data.table}}


calculateSpike <- function(data, threshold = NULL, tolerance = 2, kit = NULL,
                           quick = FALSE, debug = FALSE) {

  # Parameters that are changed by the function must be saved first.
  attr_kit <- substitute(kit)

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
    print("str(data):")
    print(str(data))
    print("threshold:")
    print(threshold)
    print("tolerance:")
    print(tolerance)
    print("kit:")
    print(kit)
  }

  # Check data ----------------------------------------------------------------

  # Columns.
  if (is.null(data$Sample.Name)) {
    stop("'Sample.Name' does not exist!")
  }
  if (is.null(data$Marker)) {
    stop("'Marker' does not exist!")
  }
  if (is.null(data$Size)) {
    stop("'Size' does not exist!")
  }

  # Check if slim format.
  if (sum(grepl("Size", names(data))) > 1) {
    stop("'data' must be in 'slim' format",
      call. = TRUE
    )
  }

  # Check data type.
  if (!is.numeric(data$Size)) {
    data$Size <- as.numeric(data$Size)
    warning("'Size' not numeric! 'data' converted.")
  }

  # Prepare -------------------------------------------------------------------

  # Convert to data.table.
  DT <- data.table::data.table(data)

  if (is.null(kit)) {

    # Detect kit.
    kit <- detectKit(data = DT, index = FALSE, debug = debug)
    kit <- kit[1]
    message(paste("Using kit:", kit))
  }

  # Getdye channels.
  kitColors <- unique(getKit(kit = kit, what = "Color", debug = debug)$Color)
  kitDyes <- addColor(data = kitColors, have = "Color", need = "Dye")

  if (is.null(threshold)) {

    # Default to number of dyes minus one to allow for one unlabeled spike.
    threshold <- length(kitDyes) - 1
    message(paste("Using default spike threshold:", threshold))
  } else if (threshold > length(kitDyes)) {

    # Threshold cannot be larger than the number of dyes in the kit.
    threshold <- length(kitDyes) - 1
    message(paste("'threshold' cannot be larger than the number of dyes in the kit"))
    message(paste("Using default spike threshold:", threshold))
  }

  # Remove NA's.
  if (any(is.na(DT$Size))) {
    DT <- DT[!is.na(DT$Size), ]
  }

  # Add Dye if not present.
  if (!"Dye" %in% names(DT)) {
    DT <- addColor(data = DT, kit = kit, need = "Dye")
  }

  # Add Id if not present.
  if (!"Id" %in% names(DT)) {
    DT$Id <- paste(DT$Sample.Name, DT$File.Name)
  }

  # Analyse -------------------------------------------------------------------

  # Define column names (to avoid notes.)
  col1 <- "File.Name"
  col2 <- "Sample.Name"
  col3 <- "Peaks"
  col4 <- "Size"
  col5 <- "Dyes"

  if (quick) {
    message(
      "Calculating spikes using the quick and dirty method based on ",
      "rounded 'Size' and the package data.table."
    )
    message(
      "NB! This method may not catch all spikes since two peaks can ",
      "be separated by rounding e.g. 200.5 and 200.6 -> 200 and 201."
    )

    # Round to nearest base pair
    DT$Round <- tolerance * round(DT$Size / tolerance)

    # Count number of peaks of same size per sample.
    res <- DT[, "Peaks" := .N, by = c("Id", "Round")]

    # Count number of dyes of same size per sample.
    res <- res[, "Dyes" := length(unique(Dye)), by = c("Id", "Round")]

    # Extract samples with at least 'threshold' peaks of the same size.
    res <- res[get(col3) >= threshold, ]
  } else {
    message(
      "Calculating spikes using the accurate method based on calculating ",
      "the distance between each peak in a sample."
    )

    # Add column for number of peaks within accepted distance.
    DT$Peaks <- as.numeric(NA)

    # Add column for index of peaks within accepted distance.
    DT$Dist <- as.character(NA)

    # Get unique Id's.
    id <- unique(DT$Id)

    # Initiate progress indicator.
    m <- 1

    # Loop through each sample.
    for (i in seq(along = id)) {

      # Show progress every 10% percent.
      if ((i / length(id)) >= (m / 10)) {
        message(100 * m / 10, "% of ", length(id), " samples analysed.")
        m <- m + 1
      }

      # Select current sample.
      selected <- DT$Id == id[i]

      # Calculate distance.
      currentDist <- dist(DT[selected, ]$Size)

      # Check if result.
      if (length(currentDist) != 0) {
        currentMat <- as.matrix(currentDist)

        # Get length of current matrix.
        lenMat <- length(row.names(currentMat))

        # Allocate vectors.
        currentDist <- rep(as.character(NA), lenMat)
        currentPeaks <- rep(as.numeric(NA), lenMat)

        for (r in seq(from = 1, to = lenMat)) {

          # Extract peaks whose difference is tolerated.
          closePeaks <- which(currentMat[r, ] <= tolerance)
          currentDist[r] <- paste(closePeaks, collapse = ",")
          currentPeaks[r] <- length(closePeaks)
        }

        # Write result.
        DT[selected, ]$Dist <- currentDist
        DT[selected, ]$Peaks <- currentPeaks
      }
    }

    # Count number of dyes of same size per sample.
    res <- DT[, "Dyes" := length(unique(Dye)), by = c("Id", "Dist")]

    # Extract samples with at least 'threshold' peaks of the same size.
    res <- res["Dist" >= threshold, ]
  }

  # Remove 'false' spikes (multiple peaks in same dye).
  res <- res[get(col5) >= threshold, ]

  # Sort table.
  res <- res[order(get(col1), get(col2), -get(col3), get(col4))]

  # Convert to data.frame.
  res <- as.data.frame(res)

  # Add attributes to result.
  attr(res, which = "kit") <- attr_kit

  # Update audit trail.
  res <- auditTrail(obj = res, f.call = match.call(), package = "strvalidator")

  if (debug) {
    print(paste("EXIT:", match.call()[[1]]))
  }

  return(res)
}
