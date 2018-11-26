################################################################################
# TODO LIST
# TODO: ...

# NOTE: Column names used for calculations with data.table is declared
# in globals.R to avoid NOTES in R CMD CHECK.

################################################################################
# CHANGE LOG (last 20 changes)
# 24.08.2018: Removed unused variables.
# 07.08.2017: Added audit trail.
# 11.05.2016: Changed from 'Peaks' to 'Peaks - 2' degrees of freedom.
# 25.04.2016: First version.

#' @title Calculate Profile Slope.
#'
#' @description
#' Calculate profile slope for samples.
#'
#' @details
#' Calculates the profile slope for each sample. The slope is calculated as a
#' linear model specified by the response (natural logarithm of peak height) by
#' the term size (in base pair). If 'Size' is not present in the dataset, one or
#' multiple kit names can be given as argument 'kit'. The specified kits will
#' be used to estimate the size of each allele. If 'kit' is NULL the kit(s)
#' will be automatically detected, and the 'Size' will be calculated.
#'
#' The column 'Group' can be used to separate datasets to be compared, and if
#' so 'kit' must be a vector of equal length as the number of groups, and in
#' the same order. If not the first 'kit' will be recycled for all groups.
#'
#' Data will be filtered using the reference profiles.
#'
#' @param data data.frame with at least columns 'Sample.Name', 'Marker', and
#' 'Height'.
#' @param ref data.frame with at least columns 'Sample.Name', 'Marker',
#' and 'Allele'
#' @param conf numeric confidence limit to calculate a confidence interval from
#' (Student t Distribution with 'Peaks'-2 degree of freedom). Default is 0.975
#' corresponding to a 95\% confidence interval.
#' @param kit character string or vector specifying the analysis kits used
#' to produce the data. If length(kit) != number of groups, kit[1] will be
#' used for all groups.
#' @param debug logical indicating printing debug information.
#' @param ... additional arguments to the \code{\link{filterProfile}} function
#'
#' @return data.frame with with columns 'Sample.Name', 'Kit', 'Group', 'Slope',
#' 'Error', 'Peaks', 'Lower', and 'Upper'.
#'
#' @importFrom utils str
#' @importFrom data.table data.table
#'
#' @export

calculateSlope <- function(data, ref, conf = 0.975, kit = NULL, debug = FALSE, ...) {

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
    print("data")
    print(str(data))
    print("ref")
    print(str(ref))
    print("conf")
    print(conf)
    print("kit")
    print(kit)
  }

  # CHECK DATA ----------------------------------------------------------------

  # Check dataset.
  if (!any(grepl("Sample.Name", names(data)))) {
    stop("'data' must contain a column 'Sample.Name'.",
         call. = TRUE)
  }

  if (!any(grepl("Allele", names(data)))) {
    stop("'data' must contain a column 'Allele'.",
         call. = TRUE)
  }

  # Check if slim format.
  if (sum(grepl("Allele", names(data)) > 1)) {
    stop("'data' must be in 'slim' format.",
         call. = TRUE)
  }

  if (!any(grepl("Height", names(data)))) {
    stop("'data' must contain a column 'Height'.",
         call. = TRUE)
  }

  # Check if slim format.
  if (sum(grepl("Height", names(data)) > 1)) {
    stop("'data' must be in 'slim' format.",
         call. = TRUE)
  }

  # Check if character data.
  if (!is.numeric(data$Height)) {
    message("'Height' must be numeric. 'data' converted.")
    data$Height <- as.numeric(data$Height)
  }

  # Check dataset.
  if (!any(grepl("Sample.Name", names(ref)))) {
    stop("'ref' must contain a column 'Sample.Name'.",
         call. = TRUE)
  }

  if (!any(grepl("Allele", names(ref)))) {
    stop("'ref' must contain a column 'Allele'.",
         call. = TRUE)
  }

  # Check if slim format.
  if (sum(grepl("Allele", names(ref)) > 1)) {
    stop("'ref' must be in 'slim' format.",
         call. = TRUE)
  }

  # Check if character data.
  if (!is.numeric(conf)) {
    stop("'conf' must be numeric.")
  }

  # PREPARE -----------------------------------------------------------------

  # Add a column for kit name.
  data$Kit <- NA

  # Add group if not present.
  if (is.null(data$Group)) {

    data$Group <- "1"

  }

  # Get groups.
  group <- unique(data$Group)

  # Filter known profile.
  data <- filterProfile(data = data, ref = ref,
                        add.missing.loci = FALSE, keep.na = FALSE,
                        debug = debug, ...)

  # Check if a size column exist.
  if (is.null(data$Size)) {

    message("'Size' not in dataset.")

    # Add a size column.
    # data$Size <- NA

    # Check if kit is specified.
    if (is.null(kit)) {

      message("'kit' not specified.")

      kit <- rep(NA, length(group))

      # Loop over groups.
      for (g in seq(along = group)) {

        # Auto detect kit. If multiple matches, use the first.
        kit[g] <- detectKit(data = data[data$Group == group[g], ],
                            debug = debug)[1]

      }

    } else {

      # Check number of groups matches number of given kits.
      if (length(group) != length(kit)) {

        kit <- rep(kit[1], length(group))

      }

    }

    # Create a new dataframe to store result.
    data.new <- data.frame(data[0, ])

    # Loop over groups.
    for (g in seq(along = group)) {

      # Get kit information.
      kitData <- getKit(kit = kit[g])

      # Add size in base pair.
      data.tmp <- addSize(data = data[data$Group == group[g], ],
                          kit = kitData, debug = debug)

      # Add kit.
      data.tmp$Kit <- kit[g]

      if (debug) {
        print(paste("Kit: ", kit[g]))
        print(head(data.tmp))
        print(tail(data.tmp))
      }

      # Combine result.
      data.new <- rbind(data.new, data.tmp)

      message("Added size according to ", kit[g], " for group ", group[g], ".")

    }

    # Overwrite the old data.frame with the new.
    data <- data.new

  }

  # Check if NA in Size column.
  if (any(is.na(data$Size))) {

    row1 <- nrow(data)

    # Remove rows with no size.
    data <- data[!is.na(data$Size), ]

    row2 <- nrow(data)

    message("Removed ", row1 - row2, " rows with Size=NA")

  }

  # Check if 0 in Height column.
  if (any(data$Height == 0)) {

    row1 <- nrow(data)

    # Remove rows with zero height.
    data <- data[!data$Height == 0, ]

    row2 <- nrow(data)

    message("Removed ", row1 - row2, " rows with Height=0")

  }

  # ANALYSE -------------------------------------------------------------------

  # Convert to data table for performance.
  DT <- data.table(data)

  # Calculate slope.
  DT <- DT[, list(Slope = summary(lm(log(Height) ~ Size))$coefficients[2],
                  Error = summary(lm(log(Height) ~ Size))$coefficients[4],
                  Peaks = .N), by = list(Sample.Name, Group, Kit)]

  # Calculate confidence interval.
  DT[, Lower := Slope - Error * qt(conf, Peaks - 2), by = Sample.Name]
  DT[, Upper := Slope + Error * qt(conf, Peaks - 2), by = Sample.Name]

  # Convert back to data.frame.
  res <- as.data.frame(DT)

  # Add attributes to result.
  attr(data, which = "kit") <- kit

  # Update audit trail.
  res <- auditTrail(obj = res, f.call = match.call(), package = "strvalidator")

  return(res)

}
