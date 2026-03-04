
# TODO: use string constants instead of hard coded.
# TODO: add reference dataset for true complete profiles.

#' @title Calculate Result Type
#'
#' @description
#' Calculate the result type for samples.
#'
#' @details
#' Calculates result types for samples in 'data'.
#' Defined types are: 'No result', 'Mixture', 'Partial', and 'Complete'.
#' Subtypes can be defined by parameters.
#' An integer passed to 'threshold' defines a subtype of 'Complete' "Complete profile all peaks >threshold".
#' An integer or vector passed to 'mixture_limits' define subtypes of 'Mixture' "> [mixture_limits] markers".
#' An integer or vector passed to 'partial_limits' define subtypes of 'Partial' "> [partial_limits] peaks".
#' A string with marker names separated by pipe (|) passed to 'marker_subset' and
#'  a string 'subset_name' defines a subtype of 'Partial' "Complete [subset_name]".
#'
#' @param data a data frame containing at least the column 'Sample.Name'.
#' @param kit character string or integer defining the kit.
#' @param add_missing_marker logical, default is TRUE which adds missing markers.
#' @param threshold integer indicating the dropout threshold.
#' @param mixture_limits integer or vector indicating subtypes of 'Mixture'.
#' @param partial_limits integer or vector indicating subtypes of 'Partial'.
#' @param subset_name string naming the subset of 'Complete'.
#' @param marker_subset string with marker names defining the subset of 'Complete'.
#' @param debug logical indicating printing debug information.
#'
#' @return data.frame with columns 'Sample.Name','Type', and 'Subtype'.
#'
#' @export
#'
#' @importFrom utils head
#'

calculate_result_type <- function(data, kit = NULL, add_missing_marker = TRUE,
                                threshold = NULL, mixture_limits = NULL,
                                partial_limits = NULL, subset_name = NA,
                                marker_subset = NULL, debug = FALSE) {
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
    print("Parameters:")
    print("head(data)")
    print(head(data))
    print("threshold")
    print(threshold)
    print("mixture_limits")
    print(mixture_limits)
    print("partial_limits")
    print(partial_limits)
    print("subset_name")
    print(subset_name)
    print("marker_subset")
    print(marker_subset)
  }

  # CHECK DATA ----------------------------------------------------------------

  # Check dataset.
  if (!any(grepl("Sample.Name", names(data)))) {
    stop("'data' must contain a column 'Sample.Name'",
      call. = TRUE
    )
  }

  if (!any(grepl("Marker", names(data)))) {
    stop("'data' must contain a column 'Marker'",
      call. = TRUE
    )
  }
  if (!any(grepl("Allele", names(data)))) {
    stop("'data' must contain a column 'Allele'",
      call. = TRUE
    )
  }

  # Check if slim format.
  if (sum(grepl("Allele", names(data))) > 1) {
    stop("'data' must be in 'slim' format",
      call. = TRUE
    )
  }

  if (add_missing_marker) {
    if (is.null(kit)) {
      stop("'kit' must be provided if 'add_missing_marker' is TRUE",
        call. = TRUE
      )
    } else {
      if (is.na(get_kit(kit = kit, what = "Short.Name"))) {
        stop(paste(
          "'kit' does not exist", "\nAvailable kits:",
          paste(get_kit(), collapse = ", ")
        ), call. = TRUE)
      }
    }
  }

  # PREPARE -------------------------------------------------------------------

  if (add_missing_marker) {
    # Add missing markers to samples.
    markers <- get_kit(kit = kit, what = "Marker")
    data <- add_marker(data = data, marker = markers, ignore_case = TRUE, debug = debug)
  }

  if (!is.numeric(data$Height)) {
    message("'Height' not numeric. Converting to numeric.")
    data$Height <- as.numeric(data$Height)
  }

  # CALCULATE -----------------------------------------------------------------
  # NB! Strings used for classification must be identical to the ones used to
  # create factors.

  # Get sample names.
  sampleNames <- unique(data$Sample.Name)

  # Create result data frame.
  res <- data.frame(matrix(NA, length(sampleNames), 3))
  # Add new column names.
  names(res) <- paste(c("Sample.Name", "Type", "Subtype"))

  # Loop over all samples.
  for (s in seq(along = sampleNames)) {
    # Show progress.
    message(paste("Calculate result type for sample (",
      s, " of ", length(sampleNames), "): ", sampleNames[s],
      sep = ""
    ))

    # Get current sample.
    sampleData <- data[data$Sample == sampleNames[s], ]

    if (debug) {
      print("Current sample data:")
      print(sampleData)
    }

    # Check result type.
    if (all(is.na(sampleData$Allele))) {
      # No result.

      res[s, ] <- c(sampleNames[s], "No result", "No result")
    } else if (max(table(sampleData$Marker)) > 2) {
      # Mixture.

      markers <- length(unique(sampleData$Marker[!is.na(sampleData$Allele)]))
      if (!is.null(mixture_limits)) {
        for (t in rev(seq(along = mixture_limits))) {
          if (markers <= mixture_limits[t]) {
            subtype <- paste("<=", mixture_limits[t], "markers")
          } else if (markers > mixture_limits[length(mixture_limits)]) {
            subtype <- paste(">", mixture_limits[length(mixture_limits)], "markers")
          }
        }
      } else {
        subtype <- paste("Mixture")
      }
      res[s, ] <- c(sampleNames[s], "Mixture", subtype)
    } else if (any(is.na(sampleData$Allele))) {
      # Partial profile.

      alleles <- sum(!is.na(sampleData$Allele))
      if (!is.null(partial_limits)) {
        for (t in rev(seq(along = partial_limits))) {
          if (alleles <= partial_limits[t]) {
            subtype <- paste("<=", partial_limits[t], "peaks")
          } else if (alleles > partial_limits[length(partial_limits)]) {
            subtype <- paste(">", partial_limits[length(partial_limits)], "peaks")
          }
        }
      } else {
        subtype <- paste("Partial")
      }
      res[s, ] <- c(sampleNames[s], "Partial", subtype)

      # Check for subset.
      if (!is.null(marker_subset)) {
        # Subset data.
        selectedMarkers <- grepl(marker_subset, sampleData$Marker)
        if (all(!is.na(sampleData$Allele[selectedMarkers]))) {
          # Full subset profile.
          res[s, ] <- c(sampleNames[s], "Partial", paste("Complete", subset_name))
        }
      }
    } else if (!any(is.na(sampleData$Allele))) {
      # Complete profile.
      res[s, ] <- c(sampleNames[s], "Complete profile", "Complete profile")

      # Check against threshold.
      if (!is.null(threshold) && all(sampleData$Height > threshold)) {
        # Complete profile, all peaks > T.
        res[s, ] <- c(sampleNames[s], "Complete profile", paste("all peaks >", threshold))
      }
    }
  }

  # FACTORS -------------------------------------------------------------------

  # Construct factor levels in correct order.
  # NB! Strings must be identical to the ones used in classification.
  factorLabels <- NULL
  blankLabels <- NULL
  mixtureLabels <- NULL
  partialLabels <- NULL
  completeLabels <- NULL

  factorLabelsSub <- NULL
  blankLabelsSub <- NULL
  mixtureLabelsSub <- NULL
  partialLabelsSub <- NULL
  completeLabelsSub <- NULL

  # Partial Labels.

  if (!is.null(marker_subset)) {
    partialLabelsSub <- c(partialLabelsSub, paste("Complete", subset_name))
  }
  if (!is.null(partial_limits)) {
    for (t in rev(seq(along = partial_limits))) {
      if (t == length(partial_limits)) {
        partialLabelsSub <- c(partialLabelsSub, paste(">", partial_limits[t], "peaks"))
      }
      partialLabelsSub <- c(partialLabelsSub, paste("<=", partial_limits[t], "peaks"))
    }
  }
  partialLabels <- "Partial"
  partialLabelsSub <- c("Partial", partialLabelsSub)

  # Mixture Labels.
  if (!is.null(mixture_limits)) {
    for (t in rev(seq(along = mixture_limits))) {
      if (t == length(mixture_limits)) {
        mixtureLabelsSub <- c(mixtureLabelsSub, paste(">", mixture_limits[t], "markers"))
      }
      mixtureLabelsSub <- c(mixtureLabelsSub, paste("<=", mixture_limits[t], "markers"))
    }
  }
  mixtureLabels <- "Mixture"
  mixtureLabelsSub <- c("Mixture", mixtureLabelsSub)

  # Complete Labels.
  if (!is.null(threshold)) {
    completeLabelsSub <- c(completeLabelsSub, paste("all peaks >", threshold))
  }
  completeLabels <- "Complete profile"
  completeLabelsSub <- c(completeLabelsSub, "Complete profile")

  # Blank Labels.
  blankLabels <- "No result"
  blankLabelsSub <- "No result"

  # All factor labels.
  factorLabels <- c(mixtureLabels, completeLabels, partialLabels, blankLabels)
  factorLabelsSub <- c(mixtureLabelsSub, completeLabelsSub, partialLabelsSub, blankLabelsSub)

  if (debug) {
    print("factorLabels")
    print(factorLabels)
    print("factorLabelsSub")
    print(factorLabelsSub)
  }

  # Assign factors.
  res$Type <- factor(res$Type, levels = factorLabels)
  res$Subtype <- factor(res$Subtype, levels = factorLabelsSub)

  # Add attributes to result.
  attr(res, which = "kit") <- kit

  # Update audit trail.
  res <- audit_trail(obj = res, f_call = match.call(), package = "strvalidator")

  if (debug) {
    print("head(res):")
    print(head(res))
    print(paste("EXIT:", match.call()[[1]]))
  }

  return(res)
}
