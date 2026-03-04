#' @title Calculate Peak Height.
#'
#' @description
#' Calculate peak height metrics for samples.
#'
#' @details
#' Calculates the total peak height (TPH), and number of observed peaks (Peaks),
#' for each sample by default. If a reference dataset is provided average peak
#' height (H), and profile proportion (Proportion) are calculated.
#'
#' H is calculated according to the formula (references [1][2]):
#' \eqn{H = sum(peak heights)/(n[het] + 2n[hom]}
#' Where:
#' n[het] = number of observed heterozygous alleles
#' n[hom] = number of observed homozygous alleles
#'
#' Important: The above formula has a drawback that when many alleles have
#' dropped out, i.e. when only few alleles are detected, H can be overestimated.
#' For example, if there are only 1 (homozygote) peak observed in the profile,
#' with a height of 100 RFU, then H=100 RFU. This means that the value of H will
#' always be between half the analytical threshold (AT/2) and the peak height of
#' the observed allele (if only one). For this reason Tvedebrink et al. actually
#' modified the estimate to take the number of expected alleles into account
#' when estimating the expected peak height (reference [3]). Basically, they adjust
#' the estimated peak height for the fact that they know how many alleles that
#' fall below the AT, such that the expected peak height could be estimated lower
#' than AT. In addition, they account for degradation using a log-linear
#' relationship on peak heights and fragment length.
#'
#' Tip: If it is known that all expected peaks are observed and no unexpected
#' peaks are present, the dataset can be used as a reference for itself.
#'
#' Note: If a reference dataset is provided the known alleles will be extracted
#' from the dataset.
#'
#' @param data data.frame with at least columns 'Sample.Name' and 'Height'.
#' @param ref data.frame with at least columns 'Sample.Name' and 'Allele'.
#' @param na_replace replaces NA values in the final result.
#' @param exclude character vector (case sensitive) e.g. "OL" excludes rows with
#'  "OL" in the 'Allele' column (not necessary when a reference dataset is provided).
#' @param add logical default is TRUE which will add or overwrite columns
#' 'TPH', 'Peaks', 'H', and 'Proportion' in the provided 'data'.
#' @param sex_rm logical, default FALSE to include sex markers in the analysis.
#' @param qs_rm logical, default TRUE to exclude quality sensors from the analysis.
#' @param kit character, required if sex_rm=TRUE or qs_rm=TRUE to define the kit.
#' @param ignore_case logical TRUE ignores case in sample name matching.
#' @param exact logical TRUE for exact sample name matching.
#' @param word logical TRUE to add word boundaries to sample name matching.
#' @param debug logical indicating printing debug information.
#'
#' @return data.frame with with at least columns 'Sample.Name', 'TPH', and 'Peaks'.
#'
#' @export
#'
#' @references
#' [1] Torben Tvedebrink, Poul Svante Eriksen, Helle Smidt Mogensen, Niels Morling,
#'  Evaluating the weight of evidence by using quantitative short tandem repeat data in DNA mixtures
#'  Journal of the Royal Statistical Society: Series C (Applied Statistics),
#'  Volume 59, Issue 5, 2010,
#'  Pages 855-874, 10.1111/j.1467-9876.2010.00722.x.
#' \doi{10.1111/j.1467-9876.2010.00722.x}
#'
#' @references
#' [2] Torben Tvedebrink, Helle Smidt Mogensen, Maria Charlotte Stene, Niels Morling,
#'  Performance of two 17 locus forensic identification STR kits-Applied Biosystems's AmpFlSTR NGMSElect and Promega's PowerPlex ESI17 kits
#'  Forensic Science International: Genetics,
#'  Volume 6, Issue 5, 2012,
#'  Pages 523-531, 10.1016/j.fsigen.2011.12.006.
#' \doi{10.1016/j.fsigen.2011.12.006}
#'
#' @references
#' [3] Torben Tvedebrink, Maria Asplund, Poul Svante Eriksen, Helle Smidt Mogensen, Niels Morling,
#'  Estimating drop-out probabilities of STR alleles accounting for stutters, detection threshold truncation and degradation
#'  Forensic Science International: Genetics Supplement Series,
#'  Volume 4, Issue 1, 2013,
#'  Pages e51-e52, 10.1016/j.fsigss.2013.10.026.
#' \doi{10.1016/j.fsigss.2013.10.026}
#'
#' @importFrom utils str
#' @importFrom data.table data.table :=


calculate_height <- function(data, ref = NULL, na_replace = NULL, add = TRUE, exclude = NULL,
                            sex_rm = FALSE, qs_rm = FALSE, kit = NULL, ignore_case = TRUE,
                            exact = FALSE, word = FALSE, debug = FALSE) {
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # CHECK DATA ----------------------------------------------------------------

  # Check dataset.
  if (!any(grepl("Sample.Name", names(data)))) {
    stop("'data' must contain a column 'Sample.Name'.", call. = TRUE)
  }

  if (!any(grepl("Height", names(data)))) {
    stop("'data' must contain a column 'Height'.", call. = TRUE)
  }

  # Check if slim format.
  if (sum(grepl("Height", names(data)) > 1)) {
    stop("'data' must be in 'slim' format.", call. = TRUE)
  }

  # Check ref.
  if (!is.null(ref)) {
    # Check dataset.
    if (!any(grepl("Sample.Name", names(ref)))) {
      stop("'ref' must contain a column 'Sample.Name'.", call. = TRUE)
    }

    if (!any(grepl("Allele", names(ref)))) {
      stop("'ref' must contain a column 'Allele'.", call. = TRUE)
    }

    # Check if slim format.
    if (sum(grepl("Allele", names(ref)) > 1)) {
      stop("'ref' must be in 'slim' format.", call. = TRUE)
    }
  }

  # Check na.
  if (length(na_replace) > 1) {
    stop("'na_replace' must be of length 1.", call. = TRUE)
  }

  # Check logical arguments.
  if (!is.logical(add)) {
    stop("'add' must be logical.", call. = TRUE)
  }

  if (!is.logical(sex_rm)) {
    stop("'sex_rm' must logical.", call. = TRUE)
  }

  if (!is.logical(qs_rm)) {
    stop("'qs_rm' must be logical.", call. = TRUE)
  }

  # Check dependencies.
  if (sex_rm) {
    if (is.null(kit)) {
      stop("'kit' can't be NULL if sex_rm=TRUE.")
    }
  }
  if (qs_rm) {
    if (is.null(kit)) {
      stop("'kit' can't be NULL if qs_rm=TRUE.")
    }
  }

  # PREPARE -----------------------------------------------------------------

  # Check if numeric data.
  if (!is.numeric(data$Height)) {
    # Convert to numeric.
    data$Height <- as.numeric(data$Height)

    message("The column 'Height' was converted to numeric.")
  }

  if (!is.null(exclude)) {
    message("Removing excluded alleles from dataset:")

    for (e in seq(along = exclude)) {
      # Remove excluded alleles, accept NA values (or will result in all NA for that row).
      tmp1 <- nrow(data)
      data <- data[data$Allele != exclude[e] | is.na(data$Allele), ]
      tmp2 <- nrow(data)
      message("Removed ", tmp1 - tmp2, " rows with Allele=", exclude[e], ".")
    }
  }

  # Check if reference dataset was provided.
  if (!is.null(ref)) {
    # Filter dataset.
    message("Extracting known alleles from dataset...")
    data <- filter_profile(
      data = data, ref = ref,
      add_missing_loci = FALSE, keep_na = TRUE, invert = FALSE,
      ignore_case = ignore_case, exact = exact, word = word,
      sex_rm = sex_rm, qs_rm = qs_rm, kit = kit, debug = debug
    )

    # Remove sex markers and quality sensors from reference dataset.
    if (sex_rm || qs_rm) {
      message("Removing gender markers and/or quality sensors from reference dataset...")
      ref <- filter_profile(
        data = ref, filter_allele = FALSE,
        sex_rm = sex_rm, qs_rm = qs_rm, kit = kit,
        debug = debug
      )
    }

    # Check if missing alleles (Y markers in female profiles.)
    if (any(is.na(ref$Allele))) {
      # Remove any row with Allele=NA.

      tmp1 <- nrow(ref)
      ref <- ref[!is.na(ref$Allele), ]
      tmp2 <- nrow(ref)
      message("Removed ", tmp1 - tmp2, " rows with Allele=NA in reference dataset.")
    }

    if (!"Copies" %in% names(ref)) {
      # Add
      ref <- calculate_copies(data = ref)
      message("Number of allele copies added to reference dataset.")
    }

    # Convert to data.table and calculate number of allele copies and expected peaks.
    DTref <- data.table(ref)
    # This code is required to handle homozygotes with double notation.
    DTref <- DTref[, list(Copies = unique(Copies)),
      by = list(Sample.Name, Marker, Allele)
    ]
    # Calculate then number of expected peaks.
    DTref[, Expected := .N, by = list(Sample.Name)]

    # Add to dataset.
    data <- add_data(
      data = data, new_data = DTref,
      by_col = "Sample.Name", then_by_col = "Marker",
      what = c("Copies", "Expected"), exact = exact,
      debug = debug
    )
    message("Expected number of alleles added to dataset.")
  } else {
    message("Reference dataset not provided.")

    # Filter quality sensors and sex markers.
    data <- filter_profile(
      data = data, ref = NULL,
      add_missing_loci = FALSE, keep_na = TRUE, invert = FALSE,
      ignore_case = ignore_case, exact = exact, word = word,
      sex_rm = sex_rm, qs_rm = qs_rm, kit = kit,
      filter_allele = FALSE, debug = debug
    )
  }

  # CALCULATE -----------------------------------------------------------------

  # Convert to data.table and calculate metrics.
  DT <- data.table(data)

  if (add & nrow(DT) > 0) {
    # Calculate and add to dataset (repeat over all rows in sample).

    # Calculate total peak height for each sample.
    DT[, TPH := sum(Height, na.rm = TRUE), by = list(Sample.Name)]

    # Calculate number of observed peaks for each sample.
    DT[, Peaks := sum(!is.na(Height)), by = list(Sample.Name)]

    if ("Copies" %in% names(DT)) {
      # Calculate number of observed allele copies for each sample.
      DT[, N.Alleles := sum(Copies[!is.na(Height)]), by = list(Sample.Name)]
    } else {
      message("A column 'Copies' was not found in 'data'.")
      message("Number of observed allele copies cannot be calculated")
      message("Provide a reference dataset to enable calculation of 'N.Alleles'.")
    }

    if ("N.Alleles" %in% names(DT)) {
      # Calculate average peak height for each sample.
      DT[, H := TPH / N.Alleles, by = list(Sample.Name)]
    } else {
      message("A column 'N.Alleles' was not found in 'data'.")
      message("Average peak height cannot be calculated.")
      message("Provide a reference dataset to enable calculation of 'H'.")
    }

    if ("Expected" %in% names(DT)) {
      # Calculate proportion observed profile for each sample.
      DT[, Proportion := Peaks / Expected, by = list(Sample.Name)]
    } else {
      message("A column 'Expected' was not found in 'data'.")
      message("Profile proportion cannot be calculated.")
      message("Provide a reference dataset to enable calculation of 'Expected'.")
    }

    # Assign to result.
    res <- DT
  } else if (!add & nrow(DT) > 0) {
    # Calculate per sample in a new dataset.

    if (!is.null(ref)) {
      # Calculate total peak height for each sample.
      DT[, TPH := sum(Height, na.rm = TRUE), by = list(Sample.Name)]

      # Calculate number of observed peaks for each sample.
      DT[, Peaks := sum(!is.na(Height)), by = list(Sample.Name)]

      # Calculate number of observed peaks for each sample.
      DT[, N.Alleles := sum(Copies[!is.na(Height)]), by = list(Sample.Name)]

      # Calculate total and average peak height, number of peaks,
      # and profile proportion for each sample.
      res <- DT[, list(
        TPH = unique(TPH),
        H = unique(TPH) / unique(N.Alleles),
        Peaks = unique(Peaks),
        Expected = unique(Expected),
        Proportion = unique(Peaks) / unique(Expected)
      ),
      by = list(Sample.Name)
      ]
    } else {
      # Calculate total peak height and number of peaks for each sample.
      res <- DT[, list(
        TPH = sum(Height, na.rm = TRUE),
        Peaks = sum(!is.na(Height))
      ),
      by = list(Sample.Name)
      ]

      message("Average peak height and profile proportion will not be calculated.")
      message("Provide a reference dataset to enable calculation.")
    }
  } else if (!nrow(DT) > 0) {
    message("Dataset is empty. Returning NULL")
    res <- NULL
  } else {
    # This should not happen.
    message("There was an unexpected error")
  }

  # Convert to data.frame to avoid unexpected results in other functions.
  res <- as.data.frame(res)

  # FINALIZE ------------------------------------------------------------------

  # Replace NA:s
  if (!is.null(na_replace)) {
    # Check if NA:s and change to 'na_replace'.

    if ("TPH" %in% names(res)) {
      if (any(is.na(res$TPH))) {
        n <- sum(is.na(res$TPH))
        res[is.na(res$TPH), ]$TPH <- na_replace
        message("Replaced ", n, " NA's in 'TPH' with '", na_replace, "'.")
      }
    }

    if ("Peaks" %in% names(res)) {
      if (any(is.na(res$Peaks))) {
        n <- sum(is.na(res$Peaks))
        res[is.na(res$Peaks), ]$Peaks <- na_replace
        message("Replaced ", n, " NA's in 'Peaks' with '", na_replace, "'.")
      }
    }

    if ("H" %in% names(res)) {
      if (any(is.na(res$H))) {
        n <- sum(is.na(res$H))
        res[is.na(res$H), ]$H <- na_replace
        message("Replaced ", n, " NA's in 'H' with '", na_replace, "'.")
      }
    }

    if ("Proportion" %in% names(res)) {
      if (any(is.na(res$Proportion))) {
        n <- sum(is.na(res$Proportion))
        res[is.na(res$Proportion), ]$Proportion <- na_replace
        message("Replaced ", n, " NA's in 'Proportion' with '", na_replace, "'.")
      }
    }
  }

  # Add attributes to result.
  attr(res, which = "kit") <- kit

  # Update audit trail.
  res <- audit_trail(obj = res, f_call = match.call(), package = "strvalidator")

  # Return result.
  return(res)
}
