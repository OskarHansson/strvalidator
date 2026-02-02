#' @title Calculate Concordance
#'
#' @description
#' Calculates concordance and discordance for DNA profiles across multiple
#' datasets, optionally including comparison against a known reference profile.
#'
#' @details
#' Takes a list of datasets as input. It is assumed that each unique sample name
#' represents a result originating from the same source DNA and thus is expected
#' to give identical DNA profiles. The function first compares profiles for each
#' sample across datasets and lists discordant results, then performs pair-wise
#' comparisons to compile a concordance table.
#'
#' If \code{known_profile} is provided, its alleles (for each marker) are
#' included in the concordance evaluation as an additional reference column.
#' Missing markers in the known profile are treated as \code{[MISSING MARKER]}.
#' This allows the user to quickly identify samples or datasets that deviate
#' from a specified known profile.
#'
#' Typing and PCR artefacts (spikes, off-ladder peaks, stutters, etc.) should be
#' removed before analysis. It is expected that the full set of markers used in
#' each dataset are present for each sample (a missing marker is counted as
#' discordant).
#'
#' @param data A list of data frames in 'slim' format, each with at least the
#' columns \code{Sample.Name}, \code{Marker}, and \code{Allele}.
#' @param kit_name Character vector of DNA typing kit names in the same order
#' and length as the datasets in \code{data}. Defaults to auto-numbered kits if
#' not provided.
#' @param no_marker Character string used when a marker is missing.
#' @param no_sample Character string used when a sample is missing.
#' @param delimiter Character used to separate alleles in a genotype (default:
#' comma).
#' @param list_all Logical; if \code{TRUE}, include samples with missing data
#' in results.
#' @param known_profile Optional data frame containing a reference DNA profile
#' with columns \code{Marker} and \code{Allele}. Each marker in this table is
#' treated as a known reference and included in the concordance comparison.
#' @param debug Logical; if \code{TRUE}, print detailed diagnostic messages.
#'
#' @return A list with two data frames:
#'   \enumerate{
#'     \item \strong{Discordance table:} samples and markers showing differences
#'     across datasets.
#'     \item \strong{Concordance summary:} pair-wise comparison table with
#'     discordance counts and rates.
#'   }
#'
#' @seealso \code{\link{combn}}, \code{\link{grep}}
#' @aliases calculateConcordance
#' @importFrom utils str combn
#' @export
calculate_concordance <- function(data, kit_name = NA, 
                                  no_marker = "[MISSING MARKER]",
                                  no_sample = "[MISSING SAMPLE]",
                                  delimiter = ",", list_all = FALSE,
                                  known_profile = NULL, debug = FALSE) {
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
    print("Parameters:")
    print("data")
    print(str(data))
    print("kit_name")
    print(kit_name)
    print("no_marker")
    print(no_marker)
    print("no_sample")
    print(no_sample)
    print("delimiter")
    print(delimiter)
  }

  # CHECK DATA ----------------------------------------------------------------

  if (no_marker == no_sample) {
    stop("'no_marker' and 'no_sample' must be distinct values.")
  }

  # Check each dataset in list.
  for (d in seq(along = data)) {
    if (!"Sample.Name" %in% names(data[[d]])) {
      stop("All datasets in 'data' must contain a column 'Sample.Name'.",
        call. = TRUE
      )
    }
    # Check dataset.
    if (!"Marker" %in% names(data[[d]])) {
      stop("All datasets in 'data' must contain a column 'Marker'.",
        call. = TRUE
      )
    }
    # Check dataset.
    if (!"Allele" %in% names(data[[d]])) {
      stop("All datasets in 'data' must contain a column 'Allele'.",
        call. = TRUE
      )
    }
    # Check if slim format.
    if (sum(grepl("Allele", names(data[[d]]))) > 1) {
      stop("'data' must be in 'slim' format.",
        call. = TRUE
      )
    }
  }

  # Check kit_name vector.
  if (all(is.na(kit_name))) {
    # Create default names.
    kit_name <- paste("Kit", seq(along = data), sep = ".")
  } else if (length(kit_name) != length(data)) {
    stop("'kit_name' must be of equal length as number of datasets.",
      call. = TRUE
    )
  }

  # Check parameter.
  if (!is.character(no_sample)) {
    stop("'no_sample' must be of type character.",
      call. = TRUE
    )
  }

  # Check parameter.
  if (!is.character(no_marker)) {
    stop("'no_marker' must be of type character.",
      call. = TRUE
    )
  }

  # Check parameter.
  if (!is.character(delimiter)) {
    stop("'delimiter' must be of type character.",
      call. = TRUE
    )
  }

  if (!is.null(known_profile)) {
    if (!all(c("Marker", "Allele") %in% names(known_profile))) {
      stop("'known_profile' must contain columns 'Marker' and 'Allele'.")
    }
  }

  # PREPARE -----------------------------------------------------------------

  # Initiate variables.
  sample_list <- list() # List for logical sample vectors.
  marker_list <- list() # List for logical marker vectors.
  res_allele_list <- list() # List for result.
  res_info_list <- list() # List for result.
  sample_names <- NULL # Unique sample names.
  marker_names <- NULL # Unique marker names.


  if (!is.null(known_profile)) {
    # Get marker names from known profile if provided.
    marker_names <- unique(c(marker_names, known_profile$Marker))
  } else {
    # Get marker names from all datasets.

    for (d in seq(along = data)) {
      marker_names <- unique(c(marker_names, data[[d]]$Marker))
    }
  }

  # Get all unique sample names.
  for (d in seq(along = data)) {
    sample_names <- unique(c(sample_names, data[[d]]$Sample.Name))
  }

  if (debug) {
    print("sample_names:")
    print(sample_names)
    print("marker_names:")
    print(marker_names)
  }

  # Add logical vectors for each dataset.
  for (d in seq(along = data)) {
    # Add pre-allocated vectors.
    sample_list[[d]] <- vector(mode = "logical", length = length(sample_names))
    marker_list[[d]] <- vector(mode = "logical", length = length(marker_names))
  }

  # CALCULATE -----------------------------------------------------------------
  # 1) A list of all discordant results across datasets.

  # Loop over all sample names.
  for (s in seq(along = sample_names)) {
    # Progress.
    message(paste("Calculate concordance for: ", sample_names[s],
      " (", s, " of ", length(sample_names), ").",
      sep = ""
    ))

    # Loop over all marker names.
    for (m in seq(along = marker_names)) {
      # Create list.
      allele_set <- list()

      # Loop over all data sets (start with 1 to handle only 1 dataset in list).
      for (d in seq(along = data)) {
        # Check if sample and add logical value.
        if (any(data[[d]]$Sample.Name == sample_names[s])) {
          sample_list[[d]][s] <- TRUE
        } else {
          sample_list[[d]][s] <- FALSE
        }

        # Check if marker and add logical value.
        if (any(data[[d]]$Marker == marker_names[m])) {
          marker_list[[d]][m] <- TRUE
        } else {
          marker_list[[d]][m] <- FALSE
        }

        # Check if marker.
        if (!(marker_list[[d]][m] & sample_list[[d]][s])) {
          if (!sample_list[[d]][s]) {
            # Sample does not exist.
            allele_set[[d]] <- no_sample
          } else if (!marker_list[[d]][m]) {
            # Marker does not exist.
            allele_set[[d]] <- no_marker
          }
        } else {
          # Get current alleles from dataset.
          allele_set[[d]] <- data[[d]][data[[d]]$Sample.Name == sample_names[s] & data[[d]]$Marker == marker_names[m], "Allele"]
        }
      }

      if (debug) {
        print("allele_set#1:")
        print(allele_set)
      }

      # Add known profile allele.
      if (!is.null(known_profile)) {
        if (any(known_profile$Marker == marker_names[m])) {
          allele_set[[length(data) + 1]] <- known_profile[known_profile$Marker == marker_names[m], "Allele"]
        } else {
          allele_set[[length(data) + 1]] <- no_marker
        }
      }

      if (debug) {
        print("allele_set#2:")
        print(allele_set)
      }

      if (list_all) {
        # Check for discordant results excluding if only difference is missing
        # marker (but including missing sample).
        tmp <- allele_set[allele_set != no_marker]
        discordance <- !length(unique(tmp)) == 1
      } else {
        # Check for discordant results excluding differences caused by missing
        # sample or missing marker.
        tmp <- allele_set[allele_set != no_sample & allele_set != no_marker]
        discordance <- !length(unique(tmp)) == 1
      }

      if (discordance) {
        # Create result vectors.
        res_allele_vec <- NULL
        res_info_vec <- NULL

        # Loop through all datasets.
        for (d in seq(along = data)) {
          res_allele_vec <- c(res_allele_vec, paste(allele_set[[d]], collapse = delimiter))
        }

        # Add data to result vector.
        res_info_vec <- c(sample_names[s], marker_names[m])

        # Add current marker to result list.
        res_allele_list[[length(res_allele_list) + 1]] <- res_allele_vec
        res_info_list[[length(res_info_list) + 1]] <- res_info_vec
      }
    }
  }


  # Convert to matrix.
  if (length(res_allele_list) > 0) {
    res_allele_m <- matrix(unlist(res_allele_list), byrow = TRUE, nrow = length(res_allele_list))
    res_info_m <- matrix(unlist(res_info_list), byrow = TRUE, nrow = length(res_info_list))

    # Make a data.frame:
    res1 <- data.frame(cbind(res_info_m, res_allele_m), stringsAsFactors = FALSE)
    names(res1) <- c("Sample.Name", "Marker", kit_name)
  } else {
    # Make a data.frame:
    res1 <- data.frame("NO DISCORDANCE", stringsAsFactors = FALSE)
  }

  # Update audit trail.
  res1 <- audit_trail(obj = res1, f_call = match.call(), package = "strvalidator")

  # CALCULATE -----------------------------------------------------------------
  # 2) A pair-wise comparison.

  # Make pair-wise combinations.
  kit_comb <- combn(kit_name, 2)
  i_comb <- combn(seq(along = kit_name), 2)
  n_comb <- ncol(kit_comb)

  # Initiate vectors.
  compare_kit <- as.vector(mode = "any", n_comb)
  common_samples <- as.vector(mode = "any", n_comb)
  common_loci <- as.vector(mode = "any", n_comb)
  alleles_tested <- as.vector(mode = "any", n_comb)
  discordant_alleles <- as.vector(mode = "any", n_comb)
  concordance_rate <- as.vector(mode = "any", n_comb)

  # Loop through all combinations.
  for (i in 1:n_comb) {
    # Current combination.
    compare_kit[i] <- paste(kit_comb[, i], collapse = " vs. ")

    # Progress.
    message(paste("Compare ", compare_kit[i],
      " (", i, " of ", n_comb, ").",
      sep = ""
    ))

    # Number of common samples.
    common_samples[i] <- sum(sample_list[[i_comb[1, i]]] & sample_list[[i_comb[2, i]]])

    # Number of common loci.
    common_loci[i] <- sum(marker_list[[i_comb[1, i]]] & marker_list[[i_comb[2, i]]])

    # Number of alleles tested.
    alleles_tested[i] <- 2 * common_samples[i] * common_loci[i]

    # Find number of discordant results.

    # Initiate variable.
    sum_discordances <- 0

    # Loop through each row of dscordant result.
    for (r in seq(along = res_allele_list)) {
      # Get alleles for current kits.
      k1 <- res_allele_list[[r]][i_comb[1, i]]
      k2 <- res_allele_list[[r]][i_comb[2, i]]

      # Split into alleles.
      k1 <- strsplit(k1, delimiter)
      k2 <- strsplit(k2, delimiter)

      # Only add if sample...
      if (!(no_sample %in% k1 | no_sample %in% k2)) {
        # ...and marker exist in both datasets.
        if (!(no_marker %in% k1 | no_marker %in% k2)) {
          # Compare alleles and count differences.
          sum_discordances <- sum_discordances + sum(!k1 %in% k2)
        }
      }
    }

    # Number of discrodances.
    discordant_alleles[i] <- sum_discordances

    # Concordance rate.
    concordance_rate[i] <- 100 * (alleles_tested[i] - discordant_alleles[i]) / alleles_tested[i]
  }

  # Create dataframe.
  res2 <- data.frame(
    Kits = compare_kit,
    Samples = common_samples,
    Loci = common_loci,
    Alleles = alleles_tested,
    Discordances = discordant_alleles,
    Concordance = concordance_rate,
    stringsAsFactors = FALSE
  )

  # Update audit trail.
  res2 <- audit_trail(obj = res2, f_call = match.call(), package = "strvalidator")

  # Return list of the two dataframes.
  res <- list(res1, res2)

  if (debug) {
    print(paste("EXIT:", match.call()[[1]]))
  }

  # Return result.
  return(res)
}

################################################################################
#' @rdname calculate_concordance
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_concordance()] instead.
################################################################################

calculateConcordance <- function(data, 
                                 kit.name = NA, 
                                 no.marker = "[MISSING MARKER]",
                                 no.sample = "[MISSING SAMPLE]",
                                 delimiter = ",",
                                 list.all = FALSE,
                                 known.profile = NULL, 
                                 debug = FALSE,
                                 ...) {
  
  .Deprecated("calculate_concordance", package = "strvalidator")
  
  # Remap arguments
  calculate_concordance(
    data = data,
    kit_name = kit.name,
    no_marker = no.marker,
    no_sample = no.sample,
    delimiter = delimiter,
    list_all = list.all,
    known_profile = known.profile,
    debug = debug,
    ...
  )
}

