################################################################################
# TODO LIST
# TODO: Option to use a minimum RFU for dropped-out alleles (per-locus)?
# TODO: Consider matrix/vectorized implementation for speed.
# TODO: Case-insensitive allele matching and configurable trimming rules.

################################################################################
# CHANGE LOG (last 20 changes)
# 15.10.2025: Rewrote function to accept a unified reference table (ref_profiles_df),
#             corrected drop-in logic, added edge-case reporting and contributor estimate.
# 13.04.2025: Fixed bug when NA alleles resulted in spurious drop-in counts (legacy).
# 07.07.2022: Fixed "...URLs which should use \\doi (with the DOI name only)". (legacy)
# 06.08.2017: Added audit trail. (legacy)
# 30.09.2016: Fixed sample-name matching bug in GUI subsetting. (legacy)
# 29.08.2014: Added check for uniqueness between reference datasets. (legacy)
# 28.08.2014: Fixed bug in drop-out of minor in If 3: AA:AB | (A-B)/(A+B). (legacy)
# 06.07.2014: First version. (legacy)

#' @title Calculate Mixture (summary by sample).
#'
#' @description
#' Summarize mixture characteristics per sample using two reference profiles
#' (major/minor) drawn from a unified reference table. For each mixture sample,
#' the function (i) estimates the number of contributors, (ii) computes the
#' average mixture proportion (`AverageMx`) from peak heights, (iii) reports the
#' percentage of expected minor alleles observed, and (iv) counts putative
#' drop-in alleles. Edge-case signals (dropout loci, off-ladder alleles, and
#' tri-allelic indications) are also reported.
#'
#' @details
#' **Inputs and pairing.**
#' Provide a single reference data frame (`ref_profiles_df`) in “slim” format
#' with at least `Sample.Name`, `Marker`, and `Allele`. The function internally
#' reshapes the references by marker and pairs *two* reference individuals
#' (putatively “major” and “minor”) for each mixture sample by **substring
#' matching** of their `Sample.Name`s inside the mixture’s `Sample.Name`
#' (order independent). In other words, if the reference names are `"A"` and
#' `"B"`, they will be paired with mixture samples whose `Sample.Name` contains
#' both `"A"` and `"B"` (e.g., `"A_B_1"`, `"B_A_run2"`).
#'
#' **Peak filtering.**
#' Only peaks with `Height >= threshold` (RFU) are used in calculations.
#'
#' **Mixture proportion (`AverageMx`).**
#' For each locus with any observed peaks matching either reference, the minor
#' share is computed as:
#' \cr \code{Mx_locus = sum(heights of minor alleles) / sum(heights of (major + minor) alleles)}.
#' The sample’s \code{AverageMx} is the mean of \code{Mx_locus} over loci used.
#' If `ignore_dropout = TRUE`, loci without any observed minor allele(s) are
#' assigned \code{Mx_locus = 0}. If `ignore_dropout = FALSE`, such loci are
#' treated as dropout (excluded from the average and listed in `DropoutLoci`).
#'
#' **Minor profile observation.**
#' For each locus, the expected set of *minor* alleles is the reference minor
#' genotype minus the major genotype (i.e., unshared minor alleles). The
#' percentage reported in `ObservedMinorPercent` is:
#' \cr \code{100 * (sum of observed minor-allele presences) / (sum of expected minor alleles)}.
#'
#' **Drop-in definition.**
#' Drop-in alleles are observed peaks (after trimming/thresholding) that are not
#' among the known alleles from either reference at that locus. NA alleles are
#' ignored (do not count as drop-in). Off-ladder tokens (e.g., `"OL"`) will be
#' counted as drop-in and also listed in `OffLadderAlleles`.
#'
#' **Edge-case signals.**
#' - `DropoutLoci`: loci where minor alleles were expected but not observed
#'   (only populated when `ignore_dropout = FALSE`).
#' - `OffLadderAlleles`: unique observed alleles not present in either reference
#'   (commonly includes `"OL"`).
#' - `TriAllelicLoci`: loci where any paired reference profile shows ≥3 unique
#'   alleles at that marker (true tri-allelic indication for an individual).
#'   Note that >2 distinct alleles in the mixture alone does not imply tri-allelic.
#'
#' **Contributor estimate.**
#' `EstimatedContributors` is a heuristic defined as
#' \code{ceiling(max_observed_alleles_per_locus / 2)} for the sample.
#'
#' **Naming requirements.**
#' Reference `Sample.Name`s must be unique in `ref_profiles_df`. Mixture sample
#' names must contain both reference names (as substrings) to be considered.
#'
#' @param data \code{data.frame} in slim format with at least columns
#'  \code{'Sample.Name'}, \code{'Marker'}, \code{'Allele'}, and \code{'Height'} (RFU).
#' @param ref_profiles_df \code{data.frame} of reference genotypes in slim format
#'  with columns \code{'Sample.Name'}, \code{'Marker'}, \code{'Allele'}. Must contain
#'  at least two distinct reference individuals whose names appear in the mixture
#'  sample names.
#' @param threshold numeric RFU threshold; peaks with \code{Height < threshold}
#'  are ignored. Default: \code{50}.
#' @param ignore_dropout logical; if \code{TRUE}, loci lacking observed minor
#'  alleles contribute \code{Mx_locus = 0} to \code{AverageMx}. If \code{FALSE},
#'  such loci are treated as dropout (excluded from averaging and listed in
#'  \code{DropoutLoci}). Default: \code{TRUE}.
#'
#' @return \code{tibble}/\code{data.frame} with one row per mixture sample and columns:
#' \describe{
#'   \item{Sample}{Mixture sample name.}
#'   \item{EstimatedContributors}{Heuristic estimate of contributor count.}
#'   \item{ObservedMinorPercent}{Percent of expected minor alleles observed (0–100).}
#'   \item{DropinAlleles}{Total count of observed alleles not explained by references.}
#'   \item{AverageMx}{Average minor/(major+minor) height fraction across loci used.}
#'   \item{DropoutLoci}{Comma-separated loci with minor dropout (if \code{ignore_dropout = FALSE}).}
#'   \item{UnhandledLoci}{Comma-separated loci skipped because reference alleles were missing.}
#'   \item{OffLadderAlleles}{Comma-separated unique off-ladder/unexpected allele labels (e.g., "OL").}
#'   \item{TriAllelicLoci}{Comma-separated loci where any paired reference
#'     profile shows ≥3 unique alleles at that marker (true tri-allelic for an
#'     individual). Note: >2 distinct alleles in the mixture alone does not
#'     imply tri-allelic.}
#' }
#'
#' @note
#' - Allele strings are \code{trimmed} and compared as character.
#' - NA alleles are ignored in drop-in counting.
#' - Messages are emitted via \code{message()} for debugging/trace; there is no
#'   \code{debug} parameter in this version.
#'
#' @examples
#' \dontrun{
#' # refs: slim data frame with Sample.Name, Marker, Allele for two individuals
#' # mix:  slim data frame with Sample.Name, Marker, Allele, Height
#' res <- calculate_mixture(
#'   data = mix,
#'   ref_profiles_df = refs,
#'   threshold = 50,
#'   ignore_dropout = TRUE
#' )
#' }
#'
#' @export
#'
#' @references
#' Bright, Jo-Anne, Jnana Turkington, and John Buckleton.
#' "Examination of the Variability in Mixed DNA Profile Parameters for the
#'  Identifiler Multiplex." Forensic Science International: Genetics 4(2), 2010:
#'  111–114. \doi{10.1016/j.fsigen.2009.07.002}

calculate_mixture <- function(data, ref_profiles_df, threshold = 50, ignore_dropout = TRUE) {
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)

  data <- data %>% mutate(Height = suppressWarnings(as.numeric(Height)))
  
  estimate_contributors <- function(observed_alleles) {
    max_alleles <- max(lengths(observed_alleles))
    ceiling(max_alleles / 2)
  }

  detect_edge_cases <- function(alleles, known_alleles) {
    off_ladder <- alleles %>% filter(!Allele %in% known_alleles)
    list(
      off_ladder = na.omit(unique(off_ladder$Allele))
    )
  }

  ref_profiles <- ref_profiles_df %>%
    mutate(Allele = as.character(str_trim(Allele))) %>%
    group_by(Sample.Name, Marker) %>%
    summarise(Alleles = list(Allele), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = Marker, values_from = Alleles) %>%
    split(.$Sample.Name) %>%
    lapply(function(x) x[setdiff(names(x), "Sample.Name")])

  process_sample <- function(sample_id, data, ref_major, ref_minor, threshold, ignore_dropout) {
    sample_data <- data %>%
      filter(Sample.Name == sample_id, Height >= threshold) %>%
      mutate(Allele = as.character(str_trim(Allele)))

    observed_alleles <- sample_data %>%
      group_by(Marker) %>%
      summarise(Alleles = list(unique(Allele)), .groups = "drop")

    estimated_contributors <- estimate_contributors(observed_alleles$Alleles)

    total_minor_alleles <- 0
    total_minor_observed <- 0
    total_dropin <- 0
    mx_values <- list()
    dropout_loci <- character()
    unhandled_loci <- character()
    off_ladder_alleles <- character()
    tri_allelic_loci <- character()

    for (locus in unique(sample_data$Marker)) {
      alleles <- sample_data %>% filter(Marker == locus)
      major_alleles <- ref_major[[locus]]
      minor_alleles <- ref_minor[[locus]]

      if (is.null(major_alleles) || is.null(minor_alleles)) {
        unhandled_loci <- c(unhandled_loci, locus)
        next
      }

      major_alleles <- str_trim(as.character(unlist(major_alleles)))
      minor_alleles <- str_trim(as.character(unlist(minor_alleles)))


      # True tri-allelic indication from references
      if (length(unique(major_alleles)) > 2 || length(unique(minor_alleles)) > 2) {
        tri_allelic_loci <- union(tri_allelic_loci, locus)
      }

      all_alleles <- str_trim(as.character(alleles$Allele))
      all_heights <- alleles$Height

      message("Marker: ", locus)
      message("Major alleles: ", paste(major_alleles, collapse = ", "))
      message("Minor alleles: ", paste(minor_alleles, collapse = ", "))
      message(paste(capture.output(str(all_alleles)), collapse = "\n"))

      known_alleles <- unique(c(major_alleles, minor_alleles))


      edge_info <- detect_edge_cases(alleles, known_alleles)
      if (length(edge_info$off_ladder) > 0) {
        off_ladder_alleles <- union(off_ladder_alleles, edge_info$off_ladder)
      }

      observed_minor <- intersect(minor_alleles, all_alleles)
      total_minor_alleles <- total_minor_alleles + length(minor_alleles)
      total_minor_observed <- total_minor_observed + length(observed_minor)

      if (length(minor_alleles) > 0 && any(all_alleles %in% minor_alleles)) {
        minor_height <- sum(all_heights[all_alleles %in% minor_alleles])
        major_height <- sum(all_heights[all_alleles %in% major_alleles])
        total_height <- minor_height + major_height
        if (total_height > 0) mx_values[[locus]] <- minor_height / total_height
      } else {
        if (!ignore_dropout) {
          dropout_loci <- c(dropout_loci, locus)   # only list when we are honoring dropout
          next
        } else {
          mx_values[[locus]] <- 0
        }
      }

      message("str(all_alleles): ", str(all_alleles))
      message("all_alleles: ", paste(all_alleles, collapse = ", "))
      message("str(known_alleles): ", str(known_alleles))
      message("known_alleles: ", paste(known_alleles, collapse = ", "))


      dropin_alleles <- setdiff(na.omit(all_alleles), known_alleles)
      message("Drop-in alleles: ", paste(dropin_alleles, collapse = ", "))
      total_dropin <- total_dropin + length(dropin_alleles)
    }

    vals <- unlist(mx_values)
    avg_mx <- if (length(vals)) mean(vals) else NA_real_

    dplyr::tibble(
      Sample = sample_id,
      EstimatedContributors = estimated_contributors,
      ObservedMinorPercent = ifelse(total_minor_alleles > 0,
                                    total_minor_observed / total_minor_alleles * 100, NA_real_),
      DropinAlleles = total_dropin,
      AverageMx = avg_mx,
      DropoutLoci = paste(dropout_loci, collapse = ", "),
      UnhandledLoci = paste(unhandled_loci, collapse = ", "),
      OffLadderAlleles = paste(off_ladder_alleles, collapse = ", "),
      TriAllelicLoci = paste(tri_allelic_loci, collapse = ", ")
    )
  }

  all_refs <- names(ref_profiles)
  all_samples <- unique(data$Sample.Name)
  result_list <- list()

  for (ref1 in all_refs) {
    matched_samples <- all_samples[str_detect(all_samples, fixed(ref1))]
    other_refs <- setdiff(all_refs, ref1)

    for (ref2 in other_refs) {
      relevant_samples <- matched_samples[str_detect(matched_samples, fixed(ref2))]
      for (s in relevant_samples) {
        temp_mx <- process_sample(s, data, ref_profiles[[ref1]], ref_profiles[[ref2]], threshold, TRUE)$AverageMx
        if (!is.na(temp_mx) && temp_mx < 0.5) {
          result_list[[s]] <- process_sample(s, data, ref_profiles[[ref1]], ref_profiles[[ref2]], threshold, ignore_dropout)
        } else {
          result_list[[s]] <- process_sample(s, data, ref_profiles[[ref2]], ref_profiles[[ref1]], threshold, ignore_dropout)
        }
      }
    }
  }

  # If no result, return empty tibble.
  if (length(result_list) == 0) {
    return(tibble(
      Sample = character(),
      EstimatedContributors = integer(),
      ObservedMinorPercent = numeric(),
      DropinAlleles = integer(),
      AverageMx = numeric(),
      DropoutLoci = character(),
      UnhandledLoci = character(),
      OffLadderAlleles = character(),
      TriAllelicLoci = character()
    ))
  }
  
  return(dplyr::bind_rows(result_list))
  
}
