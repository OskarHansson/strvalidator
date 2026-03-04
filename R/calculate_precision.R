# Internal helper: population standard deviation (Excel STDEV.P)
#' @noRd
sd_pop <- function(x, na_rm = FALSE) {
  if (na_rm) x <- x[!is.na(x)]
  n <- length(x)
  if (n <= 1) return(NA_real_)
  m <- mean(x)
  sqrt(sum((x - m)^2) / n)
}

#' @title Calculate Precision
#' 
#' @description
#' This function computes sizing precision (within-injection repeatability) and
#' reproducibility (across-injection variability) for STR alleles.
#'
#' Measured allele sizes are compared against expected allele sizes from the
#' kit specification (e.g. using `get_kit(kit, what = "Size")`). The function
#' optionally includes or excludes the true allelic ladder wells.
#' 
#' @details
#' This function computes STR sizing precision and reproducibility metrics
#' from GeneMapper export data using descriptive (population-based) statistics.
#'
#' Precision (repeatability) is defined as the variation of allele size
#' measurements within a single injection, across capillaries (i.e. across 
#' expected allele observations). For each injection (identified by Run.Name), 
#' precision is calculated as the population standard deviation of fragment 
#' sizes for a given marker and allele, using sample wells only.
#'
#' Reproducibility is defined as the variation of allele size measurements
#' between separate injections performed within the same analytical batch.
#' Optionally (recommended in reports), long-term reproducibility
#' (between analytical days or reagent lots) may be stated as not assessed
#' when only within-batch injections are included.
#' It is calculated as the population standard deviation of injection-wise mean
#' allele sizes across replicate injections. This metric reflects short-term, 
#' between-injection variability and does not represent long-term day-to-day 
#' reproducibility unless injections were performed on separate days and 
#' combined deliberately.
#'
#' Population standard deviation is used for both metrics because the purpose is
#' to describe the observed performance of a fully measured analytical system
#' (all capillaries and all planned injections), rather than to estimate variance
#' from a random sample.
#' 
#' One allele observation is expected per capillary for each allele.
#'
#' Allelic ladder wells are excluded from sample-based precision and
#' reproducibility calculations by default, as the ladder serves as the sizing
#' reference rather than a test sample.
#'
#' To avoid systematic exclusion of the same capillary across injections, it is
#' recommended that the ladder be placed in different capillaries across
#' replicate injections. With this design, most capillaries contribute to all
#' injections, while caps carrying the ladder contribute to fewer injections by
#' design.
#'
#' When data from multiple analytical days or batches are combined, the
#' reproducibility metric reflects both short-term injection variability and any
#' additional day- or batch-related effects. Such combined analyses should be
#' explicitly described as overall or multi-day reproducibility.
#'
#' Off-ladder alleles (e.g. \code{"OL"}) are excluded automatically because no
#' expected size is defined for them in \code{kit_sizes}. If such exclusions occur,
#' one or more expected allele observations are missing, and the calculated
#' precision and reproducibility metrics do not represent the true sizing
#' performance for that marker and allele. The function therefore issues a warning
#' when expected allele observations are missing. In such cases, allele calls
#' should be reviewed and corrected (e.g. manual assignment or bin adjustment)
#' before re-running the analysis.
#' 
#' @param data A data.frame containing GeneMapper export data. Must include:
#'   `Run.Name`, `Cap`, `Sample.Type`, `Marker`, `Allele`, and `Size`.
#' @param kit_sizes Data frame with columns `Marker`, `Allele`, and `Size`
#'   giving the expected allele sizes (e.g. from `getKit`).
#' @param plate_id Optional string identifier for the dataset.
#'   Default = "Plate1".
#' @param include_ladder Logical. If TRUE, both true allelic ladder
#'   wells and sample wells are included in the precision calculations.
#'   If FALSE (default), rows with Sample Type matching
#'   "allelic ladder" (case insensitive) are removed first.
#'   
#' @return A list with two tidy data frames:
#' \describe{
#'   \item{within}{Per-injection metrics per marker and allele. Columns include:
#'     Plate, Run.Name, Marker, Allele,
#'     N.Expected.Alleles, N.Observed.Alleles, N.Missing.Alleles, Obs.Complete,
#'     Mean.Size, SD, Min.Size, Max.Size, Mean.Dev, Min.Dev, Max.Dev.}
#'   \item{across}{Across-injection metrics per marker and allele, computed from
#'     injection-wise Mean.Size. Columns include:
#'     Plate, Marker, Allele, N.Runs, Mean.Size.Mean, Mean.Size.SD, 
#'     Mean.Size.Min, Mean.Size.Max.}
#' }
#'
#' @examples
#' \dontrun{
#' gm <- read.delim("Plate1_GMexport.txt", check.names = FALSE)
#' kit <- get_kit("Fusion 6C", what = "Size")
#' res <- calculate_precision(gm, kit)
#' head(res$within)
#' head(res$across)
#' }
#'
#' @export
calculate_precision <- function(data,
                                kit_sizes,
                                plate_id = "Plate1",
                                include_ladder = FALSE) {
  
  # ---- 1. Ensure allele column are all character ----------------------------
  if ("Allele" %in% names(data)) data[["Allele"]] <- as.character(data[["Allele"]])
  
  # ---- 2. Minimal long format (expects slimmed input) -----------------------
  # long_meta keeps rows even if Size is NA (important for expected counts and OL detection)
  long_meta <- data %>%
    dplyr::select(.data$Run.Name, .data$Cap, .data$Sample.Type, .data$Marker,
                  .data$Allele, .data$Size) %>%
    dplyr::mutate(
      Sample.Type = trimws(as.character(.data$Sample.Type)),
      Allele      = as.character(.data$Allele),
      Size        = suppressWarnings(as.numeric(.data$Size)),
      Cap         = as.integer(.data$Cap),
      Plate       = plate_id
    )
  
  # Analysis data: only rows with both allele + numeric size
  long <- long_meta %>%
    dplyr::filter(!is.na(.data$Allele), !is.na(.data$Size))

  # ---- 3. Optionally remove true ladders ----------------------------------
  if (!include_ladder) {
    long_meta <- long_meta %>%
      dplyr::mutate(Sample.Type = ifelse(is.na(.data$Sample.Type), "", .data$Sample.Type)) %>%
      dplyr::filter(!grepl("allelic ladder", .data$Sample.Type, ignore.case = TRUE))
    
    long <- long %>%
      dplyr::mutate(Sample.Type = ifelse(is.na(.data$Sample.Type), "", .data$Sample.Type)) %>%
      dplyr::filter(!grepl("allelic ladder", .data$Sample.Type, ignore.case = TRUE))
  }

  # ---- 3b. Expected observations per run (after ladder exclusion) ----------
  expected_caps <- long_meta %>%
    dplyr::filter(!is.na(.data$Cap)) %>%
    dplyr::group_by(.data$Plate, .data$Run.Name) %>%
    dplyr::summarise(
      N.Expected.Alleles = dplyr::n_distinct(.data$Cap),
      Expected.Caps  = list(sort(unique(.data$Cap))),
      .groups = "drop"
    )

  # Join kit sizes to BOTH: meta (for unmatched counting) and analysis (for stats)
  joined_meta <- long_meta %>%
    dplyr::filter(!is.na(.data$Allele)) %>%   # keep even if Size is NA
    dplyr::left_join(
      kit_sizes %>%
        dplyr::select(.data$Marker, .data$Allele, Expected.Size = .data$Size),
      by = c("Marker", "Allele")
    )
  
  # Count rows that fail to match kit (e.g. OL, missing alleles/markers)
  n_unmatched <- sum(is.na(joined_meta$Expected.Size), na.rm = TRUE)
  
  # Analysis join (only rows with Size present, because we compute stats on Size)
  joined <- long %>%
    dplyr::left_join(
      kit_sizes %>%
        dplyr::select(.data$Marker, .data$Allele, Expected.Size = .data$Size),
      by = c("Marker", "Allele")
    )
  
  merged <- joined %>%
    dplyr::filter(!is.na(.data$Expected.Size)) %>%
    dplyr::mutate(
      Expected.Size = as.numeric(.data$Expected.Size),
      Deviation = .data$Size - .data$Expected.Size
    )
  
  # ---- 5. Within-injection precision --------------------------------------
  within <- merged %>%
    dplyr::group_by(.data$Plate, .data$Run.Name, .data$Marker, .data$Allele) %>%
    dplyr::summarise(
      N.Observed.Alleles = dplyr::n_distinct(.data$Cap),
      Mean.Dev = mean(abs(.data$Deviation), na.rm = TRUE),
      Min.Dev  = min(abs(.data$Deviation), na.rm = TRUE),
      Max.Dev  = max(abs(.data$Deviation), na.rm = TRUE),
      Mean.Size = mean(.data$Size, na.rm = TRUE),
      SD        = sd_pop(.data$Size, na_rm = TRUE),
      Min.Size  = min(.data$Size, na.rm = TRUE),
      Max.Size  = max(.data$Size, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      expected_caps %>% dplyr::select(.data$Plate, .data$Run.Name, .data$N.Expected.Alleles),
      by = c("Plate", "Run.Name")
    )
  
  
  # ---- 5b. Detect missing allele observations per run ----------------------
  # Observed caps per marker/allele/run after kit join filtering (OL etc removed)
  observed_caps <- merged %>%
    dplyr::filter(!is.na(.data$Cap)) %>%
    dplyr::group_by(.data$Plate, .data$Run.Name, .data$Marker, .data$Allele) %>%
    dplyr::summarise(
      Observed.Caps = list(sort(unique(.data$Cap))),
      .groups = "drop"
    )
  
  # Join expected caps and compute missing caps (set difference)
  miss <- observed_caps %>%
    dplyr::left_join(expected_caps, by = c("Plate", "Run.Name")) %>%
    dplyr::mutate(
      Missing.Caps = Map(setdiff, .data$Expected.Caps, .data$Observed.Caps),
      N.Missing.Alleles = lengths(.data$Missing.Caps)
    )
  
  # Warn when expected allele observations are missing
  miss_warn <- miss %>%
    dplyr::filter(.data$N.Missing.Alleles > 0)
  
  if (nrow(miss_warn) > 0) {
    msg <- character(0)
    
    for (i in seq_len(nrow(miss_warn))) {
      run    <- miss_warn$Run.Name[i]
      marker <- miss_warn$Marker[i]
      allele <- miss_warn$Allele[i]
      caps   <- miss_warn$Missing.Caps[[i]]
      
      # Emit one warning line per missing capillary
      for (cap in caps) {
        msg <- c(
          msg,
          paste0("Run ", run, ", marker ", marker, ": allele ", allele,
                 " not detected in capillary ", cap)
        )
      }
    }
    
    message(paste(msg, collapse = "\n"), call. = FALSE)
  }

  # Add expected and missing observation info into within (now miss exists)
  within <- within %>%
    dplyr::left_join(
      miss %>% dplyr::select(.data$Plate, .data$Run.Name, .data$Marker, .data$Allele, .data$N.Missing.Alleles),
      by = c("Plate", "Run.Name", "Marker", "Allele")
    ) %>%
    dplyr::mutate(
      N.Observed.Alleles = as.integer(.data$N.Observed.Alleles),
      Obs.Complete = dplyr::coalesce(.data$N.Missing.Alleles, 0L) == 0L
    )
  
    
  # ---- 6. Across-injection reproducibility --------------------------------
  across <- within %>%
    dplyr::group_by(.data$Plate, .data$Marker, .data$Allele) %>%
    dplyr::summarise(
      N.Runs  = dplyr::n_distinct(.data$Run.Name),
      Mean.Size.Mean = mean(.data$Mean.Size, na.rm = TRUE),
      Mean.Size.SD   = sd_pop(.data$Mean.Size, na_rm = TRUE),  # reproducibility
      Mean.Size.Min  = min(.data$Mean.Size, na.rm = TRUE),
      Mean.Size.Max  = max(.data$Mean.Size, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Warn if any rows were excluded because they did not match kit_sizes
  if (n_unmatched > 0) {
    message(
      "calculate_precision(): Some observations were excluded because they did not match kit_sizes ",
      "(e.g., off-ladder alleles like 'OL' or alleles missing from the kit definition). ",
      "Precision metrics were computed on the remaining called alleles. ",
      "Excluded rows: ", n_unmatched, ".",
      call. = FALSE
    )
  }
  
  # ---- 7. Return ----------------------------------------------------------
  list(within = within, across = across)
}
