#' @title Calculate Mixture
#'
#' @description
#' Given a set of mixture results and reference profiles for the
#' major and minor components, this function calculates the mixture
#' proportion (Mx), the average Mx, and the absolute difference
#' D = |Mx - AvgMx| for each marker. It also reports the overall
#' profile percentage calculated from the observed and expected number
#' of unique minor alleles per locus. Any unknown allele is reported as
#' a drop-in.
#'
#' @details
#' All reference sample names must be unique within and between the reference
#' datasets. Each mixture sample must contain both reference names (as
#' substrings) to be paired automatically. The first match in the mixture name
#' is assumed to be the major component and the second the minor.
#'
#' Mixture proportion is calculated according to:
#' \cr Locus style (minor:MAJOR) | Mx
#' \cr AA:AB | (A-B)/(A+B)
#' \cr AB:AA | (2*B)/(A+B)
#' \cr AB:AC | B/(B+C)
#' \cr AA:BB | A/(A+B)
#' \cr AB:CC | (A+B)/(A+B+C)
#' \cr AB:CD | (A+B)/(A+B+C+D)
#' \cr AB:AB | NA - cannot be calculated
#' \cr AA:AA | NA - cannot be calculated
#'
#' Marker order in the output follows the order of appearance in the mixture
#' data, and samples are sorted alphabetically. Case-insensitive matching and
#' optional off-ladder removal are supported.
#'
#' @references
#' Bright, Jo-Anne, Jnana Turkington, and John Buckleton.
#' "Examination of the Variability in Mixed DNA Profile Parameters for the
#' Identifiler Multiplex."
#' Forensic Science International: Genetics 4(2), 2010: 111-114.
#' \doi{10.1016/j.fsigen.2009.07.002}
#'
#' @param data data.frame in slim format with columns
#'   `Sample.Name`, `Marker`, `Allele`, and `Height` (RFU).
#' @param ref_profiles_df data.frame with reference genotypes containing
#'   `Sample.Name`, `Marker`, and `Allele`.
#' @param threshold numeric RFU threshold; peaks below this are ignored.
#'   Default: 0 (include all peak heights).
#' @param dropout_pseudo_rfu optional numeric RFU to assign for missing minor
#'   alleles when `include_dropout = TRUE`. Default: NULL.
#' @param include_dropout logical; if TRUE, calculate Mx even when alleles are
#'   missing due to dropout. Default: TRUE. (formerly 'ignore_dropout')
#' @param match_case logical; if FALSE, matching ignores case. Default: FALSE.
#' @param ol_rm logical; if TRUE, removes off-ladder alleles. Default: TRUE.
#' @param ol_str Character substring identifying off-ladder alleles (e.g. "OL", "Off").
#'   All alleles whose names contain this substring are treated as off-ladder.
#'   Matching respects the `match_case` setting. Default: "OL".
#' @param auto_minor_from_mx logical; if TRUE, swaps major/minor roles when
#'   the observed average Mx > 0.5. Default: FALSE.
#' @param output_level character; `"marker"` (default) for per-marker results
#'   or `"sample"` for sample-level summaries.
#' @param debug logical indicating printing debug information.
#'
#' @return data.frame with columns:
#'   `Sample.Name, Marker, Style, Mx, Average, Difference,
#'    Observed, Expected, Profile, Dropin`.
#'
#' @importFrom dplyr mutate filter group_by summarise arrange bind_rows tibble
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_trim str_detect fixed
#' @importFrom rlang .data
#' @aliases calculateMixture
#' @export
calculate_mixture <- function(
  data,
  ref_profiles_df,
  threshold = 0,
  dropout_pseudo_rfu = NULL,
  include_dropout = TRUE,
  match_case = FALSE,
  ol_rm = TRUE,
  ol_str = "OL",
  auto_minor_from_mx = FALSE,
  output_level = c("marker", "sample"),
  debug = FALSE
) {
  output_level <- match.arg(output_level)

  # Check input
  required_cols <- c("Sample.Name", "Marker", "Allele", "Height")
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) stop("Missing required columns in 'data': ", paste(missing, collapse = ", "))

  required_cols <- c("Sample.Name", "Marker", "Allele")
  missing <- setdiff(required_cols, names(ref_profiles_df))
  if (length(missing) > 0) stop("Missing required columns in 'ref_profiles_df': ", paste(missing, collapse = ", "))

  # Preserve names and determine order
  marker_order <- unique(data$Marker)
  data$Marker_orig <- data$Marker
  data$Sample_orig <- data$Sample.Name

  # Prepare lowercase versions for internal matching
  lc <- function(x) if (match_case) tolower(x) else x
  data$Sample.Name_lc <- lc(data$Sample.Name)
  data$Marker_lc <- lc(data$Marker)
  ref_profiles_df$Sample_lc <- lc(ref_profiles_df$Sample.Name)
  ref_profiles_df$Marker_lc <- lc(ref_profiles_df$Marker)

  # Ensure numeric height
  data$Height <- suppressWarnings(as.numeric(data$Height))
  data$Height[is.na(data$Height)] <- 0

  # Remove off-ladder alleles if requested
  if (ol_rm) {
    data <- dplyr::filter(
      data,
      !stringr::str_detect(
        Allele,
        stringr::fixed(ol_str,
          ignore_case = !match_case
        )
      )
    )
  }


  # --- Build reference profiles (correct legacy structure) ---
  ref_profiles <- split(ref_profiles_df, lc(ref_profiles_df$Sample.Name))
  ref_profiles <- lapply(ref_profiles, function(df) {
    split(as.character(df$Allele), lc(df$Marker))
  })
  ref_names <- names(ref_profiles)

  # --- Marker-level processor -----------------------------------------------
    process_sample <- function(sample_id, ref_major, ref_minor, data_sub) {
      sample_data <- dplyr::filter(
        data_sub,
        .data$Sample.Name_lc == sample_id,
        .data$Height >= threshold
      )
      if (nrow(sample_data) == 0) {
        return(list(marker = tibble::tibble()))
      }
      
      obs_alleles <- sample_data |>
        dplyr::group_by(.data$Marker_lc) |>
        dplyr::summarise(Alleles = list(unique(.data$Allele)), .groups = "drop")

    rows <- list()
    total_minor <- total_minor_obs <- total_dropin <- 0

    for (locus in unique(sample_data$Marker_lc)) {
      alleles <- sample_data[sample_data$Marker_lc == locus, , drop = FALSE]
      major_a <- ref_major[[locus]]
      minor_a <- ref_minor[[locus]]
      if (is.null(major_a) || is.null(minor_a)) next

      ref1a <- stringr::str_trim(as.character(major_a))
      ref2a <- stringr::str_trim(as.character(minor_a))
      shared <- intersect(unique(ref1a), unique(ref2a))
      unshared_major <- setdiff(ref1a, ref2a)
      unshared_minor <- setdiff(ref2a, ref1a)
      exp_alleles <- union(ref1a, ref2a)

      # Handle NA and OL before dropin/Mx calculations
      obs_alleles <- alleles$Allele
      obs_heights <- alleles$Height
      obs_alleles <- obs_alleles[!is.na(obs_alleles)]

      # For Mx, ignore OL alleles
      ol_mask <- stringr::str_detect(
        obs_alleles,
        stringr::fixed(ol_str,
          ignore_case = !match_case
        )
      )
      obs_alleles_for_mx <- obs_alleles[!ol_mask]
      obs_heights_for_mx <- obs_heights[!ol_mask]

      # Then compute obs_minor, dropin etc.
      obs_minor <- sum(unshared_minor %in% obs_alleles_for_mx)
      dropin <- setdiff(obs_alleles, exp_alleles)
      dropin_n <- length(dropin)

      total_dropin <- total_dropin + dropin_n
      total_minor <- total_minor + length(unshared_minor)
      total_minor_obs <- total_minor_obs + obs_minor

      style <- NA_character_
      mx <- NA_real_
      total_unshared <- length(unshared_major) + length(unshared_minor)

      if(debug){
        message("exp_alleles = ", paste(exp_alleles, collapse = ", "))
        message("obs_alleles_for_mx = ", paste(obs_alleles_for_mx, collapse = ", "))
        message("unshared_major = ", paste(unshared_major, collapse = ", "))
        message("unshared_minor = ", paste(unshared_minor, collapse = ", "))
        message("dropout_pseudo_rfu = ", dropout_pseudo_rfu)
        message("include_dropout = ", include_dropout)
      }

      if (include_dropout || all(exp_alleles %in% obs_alleles)) {
        if (total_unshared > 0) {
          if (length(shared) == 0) {
            # AA:BB | AB:CC | AB:CD
            if (total_unshared == 2) {
              style <- "AA:BB"
            } else if (total_unshared == 3) {
              style <- "AB:CC"
            } else if (total_unshared == 4) {
              style <- "AB:CD"
            }
            num <- sum(obs_heights_for_mx[obs_alleles_for_mx %in% ref2a], na.rm = TRUE)
            den <- sum(obs_heights_for_mx, na.rm = TRUE)
            mx <- ifelse(den > 0, num / den, NA_real_)
          } else if (length(shared) == 1 && length(unshared_major) > 0 && length(unshared_minor) > 0) {
            style <- "AB:AC"
            num <- sum(obs_heights_for_mx[obs_alleles_for_mx %in% unshared_minor], na.rm = TRUE)
            den <- sum(obs_heights_for_mx[!obs_alleles_for_mx %in% shared], na.rm = TRUE)
            mx <- ifelse(den > 0, num / den, NA_real_)
          } else if (length(shared) == 1 && length(unshared_minor) == 0) {
            style <- "AA:AB"
            a <- sum(obs_heights_for_mx[obs_alleles_for_mx %in% shared], na.rm = TRUE)
            b <- sum(obs_heights_for_mx[obs_alleles_for_mx %in% unshared_major], na.rm = TRUE)
            num <- max(a - b, 0)
            den <- a + b
            mx <- ifelse(den > 0, num / den, 0)
          } else if (length(shared) == 1 && length(unshared_major) == 0) {
            style <- "AB:AA"
            a <- sum(obs_heights_for_mx[obs_alleles_for_mx %in% shared], na.rm = TRUE)
            b <- sum(obs_heights_for_mx[obs_alleles_for_mx %in% unshared_minor], na.rm = TRUE)
            num <- 2 * b
            den <- a + b
            mx <- ifelse(den > 0, num / den, 0)
          } else {
            if (length(shared) == 1) {
              style <- "AA:AA"
            } else if (length(shared) == 2) style <- "AB:AB"
          }

          # Handle dropout pseudo-RFU only when include_dropout = TRUE
          if (include_dropout &&
            !is.null(dropout_pseudo_rfu) &&
            length(unshared_minor) > 0 &&
            any(!unshared_minor %in% obs_alleles_for_mx)) {
            style <- paste0(style, "_dropout_pseudo")
            major_h <- sum(obs_heights_for_mx[obs_alleles_for_mx %in% unshared_major], na.rm = TRUE)
            minor_h <- dropout_pseudo_rfu
            total_h <- major_h + minor_h
            mx <- ifelse(total_h > 0, minor_h / total_h, 0)
          }
        } else {
          if (length(shared) == 1) {
            style <- "AA:AA"
          } else if (length(shared) == 2) style <- "AB:AB"
        }
      } else if (!is.null(dropout_pseudo_rfu) &&
        length(unshared_minor) > 0 &&
        any(!unshared_minor %in% obs_alleles_for_mx)) {
        # Handle dropout pseudo-RFU even if not ignoring dropouts
        style <- "AA:BB_dropout_pseudo"
        major_h <- sum(obs_heights_for_mx[obs_alleles_for_mx %in% unshared_major], na.rm = TRUE)
        minor_h <- dropout_pseudo_rfu
        total_h <- major_h + minor_h
        mx <- ifelse(total_h > 0, minor_h / total_h, 0)
      } else {
        style <- "Dropout"
        mx <- NA_real_
      }

      rows[[locus]] <- dplyr::tibble(
        Sample.Name = unique(alleles$Sample_orig),
        Marker = unique(alleles$Marker_orig),
        Style = style,
        Mx = mx,
        Observed = obs_minor,
        Expected = length(unshared_minor),
        Dropin = dropin_n
      )
    }

    df <- dplyr::bind_rows(rows)
    if (nrow(df) == 0) {
      return(list(marker = tibble()))
    }

    avg_mx <- mean(df$Mx[!is.na(df$Mx) & df$Style != "Dropout"], na.rm = TRUE)
    valid <- df$Expected > 0
    profile_percent <- if (sum(valid) > 0) {
      (sum(df$Observed[valid]) / sum(df$Expected[valid])) * 100
    } else {
      NA_real_
    }

    df <- dplyr::mutate(df,
      Average = avg_mx,
      Difference = abs(Mx - avg_mx),
      Profile = profile_percent
    )
    list(marker = df)
  }

  # --- Identify reference pairs ---------------------------------------------
  all_samples <- unique(data$Sample.Name_lc)
  out_list <- list()

  for (s in all_samples) {
    matches <- ref_names[
      vapply(
        ref_names, function(r) {
          stringr::str_detect(s, stringr::fixed(r, ignore_case = !match_case))
        },
        logical(1)
      )
    ]

    if (length(matches) < 2) {
      message("Skipping sample (no matching references): ", s)
      next
    } else {
      message("Processing sample: ", s)
    }

    ref_major <- ref_profiles[[matches[1]]]
    ref_minor <- ref_profiles[[matches[2]]]
    out <- process_sample(s, ref_major, ref_minor, data)
    if (auto_minor_from_mx && mean(out$marker$Mx, na.rm = TRUE) > 0.5) {
      out <- process_sample(s, ref_minor, ref_major, data)
    }
    out_list[[s]] <- out$marker
  }

  res <- dplyr::bind_rows(out_list)

  # If no results, return an empty data.frame with correct structure
  if (nrow(res) == 0) {
    return(data.frame(
      Sample.Name = character(),
      Marker = character(),
      Style = character(),
      Mx = numeric(),
      Average = numeric(),
      Difference = numeric(),
      Observed = integer(),
      Expected = integer(),
      Profile = numeric(),
      Dropin = integer(),
      stringsAsFactors = FALSE
    ))
  }

  # Maintain marker order and sample sorting
  res <- res %>%
    dplyr::mutate(Marker = factor(.data$Marker, levels = marker_order)) %>%
    dplyr::arrange(.data$Sample.Name, .data$Marker)

  # Ensure pure data.frame output (not tibble)
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  return(res)
}

