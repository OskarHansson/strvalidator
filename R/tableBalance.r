################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 07.08.2017: Added audit trail.
# 24.09.2016: Re-written to use data.table. Now handles both Lb and Hb.
# 24.09.2016: Re-named to tableBalance.
# 07.09.2016: Re-named to tableHb.
# 07.09.2016: Updated to use output from new function calculateHb.
# 09.01.2016: Added more attributes to result.
# 06.01.2016: Added attributes to result.
# 29.08.2015: Added importFrom.
# 07.05.2014: Replace 'Inf' with 'NA' (min return Inf if no value).
# 07.05.2014: Added 'suppressWarnings' around 'min' to prevent warning if no values.
# 15.02.2014: First version.

#' @title Table Balance
#'
#' @description
#' Summarize Hb or Lb analysis data in table format.
#'
#' @details
#' Summarize the Hb or Lb analysis in table format with different scope.
#' (locus, or global). Returns a dataframe with columns for marker name
#' 'Marker', number of allele ratios 'Xb.n', the minimum observed allele ratio
#' 'Xb.Min', the mean allele ratio 'Xb.Mean', its standard deviation 'Xb.Stdv',
#' the XXth percentile 'Xb.Perc.XX'
#' For more details see \code{min}, \code{mean}, \code{sd}, \code{quantile}.
#'
#' @param data data frame from a balance analysis by \code{calculateHb}
#'  and \code{calculateLb}.
#' @param scope string, summarize 'global' or 'locus'.
#' @param quant numeric, quantile to calculate.
#'
#' @return data.frame with summarized result.
#'
#' @export
#'
#' @importFrom stats sd quantile setNames
#' @importFrom data.table data.table
#'


tableBalance <- function(data, scope = "locus", quant = 0.05) {
  message("Function to calculate summary statistics for balance data.")

  # Prepare -------------------------------------------------------------------

  if ("Hb" %in% names(data)) {
    message("Hb column detected.")

    targetCol <- quote(Hb)
  } else if ("Lb" %in% names(data)) {
    message("Lb column detected.")

    targetCol <- quote(Lb)
  }

  # Create column names.
  nameN <- paste(targetCol, "n", sep = ".")
  nameMin <- paste(targetCol, "Min", sep = ".")
  nameMean <- paste(targetCol, "Mean", sep = ".")
  nameSd <- paste(targetCol, "Sd", sep = ".")
  nameMax <- paste(targetCol, "Max", sep = ".")
  namePerc <- paste(targetCol, "Perc", quant * 100, sep = ".")

  # Convert to data.table.
  DT <- data.table::data.table(data)

  # Check for and remove NA in target column.
  if (any(is.na(DT[, eval(targetCol)]))) {
    tmp1 <- nrow(DT)
    # DT <- DT[!is.na(get(targetCol))]
    DT <- DT[!is.na(eval(targetCol))]
    tmp2 <- nrow(DT)

    message("Removed ", tmp1 - tmp2, " rows with NA.")
  }

  # Calculate -----------------------------------------------------------------

  if (scope == "global") {
    message("Calculating global summary statistics across all data...")

    DT <- DT[, j = setNames(
      list(
        .N,
        min(eval(targetCol)),
        mean(eval(targetCol)),
        sd(eval(targetCol)),
        max(eval(targetCol)),
        quantile(eval(targetCol), quant)
      ),
      list(
        nameN, nameMin, nameMean, nameSd,
        nameMax, namePerc
      )
    )]
  } else if (scope == "locus") {
    message("Calculating summary statistics by locus...")

    DT <- DT[,
      j = setNames(
        list(
          .N,
          min(eval(targetCol)),
          mean(eval(targetCol)),
          sd(eval(targetCol)),
          max(eval(targetCol)),
          quantile(eval(targetCol), quant)
        ),
        list(
          nameN, nameMin, nameMean, nameSd,
          nameMax, namePerc
        )
      ),
      by = list(Marker)
    ]
  } else {
    message("Required column not in dataset. Return data unchanged.")
  }

  # Convert to data.frame.
  res <- as.data.frame(DT)

  # Update audit trail.
  res <- auditTrail(obj = res, f.call = match.call(), package = "strvalidator")

  return(res)
}
