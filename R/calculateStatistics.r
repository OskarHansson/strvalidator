################################################################################
# CHANGE LOG (last 20 changes)
# 04.08.2022: Added parameter decimals for rounding of result.
# 13.06.2020: Fixed error in argument checks.
# 09.06.2020: Added parameter count.
# 22.05.2020: First version.

#' @title Summary Statistics
#'
#' @description
#' Calculate summary statistics for the selected target and scope.
#'
#' @details
#' Calculate summary statistics for the given target column ('X') across the
#' entire dataset or grouped by one or multiple columns, and counts the number
#' of unique values in the given count column ('Y'). Returns a data.frame
#' with the grouped columns, number of unique values 'Y.n', number of
#' observations 'X.n', the minimum value 'X.Min', the mean value 'X.Mean',
#' standard deviation 'X.Stdv', and the provided percentile 'X.Perc.##'.
#' For more details see \code{unique}, \code{min}, \code{mean}, \code{sd},
#'  \code{quantile}.
#'
#' @param data data.frame containing the data of interest.
#' @param target character column to calculate summary statistics for.
#' @param group character vector of column(s) to group by, if any.
#' @param count character column to count unique values in, if any.
#' @param quant numeric quantile to calculate {0,1}, default 0.95.
#' @param decimals numeric number of decimals. Negative does not round.
#' @param debug logical indicating printing debug information.
#'
#' @return data.frame with summary statistics.
#'
#' @export
#'
#' @importFrom stats sd quantile setNames
#' @importFrom data.table data.table
#' @importFrom dplyr mutate across %>%
#'

calculateStatistics <- function(data, target, quant = 0.95,
                                group = NULL, count = NULL, 
                                decimals = -1, debug = FALSE) {
  message("General function to calculate summary statistics.")

  # Check ---------------------------------------------------------------------

  message("Dataset with columns: ", paste(names(data), collapse = ", "))

  if (is.null(target)) {
    stop("'target' must be a single column name in the dataset.")
  }

  if (length(target) > 1) {
    stop("'target' must be a single column name in the dataset.")
  }

  if (!target %in% names(data)) {
    stop("The column ", target, " was not found in the dataset.")
  }

  if (!is.null(count)) {
    if (length(count) > 1) {
      stop("'count' must be a single column name in the dataset.")
    }

    if (!count %in% names(data)) {
      stop("The column ", count, " was not found in the dataset.")
    }
  }

  if (length(quant) > 1) {
    stop("'quant' must be a single value between 0 and 1.")
  }

  if (!quant <= 0 && !quant <= 1) {
    stop("'quant' must be a single value between 0 and 1.")
  }

  if (!all(group %in% names(data))) {
    stop("The column(s) ", paste(group, collapse = ", "), " was not found in the dataset.")
  }

  message("Calculate summary statistics on column: ", target)
  message("Group by columns: ", paste(group, collapse = ", "))
  message("Count unique values in columns: ", count)
  message("Calculate the ", quant * 100, " percentile.")

  # Prepare -------------------------------------------------------------------

  # Create column names.
  nameU <- paste(count, "n", sep = ".")
  nameN <- paste(target, "n", sep = ".")
  nameMin <- paste(target, "Min", sep = ".")
  nameMean <- paste(target, "Mean", sep = ".")
  nameSd <- paste(target, "Sd", sep = ".")
  nameMax <- paste(target, "Max", sep = ".")
  namePerc <- paste(target, "Perc", quant * 100, sep = ".")

  # Convert to data.table.
  DT <- data.table::data.table(data)

  if (debug) {
    print(DT)
  }

  # Check for and remove NA in target column.
  if (any(is.na(DT[[target]]))) {
    tmp1 <- nrow(DT)
    DT <- DT[!is.na(DT[[target]])]
    tmp2 <- nrow(DT)

    message("Removed ", tmp1 - tmp2, " rows with NA.")

    if (debug) {
      print(DT)
    }
  }

  # Check if numeric.
  if (!is.numeric(DT[[target]])) {

    # Convert to numeric.
    DT[[target]] <- as.numeric(DT[[target]])

    message("Values in column ", target, " converted to numeric.")

    if (debug) {
      print(str(DT))
    }
  }


  # Calculate -----------------------------------------------------------------

  if (is.null(count)) {

    # Perform calculations without counting unique values.
    res <- DT[,
      j = setNames(
        list(
          .N,
          min(get(target)),
          mean(get(target)),
          sd(get(target)),
          max(get(target)),
          quantile(get(target), quant)
        ),
        list(
          nameN, nameMin, nameMean, nameSd,
          nameMax, namePerc
        )
      ),
      by = eval(group)
    ]
  } else {

    # Perform calculations and count unique values.
    res <- DT[,
      j = setNames(
        list(
          length(unique(get(count))),
          .N,
          min(get(target)),
          mean(get(target)),
          sd(get(target)),
          max(get(target)),
          quantile(get(target), quant)
        ),
        list(
          nameU, nameN, nameMin, nameMean,
          nameSd, nameMax, namePerc
        )
      ),
      by = eval(group)
    ]
  }

  if (debug) {
    print("result:")
    print(res)
  }
  
  # Round ---------------------------------------------------------------------
  
  if (decimals >= 0) {
    
    message("Round result to ", decimals, " decimals.")
    
    # Perform calculations without counting unique values.
    res <- res %>% mutate(across(c(nameMin, nameMean, nameSd,
                               nameMax, namePerc), ~round(., decimals)))
  } else {
    
    message("Result not rounded (", decimals, ").")
    
  }

  # Convert to data.frame to assure compatibility with strvalidator.
  res <- as.data.frame(res)

  # Update audit trail.
  res <- auditTrail(obj = res, f.call = match.call(), package = "strvalidator")

  return(res)
}
