#' @title Adds New Data Columns to a Data Frame
#'
#' @description
#' Adds values from columns in 'new_data' to 'data' by keys.
#'
#' @details
#' Information in columns in data frame 'new_data' is added to data frame
#' 'data' based on primary key value in column 'by_col', and optionally on
#' secondary key values in column 'then_by_col'.
#'
#' @param data Data frame containing your main data.
#' @param new_data Data frame containing information you want to add to 'data'.
#' @param by_col character, primary key column.
#' @param then_by_col character, secondary key column.
#' @param exact logical, TRUE matches keys exact.
#' @param ignore_case logical, TRUE ignore case.
#' @param what character vector defining columns to add. Default is all new columns.
#' @param debug logical indicating printing debug information.
#'
#' @return data.frame the original data frame containing additional columns.
#'
#' @export
#'
#' @examples
#' # Get marker names and alleles for Promega PowerPlex ESX 17.
#' x <- get_kit("ESX17", what = "Allele")
#' # Get marker names and colors for Promega PowerPlex ESX 17.
#' y <- get_kit("ESX17", what = "Color")
#' # Add color information to allele information.
#' z <- add_data(data = x, new_data = y, by_col = "Marker")
#' print(x)
#' print(y)
#' print(z)
add_data <- function(data, new_data, by_col, then_by_col = NULL, exact = TRUE,
                    ignore_case = TRUE, what = NULL, debug = FALSE) {
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  if ("data.table" %in% class(data)) {
    data <- data.frame(data)
    message("Converted data.table to data.frame (data).")
  }

  if ("data.table" %in% class(new_data)) {
    new_data <- data.frame(new_data)
    message("Converted data.table to data.frame (new_data).")
  }

  if (nrow(data) == 0) {
    message("Dataset is empty. Nothing was added.")
    return(data)
  }

  # Prepare -------------------------------------------------------------------

  # Remove unused colums.
  if (!is.null(what)) {
    new_data <- new_data[, c(by_col, then_by_col, what)]
  }

  # Get column names.
  colNames <- names(data)
  colNamesNew <- names(new_data)
  colNamesNew <- colNamesNew[colNamesNew != by_col]
  if (!is.null(then_by_col)) {
    colNamesNew <- colNamesNew[colNamesNew != then_by_col]
  }
  colNamesNew <- colNamesNew[!colNamesNew %in% colNames]

  # Add new columns to data.
  for (c in seq(along = colNamesNew)) {
    data[colNamesNew[c]] <- NA
  }

  # Get unique identifiers.
  keysData <- unique(as.character(data[, by_col]))
  keysNew <- unique(as.character(new_data[, by_col]))

  # Select keys.
  if (exact) {
    keys <- keysData
  } else {
    keys <- keysNew
  }

  if (debug) {
    print("keys")
    print(keys)
  }

  # Loop through keys.
  for (k in seq(along = keys)) {
    # Select rows.
    if (exact) {
      selectedData <- data[, by_col] == keys[k]
      selectedNewData <- new_data[, by_col] == keys[k]
    } else {
      selectedData <- grepl(keys[k], data[, by_col],
        ignore.case = ignore_case
      )
      selectedNewData <- grepl(keys[k], new_data[, by_col],
        ignore.case = ignore_case
      )
    }

    if (debug) {
      print("selectedData")
      print(data[selectedData, ])
      print("selectedNewData")
      print(new_data[selectedNewData, ])
    }

    if (!is.null(selectedData)) {
      if (is.null(then_by_col)) {
        if (any(selectedData, na.rm = TRUE) && any(selectedNewData, na.rm = TRUE)) {
          for (c in seq(along = colNamesNew)) {
            dataLen <- length(data[selectedData, colNamesNew[c]])
            dataNewLen <- length(new_data[selectedNewData, colNamesNew[c]])

            if (dataLen == dataNewLen) {
              # Add new data.
              data[selectedData, colNamesNew[c]] <-
                new_data[selectedNewData, colNamesNew[c]]
            } else {
              uniqueNewData <- unique(new_data[selectedNewData, colNamesNew[c]])

              # See if all the same value.
              if (length(uniqueNewData) == 1) {
                # Add new data.
                data[selectedData, colNamesNew[c]] <- uniqueNewData
              } else {
                message(
                  "Ambiguous data could not be added at key ",
                  keys[k], "."
                )
              }
            }
          }
        }
      } else {
        # Get unique identifiers.
        keysData2 <- unique(as.character(data[, then_by_col]))
        keysNew2 <- unique(as.character(new_data[, then_by_col]))

        # Select keys.
        if (exact) {
          keys2 <- keysData2
        } else {
          keys2 <- keysNew2
        }

        if (debug) {
          print("keys2")
          print(keys2)
        }

        # Loop through keys.
        for (k2 in seq(along = keys2)) {
          # Select rows.
          if (exact) {
            selectedData2 <- data[, then_by_col] == keys2[k2] & selectedData
            selectedNewData2 <- new_data[, then_by_col] == keys2[k2] & selectedNewData
          } else {
            selectedData2 <- grepl(keys2[k2], data[, then_by_col],
              ignore.case = ignore_case
            ) & selectedData
            selectedNewData2 <- grepl(keys2[k2], new_data[, then_by_col],
              ignore.case = ignore_case
            ) & selectedNewData
          }

          if (debug) {
            print("selectedData2")
            print(data[selectedData2, ])
            print("selectedNewData2")
            print(new_data[selectedNewData2, ])
          }

          if (any(selectedData2, na.rm = TRUE) && any(selectedNewData2, na.rm = TRUE)) {
            for (c2 in seq(along = colNamesNew)) {
              dataLen <- length(data[selectedData2, colNamesNew[c2]])
              dataNewLen <- length(new_data[selectedNewData2, colNamesNew[c2]])

              if (dataLen == dataNewLen) {
                if (debug) {
                  print("SelectedData")
                  print(data[selectedData2, colNamesNew[c2]])
                  print("SelectedNewData")
                  print(new_data[selectedNewData2, colNamesNew[c2]])
                }

                # Add new data.
                data[selectedData2, colNamesNew[c2]] <-
                  new_data[selectedNewData2, colNamesNew[c2]]
              } else {
                uniqueNewData <- unique(new_data[selectedNewData2, colNamesNew[c2]])

                if (debug) {
                  print("uniqueNewData")
                  print(uniqueNewData)
                }

                # See if all the same value.
                if (length(uniqueNewData) == 1) {
                  # Add new data.
                  data[selectedData2, colNamesNew[c2]] <- uniqueNewData
                } else {
                  message(
                    "Ambiguous data could not be added at key ", keys[k],
                    " sub key: ", keys2[k2], "."
                  )
                }
              }
            }
          }
        }
      }
    }
  }

  # Update audit trail.
  data <- audit_trail(obj = data, f_call = match.call(), package = "strvalidator")

  if (debug) {
    print(paste("EXIT:", match.call()[[1]]))
  }

  return(data)
}
