################################################################################
# CHANGE LOG (last 20 changes)
# 13.04.2020: Added language support.
# 13.04.2020: Added @export.
# 04.05.2015: 'slimcol' and 'stringcol' now accept vectors.
# 06.05.2014: First version.

#' @title Check Dataset
#'
#' @description
#' Check a data.frame before analysis.
#'
#' @details Check that the object exist, there are rows, the required columns exist,
#' if data.frame is 'fat', and if invalid strings exist. Show error message if not.
#'
#' @param name character name of data.frame.
#' @param reqcol character vector with required column names.
#' @param slim logical TRUE to check if 'slim' data.
#' @param slimcol character vector with column names to check if 'slim' data.
#' @param string character vector with invalid strings in 'stringcol', return FALSE if found.
#' @param stringcol character vector with column names to check for 'string'.
#' @param env environment where to look for the data frame.
#' @param parent parent gWidget.
#' @param debug logical indicating printing debug information.
#'
#' @export
#'

checkDataset <- function(name, reqcol = NULL, slim = FALSE, slimcol = NULL,
                         string = NULL, stringcol = NULL,
                         env = parent.frame(), parent = NULL, debug = FALSE) {

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strMsgNoRows <- "Dataset contain no rows!"
  strMsgAdditionalCols <- "Additional columns required:\n"
  strMsgFat1 <- "The dataset is too fat!\n\nThere can only be 1 column:"
  strMsgFat2 <- "\nPlease slim the dataset."
  strMsgDetected1 <- "detected in column"
  strMsgDetected2 <- "Please make sure that data is clean/filtered.\n"

  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  if (!is.null(dtStrings)) {
    # Get language strings, use default if not found.
  }

  # FUNCTION ##################################################################

  if (debug) {
    print("Parameters:")
    print("name")
    print(name)
    print("reqcol")
    print(reqcol)
    print("slim")
    print(slim)
    print("stringcol")
    print(stringcol)
    print("env")
    print(environmentName(env))
    print("parent")
    print(parent)
  }

  # Initiate variables.
  ok <- TRUE
  messageText <- NULL

  # Check if dataset exist in environment.
  if (exists(name, envir = env, inherits = FALSE)) {

    # Get dataset.
    df <- get(name, envir = env)

    # Check if empty dataset.
    if (nrow(df) == 0) {

      # Construct error message.
      messageText <- strMsgNoRows

      # Change flag.
      ok <- FALSE
    } else if (!is.null(reqcol) & !all(reqcol %in% colnames(df))) {
      # Check for required column names.

      missingCol <- reqcol[!reqcol %in% colnames(df)]

      # Construct error message.
      messageText <- paste(strMsgAdditionalCols,
        paste(missingCol, collapse = "\n"),
        sep = ""
      )

      # Change flag.
      ok <- FALSE
    } else if (slim & !is.null(slimcol)) {

      # Loop over columns to check.
      for (c in seq(along = slimcol)) {

        # Check if slimmed.
        slimmed <- sum(grepl(slimcol[c], names(df), fixed = TRUE)) == 1

        if (!slimmed) {

          # Construct error message.
          messageText <- paste(strMsgFat1, slimcol[c], strMsgFat2)

          # Change flag.
          ok <- FALSE
        }
      }
    } else if (!is.null(string) & !is.null(stringcol)) {

      # Loop over columns to check.
      for (c in seq(along = stringcol)) {
        if (any(string %in% df[, stringcol[c]])) {

          # Construct error message.
          messageText <- paste("'", string, "' ", strMsgDetected1, " ",
            stringcol[c], "!\n", strMsgDetected2,
            sep = ""
          )

          # Change flag.
          ok <- FALSE
        }
      }
    } else {

      # Dataset passed!
      ok <- TRUE
    }
  } else {

    # Construct error message.
    messageText <- NULL # NB! Can't show error message because "<Select dataset>" is in drop-downs.

    # Change flag.
    ok <- FALSE
  }

  if (debug) {
    print("ok")
    print(ok)
    print("messageText")
    print(messageText)
  }

  # Show error message.
  if (!ok & !is.null(messageText)) {
    if (is.null(parent)) {
      # Write in command prompt.
      message(messageText)
    } else {
      # Show message box.
      gmessage(messageText,
        title = fnc,
        icon = "error", parent = parent
      )
    }
  }

  if (debug) {
    print(paste("EXIT:", fnc))
  }

  # Return result.
  return(ok)
}
