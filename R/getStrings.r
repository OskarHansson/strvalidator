################################################################################
# CHANGE LOG (last 20 changes)
# 15.05.2020: Added parameters 'encoding' and 'about'.
# 15.05.2020: Changed file extension from .csv to .txt.
# 15.05.2020: Return NULL instead of NA to fix warning when vector.
# 12.05.2020: Fixed encoding (è -> Ã¨).
# 22.02.2020: First version.

#' @title Get Language Strings.
#'
#' @description
#' Accepts a language code and gui. Returns the corresponding language strings.
#'
#' @details
#' Accepts a language code and gui. Returns the corresponding language strings
#' for the specified gui function from a text file named as the language code.
#'
#' @param language character name of the language.
#' @param gui character the function name for the gui to 'translate'.
#' @param key character the key to 'translate'. Only used in combination with 'gui'.
#' @param encoding character encoding to be assumed for input strings.
#' @param about logical FALSE (default) to read key value pairs,
#' TRUE to read about as plain text.
#'
#' @export
#'
#' @return character vector or data.table with the retrieved values.
#' NULL if file or scope was not found.
#'

getStrings <- function(language = NA, gui = NA, key = NA,
                       encoding = NA, about = FALSE) {

  # If language is not specified.
  if (is.na(language)) {

    # Get language from settings.
    language <- getSetting("language")
  }

  # If encoding is not specified.
  if (is.na(encoding)) {

    # Get language from settings.
    encoding <- getSetting("encoding")
  }

  # Constants.
  fileSep <- .Platform$file.sep # Platform dependent path separator.
  languageFile <- paste(c(language, ".txt"), collapse = "")
  aboutFile <- paste(c(language, "_about", ".txt"), collapse = "")
  subFolder <- paste("extdata", "languages", sep = fileSep) # Sub folder in addition to package path.

  # Get package path. Could use getPackageName()?
  packagePath <- path.package("strvalidator", quiet = FALSE)

  if (about) {

    # Create path to language files.
    aboutFilePath <- paste(packagePath, subFolder, aboutFile, sep = fileSep)

    # If file exist.
    if (file.exists(aboutFilePath)) {

      # Read file.
      result <- readLines(con = aboutFilePath, encoding = encoding)
    } else { # If file doesn't exist.

      # Show file not found message.
      message("File ", aboutFilePath, " not found. Returning NULL.")

      # Set NULL as return value.
      result <- NULL
    }
  } else {

    # Create path to language files.
    langFilePath <- paste(packagePath, subFolder, languageFile, sep = fileSep)

    # If file exist.
    if (file.exists(langFilePath)) {

      # Read file.
      dtAll <- fread(
        file = langFilePath, sep = "auto",
        header = "auto", encoding = encoding
      )

      # Set key column.
      setkey(dtAll, key = "Key")

      # Return all data as default.
      result <- dtAll

      # If gui function is specified.
      if (!is.na(gui)) {
        message("Get langugage strings for ", gui)

        # Get strings for the specific function.
        result <- result[result$Scope == gui, ]

        if (nrow(result) == 0) {
          message("No rows found for gui=", gui, ". Returning NULL.")

          # Set NULL as return value.
          result <- NULL
        }
      }

      # If gui function and key is specified.
      if (!is.na(result) && !is.na(gui) && !is.na(key)) {
        message("Get langugage strings for key", key)

        # Get the specific gui function value by key.
        result <- result[key]$Value
      }
    } else { # If file doesn't exist.

      # Show file not found message.
      message("File ", langFilePath, " not found. Returning NULL.")

      # Set NULL as return value.
      result <- NULL
    }
  }

  return(result)
}
