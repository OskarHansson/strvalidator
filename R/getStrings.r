################################################################################
# CHANGE LOG (last 20 changes)
# 07.07.2022: Fixed Warning message: In !is.na(result) && !is.na(gui):'length(x) = 780 > 1' in coercion to 'logical(1)'
# 08.07.2020: Fix warnings about improper quoting.
# 04.07.2020: Defined unbound variables.
# 09.06.2020: Fixed Error in `:=`(value, gsub("\\n", "\n", value, fixed = TRUE))...
# 21.05.2020: Added gsub to about.
# 17.05.2020: Added gsub to fix new line coming out as \n.
# 15.05.2020: Added parameters 'encoding' and 'about'.
# 15.05.2020: Changed file extension from .csv to .txt.
# 15.05.2020: Return NULL instead of NA to fix warning when vector.
# 12.05.2020: Fixed encoding.
# 22.02.2020: First version.

#' @title Get Language Strings
#'
#' @description
#' Accepts a language code and gui. Returns the corresponding language strings.
#'
#' @details
#' Accepts a language code, gui, and key. Returns the corresponding language strings
#' for the specified gui function or key from a text file named as the language code.
#' Replaces backslash + n with new line character (only if 'gui' is specified).
#'
#'
#' @param language character name of the language.
#' @param gui character the function name for the gui to 'translate'.
#' @param key character the key to 'translate'. Only used in combination with 'gui'.
#' @param encoding character encoding to be assumed for input strings.
#' @param about logical FALSE (default) to read key value pairs,
#' TRUE to read about file as plain text.
#'
#' @export
#'
#' @return character vector or data.table with the retrieved values.
#' NULL if file or gui was not found.
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

  # Unbound variables (https://www.r-bloggers.com/no-visible-binding-for-global-variable/):
  scope <- NULL
  value <- NULL

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

      # Fix new line character.
      if (!is.null(result)) {
        result <- gsub("\\n", "\n", result, fixed = TRUE)
      }
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
        file = langFilePath, sep = "\t",
        header = "auto", encoding = encoding, quote = ""
      )

      # Set key column.
      setkey(dtAll, key = "key")

      # Return all data as default.
      result <- dtAll

      # If gui function is specified.
      if (!is.na(gui)) {
        message("Get langugage strings for gui = ", gui)

        # Get strings for the specific function.
        result <- result[scope == gui, ]
        # Note: variable named as column name does not work i.e. scope == scope.

        if (nrow(result) == 0) {
          message("No rows found for gui = ", gui, ". Returning NULL.")

          # Set NULL as return value.
          result <- NULL
        } else {

          # Fix new line character.
          result[, value := gsub("\\n", "\n", value, fixed = TRUE)]
        }
      }

      # If gui function and key is specified.
      if (!is.null(result) && !is.na(gui) && !is.na(key)) {
        message("Get langugage strings for key = ", key)

        # Get the specific gui function value by key.
        result <- result[key]$value
      } else {

        # Fix new line character.
        if (!is.null(result)) {
          result[, value := gsub("\\n", "\n", value, fixed = TRUE)]
        }
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
