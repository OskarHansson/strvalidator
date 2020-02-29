################################################################################
# CHANGE LOG (last 20 changes)
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
#'
#' @return character the retrieved value or NA if not found.
#'

getStrings <- function(language = NA, gui = NA, key = NA) {

  # If language is not specified.
  if (is.na(language)) {

    # Get language from settings.
    language <- getSetting("language")
  }

  # Constants.
  fileSep <- .Platform$file.sep # Platform dependent path separator.
  languageFile <- paste(c(language, ".csv"), collapse = "")
  subFolder <- paste("extdata", "languages", sep = fileSep) # Sub folder in addition to package path.

  # Create path to language file.
  langFilePath <- paste(packagePath, subFolder, languageFile, sep = .separator)

  # If file exist.
  if (file.exists(langFilePath)) {

    # Read file.
    dtAll <- fread(file = langFilePath, sep = "auto", quote = "\"", header = "auto")

    # Set key column.
    setkey(dtAll, key = "Key")

    # Return all data as default.
    dtRet <- dtAll

    # If gui function is specified.
    if (!is.na(gui)) {

      # Get strings for the specific function.
      dtRet <- dtRet[dtRet$Scope == gui, ]
    }

    # If gui function and key is specified.
    if (!is.na(gui) && !is.na(key)) {

      # Get the specific gui function value by key.
      dtRet <- dtRet[key]$Value
    }
  } else { # If file don't exist.

    # Show file not found message.
    message("File ", langFilePath, " not found. Returning NA.")

    # Set NA as return value.
    dtRet <- NA
  }

  return(dtRet)
}

