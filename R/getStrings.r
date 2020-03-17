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
#' @export
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

  # Get package path. Could use getPackageName()?
  packagePath <- path.package("strvalidator", quiet = FALSE)
  
  # Create path to language file.
  langFilePath <- paste(packagePath, subFolder, languageFile, sep = fileSep)

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
      message("Get langugage strings for ", gui)
      
      # Get strings for the specific function.
      dtRet <- dtRet[dtRet$Scope == gui, ]
      
      if(nrow(dtRet) == 0){
        
        message("No rows found for gui=", gui, ". Returning NA.")
        
        # Set NA as return value.
        dtRet <- NA
        
      }
      
    }

    # If gui function and key is specified.
    if (!is.na(dtRet) && !is.na(gui) && !is.na(key)) {
      message("Get langugage strings for key", key)
      
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

