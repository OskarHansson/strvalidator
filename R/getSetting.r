################################################################################
# CHANGE LOG (last 20 changes)
# 22.02.2020: First version.

#' @title Get Settings.
#'
#' @description
#' Accepts a key string and returns the corresponding value.
#'
#' @details
#' Accepts a key string and returns the corresponding value from the settings.txt
#' file located within the package folders exdata sub folder.
#'
#' @param key character key for value to return.
#'
#' @importFrom data.table fread setkey
#' 
#' @return character the retrieved value or NA if not found.
#'

getSetting <- function(key) {
  
  # Constants
  fileName <- "settings.txt" # Name of settings file with file extension.
  subFolder <- "extdata" # Sub folder in addition to package path.
  fileSep <- .Platform$file.sep # Platform dependent path separator.
  
  # Get package path. Could use getPackageName()?
  packagePath <- path.package("strvalidator", quiet = FALSE)
  
  # Create path to settings file.
  filePath <- paste(packagePath, subFolder, fileName, sep = fileSep)
  
  # If file exist.
  if (file.exists(filePath)) {
    
    # Read settings file.
    dtable <- fread(file = filePath, sep = "auto",
                    quote = "\"", header = "auto", 
                    col.names = c("Key", "Value"))
    
    # Set key column.
    setkey(dtable, key = "Key")
    
    # Get value.
    value <- dtable[key]$Value
    
  } else { # If file don't exist.
    
    # Show file not found message.
    message("File ", filePath, " not found. Returning NA.")
    
    # Set NA as return value.
    value <- NA
  }
  
  return(value)
}