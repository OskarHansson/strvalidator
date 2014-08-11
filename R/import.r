################################################################################
# TODO LIST
# TODO: make ignore.case as parameter.
# TODO: Use choose.files instead of file.choose to avoid error if no file?
# TODO: re-make function to read line and specify type for each column.
# TODO: Expand to handle other separators (e.g. comma).

################################################################################
# CHANGE LOG
# 20.01.2014: Added parameter 'colClasses = "character"' to 'read.table'.
# 15.01.2014: Added message to show progress.
# 13.01.2014: Added parameter 'na.strings = c("NA","")' to 'read.table'.
# 13.01.2014: Fixed bug when no matching files in folder.
# 10.12.2013: Changed names on parameters 'resultFiles' -> 'fileName'
#              and 'resultFolder' -> 'folderName'.
# 12.11.2013: Changed 'rbind' to 'rbind.fill' from package 'plyr'.
# 13.06.2013: Added parameter 'debug'. Fixed regexbug when importing from folder.
# <13.06.2013: Renamed from importGM to import.
# <13.06.2013: Added parameter 'fileName' and 'folderName' for direct import.
# <13.06.2013: Changed regex from (".",".",extension, sep="") to (".*","\\.",extension, sep="")
# <13.06.2013: Roxygenized.
# <13.06.2013: add column 'File' when importing from a folder.
# <13.06.2013: new parameter 'extension' (fixes error in folder import)

#' @title Import GeneMapper.
#'
#' @description
#' \code{import} imports text files exported from GeneMapper.
#'
#' @details
#' Imports GeneMapper results exported as tab delimited text files.
#' NB! Empty strings ("") and NA strings ("NA") are converted to NA.
#' 
#' @param folder logical, TRUE all files in folder will be imported,
#' FALSE only selected file will be imported.
#' @param suffix string, only files with specified suffix will be imported.
#' @param prefix string, only files with specified prefix will be imported.
#' @param fileName string if file name is provided file will be imported
#' without showing the file open dialogue. 
#' @param folderName string if folder name is provided files in folder
#' will be imported without showing the select folder dialogue. 
#' @param extension string providing the file extension.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with imported result.


import <- function (folder = TRUE, extension="txt", 
                    suffix = NA, prefix = NA, 
                    fileName=NA, folderName=NA,
                    debug=FALSE){
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  if(debug){
    print("folder")
    print(folder)
    print("extension")
    print(extension)
    print("suffix")
    print(suffix)
    print("prefix")
    print(prefix)
    print("fileName")
    print(fileName)
    print("folderName")
    print(folderName)
  }
  
  
  manualPick <- is.na(fileName) && is.na(folderName)
  
  if(debug){
    print("manualPick")
    print(manualPick)
  }  
  
  # Initialise result data.frame (no match return empty dataframe)
  res <- data.frame()
  
  # Check if result files in folder.
  if (folder) {
    
    if(manualPick){
      # Ask user to select a folder.
      resFolder <- choose.dir()
    } else {
      
      resFolder <- folderName
    }
    
    # Check if folder is specified.
    if (!is.na(resFolder)) {
      
      # Create file filter.
      fileFilter <- paste(".*", sep="")
      if (!is.na(prefix) && nchar(prefix) > 0) {
        fileFilter <- paste(prefix, fileFilter, sep="") 
        if(debug){
          print("prefix added:")
          print(fileFilter)
        }  
      }
      if (!is.na(suffix) && nchar(suffix) > 0) {
        fileFilter <- paste(fileFilter, suffix, sep="") 
        if(debug){
          print("suffix added:")
          print(fileFilter)
        }  
      }
      fileFilter <- paste(fileFilter,"\\.", extension, sep="") 
      if(debug){
        print("fileFilter")
        print(fileFilter)
        print("resFolder")
        print(resFolder)
      }  
      
      # Get list of result files.
      fileName <- list.files(path = resFolder, pattern = fileFilter,
                                full.names = TRUE, recursive = FALSE,
                                ignore.case = TRUE, include.dirs = FALSE)
    }
    
  } else if (manualPick) {
    
    # Ask user to select a file.
    fileName <- file.choose()
    
  }
  
  if(debug){
    print("fileName")
    print(fileName)
  }  
  
  # Check if files are specified.
  if (any(length(fileName) > 0, !is.na(fileName))) {
    
    # Read first file to create data frame.		
    res <- read.table(fileName[1], header = TRUE,
                      sep = "\t", fill = TRUE,
                      na.strings = c("NA",""),
                      colClasses = "character",
                      stringsAsFactors=FALSE)
    
    # Create a colum name for file name.
    colName <- "File"
    if(colName %in% names(res)){
      tmpName <- make.unique(c(names(res),colName))
      colName <- tmpName[length(tmpName)]
    }
    
    # Add column and save file name.
    res[colName] <- basename(fileName[1])

    # Get number of files.
    files <- length(fileName)

    # Show progress.
    message(paste("Importing (", 1, " of ", files,"): ",
                  fileName[1], sep=""))
    
    # Read additional files.
    if (files > 1) {
      for (f in 2:files) {
        
        # Read a file.	
        tmp <- read.table(fileName[f], header = TRUE,
                          sep = "\t", fill = TRUE,
                          na.strings = c("NA",""),
                          colClasses = "character",
                          stringsAsFactors=FALSE)
        
        # Add column and save file name.
        tmp[colName] <- basename(fileName[f])
        
        # Show progress.
        message(paste("Importing (", f, " of ", files,"): ",
                      fileName[f], sep=""))
        
        # Add to data frame.
        res <- plyr::rbind.fill(res, tmp)
        
      }
    }
    
  }
  
  return(res)
}
