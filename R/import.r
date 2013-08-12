################################################################################
# TODO LIST
# TODO: Use choose.files instead of file.choose to avoid error if no file?
# TODO: re-make function to read line and specify type for each column.
# TODO: now sample name "F" is converted to "FALSE".

################################################################################
# CHANGE LOG
# 13.06.2013: Added parameter 'debug'. Fixed regexbug when importing from folder.
# <13.06.2013: Renamed from importGM to import.
# <13.06.2013: Added parameter 'resultFiles' and 'ResultFolder' for direct import.
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
#' 
#' @param folder logical, TRUE all files in folder will be imported,
#' FALSE only selected file will be imported.
#' @param suffix string, only files with specified suffix will be imported.
#' @param prefix string, only files with specified prefix will be imported.
#' @param resultFiles string if file name is provided file will be imported
#' without showing the file open dialogue. 
#' @param resultFolder string if fodler name is provided files in folder
#' will be imported without showing the select folder dialogue. 
#' @param extension string providing the file extension.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with imported result.


import <- function (folder = TRUE, extension="txt", 
                    suffix = NA, prefix = NA, 
                    resultFiles=NA, resultFolder=NA,
                    debug=FALSE){

  if(debug){
    print("IN: import")
    print("folder")
    print(folder)
    print("extension")
    print(extension)
    print("suffix")
    print(suffix)
    print("prefix")
    print(prefix)
    print("resultFiles")
    print(resultFiles)
    print("resultFolder")
    print(resultFolder)
  }
  
  
  manualPick <- is.na(resultFiles) && is.na(resultFolder)

  if(debug){
    print("manualPick")
    print(manualPick)
  }  
  
	# Check if result files in folder.
	if (folder) {
    
    if(manualPick){
  		# Ask user to select a folder.
  		resFolder <- choose.dir()
    } else {
      
      resFolder <- resultFolder
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
			resultFiles <- list.files(path = resFolder, pattern = fileFilter,
						full.names = TRUE, recursive = FALSE,
						ignore.case = TRUE, include.dirs = FALSE)
		}

	} else if (manualPick) {

		# Ask user to select a file.
		resultFiles <- file.choose()

	}


  if(debug){
    print("resultFiles")
    print(resultFiles)
  }  
  
  # Check if files are specified.
	if (length(resultFiles)>1 || !is.na(resultFiles)) {

		# Read first file to create data frame.		
		res <- read.table(resultFiles[1], header = TRUE,
				sep = "\t", fill = TRUE, stringsAsFactors=FALSE)

		# Create a colum name for file name.
		colName <- "File"
		if(colName %in% names(res)){
			tmpName <- make.unique(c(names(res),colName))
			colName <- tmpName[length(tmpName)]
		}

		# Add column and save file name.
		res[colName] <- basename(resultFiles[1])

		# Get number of files.
		files <- length(resultFiles)

		# Read additional files.
		if (files > 1) {
			for (f in 2:files) {

				# Read a file.	
				tmp <- read.table(resultFiles[f], header = TRUE,
						sep = "\t", fill = TRUE, stringsAsFactors=FALSE)

				# Add column and save file name.
				tmp[colName] <- basename(resultFiles[f])

				# Add to data frame.
				res <- rbind(res, tmp)

			}
		}
	
	}

	return(res)
}
