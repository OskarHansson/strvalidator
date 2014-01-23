################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 01.10.2013: First version.

#' @title Get allele frequency database
#'
#' @description
#' \code{getDb} gives access to allele frequency databases.
#'
#' @details
#' The function provides access to allele frequency databases stored in
#' the file database.txt in the package directory.
#' It returns the specified allele frequency database.
#' If no matching database or database index is found NA is returned.
#' If NULL a vector of available databases is returned.
#' 
#' @param dbNameOrIndex string or integer specifying the database.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with allele frequency database information.
#' 
#' @keywords internal
#' 
#' @export 
#' @examples
#' # Show available allele frequency databases.
#' getDb()

getDb <- function(dbNameOrIndex=NULL, debug=FALSE) {

  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }

  .separator <- .Platform$file.sep # Platform dependent path separator.
  
  # LOAD DATABASE INFO  #######################################################

  # Get package path.
  packagePath <- path.package("strvalidator", quiet = FALSE)
  subFolder <- "extdata"
  fileName <- "database.txt"
  
  filePath <- paste(packagePath, subFolder, fileName, sep=.separator)
  
  .db <- read.delim(file=filePath, header = TRUE, sep = "\t", quote = "\"",
                         dec = ".", fill = TRUE, stringsAsFactors=FALSE)
    
  # Available databases. Must match else if construct.
  databases<-unique(.db$Database)
  
	# Check if NULL
	if (is.null(dbNameOrIndex)) {

		db <- databases

	# String provided.
	} else {

		# Check if number or string.
		if (is.numeric(dbNameOrIndex)) {

			# Set index to number.
			index <- dbNameOrIndex

		} else {

			# Find matching database index (case insensitive)
			index <- match(toupper(dbNameOrIndex),toupper(databases))

		}

		# No matching database.
		if (is.na(index)) {
			
			db <- NA

		# Assign matching database information.
		} else {
		  
		  db <- .db[.db$Database==databases[index], ]
      
		} 

	}
  
  return(db)

}
