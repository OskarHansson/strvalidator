################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 28.10.2013: First version.

#' @title Load packages
#'
#' @description
#' \code{loadPackages} loads the specified packages.
#'
#' @details
#' This is a helper function. Provided a string vector of package names it
#' tries to load the packages. If unsuccessful  it tries to install the package
#' and then load it. If still unsuccessful the execution is halted or a warning
#' is issued (option).
#' 
#' @param packages string vector of package names.
#' @param halt logical if TRUE a 'stop' is issued on failure, else a 'warning' is given.
#' @param silent logical if TRUE messages are printed.
#' 
#' @keywords internal

loadPackage <- function(packages, halt=TRUE, silent=FALSE){
  
  # Loop over all packages.
  for(p in seq(along=packages)){
    
    # Attempt to load the package.
    if(!require(packages[p], character.only = TRUE)){
      
      if(!silent){
        message(paste("Trying to install", packages[p]))
      }

      # Install package.
      install.packages(packages[p])
      
      # Try loading again.
      if(require(packages[p], character.only = TRUE)){
        
        if(!silent){
          message(paste(packages[p], "installed and loaded"))
        }
        
      } else {

        # Check if stop or warning.
        if(halt){
          stop(paste("Could not install", packages[p]))
        } else {
          warning(paste("Could not install", packages[p]))
        }
        
      }
    }    
    
  }
  
  return(NULL)
  
}






