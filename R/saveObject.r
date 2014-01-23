################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 20.01.2014: Added 'debug' parameter.
# 17.07.2013: First version.

#' @title Save object
#'
#' @description
#' Saves an object in the specified environment.
#'
#' @details Saves an object with the given name in the specified environment
#' if it does not exist.
#' If the object exist a message box ask if the object should be overwritten.
#' 
#' @param name character giving the name of the object.
#' @param object object to save.
#' @param parent object specifying the parent GUI object to center the message box.
#' @param env environment in wich to save and search for existing objects.
#' @param debug logical indicating printing debug information.
#' 
#' @return logical TRUE if object was saved FALSE if not.
#' 

saveObject <- function(name, object, parent=NULL,
                       env=parent.frame(), debug=FALSE){

  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("name:")
    print(name)
  }
  
  # Initiate flag.
  ok <- TRUE
  
  # Check that a name has been provided for the new data object.
  if(nchar(name) > 0){
    
    # Check for existing object and ask for user input.
    if(exists(name, envir=env, inherits = FALSE)){
      
      dialog <- gbasicdialog(title="Warning!", parent=parent, do.buttons=TRUE)
      
      msg <- glabel(text=paste("An object named '",name,"' already exist.\n\n",
                               "Do you want to overwrite?", sep=""),
                    container=dialog)
      
      ok <- visible(dialog, set=TRUE) # Set flag by user input.
      
    }
    
    if(ok){
      
      # Save data.
      assign(name, object, envir=env)
      
    } else {
      
      # Ask for new name.
      name <- ginput(message="New name",
                     text=name, 
                     title="Input", 
                     icon = "info",
                     parent=parent)
      
      # Exit if cancel.
      if(is.na(name)){
        return(ok)
      }
      
    }
    
  } else {
    gmessage("A name must be provided.",
             title="Error", icon="error", parent=parent)
    ok <- FALSE  # Set flag.
  }

  return(ok)

}
