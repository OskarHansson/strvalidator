################################################################################
# CHANGE LOG (last 20 changes)
# 03.05.2020: Added language support.
# 20.02.2019: Fixed "error in if(ok) argument not logical" using tcltk.
# 29.04.2016: Fixed object not saved if an object existed and the new object is renamed.
# 18.07.2014: Added syntactically valid name check.
# 18.07.2014: Added 'remove' and 'suggested' parameter.
# 20.01.2014: Added 'debug' parameter.
# 17.07.2013: First version.

#' @title Save Object
#'
#' @description
#' Save an object in the specified environment.
#'
#' @details Saves an object with the given name in the specified environment
#' if it does not exist. If the object exist a message box ask if the object
#' should be overwritten. The function can also be used to re-name if 'name'
#' is not provided (NULL). The 'suggest' provides a suggested name for the
#' object to re-name.
#'
#' @param name character giving the name of the object to save.
#' If NULL a dialogue asks for a name.
#' @param object object to save.
#' @param parent object specifying the parent GUI object to center the message box.
#' @param suggest character string for a suggested name for the object to save/re-name.
#' @param env environment in which to save and search for existing objects.
#' @param remove character string for a named object to remove (e.g. the original object if re-naming).
#' @param debug logical indicating printing debug information.
#'
#' @keywords internal
#'
#' @return logical TRUE if object was saved FALSE if not.
#'

saveObject <- function(name = NULL, object, parent = NULL, suggest = "",
                       env = parent.frame(), remove = NULL, debug = FALSE) {

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strMsgName <- "Enter name"
  strMsgSyntax <- "is not a syntactically valid name!\n\nThe object will be saved as:"
  strMsgExists <- "already exist.\n\nDo you want to overwrite?"
  strMsgNewName <- "New name"
  strMsgMissing <- "A name must be provided."
  strMsgTitleInput <- "Input"
  strMsgTitleInvalid <- "Invalid name"
  strMsgTitleWarning <- "Warning!"
  strMsgTitleError <- "Error"

  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  if (!is.na(dtStrings)) {
    # Get language strings, use default if not found.

    strTmp <- dtStrings["strMsgName"]$value
    strMsgName <- ifelse(is.na(strtmp), strMsgName, strtmp)

    strTmp <- dtStrings["strMsgSyntax"]$value
    strMsgSyntax <- ifelse(is.na(strtmp), strMsgSyntax, strtmp)

    strTmp <- dtStrings["strMsgExists"]$value
    strMsgExists <- ifelse(is.na(strtmp), strMsgExists, strtmp)

    strTmp <- dtStrings["strMsgNewName"]$value
    strMsgNewName <- ifelse(is.na(strtmp), strMsgNewName, strtmp)

    strTmp <- dtStrings["strMsgMissing"]$value
    strMsgMissing <- ifelse(is.na(strtmp), strMsgMissing, strtmp)

    strTmp <- dtStrings["strMsgTitleInput"]$value
    strMsgTitleInput <- ifelse(is.na(strtmp), strMsgTitleInput, strtmp)

    strTmp <- dtStrings["strMsgTitleInvalid"]$value
    strMsgTitleInvalid <- ifelse(is.na(strtmp), strMsgTitleInvalid, strtmp)

    strTmp <- dtStrings["strMsgTitleWarning"]$value
    strMsgTitleWarning <- ifelse(is.na(strtmp), strMsgTitleWarning, strtmp)

    strTmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)
  }

  # FUNCTION ##################################################################

  if (debug) {
    print("name:")
    print(name)
    print("names(object)")
    print(names(object))
  }

  # Initiate flag.
  ok <- TRUE

  # Check if name is provided.
  if (is.null(name)) {

    # Show dialogue.
    name <- ginput(
      msg = strMsgName, text = suggest,
      title = strMsgTitleInput, icon = "info", parent = parent
    )

    if (is.na(name)) {
      if (debug) {
        print("User pressed cancel!")
      }

      # Return FALSE.
      return(ok)
    }

    if (debug) {
      print(paste("Input name:", name))
    }
  }

  # Check that a name has been provided for the new data object.
  if (nchar(name) > 0) {

    # Make syntactically valid name.
    orgName <- name
    name <- make.names(name)

    if (name != orgName) {

      # Show message.
      gmessage(
        msg = paste(orgName, strMsgSyntax, name),
        title = strMsgTitleInvalid,
        icon = "warning",
        parent = parent
      )
    }

    # Check for existing object and ask for user input.
    if (exists(name, envir = env, inherits = FALSE)) {
      if (debug) {
        print(paste("Object", name, "exists!"))
      }

      ok <- gconfirm(
        msg = paste(name, strMsgExists),
        title = strMsgTitleWarning,
        icon = "warning", parent = parent
      )
    }

    if (ok) {

      # Save data.
      assign(name, object, envir = env)

      if (debug) {
        print(paste("Object", name, "saved!"))
      }
    } else {

      # Ask for new name.
      name <- ginput(
        msg = strMsgNewName,
        text = name,
        title = strMsgTitleInput,
        icon = "info", parent = parent
      )

      # Exit if cancel.
      if (is.na(name)) {
        if (debug) {
          print("User pressed cancel!")
        }

        # Return FALSE.
        return(ok)
      } else {

        # Save data.
        assign(name, object, envir = env)

        if (debug) {
          print(paste("New name:", name))
        }
      }
    }

    # Remove only if different from final name.
    if (!is.null(remove) && remove != name) {

      # Delete object.
      remove(list = remove, envir = env)

      if (debug) {
        print(paste("Object", remove, "deleted!"))
      }
    }
  } else {
    gmessage(
      msg = strMsgMissing,
      title = strMsgTitleError,
      icon = "error",
      parent = parent
    )

    ok <- FALSE # Set flag.
  }

  return(ok)
}
