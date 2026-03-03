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

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_MSG_NAME            = "Enter name",
    STR_MSG_SYNTAX          = "is not a syntactically valid name!\n\nThe object will be saved as:",
    STR_MSG_EXISTS          = "already exist.\n\nDo you want to overwrite?",
    STR_MSG_NEW_NAME        = "New name",
    STR_MSG_MISSING         = "A name must be provided.",
    STR_MSG_TITLE_INPUT     = "Input",
    STR_MSG_TITLE_INVALID   = "Invalid name",
    STR_MSG_TITLE_WARNING   = "Warning!",
    STR_MSG_TITLE_ERROR     = "Error"
  )

  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

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
      msg = strings$STR_MSG_NAME, text = suggest,
      title = strings$STR_MSG_TITLE_INPUT, icon = "info", parent = parent
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
        msg = paste(orgName, strings$STR_MSG_SYNTAX, name),
        title = strings$STR_MSG_TITLE_INVALID,
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
        msg = paste(name, strings$STR_MSG_EXISTS),
        title = strings$STR_MSG_TITLE_WARNING,
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
        msg = strings$STR_MSG_NEW_NAME,
        text = name,
        title = strings$STR_MSG_TITLE_INPUT,
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
      msg = strings$STR_MSG_MISSING,
      title = strings$STR_MSG_TITLE_ERROR,
      icon = "error",
      parent = parent
    )

    ok <- FALSE # Set flag.
  }

  return(ok)
}
