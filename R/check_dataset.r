#' @title Check Dataset
#'
#' @description
#' Validate that a dataset exists and contains the required structure before 
#' analysis.
#'
#' @details
#' Performs these checks in order:
#' * dataset exists in the specified environment
#' * dataset has at least one row
#' * required columns exist
#' * slim/fat structure is correct (substring-based column matching)
#' * specified columns do not contain invalid string values
#'
#' Returns `TRUE` if all checks pass, otherwise returns `FALSE` and
#' displays a message in the console or via a GUI parent widget.
#'
#' @param name Character name of the data.frame to check.
#' @param reqcol Character vector of required column names.
#' @param slim Logical indicating whether to enforce "slim" structure.
#' @param slimcol Character vector of substrings used to test slim/fat structure.
#' @param string Character vector of invalid values.
#' @param stringcol Character vector of columns to scan for invalid values.
#' @param env Environment in which the dataset is located.
#' @param parent Optional gWidgets widget for displaying GUI messages.
#' @param debug Logical; print debugging output.
#'
#' @export
check_dataset <- function(
    name,
    reqcol     = NULL,
    slim       = FALSE,
    slimcol    = NULL,
    string     = NULL,
    stringcol  = NULL,
    env        = parent.frame(),
    parent     = NULL,
    debug      = FALSE
) {
  fnc <- get_gui_scope()
  
  if (debug) {
    message("IN: ", fnc)
    print(list(
      name      = name,
      reqcol    = reqcol,
      slim      = slim,
      slimcol   = slimcol,
      string    = string,
      stringcol = stringcol,
      env       = environmentName(env),
      parent    = parent
    ))
  }
  
  # ---------------------------------------------------------------------------
  # Language Strings
  # ---------------------------------------------------------------------------
  
  # Load language file for this function
  lng_strings <- get_strings(gui = fnc)
  
  # Default strings
  default_strings <- list(
    STR_MSG_NA             = "Dataset does not exist.",
    STR_MSG_NO_DF          = "Object exists but is not a data.frame.",
    STR_MSG_NO_ROWS        = "Dataset contain no rows!",
    STR_MSG_REQ_COLS       = "Additional columns required:\n",
    STR_MSG_FAT1           = "The dataset is too fat!\n\nThere can only be 1 column:",
    STR_MSG_FAT2           = "\nPlease slim the dataset.",
    STR_MSG_DETECTED1      = "detected in column",
    STR_MSG_DETECTED2      = "Please make sure that data is clean/filtered.\n"
  )
  
  # Override defaults with language-specific values
  strings <- update_strings_with_language_file(default_strings, lng_strings$value)
  
  # ---------------------------------------------------------------------------
  # Helper to show errors
  # ---------------------------------------------------------------------------
  show_error <- function(msg) {
    if (is.null(parent)) {
      message(msg)
    } else {
      gmessage(msg, title = fnc, icon = "error", parent = parent)
    }
  }
  
  # ---------------------------------------------------------------------------
  # Main Validation Logic
  # ---------------------------------------------------------------------------
  
  # 1) Dataset exists
  if (!exists(name, envir = env, inherits = FALSE)) {
    show_error(strings$STR_MSG_NA)
    return(FALSE)
  }
  
  df <- get(name, envir = env)
  
  # 2) Must be a data.frame
  if (!is.data.frame(df)) {
    show_error(strings$STR_MSG_NO_DF)
    return(FALSE)
  }
  
  # 3) Must have rows
  if (nrow(df) == 0) {
    show_error(strings$STR_MSG_NO_ROWS)
    return(FALSE)
  }
  
  # 4) Required columns
  if (!is.null(reqcol)) {
    missing_cols <- setdiff(reqcol, colnames(df))
    if (length(missing_cols) > 0) {
      msg <- paste0(strings$STR_MSG_REQ_COLS,
                    paste(missing_cols, collapse = "\n"))
      show_error(msg)
      return(FALSE)
    }
  }
  
  # 5) Slim/fat structure
  if (slim && !is.null(slimcol)) {
    for (pattern in slimcol) {
      matched <- grep(pattern, colnames(df), fixed = TRUE)
      if (length(matched) != 1) {
        msg <- paste0(
          strings$STR_MSG_FAT1, " ", pattern, strings$STR_MSG_FAT2
        )
        show_error(msg)
        return(FALSE)
      }
    }
  }
  
  # 6) Invalid strings
  if (!is.null(string) && !is.null(stringcol)) {
    for (col in stringcol) {
      if (!col %in% colnames(df)) next
      
      values <- df[[col]]
      found <- string[string %in% values]
      
      if (length(found) > 0) {
        msg <- paste0(
          "'", paste(found, collapse = ", "), "' ",
          strings$STR_MSG_DETECTED1, " ", col, "!\n",
          strings$STR_MSG_DETECTED2
        )
        show_error(msg)
        return(FALSE)
      }
    }
  }
  
  if (debug) message("EXIT: ", fnc, " (ok = TRUE)")
  return(TRUE)
}

