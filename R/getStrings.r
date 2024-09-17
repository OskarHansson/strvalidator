################################################################################
# CHANGE LOG (last 20 changes)
# 17.08.2024: Added helper functions for string handling and updated getStrings.
# 15.08.2024: Fixed spelling error, removed unnecessary duplicate code.
# 07.07.2022: Fixed Warning message: In !is.na(result) && !is.na(gui):'length(x) = 780 > 1' in coercion to 'logical(1)'
# 08.07.2020: Fix warnings about improper quoting.
# 04.07.2020: Defined unbound variables.
# 09.06.2020: Fixed Error in `:=`(value, gsub("\\n", "\n", value, fixed = TRUE))...
# 22.02.2020: First version.

#' @title Get Language Strings
#'
#' @description
#' Accepts a language code and GUI. Returns the corresponding language strings.
#'
#' @details
#' Accepts a language code, GUI, and key. Returns the corresponding language strings
#' for the specified GUI function or key from a text file named as the language code.
#' Replaces backslash + n with a new line character (only if 'GUI' is specified).
#'
#' @param language character name of the language.
#' @param gui character the function name for the GUI to 'translate'.
#' @param key character the key to 'translate'. Only used in combination with 'gui'.
#' @param encoding character encoding to be assumed for input strings.
#' @param about logical FALSE (default) to read key-value pairs,
#' TRUE to read about file as plain text.
#'
#' @return character vector or data.table with the retrieved values.
#' NULL if file or GUI was not found.
#'
#' @export
#'

getStrings <- function(language = NA, gui = NA, key = NA,
                       encoding = NA, about = FALSE) {
  if (is.na(language)) {
    language <- getSetting("language")
  }
  
  if (is.na(encoding)) {
    encoding <- getSetting("encoding")
  }
  
  scope <- NULL
  value <- NULL
  
  file_sep <- .Platform$file.sep
  language_file <- paste0(language, ".txt")
  about_file <- paste0(language, "_about.txt")
  sub_folder <- paste("extdata", "languages", sep = file_sep)
  
  package_path <- path.package("strvalidator", quiet = FALSE)
  
  if (about) {
    about_file_path <- file.path(package_path, sub_folder, about_file)
    
    if (file.exists(about_file_path)) {
      result <- readLines(con = about_file_path, encoding = encoding)
      
      if (!is.null(result)) {
        result <- gsub("\\n", "\n", result, fixed = TRUE)
      }
    } else {
      message("File ", about_file_path, " not found. Returning NULL.")
      result <- NULL
    }
  } else {
    lang_file_path <- file.path(package_path, sub_folder, language_file)
    
    if (file.exists(lang_file_path)) {
      dt_all <- fread(file = lang_file_path, sep = "\t", header = "auto", encoding = encoding, quote = "")
      setkey(dt_all, "key")
      result <- dt_all
      
      if (!is.na(gui)) {
        message("Get language strings for gui = ", gui)
        result <- result[scope == gui, ]
        
        if (nrow(result) == 0) {
          message("No rows found for gui = ", gui, ". Returning NULL.")
          result <- NULL
        }
      }
      
      if (!is.null(result) && !is.na(gui) && !is.na(key)) {
        message("Get language strings for key = ", key)
        result <- result[key]$value
      }
      
      if (!is.null(result)) {
        result[, value := gsub("\\n", "\n", value, fixed = TRUE)]
      }
    } else {
      message("File ", lang_file_path, " not found. Returning NULL.")
      result <- NULL
    }
  }
  
  return(result)
}

#' @title Update Strings with Language File
#'
#' @description
#' Updates the default strings with the values from the language file.
#'
#' @param default_strings list of default strings.
#' @param language_strings list of language-specific strings.
#'
#' @return list of updated strings.
#'
#' @export
#'

update_strings_with_language_file <- function(default_strings, language_strings) {
  if (is.null(language_strings)) {
    return(default_strings)
  }
  
  for (key in names(default_strings)) {
    if (key %in% names(language_strings)) {
      default_strings[[key]] <- language_strings[[key]]
    }
  }
  
  return(default_strings)
}
