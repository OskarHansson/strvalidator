#' @title Get Language Strings
#'
#' @description
#' Returns language-specific strings for a given GUI or key from the package'ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢s'
#' localization files.
#'
#' @details
#' Language files are stored under `inst/extdata/languages/` in tab-delimited
#' format with columns `scope`, `key`, and `value`. When a GUI name is provided,
#' the function returns all strings for that scope; when both `gui` and `key`
#' are provided, it returns the specific translation value.
#'
#' @param language character; language code (e.g. `"en"`, `"sv"`).
#' @param gui character; the GUI function name to retrieve strings for.
#' @param key character; specific key to retrieve within the selected GUI.
#' @param encoding character; file encoding to assume (default taken from settings).
#' @param about logical; if `TRUE`, reads the `_about.txt` file as plain text.
#' @param debug Logical; if \code{TRUE}, print detailed diagnostic messages.
#'
#' @return A `data.table` or character vector containing the requested strings,
#' or `NULL` if the file or scope was not found.
#'
#' @importFrom data.table fread setkey
#' @importFrom utils read.table
#'
#' @aliases getStrings
#' @export

get_strings <- function(language = NA, gui = NA, key = NA,
                        encoding = NA, about = FALSE, debug = FALSE) {
  if (is.na(language)) {
    language <- get_setting("language")
  }

  if (is.na(encoding)) {
    encoding <- get_setting("encoding")
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
      dt_all <- fread(file = lang_file_path, sep = "\t", 
                      header = "auto", 
                      encoding = encoding, quote = "")
      setkey(dt_all, "key")
      result <- dt_all

      if (!is.na(gui)) {
        if (isTRUE(debug)) message("Get language strings for gui = ", gui)
        result <- result[scope == gui, ]

        if (nrow(result) == 0) {
          if (isTRUE(debug)) message("No rows found for gui = ", 
                                     gui, ". Returning NULL.")
          result <- NULL
        }
      }

      if (!is.null(result) && !is.na(gui) && !is.na(key)) {
        if (isTRUE(debug)) message("Get language strings for key = ", key)
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

  invisible(result)
}

#' @title Update Strings with Language File
#'
#' @description
#' Merges a set of default strings with overrides from a language file.
#'
#' @param default_strings list of default strings.
#' @param language_strings list of language-specific replacements.
#'
#' @return A list of updated strings with localized overrides applied.
#' @export
update_strings_with_language_file <- function(default_strings, 
                                              language_strings) {
  if (is.null(language_strings)) {
    return(default_strings)
  }

  for (key in names(default_strings)) {
    if (key %in% names(language_strings)) {
      default_strings[[key]] <- language_strings[[key]]
    }
  }

  invisible(default_strings)
}

#' @title Get the GUI scope name safely
#'
#' @description
#' Detects the name of the currently running GUI function automatically.
#' This is typically used to load the correct language string set for the
#' active GUI (for example, in `get_strings(gui = ...)`).
#'
#' It works by inspecting the current function call stack via
#' [base::sys.call()], and safely extracting the function name.
#'
#' If detection fails (for example, when called from an event handler,
#' `do.call()`, or interactive context), it returns a fallback name
#' (`"default"` by default).
#'
#' @param default character. The name to return if automatic detection fails.
#' Defaults to `"default"`.
#'
#' @return A character string containing the current function name or the
#' fallback value.
#'
#' @examples
#' \dontrun{
#' calculate_concordance_gui <- function() {
#'   gui_scope <- get_gui_scope()
#'   lng_strings <- get_strings(gui = gui_scope)
#'   print(gui_scope)
#' }
#' calculate_concordance_gui()
#' #> "calculate_concordance_gui"
#' }
#'
#' @export
#'
get_gui_scope <- function(default = "default") {
  gui_name <- tryCatch(
    as.character(sys.call()[[1]]),
    error = function(e) NA
  )
  
  if (is.na(gui_name) || gui_name == "") {
    gui_name <- default
  }
  
  return(gui_name)
}

