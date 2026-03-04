
# TODO: Update to use 'fread' when problem with colClasses is solved.

#' @title Import Data
#'
#' @description
#' Import text files and apply post processing.
#'
#' @details
#' Imports text files (e.g. GeneMapper results exported as text files)
#' as data frames. Options to import one or multiple files. For multiple
#' files it is possible to specify prefix, suffix, and file extension
#' to create a file name filter. The file name and/or file time stamp
#' can be imported.
#' NB! Empty strings ("") and NA strings ("NA") are converted to NA.
#' See \code{\link{list.files}} and \code{\link{read.table}} for additional details.
#'
#' @param folder logical, TRUE all files in folder will be imported,
#' FALSE only selected file will be imported.
#' @param suffix string, only files with specified suffix will be imported.
#' @param prefix string, only files with specified prefix will be imported.
#' @param import_file string if file name is provided file will be imported
#' without showing the file open dialogue.
#' @param folder_name string if folder name is provided files in folder
#' will be imported without showing the select folder dialogue.
#' @param extension string providing the file extension.
#' @param file_name logical if TRUE the file name is written in a column 'File.Name'.
#' NB! Any existing 'File.Name' column is overwritten.
#' @param time_stamp logical if TRUE the file modified time stamp is written
#' in a column 'Time'.
#' NB! Any existing 'Time' column is overwritten.
#' @param separator character for the delimiter used to separate columns
#'  (see 'sep' in \code{\link{read.table}} for details).
#' @param ignore_case logical indicating if case should be ignored. Only applies
#' to multiple file import option.
#' @param auto_trim logical indicating if dataset should be trimmed.
#' @param trim_samples character vector with sample names to trim.
#' @param trim_invert logical to keep (TRUE) or remove (FALSE) samples.
#' @param auto_slim logical indicating if dataset should be slimmed.
#' @param slim_na logical indicating if rows without data should remain.
#' @param na_strings character vector with strings to be replaced by NA.
#' @param debug logical indicating printing debug information.
#'
#'
#' @return data.frame with imported result.
#'
#' @export
#'
#' @importFrom plyr rbind.fill
#' @importFrom utils read.table
# @importFrom data.table fread
#'
#' @seealso \code{\link{trim}}, \code{\link{slim}}, \code{\link{list.files}}, \code{\link{read.table}}



import <- function(folder = TRUE, extension = "txt",
                   suffix = NA, prefix = NA,
                   import_file = NA, folder_name = NA,
                   file_name = TRUE, time_stamp = TRUE,
                   separator = "\t", ignore_case = TRUE,
                   auto_trim = FALSE, trim_samples = NULL,
                   trim_invert = FALSE,
                   auto_slim = FALSE, slim_na = TRUE,
                   na_strings = c("NA", ""),
                   debug = FALSE) {
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  if (debug) {
    print("folder")
    print(folder)
    print("extension")
    print(extension)
    print("suffix")
    print(suffix)
    print("prefix")
    print(prefix)
    print("import_file")
    print(import_file)
    print("folder_name")
    print(folder_name)
    print("file_name")
    print(file_name)
    print("time_stamp")
    print(time_stamp)
    print("ignore_case")
    print(ignore_case)
  }

  # Check data ----------------------------------------------------------------

  # Check data.
  if (is.na(import_file) && is.na(folder_name)) {
    stop("Either 'import_file' or 'folder_name' must be provided")
  }

  # Import --------------------------------------------------------------------

  # Initialise result data.frame (no match return empty dataframe)
  res <- data.frame()

  # Check if result files in folder.
  if (folder) {
    # Get path.
    folder <- folder_name

    # Check if folder is specified.
    if (!is.na(folder)) {
      # Create file filter.
      fileFilter <- paste(".*", sep = "")
      if (!is.na(prefix) && nchar(prefix) > 0) {
        fileFilter <- paste("^", prefix, fileFilter, sep = "")
        if (debug) {
          print("prefix added:")
          print(fileFilter)
        }
      }
      if (!is.na(suffix) && nchar(suffix) > 0) {
        fileFilter <- paste(fileFilter, suffix, sep = "")
        if (debug) {
          print("suffix added:")
          print(fileFilter)
        }
      }
      fileFilter <- paste(fileFilter, "\\.", extension, sep = "")
      if (debug) {
        print("fileFilter")
        print(fileFilter)
        print("folder")
        print(folder)
      }

      # Get list of files.
      import_file <- list.files(
        path = folder, pattern = fileFilter,
        full.names = TRUE, recursive = FALSE,
        ignore.case = ignore_case, include.dirs = FALSE
      )
    }
  }

  if (debug) {
    print("import_file")
    print(import_file)
  }

  # Check if files are specified.
  if (any(length(import_file) > 0, !is.na(import_file))) {
    # Autotrim message (function inside loop).
    if (auto_trim) {
      message(paste(
        "Auto trim samples:", trim_samples,
        " invert =", trim_invert
      ))
    }

    # Read files.
    for (f in seq(along = import_file)) {
      # TODO: Should change to more efficient and simpler 'fread' but
      # problem is that autodetection of colClasses does not always work
      # and it is not possible(?) to set all to character.
      # Use read.table for the time being.
      # Read a file.
      # tmpdf <- data.table::fread(import_file[f], data.table=FALSE)

      # Ensures column names are identical as when read.table was used.
      # Needed since many functions specify columns by name.
      # names(tmpdf) <- make.names(colnames(tmpdf))

      # Read a file.
      tmpdf <- read.table(import_file[f],
        header = TRUE,
        sep = separator, fill = TRUE,
        na_strings = na_strings,
        colClasses = "character",
        stringsAsFactors = FALSE
      )

      # Autotrim datset (message before loop).
      if (auto_trim) {
        tmpdf <- trim(
          data = tmpdf, samples = trim_samples,
          invert_s = trim_invert, debug = debug
        )
      }

      # Show progress.
      message(paste("Importing (", f, " of ", length(import_file), "): ",
        import_file[f],
        sep = ""
      ))

      # Check if file path should be saved.
      if (file_name) {
        # Add column and save file name.
        tmpdf$File.Name <- basename(import_file[f])
      }

      # Check if time stamp should be saved.
      if (time_stamp) {
        # Add column and save file name.
        tmptime <- file.info(import_file[f])
        tmpdf$File.Time <- as.character(tmptime$mtime)
      }

      # Check if multiple files.
      if (f > 1) {
        # Add to result data frame.
        res <- plyr::rbind.fill(res, tmpdf)
      } else {
        # Create result data frame.
        res <- tmpdf
      }
    }

    # Autoslim dataset.
    if (auto_slim) {
      # Autodetect column names to keep fixed.
      fixCol <- col_names(data = res, slim = TRUE, numbered = TRUE, concatenate = "|")

      # Autodetect column names to stack.
      stackCol <- col_names(data = res, slim = FALSE, numbered = TRUE, concatenate = "|")

      # Progress.
      message("Auto slim dataset...")
      message(paste("  Stack columns:", stackCol))
      message(paste("  Fix columns:", fixCol))

      # Slim require a vector of strings.
      fixCol <- unlist(strsplit(fixCol, "|", fixed = TRUE))
      stackCol <- unlist(strsplit(stackCol, "|", fixed = TRUE))

      # Slim data.
      res <- slim(
        data = res, fix = fixCol, stack = stackCol,
        keep_na = slim_na, debug = debug
      )
    }
  }

  # Update audit trail.
  res <- audit_trail(obj = res, f_call = match.call(), package = "strvalidator")

  # Convert common known numeric columns.
  res <- col_convert(data = res)

  return(res)
}
