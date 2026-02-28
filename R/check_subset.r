#' @title Check Subset
#'
#' @description
#' Check the result of subsetting, optionally reversing the match direction.
#'
#' @details
#' When \code{reverse = FALSE} (default), checks which data sample names
#' contain each reference name.  
#' When \code{reverse = TRUE}, checks which reference names are found
#' within each data sample name (useful for mixture naming).
#'
#' @param data a data frame in GeneMapper format containing column \code{Sample.Name}.
#' @param ref a data frame in GeneMapper format containing column \code{Sample.Name},
#'  or an atomic vector (e.g. a single sample name string).
#' @param console logical; if \code{TRUE}, result is printed to the console,
#' otherwise a string is returned.
#' @param ignore_case logical; if \code{TRUE}, case-insensitive matching is used.
#' @param word logical; if \code{TRUE}, only word matching (regex word boundaries).
#' @param exact logical; if \code{TRUE}, requires an exact match.
#' @param reverse logical; if \code{TRUE}, reverses direction to data→ref matching.
#' @param debug logical indicating printing of debug information.
#'
#' @return If \code{console = TRUE}, prints results to the console.
#' If \code{console = FALSE}, returns a character string with formatted results.
#'
#' @seealso \code{\link{grep}}
#' @aliases checkSubset
#' @export

check_subset <- function(data, ref, console = TRUE, ignore_case = TRUE,
                        word = FALSE, exact = FALSE, reverse = FALSE,
                        debug = FALSE) {
  if (debug) {
    message("--- check_subset() debug ---")
    print(list(ignore_case = ignore_case, word = word, exact = exact, reverse = reverse))
  }
  
  res <- list()
  
  # ---- Extract reference names ----
  if (is.atomic(ref)) {
    ref_names <- ref
  } else if ("Sample.Name" %in% names(ref)) {
    ref_names <- unique(ref$Sample.Name)
  } else if ("Sample.File.Name" %in% names(ref)) {
    ref_names <- unique(ref$Sample.File.Name)
  } else if (any(grepl("SAMPLE", names(ref), ignore.case = TRUE))) {
    sample_col <- names(ref)[grep("SAMPLE", names(ref), ignore.case = TRUE)[1]]
    ref_names <- unique(ref[[sample_col]])
  } else {
    stop("'ref' must contain a column 'Sample.Name', 'Sample.File.Name', or 'Sample'")
  }
  
  # ---- Extract data sample names ----
  if ("Sample.Name" %in% names(data)) {
    samples <- unique(data$Sample.Name)
  } else if ("Sample.File.Name" %in% names(data)) {
    samples <- unique(data$Sample.File.Name)
  } else if (any(grepl("SAMPLE", names(data), ignore.case = TRUE))) {
    sample_col <- names(data)[grep("SAMPLE", names(data), ignore.case = TRUE)[1]]
    samples <- unique(data[[sample_col]])
  } else {
    stop("'data' must contain a column 'Sample.Name', 'Sample.File.Name', or 'Sample'")
  }
  
  # ---- Defensive checks ----
  if (length(ref_names) == 0) {
    warning("No reference names found.")
    return(invisible(NULL))
  }
  if (length(samples) == 0) {
    warning("No sample names found in 'data'.")
    return(invisible(NULL))
  }
  
  # ---- Pattern helper ----
  make_pattern <- function(x) {
    if (word)  x <- paste0("\\b", x, "\\b")
    if (exact) x <- paste0("^", x, "$")
    x
  }
  
  # ---- Matching direction ----
  if (!reverse) {
    # normal direction: which data samples match each reference name?
    for (n in seq_along(ref_names)) {
      cRef <- make_pattern(ref_names[n])
      cSamples <- grep(cRef, samples, value = TRUE, ignore.case = ignore_case)
      matched <- if (length(cSamples) == 0) "<none>" else paste(cSamples, collapse = ", ")
      res[n] <- paste("Reference name: ", ref_names[n], "\n",
                      "Matched samples: ", matched, "\n\n", sep = "")
      if (debug) print(list(ref = cRef, matched_samples = cSamples))
    }
  } else {
    # reversed direction: which reference names are found in each data sample?
    for (n in seq_along(samples)) {
      matched_refs <- c()
      for (r in ref_names) {
        pattern <- make_pattern(r)
        if (length(grep(pattern, samples[n], ignore.case = ignore_case)) > 0) {
          matched_refs <- c(matched_refs, r)
        }
      }
      matched <- if (length(matched_refs) == 0) "<none>" else paste(matched_refs, collapse = ", ")
      res[n] <- paste("Sample name: ", samples[n], "\n",
                      "Matched references: ", matched, "\n\n", sep = "")
      if (debug) print(list(sample = samples[n], matched_refs = matched_refs))
    }
  }
  
  # ---- Output ----
  txt <- paste(unlist(res), collapse = "")
  if (console) cat(txt, sep = "\n") else return(txt)
}

################################################################################
#' @rdname check_subset
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [check_subset()] instead.
################################################################################

checkSubset <- function(data, 
                        ref, 
                        console = TRUE, 
                        ignore.case = TRUE,
                        word = FALSE, 
                        exact = FALSE, 
                        reverse = FALSE,
                        debug = FALSE,
                        ...) {
  
  .Deprecated("check_subset", package = "strvalidator")
  
  # Remap arguments
  check_subset(
    data = data,
    ref = ref,
    console = console,
    ignore_case = ignore.case,
    word = word,
    exact = exact,
    reverse = reverse,
    debug = debug,
    ...
  )
}