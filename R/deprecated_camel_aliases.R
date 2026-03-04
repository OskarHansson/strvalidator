#' @title Deprecated camelCase aliases
#' @description
#' Backward-compatible wrappers for renamed snake_case functions.

.to_snake_arg_name <- function(x) {
  x <- gsub("\\.", "_", x)
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x, perl = TRUE)
  x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", x, perl = TRUE)
  tolower(x)
}

.remap_legacy_args <- function(args) {
  nms <- names(args)
  if (is.null(nms) || !length(nms)) return(args)
  new_nms <- vapply(nms, .to_snake_arg_name, character(1))
  for (i in seq_along(nms)) {
    if (is.na(nms[i]) || nms[i] == "") next
    if (new_nms[i] != nms[i] && !(new_nms[i] %in% nms)) {
      nms[i] <- new_nms[i]
    }
  }
  names(args) <- nms
  args
}

#' @rdname add_data
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [add_data()] instead.
addData <- function(...) {
  .Deprecated("add_data", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(add_data, args)
}

#' @rdname add_color
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [add_color()] instead.
addColor <- function(...) {
  .Deprecated("add_color", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(add_color, args)
}

#' @rdname add_marker
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [add_marker()] instead.
addMarker <- function(...) {
  .Deprecated("add_marker", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(add_marker, args)
}

#' @rdname add_order
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [add_order()] instead.
addOrder <- function(...) {
  .Deprecated("add_order", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(add_order, args)
}

#' @rdname add_size
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [add_size()] instead.
addSize <- function(...) {
  .Deprecated("add_size", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  if ("bins" %in% names(args)) {
    message("Argument 'bins' is deprecated and ignored. Allele-specific sizes from 'kit' are always used when available.")
    args$bins <- NULL
  }
  do.call(add_size, args)
}

#' @rdname audit_trail
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [audit_trail()] instead.
auditTrail <- function(...) {
  .Deprecated("audit_trail", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(audit_trail, args)
}

#' @rdname calculate_allele
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_allele()] instead.
calculateAllele <- function(...) {
  .Deprecated("calculate_allele", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_allele, args)
}

#' @rdname calculate_all_t
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_all_t()] instead.
calculateAllT <- function(...) {
  .Deprecated("calculate_all_t", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_all_t, args)
}

#' @rdname calculate_at
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_at()] instead.
calculateAT <- function(...) {
  .Deprecated("calculate_at", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_at, args)
}

#' @rdname calculate_at6
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_at6()] instead.
calculateAT6 <- function(...) {
  .Deprecated("calculate_at6", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_at6, args)
}

#' @rdname calculate_capillary
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_capillary()] instead.
calculateCapillary <- function(...) {
  .Deprecated("calculate_capillary", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_capillary, args)
}

#' @rdname calculate_concordance
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_concordance()] instead.
calculateConcordance <- function(...) {
  .Deprecated("calculate_concordance", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_concordance, args)
}

#' @rdname calculate_copies
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_copies()] instead.
calculateCopies <- function(...) {
  .Deprecated("calculate_copies", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_copies, args)
}

#' @rdname calculate_dropout
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_dropout()] instead.
calculateDropout <- function(...) {
  .Deprecated("calculate_dropout", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_dropout, args)
}

#' @rdname calculate_hb
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_hb()] instead.
calculateHb <- function(...) {
  .Deprecated("calculate_hb", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_hb, args)
}

#' @rdname calculate_height
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_height()] instead.
calculateHeight <- function(...) {
  .Deprecated("calculate_height", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_height, args)
}

#' @rdname calculate_mixture
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_mixture()] instead.
calculateMixture <- function(...) {
  .Deprecated("calculate_mixture", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  ref1 <- if ("ref1" %in% names(args)) args$ref1 else NULL
  ref2 <- if ("ref2" %in% names(args)) args$ref2 else NULL
  if (!is.null(ref1) && !is.null(ref2)) args$ref_profiles_df <- rbind(ref1, ref2)
  args$ref1 <- NULL
  args$ref2 <- NULL
  if ("ignore_dropout" %in% names(args)) {
    args$include_dropout <- args$ignore_dropout
    args$ignore_dropout <- NULL
  }
  if (!("threshold" %in% names(args))) args$threshold <- 0
  if (!("dropout_pseudo_rfu" %in% names(args))) args$dropout_pseudo_rfu <- NULL
  if (!("match_case" %in% names(args))) args$match_case <- FALSE
  if (!("ol_str" %in% names(args))) args$ol_str <- "OL"
  if (!("auto_minor_from_mx" %in% names(args))) args$auto_minor_from_mx <- FALSE
  if (!("output_level" %in% names(args))) args$output_level <- "marker"
  if (length(args$output_level) > 1) args$output_level <- args$output_level[1]
  do.call(calculate_mixture, args)
}

#' @rdname calculate_lb
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_lb()] instead.
calculateLb <- function(...) {
  .Deprecated("calculate_lb", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_lb, args)
}

#' @rdname calculate_ol
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_ol()] instead.
calculateOL <- function(...) {
  .Deprecated("calculate_ol", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_ol, args)
}

#' @rdname calculate_overlap
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_overlap()] instead.
calculateOverlap <- function(...) {
  .Deprecated("calculate_overlap", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_overlap, args)
}

#' @rdname calculate_peaks
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_peaks()] instead.
calculatePeaks <- function(...) {
  .Deprecated("calculate_peaks", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_peaks, args)
}

#' @rdname calculate_pullup
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_pullup()] instead.
calculatePullup <- function(...) {
  .Deprecated("calculate_pullup", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_pullup, args)
}

#' @rdname calculate_ratio
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_ratio()] instead.
calculateRatio <- function(...) {
  .Deprecated("calculate_ratio", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_ratio, args)
}

#' @rdname calculate_result_type
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_result_type()] instead.
calculateResultType <- function(...) {
  .Deprecated("calculate_result_type", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_result_type, args)
}

#' @rdname calculate_slope
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_slope()] instead.
calculateSlope <- function(...) {
  .Deprecated("calculate_slope", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_slope, args)
}

#' @rdname calculate_spike
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_spike()] instead.
calculateSpike <- function(...) {
  .Deprecated("calculate_spike", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_spike, args)
}

#' @rdname calculate_statistics
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_statistics()] instead.
calculateStatistics <- function(...) {
  .Deprecated("calculate_statistics", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_statistics, args)
}

#' @rdname calculate_stutter
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_stutter()] instead.
calculateStutter <- function(...) {
  .Deprecated("calculate_stutter", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_stutter, args)
}

#' @rdname calculate_t
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_t()] instead.
calculateT <- function(...) {
  .Deprecated("calculate_t", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_t, args)
}

#' @rdname check_dataset
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [check_dataset()] instead.
checkDataset <- function(...) {
  .Deprecated("check_dataset", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(check_dataset, args)
}

#' @rdname check_subset
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [check_subset()] instead.
checkSubset <- function(...) {
  .Deprecated("check_subset", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(check_subset, args)
}

#' @rdname col_convert
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [col_convert()] instead.
colConvert <- function(...) {
  .Deprecated("col_convert", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(col_convert, args)
}

#' @rdname col_names
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [col_names()] instead.
colNames <- function(...) {
  .Deprecated("col_names", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(col_names, args)
}

#' @rdname detect_kit
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [detect_kit()] instead.
detectKit <- function(...) {
  .Deprecated("detect_kit", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(detect_kit, args)
}

#' @rdname filter_profile
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [filter_profile()] instead.
filterProfile <- function(...) {
  .Deprecated("filter_profile", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(filter_profile, args)
}

#' @rdname generate_epg
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [generate_epg()] instead.
generateEPG <- function(...) {
  .Deprecated("generate_epg", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  if ("collapse" %in% names(args) && !("sum_profiles" %in% names(args))) {
    args$sum_profiles <- args$collapse
    args$collapse <- NULL
  }
  do.call(generate_epg, args)
}

#' @rdname get_db
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [get_db()] instead.
getDb <- function(...) {
  .Deprecated("get_db", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(get_db, args)
}

#' @rdname get_kit
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [get_kit()] instead.
getKit <- function(...) {
  .Deprecated("get_kit", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(get_kit, args)
}

#' @rdname get_setting
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [get_setting()] instead.
getSetting <- function(...) {
  .Deprecated("get_setting", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(get_setting, args)
}

#' @rdname get_strings
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [get_strings()] instead.
getStrings <- function(...) {
  .Deprecated("get_strings", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(get_strings, args)
}

#' @rdname guess_profile
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [guess_profile()] instead.
guessProfile <- function(...) {
  .Deprecated("guess_profile", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(guess_profile, args)
}

#' @rdname list_objects
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [list_objects()] instead.
listObjects <- function(...) {
  .Deprecated("list_objects", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(list_objects, args)
}

#' @rdname mask_at
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [mask_at()] instead.
maskAT <- function(...) {
  .Deprecated("mask_at", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(mask_at, args)
}

#' @rdname plot_epg2
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_epg2()] instead.
plotEPG2 <- function(...) {
  .Deprecated("plot_epg2", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_epg2, args)
}

#' @rdname remove_artefact
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [remove_artefact()] instead.
removeArtefact <- function(...) {
  .Deprecated("remove_artefact", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(remove_artefact, args)
}

#' @rdname remove_spike
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [remove_spike()] instead.
removeSpike <- function(...) {
  .Deprecated("remove_spike", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(remove_spike, args)
}

#' @rdname sample_table_to_list
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [sample_table_to_list()] instead.
sample_tableToList <- function(...) {
  .Deprecated("sample_table_to_list", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(sample_table_to_list, args)
}

#' @rdname save_object
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [save_object()] instead.
saveObject <- function(...) {
  .Deprecated("save_object", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(save_object, args)
}

#' @rdname scramble_alleles
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [scramble_alleles()] instead.
scrambleAlleles <- function(...) {
  .Deprecated("scramble_alleles", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(scramble_alleles, args)
}

#' @rdname sort_markers
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [sort_markers()] instead.
sort_marker <- function(...) {
  .Deprecated("sort_markers", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(sort_markers, args)
}

#' @rdname sort_markers
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [sort_markers()] instead.
sortMarker <- function(...) {
  .Deprecated("sort_markers", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(sort_markers, args)
}

