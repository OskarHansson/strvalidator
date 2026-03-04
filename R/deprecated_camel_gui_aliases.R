#' @title Deprecated camelCase GUI aliases
#' @description
#' Backward-compatible wrappers for renamed snake_case GUI functions.

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

#' @rdname add_data_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [add_data_gui()] instead.
addData_gui <- function(...) {
  .Deprecated("add_data_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(add_data_gui, args)
}

#' @rdname add_dye_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [add_dye_gui()] instead.
addDye_gui <- function(...) {
  .Deprecated("add_dye_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(add_dye_gui, args)
}

#' @rdname add_marker_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [add_marker_gui()] instead.
addMarker_gui <- function(...) {
  .Deprecated("add_marker_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(add_marker_gui, args)
}

#' @rdname calculate_allele_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_allele_gui()] instead.
calculateAllele_gui <- function(...) {
  .Deprecated("calculate_allele_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_allele_gui, args)
}

#' @rdname calculate_all_t_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_all_t_gui()] instead.
calculateAllT_gui <- function(...) {
  .Deprecated("calculate_all_t_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_all_t_gui, args)
}

#' @rdname calculate_at_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_at_gui()] instead.
calculateAT_gui <- function(...) {
  .Deprecated("calculate_at_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_at_gui, args)
}

#' @rdname calculate_at6_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_at6_gui()] instead.
calculateAT6_gui <- function(...) {
  .Deprecated("calculate_at6_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_at6_gui, args)
}

#' @rdname calculate_capillary_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_capillary_gui()] instead.
calculateCapillary_gui <- function(...) {
  .Deprecated("calculate_capillary_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_capillary_gui, args)
}

#' @rdname calculate_copies_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_copies_gui()] instead.
calculateCopies_gui <- function(...) {
  .Deprecated("calculate_copies_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_copies_gui, args)
}

#' @rdname calculate_dropout_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_dropout_gui()] instead.
calculateDropout_gui <- function(...) {
  .Deprecated("calculate_dropout_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_dropout_gui, args)
}

#' @rdname calculate_hb_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_hb_gui()] instead.
calculateHb_gui <- function(...) {
  .Deprecated("calculate_hb_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_hb_gui, args)
}

#' @rdname calculate_height_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_height_gui()] instead.
calculateHeight_gui <- function(...) {
  .Deprecated("calculate_height_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_height_gui, args)
}

#' @rdname calculate_lb_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_lb_gui()] instead.
calculateLb_gui <- function(...) {
  .Deprecated("calculate_lb_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_lb_gui, args)
}

#' @rdname calculate_ol_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_ol_gui()] instead.
calculateOL_gui <- function(...) {
  .Deprecated("calculate_ol_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_ol_gui, args)
}

#' @rdname calculate_overlap_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_overlap_gui()] instead.
calculateOverlap_gui <- function(...) {
  .Deprecated("calculate_overlap_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_overlap_gui, args)
}

#' @rdname calculate_peaks_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_peaks_gui()] instead.
calculatePeaks_gui <- function(...) {
  .Deprecated("calculate_peaks_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_peaks_gui, args)
}

#' @rdname calculate_pullup_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_pullup_gui()] instead.
calculatePullup_gui <- function(...) {
  .Deprecated("calculate_pullup_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_pullup_gui, args)
}

#' @rdname calculate_ratio_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_ratio_gui()] instead.
calculateRatio_gui <- function(...) {
  .Deprecated("calculate_ratio_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_ratio_gui, args)
}

#' @rdname calculate_result_type_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_result_type_gui()] instead.
calculateResultType_gui <- function(...) {
  .Deprecated("calculate_result_type_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_result_type_gui, args)
}

#' @rdname calculate_slope_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_slope_gui()] instead.
calculateSlope_gui <- function(...) {
  .Deprecated("calculate_slope_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_slope_gui, args)
}

#' @rdname calculate_spike_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_spike_gui()] instead.
calculateSpike_gui <- function(...) {
  .Deprecated("calculate_spike_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_spike_gui, args)
}

#' @rdname calculate_statistics_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_statistics_gui()] instead.
calculateStatistics_gui <- function(...) {
  .Deprecated("calculate_statistics_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_statistics_gui, args)
}

#' @rdname calculate_stutter_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_stutter_gui()] instead.
calculateStutter_gui <- function(...) {
  .Deprecated("calculate_stutter_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_stutter_gui, args)
}

#' @rdname crop_data_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [crop_data_gui()] instead.
cropData_gui <- function(...) {
  .Deprecated("crop_data_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(crop_data_gui, args)
}

#' @rdname edit_data_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [edit_data_gui()] instead.
editData_gui <- function(...) {
  .Deprecated("edit_data_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(edit_data_gui, args)
}

#' @rdname filter_profile_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [filter_profile_gui()] instead.
filterProfile_gui <- function(...) {
  .Deprecated("filter_profile_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(filter_profile_gui, args)
}

#' @rdname guess_profile_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [guess_profile_gui()] instead.
guessProfile_gui <- function(...) {
  .Deprecated("guess_profile_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(guess_profile_gui, args)
}

#' @rdname model_dropout_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [model_dropout_gui()] instead.
modelDropout_gui <- function(...) {
  .Deprecated("model_dropout_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(model_dropout_gui, args)
}

#' @rdname plot_at_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_at_gui()] instead.
plotAT_gui <- function(...) {
  .Deprecated("plot_at_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_at_gui, args)
}

#' @rdname plot_balance_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_balance_gui()] instead.
plotBalance_gui <- function(...) {
  .Deprecated("plot_balance_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_balance_gui, args)
}

#' @rdname plot_capillary_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_capillary_gui()] instead.
plotCapillary_gui <- function(...) {
  .Deprecated("plot_capillary_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_capillary_gui, args)
}

#' @rdname plot_contamination_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_contamination_gui()] instead.
plotContamination_gui <- function(...) {
  .Deprecated("plot_contamination_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_contamination_gui, args)
}

#' @rdname plot_distribution_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_distribution_gui()] instead.
plotDistribution_gui <- function(...) {
  .Deprecated("plot_distribution_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_distribution_gui, args)
}

#' @rdname plot_dropout_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_dropout_gui()] instead.
plotDropout_gui <- function(...) {
  .Deprecated("plot_dropout_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_dropout_gui, args)
}

#' @rdname plot_epg2_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_epg2_gui()] instead.
plotEPG2_gui <- function(...) {
  .Deprecated("plot_epg2_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_epg2_gui, args)
}

#' @rdname plot_groups_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_groups_gui()] instead.
plotGroups_gui <- function(...) {
  .Deprecated("plot_groups_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_groups_gui, args)
}

#' @rdname plot_kit_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_kit_gui()] instead.
plotKit_gui <- function(...) {
  .Deprecated("plot_kit_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_kit_gui, args)
}

#' @rdname plot_peaks_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_peaks_gui()] instead.
plotPeaks_gui <- function(...) {
  .Deprecated("plot_peaks_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_peaks_gui, args)
}

#' @rdname plot_precision_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_precision_gui()] instead.
plotPrecision_gui <- function(...) {
  .Deprecated("plot_precision_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_precision_gui, args)
}

#' @rdname plot_pullup_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_pullup_gui()] instead.
plotPullup_gui <- function(...) {
  .Deprecated("plot_pullup_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_pullup_gui, args)
}

#' @rdname plot_ratio_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_ratio_gui()] instead.
plotRatio_gui <- function(...) {
  .Deprecated("plot_ratio_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_ratio_gui, args)
}

#' @rdname plot_result_type_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_result_type_gui()] instead.
plotResultType_gui <- function(...) {
  .Deprecated("plot_result_type_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_result_type_gui, args)
}

#' @rdname plot_slope_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_slope_gui()] instead.
plotSlope_gui <- function(...) {
  .Deprecated("plot_slope_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_slope_gui, args)
}

#' @rdname plot_stutter_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [plot_stutter_gui()] instead.
plotStutter_gui <- function(...) {
  .Deprecated("plot_stutter_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(plot_stutter_gui, args)
}

#' @rdname remove_artefact_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [remove_artefact_gui()] instead.
removeArtefact_gui <- function(...) {
  .Deprecated("remove_artefact_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(remove_artefact_gui, args)
}

#' @rdname remove_spike_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [remove_spike_gui()] instead.
removeSpike_gui <- function(...) {
  .Deprecated("remove_spike_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(remove_spike_gui, args)
}

#' @rdname add_size_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [add_size_gui()] instead.
addSize_gui <- function(...) {
  .Deprecated("add_size_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(add_size_gui, args)
}

#' @rdname calculate_concordance_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_concordance_gui()] instead.
calculateConcordance_gui <- function(...) {
  .Deprecated("calculate_concordance_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_concordance_gui, args)
}

#' @rdname calculate_mixture_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [calculate_mixture_gui()] instead.
calculateMixture_gui <- function(...) {
  .Deprecated("calculate_mixture_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(calculate_mixture_gui, args)
}

#' @rdname check_subset_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [check_subset_gui()] instead.
checkSubset_gui <- function(...) {
  .Deprecated("check_subset_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(check_subset_gui, args)
}

#' @rdname generate_epg_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [generate_epg_gui()] instead.
generateEPG_gui <- function(...) {
  .Deprecated("generate_epg_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(generate_epg_gui, args)
}

#' @rdname manage_kits_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [manage_kits_gui()] instead.
manageKits_gui <- function(...) {
  .Deprecated("manage_kits_gui", package = "strvalidator")
  args <- .remap_legacy_args(list(...))
  do.call(manage_kits_gui, args)
}

