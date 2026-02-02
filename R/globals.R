# -------------------------------------------------------------------------
# Avoid NOTES in R CMD check caused by non-standard evaluation (NSE)
# -------------------------------------------------------------------------
# Reference:
# https://stackoverflow.com/questions/9439256/
# https://stat.ethz.ch/pipermail/r-devel/2007-June/046048.html
# -------------------------------------------------------------------------

if (base::getRversion() >= "2.15.1") {
  
  # -----------------------------------------------------------------------
  # gWidgets global bindings (legacy GUI support)
  # -----------------------------------------------------------------------
  utils::globalVariables(c(
    "addDropSource", "addDropTarget", "addHandlerChanged",
    "addHandlerDoubleclick", "addHandlerFocus", "blockHandler",
    "dispose", "enabled<-", "gbasicdialog", "gbutton", "gcheckbox",
    "gcombobox", "gedit", "gexpandgroup", "gfile", "gfilebrowse",
    "gframe", "ggroup", "ginput", "glayout", "glabel", "gmessage",
    "gnotebook", "gradio", "gspinbutton", "gtable", "gtext",
    "gwindow", "svalue", "svalue<-", "visible", "visible<-"
  ))
  
  # -----------------------------------------------------------------------
  # data.table and calculation globals (used across multiple functions)
  # -----------------------------------------------------------------------
  utils::globalVariables(c(
    "Allele", "Allele.Frequency", "Allele.Proportion", "Amount",
    "Axis", "By", "Copies", "Delta", "Dye", "Error", "Expected",
    "Group", "H", "Hb", "Height", "HMW", "Id", "Kit", "Large",
    "Lb", "LMW", "Lower", "Marker", "Masked", "Max", "Max.Height",
    "Max.TPH", "Min", "Min.Height", "Min.TPH", "MPH", "MTPH",
    "N", "N.Alleles", "Observed", "Peaks", "Proportion", "Ratio",
    "Samples", "Sample.File.Name", "Sample.Name", "Size", "Slope",
    "Small", "Sum.Peaks", "TPH", "TPPH", "Total.Peaks", "Upper"
  ))
  
  # -----------------------------------------------------------------------
  # Mixture calculation, STR-validator 3.x specific globals
  # -----------------------------------------------------------------------
  utils::globalVariables(c(
    # Column variables created/used by calculate_mixture()
    "Average", "Difference", "Dropin", "Expected", "Marker_lc",
    "Mx", "Observed", "Profile", "Sample.Name_lc", "Sample_lc",
    "Style", "unshared_major", "unshared_minor", "shared",
    # Internal temporary helpers
    "ref_profiles_df", "obs_alleles_for_mx", "exp_alleles"
  ))
  
}
