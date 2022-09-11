# IMPORTANT: To manually run R CMD check in RStudio all packages must be installed in
# both the 32 and 64 bit version. Make sure it is possible to start manually
# (GTK+ must be installed by clicking 'OK' on the message box).

# See http://r-pkgs.had.co.nz/release.html for advice on release.
# IMPORTANT: Use devtools::spell_check() to check spelling.
# IMPORTANT: Use devtools::check_win_devel() to check on R-dev.
# IMPORTANT: Use devtools::check_win_release() to check on current R.
# IMPORTANT: Use devtools::check_win_oldrelease() to test on previous major R.
# IMPORTANT: Use revdepcheck::revdep_check() to check reverse dependencies?
# IMPORTANT: Use devtools::check_rhub() to check on multiple platforms.
# IMPORTANT: Use devtools::release() to submit to CRAN.
# NB! The error below indicates some problem with the test server (try again later).
# Error in curl::curl_fetch_memory(url, handle = h) : Timeout was reached

# Versioning convention (x.yy.z[.9###]):
# Increment x on major change.
# Increment yy on new features.
# Increment z on minor changes and bug fixes.
# [optional]Increment ### on development versions.
# NB! Write changes in NEWS for x.yy.z.9### versions, but move changes to NEWS
# under x.yy.z upon release official version.

# NOTE: Can't import data frame named 'drop'
# NOTE: Buttons named 'Plot' will show up 'plot'.
# NOTE: Some button names will change due to locale.

################################################################################
# CHANGE LOG (last 20 changes)
# 10.09.2022: Fixed warning when selecting multiple objects for export.
# 02.09.2022: Added "datatables" to supported classes in view function.
# 21.08.2022: Fixed workspace not refreshed when no supported objects in environment and after loading a previous project.
# 12.08.2022: Added "plotly" to supported classes and view function.
# 23.07.2022: Added button to plotEPG2.
# 28.06.2020: Description made gexpandgroup and loads only if opened.
# 07.06.2020: Added calls to calculateStatistics_gui. Depracated table*.
# 21.05.2020: Added language support for welcome tab/about.
# 11.05.2020: Fixed bugs in language support.
# 04.05.2020: Added language support.
# 06.09.2019: Changed new.env() to new.env(parent = emptyenv())
# 16.03.2019: Added button to YouTube channel.
# 14.03.2019: Updated about. Fixed R-Check note.
# 22.02.2019: Reset projects list and description field if no project in folder.
# 19.02.2019: Fixed previous project activated in Description, Projects tab.
# 19.02.2019: Expand text box in welcome tab.
# 15.02.2019: Rearranged buttons on welcome tab.
# 14.02.2019: Adaptations to gWidgets2tcltk and updated welcome tab.
# 19.01.2019: Adaptations to gWidgets2tcltk.
# 18.07.2018: Added button to plot groups in 'Result' tab.
# 17.07.2018: Added button to calculate stochastic thresholds in 'Dropout' tab.
# 02.08.2017: Allow multiple objects to be removed from workspace.

#' @title Graphical User Interface For The STR-validator Package
#'
#' @description
#' GUI simplifying the use of the strvalidator package.
#'
#' @details The graphical user interface give easy access to all graphical
#' versions of the functions available in the strvalidator package. It connects
#' functions 'under the hood' to allow a degree of automation not available
#' using the command based functions. In addition it provides a project based
#' workflow.\cr\cr
#' Click \code{Index} at the bottom of the help page to see a complete list
#' of functions.
#'
#' @param debug logical indicating printing debug information.
#'
#' @return TRUE
#'
#' @import gWidgets2
#' @import gWidgets2tcltk
#' @importFrom utils packageVersion help object.size browseURL
#' @importFrom graphics title
#'
#' @export
#'
#' @examples
#' # To start the graphical user interface.
#' \dontrun{
#' strvalidator()
#' }
#'
strvalidator <- function(debug = FALSE) {

  # Global variables.
  .strvalidator_env <- new.env(parent = emptyenv())
  .separator <- .Platform$file.sep # Platform dependent path separator.
  .save_gui <- TRUE
  .ws_last_open_dir <- getwd()
  .object_classes_view <- c("data.frame", "ggplot", "plotly")
  .object_classes_import <- c("data.frame", "ggplot", "plotly")
  .project_description_variable <- ".strvalidator_project_description"
  .project_tmp_env <- new.env(parent = emptyenv())
  .project_name_list <- NULL
  .project_path_list <- NULL
  .ws_name_variable <- ".strvalidator_project_name"
  .ws_path_variable <- ".strvalidator_project_path"
  .object_empty_df <- data.frame(
    Object = "[Object]", Size = "[Size]",
    stringsAsFactors = FALSE
  )

  # Language ------------------------------------------------------------------

  # Get text for welcome tab from:
  # .../strvalidator/extdata/languages/xx_YY_about.txt
  .about_txt <- getStrings(about = TRUE)

  if (is.null(.about_txt)) {
    .about_txt <- "Language file not found."
  }

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strTabWelcome <- "Welcome"
  strTabWorkspace <- "Workspace"
  strTabProject <- "Projects"
  strTabDryLab <- "DryLab"
  strTabTools <- "Tools"
  strTabAT <- "AT"
  strTabStutter <- "Stutter"
  strTabBalance <- "Balance"
  strTabConcordance <- "Concordance"
  strTabDroput <- "Dropout"
  strTabMixture <- "Mixture"
  strTabResult <- "Result"
  strTabPrecision <- "Precision"
  strTabPullup <- "Pull-up"
  strBtnWebpage <- "STR-validator website"
  strTipWebpage <- "General information, workshops, and tutorials"
  strBtnVideo <- "Video tutorials"
  strTipVideo <- "STR-validator YouTube channel"
  strBtnFacebook <- "Facebook page"
  strTipFacebook <- "News, tips, and other information"
  strBtnSupport <- "Support forum"
  strTipSupport <- "Get help from the Facebook user community"
  strBtnReport <- "Report bugs"
  strTipReport <- "Report bugs, errors, and issues"
  strBtnSource <- "Source code"
  strTipSource <- "Take a look at future, current, and past source code"
  strBtnCran <- "CRAN page"
  strTipCran <- "Official CRAN page with address to maintainer and version archive"
  strBtnLicense <- "License"
  strLblFolder <- "Folder:"
  strFrmProject <- "Projects"
  strBtnOpen <- "Open"
  strTipOpen <- "Open selected project"
  strBtnAdd <- "Add"
  strTipAdd <- "Merge a project with the current project"
  strBtnDelete <- "Delete"
  strTipDelete <- "Delete selected project from the file system"
  strLblProject <- "Project:"
  strStrNoProject <- "[No project found]"
  strFrmDescription <- "Description"
  strStrDescription <- "Write a project description here!"
  strStrProjectDescription <- "[Project description]"
  strBtnSave <- "Save"
  strTipSaveDescription <- "Save project description"
  strBtnNew <- "New"
  strTipNewProject <- "Create a new project"
  strTipOpenProject <- "Open project"
  strTipSaveProject <- "Save project"
  strBtnSaveAs <- "Save As"
  strTipSaveAs <- "Choose a location and save project"
  strBtnImport <- "Import"
  strTipImport <- "Import data from file"
  strBtnExport <- "Export"
  strTipExport <- "Open the export dialogue with the selected objects"
  strBtnRefresh <- "Refresh"
  strTipRefresh <- "Refresh the workspace"
  strTipDeleteObject <- "Delete selected object"
  strBtnRename <- "Rename"
  strTipRenameObject <- "Rename selected object"
  strBtnView <- "View"
  strTipView <- "View selected object"
  strMsgNew <- "Are you sure you want to create a new project?\nAny changes to current project since last save will be lost!"
  strMsgRename <- "Currently you can only rename one object at a time!"
  strMsgSelectWorkspace <- "Select a saved workspace or dataset"
  strMsgNotFound <- "The workspace file was not found"
  strMsgTitleNotFound <- "File not found"
  strMsgExport <- "Please select the objects to export!"
  strMsgNoObjectSelected <- "No object selected!"
  strMsgTitleError <- "Error"
  strMsgTypeNotSupported <- "object type not supported!"
  strMsgTitleNotSupported <- "Unable to view object"
  strMsgSelectObject <- "Please select an object!"
  strMsgProjectSaved <- "Project saved!\n\n"
  strMsgDirNotFound <- "The project directory was not found"
  strMsgTitleDirNotFound <- "Directory not found"
  strMsgFileNameMissing <- "A file name must be provided"
  strMsgFileNameRequired <- "File name required"
  strMsgUseSaveAs <- "No project name or path!\nUse 'Save As' instead"
  strMsgPropertyNotFound <- "Property not found"
  strMsgSelectDirSave <- "Select a directory to save project in"
  strMsgInputProject <- "Input project name"
  strMsgTitleSaveAs <- "Save as"
  strMsgOverwrite <- "\nalready exist!\n\n Overwrite?"
  strMsgTitleConfirm <- "Confirm"
  strMsgProjectNotSaved <- "Project was not saved!"
  strMsgTitleInfo <- "Info"
  strFrmRworkspace <- "Load objects from R workspace"
  strBtnLoad <- "Load object"
  strDrpObject <- "<Select object>"
  strLblViewDataset <- "View a dataset"
  strBtnKits <- "Kits"
  strLblKits <- "Add new kits or edit kits file"
  strBtnPlotKit <- "Plot Kit"
  strLblPlotKit <- "Plot marker ranges for kits"
  strBtnBins <- "Analyse Overlap"
  strLblBins <- "Compare bins overlap for kits"
  strBtnOl <- "Analyse OL"
  strLblOl <- "Compare risk of getting off-ladder alleles for kits"
  strBtnEdit <- "Edit"
  strLblEdit <- "Edit a dataset"
  strBtnTrim <- "Trim"
  strLblTrim <- "Trim/discard samples or columns from a dataset"
  strBtnSlim <- "Slim"
  strLblSlim <- "Slim a dataset to 'long' format"
  strBtnFilter <- "Filter"
  strLblFilter <- "Filter a dataset using a reference set"
  strBtnCrop <- "Crop"
  strLblCrop <- "Discard, or replace data"
  strBtnGuess <- "Guess"
  strLblGuess <- "Guess the profile from raw DNA result"
  strBtnDye <- "Dye"
  strLblDye <- "Add dye information according to kit"
  strBtnMarker <- "Marker"
  strLblMarker <- "Add missing markers to dataset"
  strBtnSize <- "Size"
  strLblSize <- "Add approximate size to alleles in a dataset"
  strBtnData <- "Data"
  strLblData <- "Add new information to a dataset"
  strBtnCheck <- "Check"
  strLblCheck <- "Check the subsetting of a dataset"
  strBtnCombine <- "Combine"
  strLblCombine <- "Combine two datasets"
  strBtnColumns <- "Columns"
  strLblColumns <- "Perform actions on columns"
  strBtnCopies <- "Copies"
  strLblCopies <- "Calculate allele copies"
  strBtnHeight <- "Height"
  strLblHeight <- "Calculate peak height metrics"
  strBtnEPG <- "EPG"
  strLblEPG <- "Generate EPG like plot"
  strBtnEPG2 <- "EPG2"
  strLblEPG2 <- "Generate interactive EPG like plot"
  strBtnCalculate <- "Calculate"
  strLblAT <- "Calculate analytical threshold (AT1, AT2, AT4, AT7)"
  strLblAT6 <- "Calculate analytical threshold (AT6)"
  strBtnPlot <- "Plot"
  strLblPlotAT6 <- "Create plots for analysed data (AT6)"
  strLblStutter <- "Calculate stutters for a dataset"
  strLblPlotStutter <- "Create plots for stutter data"
  strBtnStatistics <- "Statistics"
  strLblStatStutterGlobal <- "Calculate global summary statistics"
  strLblStatStutterMarker <- "Calculate summary statistics by marker"
  strLblStatStutterStutter <- "Calculate summary statistics by marker and stutter type"
  strFrmHb <- "Heterozygote balance (intra-locus)"
  strFrmLb <- "Profile balance (inter-locus)"
  strLblHb <- "Calculate heterozygote balance"
  strLblLb <- "Calculate profile balance"
  strLblPlotBalance <- "Create plots for analysed data"
  strLblStatBalanceGlobal <- "Calculate global summary statistics"
  strLblStatBalanceMarker <- "Calculate summary statistics by marker"
  strFrmCapillary <- "Capillary balance"
  strLblCapillary <- "Calculate capillary balance for a dataset"
  strLblPlotCapillary <- "Create plots for capillary balance data"
  strLblStatCapillaryCap <- "Calculate summary statistics by capillary"
  strLblStatCapillaryInj <- "Calculate summary statistics by injection"
  strLblStatCapillaryRow <- "Calculate summary statistics by plate row"
  strLblStatCapillaryRun <- "Calculate summary statistics by run"
  strLblStatCapillaryIns <- "Calculate summary statistics by instrument"
  strFrmRatio <- "Marker peak height ratio"
  strLblRatio <- "Calculate locus ratio for a dataset"
  strLblPlotRatio <- "Create plots for marker ratio data"
  strLblConcordance <- "Calculate concordance between multiple datasets"
  strBtnScore <- "Score"
  strLblScore <- "Score dropouts for a dataset"
  strLblDropout <- "Calculate stochastic thresholds"
  strBtnModel <- "Model"
  strLblModel <- "Model and plot dropout risk"
  strLblPlotDropout <- "Create plots for analysed data"
  strLblMixture <- "Calculate mixture for a dataset"
  strFrmType <- "Result types"
  strLblType <- "Calculate result types for a dataset"
  strLblPlotType <- "Create plots for result type data"
  strFrmPeaks <- "Number of peaks"
  strLblPeaks <- "Count the number of peaks in sample"
  strLblPlotPeaks <- "Create plots for peak data"
  strFrmStatistics <- "Summary statistics"
  strLblStatistics <- "Calculate summary statistics"
  strFrmDistribution <- "Distributions"
  strLblDistribution <- "Plot distributions for data"
  strLblGroups <- "Plot cumulative distribution for multiple groups"
  strFrmDropin <- "Drop-in tools"
  strLblSpikes <- "Identify possible spikes"
  strLblFilterSpikes <- "Remove spikes"
  strLblArtefacts <- "Identify possible artefacts"
  strLblFilterArtefacts <- "Remove artefacts"
  strLblPlotContamination <- "Plot contamination"
  strFrmSlope <- "Profile slope"
  strLblSlope <- "Calculate the profile slope"
  strLblPlotSlope <- "Plot slope data"
  strLblPrecision <- "Create precision plots"
  strLblStatPrecisionSize <- "Calculate summary statistics for Size"
  strLblStatPrecisionDataPoint <- "Calculate summary statistics for Data.Point"
  strLblStatPrecisionHeight <- "Calculate summary statistics for Height"
  strLblPullup <- "Calculate spectral pull-up/bleed-through"
  strLblPlotPullup <- "Create plots for pull-up data"

  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  if (!is.null(dtStrings)) {
    # Get language strings, use default if not found.

    strtmp <- dtStrings["strChkGui"]$value
    strChkGui <- ifelse(is.na(strtmp), strChkGui, strtmp)

    strtmp <- dtStrings["strBtnHelp"]$value
    strBtnHelp <- ifelse(is.na(strtmp), strBtnHelp, strtmp)

    strtmp <- dtStrings["strBtnCalculate"]$value
    strBtnCalculate <- ifelse(is.na(strtmp), strBtnCalculate, strtmp)

    strtmp <- dtStrings["strTabWelcome"]$value
    strTabWelcome <- ifelse(is.na(strtmp), strTabWelcome, strtmp)

    strtmp <- dtStrings["strTabWorkspace"]$value
    strTabWorkspace <- ifelse(is.na(strtmp), strTabWorkspace, strtmp)

    strtmp <- dtStrings["strTabProject"]$value
    strTabProject <- ifelse(is.na(strtmp), strTabProject, strtmp)

    strtmp <- dtStrings["strTabDryLab"]$value
    strTabDryLab <- ifelse(is.na(strtmp), strTabDryLab, strtmp)

    strtmp <- dtStrings["strTabTools"]$value
    strTabTools <- ifelse(is.na(strtmp), strTabTools, strtmp)

    strtmp <- dtStrings["strTabAT"]$value
    strTabAT <- ifelse(is.na(strtmp), strTabAT, strtmp)

    strtmp <- dtStrings["strTabStutter"]$value
    strTabStutter <- ifelse(is.na(strtmp), strTabStutter, strtmp)

    strtmp <- dtStrings["strTabBalance"]$value
    strTabBalance <- ifelse(is.na(strtmp), strTabBalance, strtmp)

    strtmp <- dtStrings["strTabConcordance"]$value
    strTabConcordance <- ifelse(is.na(strtmp), strTabConcordance, strtmp)

    strtmp <- dtStrings["strTabDroput"]$value
    strTabDroput <- ifelse(is.na(strtmp), strTabDroput, strtmp)

    strtmp <- dtStrings["strTabMixture"]$value
    strTabMixture <- ifelse(is.na(strtmp), strTabMixture, strtmp)

    strtmp <- dtStrings["strTabResult"]$value
    strTabResult <- ifelse(is.na(strtmp), strTabResult, strtmp)

    strtmp <- dtStrings["strTabPrecision"]$value
    strTabPrecision <- ifelse(is.na(strtmp), strTabPrecision, strtmp)

    strtmp <- dtStrings["strTabPullup"]$value
    strTabPullup <- ifelse(is.na(strtmp), strTabPullup, strtmp)

    strtmp <- dtStrings["strBtnWebpage"]$value
    strBtnWebpage <- ifelse(is.na(strtmp), strBtnWebpage, strtmp)

    strtmp <- dtStrings["strTipWebpage"]$value
    strTipWebpage <- ifelse(is.na(strtmp), strTipWebpage, strtmp)

    strtmp <- dtStrings["strBtnVideo"]$value
    strBtnVideo <- ifelse(is.na(strtmp), strBtnVideo, strtmp)

    strtmp <- dtStrings["strTipVideo"]$value
    strTipVideo <- ifelse(is.na(strtmp), strTipVideo, strtmp)

    strtmp <- dtStrings["strBtnFacebook"]$value
    strBtnFacebook <- ifelse(is.na(strtmp), strBtnFacebook, strtmp)

    strtmp <- dtStrings["strTipFacebook"]$value
    strTipFacebook <- ifelse(is.na(strtmp), strTipFacebook, strtmp)

    strtmp <- dtStrings["strBtnSupport"]$value
    strBtnSupport <- ifelse(is.na(strtmp), strBtnSupport, strtmp)

    strtmp <- dtStrings["strTipSupport"]$value
    strTipSupport <- ifelse(is.na(strtmp), strTipSupport, strtmp)

    strtmp <- dtStrings["strBtnReport"]$value
    strBtnReport <- ifelse(is.na(strtmp), strBtnReport, strtmp)

    strtmp <- dtStrings["strTipReport"]$value
    strTipReport <- ifelse(is.na(strtmp), strTipReport, strtmp)

    strtmp <- dtStrings["strBtnSource"]$value
    strBtnSource <- ifelse(is.na(strtmp), strBtnSource, strtmp)

    strtmp <- dtStrings["strTipSource"]$value
    strTipSource <- ifelse(is.na(strtmp), strTipSource, strtmp)

    strtmp <- dtStrings["strBtnCran"]$value
    strBtnCran <- ifelse(is.na(strtmp), strBtnCran, strtmp)

    strtmp <- dtStrings["strTipCran"]$value
    strTipCran <- ifelse(is.na(strtmp), strTipCran, strtmp)

    strtmp <- dtStrings["strBtnLicense"]$value
    strBtnLicense <- ifelse(is.na(strtmp), strBtnLicense, strtmp)

    strtmp <- dtStrings["strLblFolder"]$value
    strLblFolder <- ifelse(is.na(strtmp), strLblFolder, strtmp)

    strtmp <- dtStrings["strFrmProject"]$value
    strFrmProject <- ifelse(is.na(strtmp), strFrmProject, strtmp)

    strtmp <- dtStrings["strBtnOpen"]$value
    strBtnOpen <- ifelse(is.na(strtmp), strBtnOpen, strtmp)

    strtmp <- dtStrings["strTipOpen"]$value
    strTipOpen <- ifelse(is.na(strtmp), strTipOpen, strtmp)

    strtmp <- dtStrings["strBtnAdd"]$value
    strBtnAdd <- ifelse(is.na(strtmp), strBtnAdd, strtmp)

    strtmp <- dtStrings["strTipAdd"]$value
    strTipAdd <- ifelse(is.na(strtmp), strTipAdd, strtmp)

    strtmp <- dtStrings["strBtnDelete"]$value
    strBtnDelete <- ifelse(is.na(strtmp), strBtnDelete, strtmp)

    strtmp <- dtStrings["strTipDelete"]$value
    strTipDelete <- ifelse(is.na(strtmp), strTipDelete, strtmp)

    strtmp <- dtStrings["strLblProject"]$value
    strLblProject <- ifelse(is.na(strtmp), strLblProject, strtmp)

    strtmp <- dtStrings["strStrNoProject"]$value
    strStrNoProject <- ifelse(is.na(strtmp), strStrNoProject, strtmp)

    strtmp <- dtStrings["strFrmDescription"]$value
    strFrmDescription <- ifelse(is.na(strtmp), strFrmDescription, strtmp)

    strtmp <- dtStrings["strStrDescription"]$value
    strStrDescription <- ifelse(is.na(strtmp), strStrDescription, strtmp)

    strtmp <- dtStrings["strStrProjectDescription"]$value
    strStrProjectDescription <- ifelse(is.na(strtmp), strStrProjectDescription, strtmp)

    strtmp <- dtStrings["strBtnSave"]$value
    strBtnSave <- ifelse(is.na(strtmp), strBtnSave, strtmp)

    strtmp <- dtStrings["strTipSaveDescription"]$value
    strTipSaveDescription <- ifelse(is.na(strtmp), strTipSaveDescription, strtmp)

    strtmp <- dtStrings["strBtnNew"]$value
    strBtnNew <- ifelse(is.na(strtmp), strBtnNew, strtmp)

    strtmp <- dtStrings["strTipNewProject"]$value
    strTipNewProject <- ifelse(is.na(strtmp), strTipNewProject, strtmp)

    strtmp <- dtStrings["strTipOpenProject"]$value
    strTipOpenProject <- ifelse(is.na(strtmp), strTipOpenProject, strtmp)

    strtmp <- dtStrings["strTipSaveProject"]$value
    strTipSaveProject <- ifelse(is.na(strtmp), strTipSaveProject, strtmp)

    strtmp <- dtStrings["strBtnSaveAs"]$value
    strBtnSaveAs <- ifelse(is.na(strtmp), strBtnSaveAs, strtmp)

    strtmp <- dtStrings["strTipSaveAs"]$value
    strTipSaveAs <- ifelse(is.na(strtmp), strTipSaveAs, strtmp)

    strtmp <- dtStrings["strBtnImport"]$value
    strBtnImport <- ifelse(is.na(strtmp), strBtnImport, strtmp)

    strtmp <- dtStrings["strTipImport"]$value
    strTipImport <- ifelse(is.na(strtmp), strTipImport, strtmp)

    strtmp <- dtStrings["strBtnExport"]$value
    strBtnExport <- ifelse(is.na(strtmp), strBtnExport, strtmp)

    strtmp <- dtStrings["strTipExport"]$value
    strTipExport <- ifelse(is.na(strtmp), strTipExport, strtmp)

    strtmp <- dtStrings["strBtnRefresh"]$value
    strBtnRefresh <- ifelse(is.na(strtmp), strBtnRefresh, strtmp)

    strtmp <- dtStrings["strTipRefresh"]$value
    strTipRefresh <- ifelse(is.na(strtmp), strTipRefresh, strtmp)

    strtmp <- dtStrings["strTipDeleteObject"]$value
    strTipDeleteObject <- ifelse(is.na(strtmp), strTipDeleteObject, strtmp)

    strtmp <- dtStrings["strBtnRename"]$value
    strBtnRename <- ifelse(is.na(strtmp), strBtnRename, strtmp)

    strtmp <- dtStrings["strTipRenameObject"]$value
    strTipRenameObject <- ifelse(is.na(strtmp), strTipRenameObject, strtmp)

    strtmp <- dtStrings["strBtnView"]$value
    strBtnView <- ifelse(is.na(strtmp), strBtnView, strtmp)

    strtmp <- dtStrings["strTipView"]$value
    strTipView <- ifelse(is.na(strtmp), strTipView, strtmp)

    strtmp <- dtStrings["strMsgNew"]$value
    strMsgNew <- ifelse(is.na(strtmp), strMsgNew, strtmp)

    strtmp <- dtStrings["strMsgRename"]$value
    strMsgRename <- ifelse(is.na(strtmp), strMsgRename, strtmp)

    strtmp <- dtStrings["strMsgSelectWorkspace"]$value
    strMsgSelectWorkspace <- ifelse(is.na(strtmp), strMsgSelectWorkspace, strtmp)

    strtmp <- dtStrings["strMsgNotFound"]$value
    strMsgNotFound <- ifelse(is.na(strtmp), strMsgNotFound, strtmp)

    strtmp <- dtStrings["strMsgTitleNotFound"]$value
    strMsgTitleNotFound <- ifelse(is.na(strtmp), strMsgTitleNotFound, strtmp)

    strtmp <- dtStrings["strMsgExport"]$value
    strMsgExport <- ifelse(is.na(strtmp), strMsgExport, strtmp)

    strtmp <- dtStrings["strMsgNoObjectSelected"]$value
    strMsgNoObjectSelected <- ifelse(is.na(strtmp), strMsgNoObjectSelected, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)

    strtmp <- dtStrings["strMsgTypeNotSupported"]$value
    strMsgTypeNotSupported <- ifelse(is.na(strtmp), strMsgTypeNotSupported, strtmp)

    strtmp <- dtStrings["strMsgTitleNotSupported"]$value
    strMsgTitleNotSupported <- ifelse(is.na(strtmp), strMsgTitleNotSupported, strtmp)

    strtmp <- dtStrings["strMsgSelectObject"]$value
    strMsgSelectObject <- ifelse(is.na(strtmp), strMsgSelectObject, strtmp)

    strtmp <- dtStrings["strMsgProjectSaved"]$value
    strMsgProjectSaved <- ifelse(is.na(strtmp), strMsgProjectSaved, strtmp)

    strtmp <- dtStrings["strMsgDirNotFound"]$value
    strMsgDirNotFound <- ifelse(is.na(strtmp), strMsgDirNotFound, strtmp)

    strtmp <- dtStrings["strMsgTitleDirNotFound"]$value
    strMsgTitleDirNotFound <- ifelse(is.na(strtmp), strMsgTitleDirNotFound, strtmp)

    strtmp <- dtStrings["strMsgFileNameMissing"]$value
    strMsgFileNameMissing <- ifelse(is.na(strtmp), strMsgFileNameMissing, strtmp)

    strtmp <- dtStrings["strMsgFileNameRequired"]$value
    strMsgFileNameRequired <- ifelse(is.na(strtmp), strMsgFileNameRequired, strtmp)

    strtmp <- dtStrings["strMsgUseSaveAs"]$value
    strMsgUseSaveAs <- ifelse(is.na(strtmp), strMsgUseSaveAs, strtmp)

    strtmp <- dtStrings["strMsgPropertyNotFound"]$value
    strMsgPropertyNotFound <- ifelse(is.na(strtmp), strMsgPropertyNotFound, strtmp)

    strtmp <- dtStrings["strMsgSelectDirSave"]$value
    strMsgSelectDirSave <- ifelse(is.na(strtmp), strMsgSelectDirSave, strtmp)

    strtmp <- dtStrings["strMsgInputProject"]$value
    strMsgInputProject <- ifelse(is.na(strtmp), strMsgInputProject, strtmp)

    strtmp <- dtStrings["strMsgTitleSaveAs"]$value
    strMsgTitleSaveAs <- ifelse(is.na(strtmp), strMsgTitleSaveAs, strtmp)

    strtmp <- dtStrings["strMsgOverwrite"]$value
    strMsgOverwrite <- ifelse(is.na(strtmp), strMsgOverwrite, strtmp)

    strtmp <- dtStrings["strMsgTitleConfirm"]$value
    strMsgTitleConfirm <- ifelse(is.na(strtmp), strMsgTitleConfirm, strtmp)

    strtmp <- dtStrings["strMsgProjectNotSaved"]$value
    strMsgProjectNotSaved <- ifelse(is.na(strtmp), strMsgProjectNotSaved, strtmp)

    strtmp <- dtStrings["strMsgTitleInfo"]$value
    strMsgTitleInfo <- ifelse(is.na(strtmp), strMsgTitleInfo, strtmp)

    strtmp <- dtStrings["strFrmRworkspace"]$value
    strFrmRworkspace <- ifelse(is.na(strtmp), strFrmRworkspace, strtmp)

    strtmp <- dtStrings["strBtnLoad"]$value
    strBtnLoad <- ifelse(is.na(strtmp), strBtnLoad, strtmp)

    strtmp <- dtStrings["strDrpObject"]$value
    strDrpObject <- ifelse(is.na(strtmp), strDrpObject, strtmp)

    strtmp <- dtStrings["strLblViewDataset"]$value
    strLblViewDataset <- ifelse(is.na(strtmp), strLblViewDataset, strtmp)

    strtmp <- dtStrings["strBtnKits"]$value
    strBtnKits <- ifelse(is.na(strtmp), strBtnKits, strtmp)

    strtmp <- dtStrings["strLblKits"]$value
    strLblKits <- ifelse(is.na(strtmp), strLblKits, strtmp)

    strtmp <- dtStrings["strBtnPlotKit"]$value
    strBtnPlotKit <- ifelse(is.na(strtmp), strBtnPlotKit, strtmp)

    strtmp <- dtStrings["strLblPlotKit"]$value
    strLblPlotKit <- ifelse(is.na(strtmp), strLblPlotKit, strtmp)

    strtmp <- dtStrings["strBtnBins"]$value
    strBtnBins <- ifelse(is.na(strtmp), strBtnBins, strtmp)

    strtmp <- dtStrings["strLblBins"]$value
    strLblBins <- ifelse(is.na(strtmp), strLblBins, strtmp)

    strtmp <- dtStrings["strBtnOl"]$value
    strBtnOl <- ifelse(is.na(strtmp), strBtnOl, strtmp)

    strtmp <- dtStrings["strLblOl"]$value
    strLblOl <- ifelse(is.na(strtmp), strLblOl, strtmp)

    strtmp <- dtStrings["strBtnEdit"]$value
    strBtnEdit <- ifelse(is.na(strtmp), strBtnEdit, strtmp)

    strtmp <- dtStrings["strLblEdit"]$value
    strLblEdit <- ifelse(is.na(strtmp), strLblEdit, strtmp)

    strtmp <- dtStrings["strBtnTrim"]$value
    strBtnTrim <- ifelse(is.na(strtmp), strBtnTrim, strtmp)

    strtmp <- dtStrings["strLblTrim"]$value
    strLblTrim <- ifelse(is.na(strtmp), strLblTrim, strtmp)

    strtmp <- dtStrings["strBtnSlim"]$value
    strBtnSlim <- ifelse(is.na(strtmp), strBtnSlim, strtmp)

    strtmp <- dtStrings["strLblSlim"]$value
    strLblSlim <- ifelse(is.na(strtmp), strLblSlim, strtmp)

    strtmp <- dtStrings["strBtnFilter"]$value
    strBtnFilter <- ifelse(is.na(strtmp), strBtnFilter, strtmp)

    strtmp <- dtStrings["strLblFilter"]$value
    strLblFilter <- ifelse(is.na(strtmp), strLblFilter, strtmp)

    strtmp <- dtStrings["strBtnCrop"]$value
    strBtnCrop <- ifelse(is.na(strtmp), strBtnCrop, strtmp)

    strtmp <- dtStrings["strLblCrop"]$value
    strLblCrop <- ifelse(is.na(strtmp), strLblCrop, strtmp)

    strtmp <- dtStrings["strBtnGuess"]$value
    strBtnGuess <- ifelse(is.na(strtmp), strBtnGuess, strtmp)

    strtmp <- dtStrings["strLblGuess"]$value
    strLblGuess <- ifelse(is.na(strtmp), strLblGuess, strtmp)

    strtmp <- dtStrings["strBtnDye"]$value
    strBtnDye <- ifelse(is.na(strtmp), strBtnDye, strtmp)

    strtmp <- dtStrings["strLblDye"]$value
    strLblDye <- ifelse(is.na(strtmp), strLblDye, strtmp)

    strtmp <- dtStrings["strBtnMarker"]$value
    strBtnMarker <- ifelse(is.na(strtmp), strBtnMarker, strtmp)

    strtmp <- dtStrings["strLblMarker"]$value
    strLblMarker <- ifelse(is.na(strtmp), strLblMarker, strtmp)

    strtmp <- dtStrings["strBtnSize"]$value
    strBtnSize <- ifelse(is.na(strtmp), strBtnSize, strtmp)

    strtmp <- dtStrings["strLblSize"]$value
    strLblSize <- ifelse(is.na(strtmp), strLblSize, strtmp)

    strtmp <- dtStrings["strBtnData"]$value
    strBtnData <- ifelse(is.na(strtmp), strBtnData, strtmp)

    strtmp <- dtStrings["strLblData"]$value
    strLblData <- ifelse(is.na(strtmp), strLblData, strtmp)

    strtmp <- dtStrings["strBtnCheck"]$value
    strBtnCheck <- ifelse(is.na(strtmp), strBtnCheck, strtmp)

    strtmp <- dtStrings["strLblCheck"]$value
    strLblCheck <- ifelse(is.na(strtmp), strLblCheck, strtmp)

    strtmp <- dtStrings["strBtnCombine"]$value
    strBtnCombine <- ifelse(is.na(strtmp), strBtnCombine, strtmp)

    strtmp <- dtStrings["strLblCombine"]$value
    strLblCombine <- ifelse(is.na(strtmp), strLblCombine, strtmp)

    strtmp <- dtStrings["strBtnColumns"]$value
    strBtnColumns <- ifelse(is.na(strtmp), strBtnColumns, strtmp)

    strtmp <- dtStrings["strLblColumns"]$value
    strLblColumns <- ifelse(is.na(strtmp), strLblColumns, strtmp)

    strtmp <- dtStrings["strBtnCopies"]$value
    strBtnCopies <- ifelse(is.na(strtmp), strBtnCopies, strtmp)

    strtmp <- dtStrings["strLblCopies"]$value
    strLblCopies <- ifelse(is.na(strtmp), strLblCopies, strtmp)

    strtmp <- dtStrings["strBtnHeight"]$value
    strBtnHeight <- ifelse(is.na(strtmp), strBtnHeight, strtmp)

    strtmp <- dtStrings["strLblHeight"]$value
    strLblHeight <- ifelse(is.na(strtmp), strLblHeight, strtmp)

    strtmp <- dtStrings["strBtnEPG"]$value
    strBtnEPG <- ifelse(is.na(strtmp), strBtnEPG, strtmp)

    strtmp <- dtStrings["strLblEPG"]$value
    strLblEPG <- ifelse(is.na(strtmp), strLblEPG, strtmp)

    strtmp <- dtStrings["strBtnEPG2"]$value
    strBtnEPG2 <- ifelse(is.na(strtmp), strBtnEPG2, strtmp)

    strtmp <- dtStrings["strLblEPG2"]$value
    strLblEPG2 <- ifelse(is.na(strtmp), strLblEPG2, strtmp)

    strtmp <- dtStrings["strLblAT"]$value
    strLblAT <- ifelse(is.na(strtmp), strLblAT, strtmp)

    strtmp <- dtStrings["strLblAT6"]$value
    strLblAT6 <- ifelse(is.na(strtmp), strLblAT6, strtmp)

    strtmp <- dtStrings["strBtnPlot"]$value
    strBtnPlot <- ifelse(is.na(strtmp), strBtnPlot, strtmp)

    strtmp <- dtStrings["strLblPlotAT6"]$value
    strLblPlotAT6 <- ifelse(is.na(strtmp), strLblPlotAT6, strtmp)

    strtmp <- dtStrings["strLblStutter"]$value
    strLblStutter <- ifelse(is.na(strtmp), strLblStutter, strtmp)

    strtmp <- dtStrings["strLblPlotStutter"]$value
    strLblPlotStutter <- ifelse(is.na(strtmp), strLblPlotStutter, strtmp)

    strtmp <- dtStrings["strBtnStatistics"]$value
    strBtnStatistics <- ifelse(is.na(strtmp), strBtnStatistics, strtmp)

    strtmp <- dtStrings["strLblStatStutterGlobal"]$value
    strLblStatStutterGlobal <- ifelse(is.na(strtmp), strLblStatStutterGlobal, strtmp)

    strtmp <- dtStrings["strLblStatStutterMarker"]$value
    strLblStatStutterMarker <- ifelse(is.na(strtmp), strLblStatStutterMarker, strtmp)

    strtmp <- dtStrings["strLblStatStutterStutter"]$value
    strLblStatStutterStutter <- ifelse(is.na(strtmp), strLblStatStutterStutter, strtmp)

    strtmp <- dtStrings["strFrmHb"]$value
    strFrmHb <- ifelse(is.na(strtmp), strFrmHb, strtmp)

    strtmp <- dtStrings["strFrmLb"]$value
    strFrmLb <- ifelse(is.na(strtmp), strFrmLb, strtmp)

    strtmp <- dtStrings["strLblHb"]$value
    strLblHb <- ifelse(is.na(strtmp), strLblHb, strtmp)

    strtmp <- dtStrings["strLblLb"]$value
    strLblLb <- ifelse(is.na(strtmp), strLblLb, strtmp)

    strtmp <- dtStrings["strLblPlotBalance"]$value
    strLblPlotBalance <- ifelse(is.na(strtmp), strLblPlotBalance, strtmp)

    strtmp <- dtStrings["strLblStatBalanceGlobal"]$value
    strLblStatBalanceGlobal <- ifelse(is.na(strtmp), strLblStatBalanceGlobal, strtmp)

    strtmp <- dtStrings["strLblStatBalanceMarker"]$value
    strLblStatBalanceMarker <- ifelse(is.na(strtmp), strLblStatBalanceMarker, strtmp)

    strtmp <- dtStrings["strFrmCapillary"]$value
    strFrmCapillary <- ifelse(is.na(strtmp), strFrmCapillary, strtmp)

    strtmp <- dtStrings["strLblCapillary"]$value
    strLblCapillary <- ifelse(is.na(strtmp), strLblCapillary, strtmp)

    strtmp <- dtStrings["strLblPlotCapillary"]$value
    strLblPlotCapillary <- ifelse(is.na(strtmp), strLblPlotCapillary, strtmp)

    strtmp <- dtStrings["strLblStatCapillaryCap"]$value
    strLblStatCapillaryCap <- ifelse(is.na(strtmp), strLblStatCapillaryCap, strtmp)

    strtmp <- dtStrings["strLblStatCapillaryInj"]$value
    strLblStatCapillaryInj <- ifelse(is.na(strtmp), strLblStatCapillaryInj, strtmp)

    strtmp <- dtStrings["strLblStatCapillaryRow"]$value
    strLblStatCapillaryRow <- ifelse(is.na(strtmp), strLblStatCapillaryRow, strtmp)

    strtmp <- dtStrings["strLblStatCapillaryRun"]$value
    strLblStatCapillaryRun <- ifelse(is.na(strtmp), strLblStatCapillaryRun, strtmp)

    strtmp <- dtStrings["strLblStatCapillaryIns"]$value
    strLblStatCapillaryIns <- ifelse(is.na(strtmp), strLblStatCapillaryIns, strtmp)

    strtmp <- dtStrings["strFrmRatio"]$value
    strFrmRatio <- ifelse(is.na(strtmp), strFrmRatio, strtmp)

    strtmp <- dtStrings["strLblRatio"]$value
    strLblRatio <- ifelse(is.na(strtmp), strLblRatio, strtmp)

    strtmp <- dtStrings["strLblPlotRatio"]$value
    strLblPlotRatio <- ifelse(is.na(strtmp), strLblPlotRatio, strtmp)

    strtmp <- dtStrings["strLblConcordance"]$value
    strLblConcordance <- ifelse(is.na(strtmp), strLblConcordance, strtmp)

    strtmp <- dtStrings["strBtnScore"]$value
    strBtnScore <- ifelse(is.na(strtmp), strBtnScore, strtmp)

    strtmp <- dtStrings["strLblScore"]$value
    strLblScore <- ifelse(is.na(strtmp), strLblScore, strtmp)

    strtmp <- dtStrings["strLblDropout"]$value
    strLblDropout <- ifelse(is.na(strtmp), strLblDropout, strtmp)

    strtmp <- dtStrings["strBtnModel"]$value
    strBtnModel <- ifelse(is.na(strtmp), strBtnModel, strtmp)

    strtmp <- dtStrings["strLblModel"]$value
    strLblModel <- ifelse(is.na(strtmp), strLblModel, strtmp)

    strtmp <- dtStrings["strLblPlotDropout"]$value
    strLblPlotDropout <- ifelse(is.na(strtmp), strLblPlotDropout, strtmp)

    strtmp <- dtStrings["strLblMixture"]$value
    strLblMixture <- ifelse(is.na(strtmp), strLblMixture, strtmp)

    strtmp <- dtStrings["strFrmType"]$value
    strFrmType <- ifelse(is.na(strtmp), strFrmType, strtmp)

    strtmp <- dtStrings["strLblType"]$value
    strLblType <- ifelse(is.na(strtmp), strLblType, strtmp)

    strtmp <- dtStrings["strLblPlotType"]$value
    strLblPlotType <- ifelse(is.na(strtmp), strLblPlotType, strtmp)

    strtmp <- dtStrings["strFrmPeaks"]$value
    strFrmPeaks <- ifelse(is.na(strtmp), strFrmPeaks, strtmp)

    strtmp <- dtStrings["strLblPeaks"]$value
    strLblPeaks <- ifelse(is.na(strtmp), strLblPeaks, strtmp)

    strtmp <- dtStrings["strLblPlotPeaks"]$value
    strLblPlotPeaks <- ifelse(is.na(strtmp), strLblPlotPeaks, strtmp)

    strtmp <- dtStrings["strFrmStatistics"]$value
    strFrmStatistics <- ifelse(is.na(strtmp), strFrmStatistics, strtmp)

    strtmp <- dtStrings["strLblStatistics"]$value
    strLblStatistics <- ifelse(is.na(strtmp), strLblStatistics, strtmp)

    strtmp <- dtStrings["strFrmDistribution"]$value
    strFrmDistribution <- ifelse(is.na(strtmp), strFrmDistribution, strtmp)

    strtmp <- dtStrings["strLblDistribution"]$value
    strLblDistribution <- ifelse(is.na(strtmp), strLblDistribution, strtmp)

    strtmp <- dtStrings["strLblGroups"]$value
    strLblGroups <- ifelse(is.na(strtmp), strLblGroups, strtmp)

    strtmp <- dtStrings["strFrmDropin"]$value
    strFrmDropin <- ifelse(is.na(strtmp), strFrmDropin, strtmp)

    strtmp <- dtStrings["strLblSpikes"]$value
    strLblSpikes <- ifelse(is.na(strtmp), strLblSpikes, strtmp)

    strtmp <- dtStrings["strLblFilterSpikes"]$value
    strLblFilterSpikes <- ifelse(is.na(strtmp), strLblFilterSpikes, strtmp)

    strtmp <- dtStrings["strLblArtefacts"]$value
    strLblArtefacts <- ifelse(is.na(strtmp), strLblArtefacts, strtmp)

    strtmp <- dtStrings["strLblFilterArtefacts"]$value
    strLblFilterArtefacts <- ifelse(is.na(strtmp), strLblFilterArtefacts, strtmp)

    strtmp <- dtStrings["strLblPlotContamination"]$value
    strLblPlotContamination <- ifelse(is.na(strtmp), strLblPlotContamination, strtmp)

    strtmp <- dtStrings["strFrmSlope"]$value
    strFrmSlope <- ifelse(is.na(strtmp), strFrmSlope, strtmp)

    strtmp <- dtStrings["strLblSlope"]$value
    strLblSlope <- ifelse(is.na(strtmp), strLblSlope, strtmp)

    strtmp <- dtStrings["strLblPlotSlope"]$value
    strLblPlotSlope <- ifelse(is.na(strtmp), strLblPlotSlope, strtmp)

    strtmp <- dtStrings["strLblPrecision"]$value
    strLblPrecision <- ifelse(is.na(strtmp), strLblPrecision, strtmp)

    strtmp <- dtStrings["strLblStatPrecisionSize"]$value
    strLblStatPrecisionSize <- ifelse(is.na(strtmp), strLblStatPrecisionSize, strtmp)

    strtmp <- dtStrings["strLblStatPrecisionDataPoint"]$value
    strLblStatPrecisionDataPoint <- ifelse(is.na(strtmp), strLblStatPrecisionDataPoint, strtmp)

    strtmp <- dtStrings["strLblStatPrecisionHeight"]$value
    strLblStatPrecisionHeight <- ifelse(is.na(strtmp), strLblStatPrecisionHeight, strtmp)

    strtmp <- dtStrings["strLblPullup"]$value
    strLblPullup <- ifelse(is.na(strtmp), strLblPullup, strtmp)

    strtmp <- dtStrings["strLblPlotPullup"]$value
    strLblPlotPullup <- ifelse(is.na(strtmp), strLblPlotPullup, strtmp)
  }

  # WINDOW ####################################################################

  # Main window.
  w <- gwindow(
    title = paste(
      "STR-validator", packageVersion("strvalidator"),
      " - a forensic validation toolbox"
    ),
    visible = FALSE,
    name = title
  )

  # Vertical main group.
  gv <- ggroup(
    horizontal = FALSE,
    use.scrollwindow = FALSE,
    container = w,
    expand = TRUE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(text = strChkGui, checked = TRUE, container = gh)

  addHandlerChanged(savegui_chk, handler = function(h, ...) {

    # Update variable.
    .save_gui <<- svalue(savegui_chk)
  })

  addSpring(gh)

  help_btn <- gbutton(text = strBtnHelp, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # Main client area.
  nb <- gnotebook(
    closebuttons = FALSE,
    dontCloseThese = NULL,
    container = gv
  )


  # NOTEBOOK ##################################################################

  # Define groups.
  start_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabWelcome,
    index = 1
  )

  project_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabProject,
    index = 2
  )

  file_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabWorkspace,
    index = 3
  )

  drylab_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabDryLab,
    index = 4
  )

  tools_tab <- ggroup(
    horizontal = FALSE,
    spacing = 2,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabTools,
    index = 5
  )

  at_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabAT,
    index = 6
  )

  stutter_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabStutter,
    index = 7
  )

  balance_tab <- ggroup(
    horizontal = FALSE,
    spacing = 2,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabBalance,
    index = 8
  )

  concordance_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabConcordance,
    index = 9
  )

  drop_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabDroput,
    index = 10
  )

  mixture_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabMixture,
    index = 11
  )

  result_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabResult,
    index = 12
  )

  precision_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabPrecision,
    index = 13
  )

  pullup_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strTabPullup,
    index = 14
  )

  # START #####################################################################

  # Vertical main group.
  start_f1 <- gframe(
    horizontal = FALSE,
    container = start_tab,
    spacing = 2,
    expand = TRUE,
    fill = TRUE
  )

  # STR TYPING KIT ------------------------------------------------------------

  gtext(
    text = .about_txt, width = NULL, height = NULL, font.attr = NULL,
    wrap = TRUE, expand = TRUE, container = start_f1, fill = TRUE,
    anchor = c(-1, 0)
  )

  button_group <- ggroup(container = start_f1)

  webpage_btn <- gbutton(text = strBtnWebpage, container = button_group)
  tooltip(webpage_btn) <- strTipWebpage

  addHandlerChanged(webpage_btn, handler = function(h, ...) {
    browseURL("https://sites.google.com/site/forensicapps/strvalidator")
  })

  youtube_btn <- gbutton(text = strBtnVideo, container = button_group)
  tooltip(youtube_btn) <- strTipVideo

  addHandlerChanged(youtube_btn, handler = function(h, ...) {
    browseURL("https://www.youtube.com/channel/UCs7TxzK21OKvWebQygxAHHA")
  })

  facebook_btn <- gbutton(text = strBtnFacebook, container = button_group)
  tooltip(facebook_btn) <- strTipFacebook

  addHandlerChanged(facebook_btn, handler = function(h, ...) {
    browseURL("https://www.facebook.com/STRvalidator")
  })

  community_btn <- gbutton(text = strBtnSupport, container = button_group)
  tooltip(community_btn) <- strTipSupport

  addHandlerChanged(community_btn, handler = function(h, ...) {
    browseURL("https://www.facebook.com/groups/strvalidator/")
  })

  report_btn <- gbutton(text = strBtnReport, container = button_group)
  tooltip(report_btn) <- strTipReport

  addHandlerChanged(report_btn, handler = function(h, ...) {
    browseURL("https://github.com/OskarHansson/strvalidator/issues")
  })

  source_btn <- gbutton(text = strBtnSource, container = button_group)
  tooltip(source_btn) <- strTipSource

  addHandlerChanged(source_btn, handler = function(h, ...) {
    browseURL("https://github.com/OskarHansson/strvalidator")
  })

  cran_btn <- gbutton(text = strBtnCran, container = button_group)
  tooltip(cran_btn) <- strTipCran

  addHandlerChanged(cran_btn, handler = function(h, ...) {
    browseURL("https://cran.r-project.org/web/packages/strvalidator/index.html")
  })

  start_license_btn <- gbutton(text = strBtnLicense, container = button_group, expand = FALSE)

  addHandlerChanged(start_license_btn, handler = function(h, ...) {
    license_txt <- paste("Copyright (C) 2013 Oskar Hansson\n\n",
      "This program is free software; you can redistribute it and/or ",
      "modify it under the terms of the GNU General Public License ",
      "as published by the Free Software Foundation; either version 2 ",
      "of the License, or (at your option) any later version.\n\n",
      "This program is distributed in the hope that it will be useful, ",
      "but WITHOUT ANY WARRANTY; without even the implied warranty of ",
      "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the ",
      "GNU General Public License for more details.\n\n",
      "You should have received a copy of the GNU General Public License ",
      "along with this program; if not, write to the Free Software ",
      "Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, ",
      "MA  02110-1301, USA.",
      sep = ""
    )

    gmessage(
      msg = license_txt,
      title = "License",
      icon = "info",
      parent = w
    )
  })

  # PROJECT MANAGER ###########################################################

  # Vertical main group.
  project_f1 <- ggroup(
    horizontal = FALSE,
    container = project_tab,
    spacing = 2,
    expand = FALSE
  )

  # FOLDER --------------------------------------------------------------------

  glabel(text = strLblFolder, anchor = c(-1, 0), container = project_f1)

  project_fb <- gfilebrowse(
    type = "selectdir", quote = FALSE,
    container = project_f1
  )

  addHandlerChanged(project_fb, handler = function(h, ...) {
    .updateProjectList()
  })

  # PROJECTS ------------------------------------------------------------------

  # Horizontal main group.
  project_f2 <- gframe(
    text = strFrmProject,
    horizontal = TRUE,
    spacing = 2,
    container = project_f1,
    expand = TRUE
  )

  # Button group.
  project_g1 <- ggroup(
    horizontal = FALSE,
    spacing = 2,
    container = project_f2,
    expand = FALSE
  )

  project_open_btn <- gbutton(text = strBtnOpen, container = project_g1)
  tooltip(project_open_btn) <- strTipOpen

  project_add_btn <- gbutton(text = strBtnAdd, container = project_g1)
  tooltip(project_add_btn) <- strTipAdd

  project_delete_btn <- gbutton(text = strBtnDelete, container = project_g1)
  tooltip(project_delete_btn) <- strTipDelete

  addSpring(project_g1)

  addHandlerChanged(project_open_btn, handler = function(h, ...) {

    # Get selected projects file name.
    val_name <- svalue(project_tbl)
    val_id <- as.numeric(project_tbl[svalue(project_tbl, index = TRUE), "Id"])
    val_prj <- .project_path_list[val_id]
    val_env <- .strvalidator_env

    if (debug) {
      print(paste("Selected path", val_prj))
      print(paste("Selected project", val_name))
      print(paste("Selected index", val_id))
    }

    # Check if file exist.
    if (length(val_prj) > 0) {
      if (file.exists(val_prj)) {

        # Clear environment.
        remove(
          list = ls(envir = val_env, all.names = TRUE),
          envir = val_env, inherits = FALSE
        )

        # Load project to workspace.
        load(file = val_prj, envir = val_env, verbose = FALSE)

        # Move to workspace tab.
        svalue(nb) <- match(strTabWorkspace, names(nb))
      }
    }
  })

  addHandlerChanged(project_add_btn, handler = function(h, ...) {

    # Get selected projects file name.
    val_name <- svalue(project_tbl)
    val_id <- as.numeric(project_tbl[svalue(project_tbl, index = TRUE), "Id"])
    val_prj <- .project_path_list[val_id]
    val_env <- .strvalidator_env

    if (debug) {
      print(paste("Selected path", val_prj))
      print(paste("Selected project", val_name))
      print(paste("Selected index", val_id))
    }

    # Check if file exist.
    if (length(val_prj) > 0) {
      if (file.exists(val_prj)) {

        # Load project to workspace.
        load(file = val_prj, envir = val_env, verbose = FALSE)
        message(paste("Loaded", val_prj))
      }
    }
  })

  addHandlerChanged(project_delete_btn, handler = function(h, ...) {

    # Get selected projects file name.
    val_name <- svalue(project_tbl)
    val_id <- as.numeric(project_tbl[svalue(project_tbl, index = TRUE), "Id"])
    val_prj <- .project_path_list[val_id]

    if (debug) {
      print(paste("Selected path", val_prj))
      print(paste("Selected project", val_name))
      print(paste("Selected index", val_id))
    }

    # Check if file exist.
    if (length(val_prj) > 0) {
      if (file.exists(val_prj)) {

        # Delete project file and update list.
        file.remove(val_prj)
        message("Deleted", val_prj)
        .updateProjectList()

        # Clear description box.
        svalue(proj_info_lbl) <- strLblProject
        svalue(proj_info_txt) <- ""
      }
    }
  })

  # Projects group.
  project_g2 <- ggroup(
    horizontal = FALSE,
    use.scrollwindow = FALSE,
    container = project_f2,
    expand = TRUE,
    fill = TRUE
  )

  # Projects list.
  project_tbl <- gWidgets2::gtable(
    items = data.frame(
      Name = strStrNoProject, Date = "",
      Size = "", Id = "",
      stringsAsFactors = FALSE
    ),
    multiple = TRUE,
    chosencol = 1,
    expand = TRUE,
    container = project_g2
  )

  addHandlerSelectionChanged(project_tbl, handler = function(h, ...) {

    # Get selected projects file name.
    val_name <- svalue(project_tbl)
    val_id <- as.numeric(project_tbl[svalue(project_tbl, index = TRUE), "Id"])
    val_prj <- .project_path_list[val_id]
    val_obj <- .project_description_variable
    val_env <- .project_tmp_env

    if (debug) {
      print(paste("In addHandlerClicked(project_tbl"))
      print(paste("Selected path", val_prj))
      print(paste("Selected project", val_name))
      print(paste("Selected index", val_id))
    }

    # Enable possibly disabled save button upon changed selectioin.
    enabled(project_save_btn) <- TRUE

    # Clear environment.
    remove(list = ls(envir = val_env, all.names = TRUE), envir = val_env, inherits = FALSE)

    # Check if file exist.
    if (length(val_prj) > 0) {
      if (file.exists(val_prj)) {

        # Check if description should be loaded.
        if (visible(project_f3)) {

          # Load project in temporary environment.
          load(file = val_prj, envir = val_env, verbose = FALSE)
          if (exists(x = val_obj, envir = val_env, inherits = FALSE)) {
            description <- get(x = val_obj, envir = val_env, inherits = FALSE)
          } else {
            description <- strStrDescription
          }

          # Load description.
          svalue(proj_info_lbl) <- paste(strLblProject, val_name)
          svalue(proj_info_txt) <- description
        }
      }
    } else {

      # Reset description.
      svalue(proj_info_lbl) <- strLblProject
      svalue(proj_info_txt) <- strStrProjectDescription
    }
  })

  # DESCRIPTION ---------------------------------------------------------------

  # Horizontal main group.
  project_f3 <- gexpandgroup(
    text = strFrmDescription,
    horizontal = TRUE,
    container = project_f1,
    expand = TRUE
  )

  # Default is to not show description.
  visible(project_f3) <- FALSE

  # Button group.
  project_g3 <- ggroup(
    horizontal = FALSE, spacing = 2,
    container = project_f3, expand = FALSE
  )

  project_save_btn <- gbutton(text = strBtnSave, container = project_g3)
  tooltip(project_save_btn) <- strTipSaveDescription

  addHandlerChanged(project_save_btn, handler = function(h, ...) {
    enabled(project_save_btn) <- FALSE

    # Get selected projects file name.
    val_name <- svalue(project_tbl)
    val_id <- project_tbl[svalue(project_tbl, index = TRUE), "Id"]
    val_id <- as.numeric(val_id)
    val_prj <- .project_path_list[val_id]
    val_obj <- .project_description_variable
    val_env <- .project_tmp_env
    val_description <- svalue(proj_info_txt)

    # Check if selected project.
    if (length(val_prj) > 0) {

      # Save project description and write to disc.
      message("Assign: ", val_obj)
      assign(x = val_obj, value = val_description, envir = val_env, inherits = FALSE)

      message("Save: ", val_prj)
      save(file = val_prj, list = ls(envir = val_env, all.names = TRUE), envir = val_env)
    } else {
      message("No valid project selected!")
    }

    enabled(project_save_btn) <- TRUE
  })

  # Button group.
  project_g4 <- ggroup(
    horizontal = FALSE,
    spacing = 2,
    container = project_f3,
    expand = TRUE,
    fill = TRUE
  )

  # Project description window.
  proj_info_lbl <- glabel(
    text = strLblProject, anchor = c(-1, 0),
    container = project_g4
  )
  proj_info_txt <- gtext(
    text = strStrProjectDescription, height = 50, expand = TRUE,
    wrap = TRUE, container = project_g4, fill = TRUE
  )

  # WORKSPACE #################################################################

  # LOADED DATASETS -----------------------------------------------------------

  workspace_f1 <- gframe(
    text = strFrmProject,
    markup = FALSE,
    pos = 0,
    horizontal = TRUE,
    container = file_tab,
    expand = TRUE,
    fill = TRUE
  )

  workspace_f1g1 <- ggroup(
    horizontal = FALSE,
    container = workspace_f1,
    expand = FALSE
  )

  ws_new_btn <- gbutton(text = strBtnNew, container = workspace_f1g1)
  tooltip(ws_new_btn) <- strTipNewProject

  ws_open_btn <- gbutton(text = strBtnOpen, container = workspace_f1g1)
  tooltip(ws_open_btn) <- strTipOpenProject

  ws_save_btn <- gbutton(text = strBtnSave, container = workspace_f1g1)
  tooltip(ws_save_btn) <- strTipSaveProject

  ws_saveas_btn <- gbutton(text = strBtnSaveAs, container = workspace_f1g1)
  tooltip(ws_saveas_btn) <- strTipSaveAs

  ws_import_btn <- gbutton(text = strBtnImport, container = workspace_f1g1)
  tooltip(ws_import_btn) <- strTipImport

  ws_export_btn <- gbutton(text = strBtnExport, container = workspace_f1g1)
  tooltip(ws_export_btn) <- strTipExport

  ws_add_btn <- gbutton(text = strBtnAdd, container = workspace_f1g1)
  tooltip(ws_add_btn) <- strTipAdd

  ws_refresh_btn <- gbutton(text = strBtnRefresh, container = workspace_f1g1)
  tooltip(ws_refresh_btn) <- strTipRefresh

  ws_remove_btn <- gbutton(text = strBtnDelete, container = workspace_f1g1)
  tooltip(ws_remove_btn) <- strTipDeleteObject

  ws_rename_btn <- gbutton(text = strBtnRename, container = workspace_f1g1)
  tooltip(ws_rename_btn) <- strTipRenameObject

  ws_view_btn <- gbutton(text = strBtnView, container = workspace_f1g1)
  tooltip(ws_view_btn) <- strTipView

  ws_loaded_tbl <- gWidgets2::gtable(
    items = .object_empty_df,
    multiple = TRUE,
    chosencol = 1,
    expand = TRUE,
    container = workspace_f1
  )

  addHandlerChanged(ws_new_btn, handler = function(h, ...) {
    blockHandlers(w)
    response <- gconfirm(msg = strMsgNew)
    unblockHandlers(w)

    if (response) {

      # Create a new environment.
      .strvalidator_env <<- new.env(parent = emptyenv())
      message("A new project environment was created.")

      # Refresh workspace.
      .refreshLoaded()
    }
  })

  addHandlerChanged(ws_rename_btn, handler = function(h, ...) {
    objectName <- svalue(ws_loaded_tbl)

    if (length(objectName) == 1) {

      # Get the object to save.
      datanew <- get(objectName, envir = .strvalidator_env)

      # Save data.
      saveObject(
        name = NULL, object = datanew, suggest = objectName,
        parent = w, remove = objectName, env = .strvalidator_env,
        debug = debug
      )

      .refreshLoaded()
    } else {
      gmessage(
        msg = strMsgRename,
        title = strMsgTitleError,
        icon = "error",
        parent = w
      )
    }
  })

  addHandlerChanged(ws_open_btn, handler = function(h, ...) {
    val_env <- .strvalidator_env

    blockHandlers(w)
    ws_path <- gfile(
      text = strMsgSelectWorkspace, type = "open",
      filter = list("R files" = list(patterns = c("*.R", "*.Rdata"))),
      multi = FALSE, initial.dir = .ws_last_open_dir
    )
    unblockHandlers(w)

    if (length(ws_path) > 0) {
      if (!is.na(ws_path)) {
        if (file.exists(ws_path)) {

          # Clear environment.
          remove(
            list = ls(envir = val_env, all.names = TRUE),
            envir = val_env, inherits = FALSE
          )

          # Load new project.
          load(file = ws_path, envir = .strvalidator_env)

          # Refresh workspace.
          .refreshLoaded()

          # Load saved project settings.
          .loadSavedSettings()

          # Save last used directory.
          .ws_last_open_dir <<- dirname(ws_path)
        } else {
          blockHandlers(w)
          gmessage(
            msg = strMsgNotFound,
            title = strMsgTitleNotFound,
            icon = "error",
            parent = w
          )
          unblockHandlers(w)
        }
      }
    }
  })

  addHandlerChanged(ws_add_btn, handler = function(h, ...) {
    val_env <- .strvalidator_env

    blockHandlers(w)
    ws_path <- gfile(
      text = strMsgSelectWorkspace, type = "open",
      filter = list("R files" = list(patterns = c("*.R", "*.Rdata"))),
      multi = FALSE, initial.dir = .ws_last_open_dir
    )
    unblockHandlers(w)

    if (length(ws_path) > 0) {
      if (!is.na(ws_path)) {
        if (file.exists(ws_path)) {

          # Add new project.
          load(file = ws_path, envir = val_env)
          .loadSavedSettings()

          # Save last used directory.
          .ws_last_open_dir <<- dirname(ws_path)
        } else {
          blockHandlers(w)
          gmessage(
            msg = strMsgNotFound,
            title = strMsgTitleNotFound,
            icon = "error",
            parent = w
          )
          unblockHandlers(w)
        }
      }
    }
  })

  addHandlerChanged(ws_import_btn, handler = function(h, ...) {

    # Open GUI.
    import_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  addHandlerChanged(ws_export_btn, handler = function(h, ...) {

    # Get selected items.
    val <- svalue(ws_loaded_tbl)

    if (!any(is.null(val)) && !any(is.na(val)) && length(val) > 0) {

      # List selected objects.
      message("Objects selected for export: ", paste(val, collapse = ", "))

      # Open GUI.
      export_gui(
        obj = val, env = .strvalidator_env, savegui = .save_gui,
        debug = debug, parent = w
      )
    } else {
      blockHandlers(w)
      gmessage(
        msg = strMsgExport,
        title = strMsgNoObjectSelected, icon = "info", parent = w
      )
      unblockHandlers(w)
    }
  })

  addHandlerChanged(ws_refresh_btn, handler = function(h, ...) {
    .refreshLoaded()
  })

  addHandlerChanged(ws_view_btn, handler = function(h, ...) {

    # Get selected dataset name(s).
    val_obj <- svalue(ws_loaded_tbl)

    if (debug) {
      print(paste("IN:", match.call()[[1]]))
      print("Changed, ws_view_btn")
      print(val_obj)
    }

    if (!is.null(val_obj) && !is.na(val_obj) && length(val_obj) > 0) {

      # Get data and class.
      val_data <- get(val_obj, envir = .strvalidator_env)
      val_class <- class(val_data)

      if ("data.frame" %in% val_class) {

        # Convert to DT and view.
        dt <- DT::datatable(val_data)
        print(dt)
      } else if (any(c("ggplot", "plotly", "datatables") %in% val_class)) {

        # View object.
        print(val_data)
      } else {
        blockHandlers(w)
        gmessage(
          msg = paste(val_class, strMsgTypeNotSupported),
          title = strMsgTitleNotSupported, icon = "error", parent = w
        )
        unblockHandlers(w)
      }
    } else {
      blockHandlers(w)
      gmessage(
        msg = strMsgSelectObject,
        title = strMsgNoObjectSelected, icon = "info", parent = w
      )
      unblockHandlers(w)
    }
  })

  addHandlerChanged(ws_remove_btn, handler = function(h, ...) {

    # Get selected dataset name(s).
    val_obj <- svalue(ws_loaded_tbl)

    if (length(val_obj) > 0) {
      if (!is.null(val_obj) && !is.na(val_obj)) {

        # Get active reference data frame.
        remove(list = val_obj, envir = .strvalidator_env)

        message(
          "The following objects were removed: ",
          paste(val_obj, collapse = ", ")
        )

        .refreshLoaded()
      }
    } else if (length(val_obj) == 0) {
      blockHandlers(w)
      gmessage(
        msg = strMsgNoObjectSelected, title = strMsgTitleError,
        icon = "error", parent = w
      )
      unblockHandlers(w)
    } else {
      message(
        "Negative return value should not be possible.",
        "Nothing was removed!"
      )
    }
  })


  addHandlerChanged(ws_save_btn, handler = function(h, ...) {

    # Initiate flag.
    ok <- TRUE

    # Get project name if available.
    if (exists(.ws_name_variable, envir = .strvalidator_env)) {
      ws_name <- get(.ws_name_variable,
        envir = .strvalidator_env,
        inherits = FALSE
      )
      message("Last project name loaded: ", ws_name)
    } else {
      ok <- FALSE
    }

    # Get project path if available.
    if (exists(.ws_path_variable, envir = .strvalidator_env)) {
      ws_save_path <- get(.ws_path_variable,
        envir = .strvalidator_env,
        inherits = FALSE
      )
      message("Last project save path loaded: ", ws_save_path)
    } else {
      ok <- FALSE
    }

    if (ok) {
      if (!is.na(ws_name) && !ws_name == "") {
        ws_full_name <- paste(ws_save_path, .separator, ws_name, ".RData", sep = "")

        if (file.exists(ws_save_path)) {
          .saveSettings()

          save(
            file = ws_full_name,
            list = ls(envir = .strvalidator_env, all.names = TRUE),
            envir = .strvalidator_env
          )

          blockHandlers(w)
          gmessage(
            msg = paste(strMsgProjectSaved, ws_full_name),
            title = "STR-validator",
            icon = "info",
            parent = w
          )
          unblockHandlers(w)

          message("Project saved as: ", ws_full_name)
        } else {
          blockHandlers(w)
          gmessage(
            msg = strMsgDirNotFound,
            title = strMsgTitleDirNotFound,
            icon = "error",
            parent = w
          )
          unblockHandlers(w)
        }
      } else {
        blockHandlers(w)
        gmessage(
          msg = strMsgFileNameMissing,
          title = strMsgFileNameRequired,
          icon = "error",
          parent = w
        )
        unblockHandlers(w)
      }
    } else {
      blockHandlers(w)
      gmessage(
        msg = strMsgUseSaveAs,
        title = strMsgPropertyNotFound,
        icon = "error",
        parent = w
      )
      unblockHandlers(w)
    }
  })

  addHandlerChanged(ws_saveas_btn, handler = function(h, ...) {

    # Initiate flag.
    ok <- TRUE

    # Pick save location.
    blockHandlers(w)
    ws_save_path <- gfile(
      text = strMsgSelectDirSave,
      type = "selectdir",
      filter = list("R files" = list(patterns = c("*.R", "*.Rdata"))),
      multi = FALSE
    )
    unblockHandlers(w)

    # Ask for project name.
    blockHandlers(w)
    ws_name <- ginput(
      msg = strMsgInputProject,
      text = "",
      title = strMsgTitleSaveAs,
      icon = "info",
      parent = w
    )
    unblockHandlers(w)

    # Check if valid name.
    if (!is.na(ws_name) && !ws_name == "") {

      # Create complete path.
      ws_full_name <- paste(ws_save_path, .separator, ws_name, ".RData", sep = "")

      if (debug) {
        print(ws_full_name)
      }

      # Check if file exist.
      if (file.exists(ws_full_name)) {

        # Ask if overwrite.
        blockHandlers(w)
        ok <- gconfirm(
          msg = paste(
            ws_full_name,
            strMsgOverwrite
          ),
          title = strMsgTitleConfirm, icon = "question", parent = w
        )
        unblockHandlers(w)
      }

      # Check if ok to overwrite.
      if (ok) {

        # Save project.
        if (file.exists(ws_save_path)) {

          # Save project variables in workspace.
          assign(x = .ws_name_variable, value = ws_name, envir = .strvalidator_env)
          assign(x = .ws_path_variable, value = ws_save_path, envir = .strvalidator_env)

          # Save settings.
          .saveSettings()

          # Save project.
          save(
            file = ws_full_name,
            list = ls(envir = .strvalidator_env, all.names = TRUE),
            envir = .strvalidator_env
          )

          blockHandlers(w)
          gmessage(
            msg = paste(strMsgProjectSaved, ws_full_name),
            title = "STR-validator",
            icon = "info",
            parent = w
          )
          unblockHandlers(w)
        } else {
          blockHandlers(w)
          gmessage(
            msg = strMsgDirNotFound,
            title = strMsgTitleDirNotFound,
            icon = "error",
            parent = w
          )
          unblockHandlers(w)
        }
      } else {
        blockHandlers(w)
        gmessage(
          msg = strMsgProjectNotSaved,
          title = strMsgTitleInfo,
          icon = "info",
          parent = w
        )
        unblockHandlers(w)
      }
    } else {
      blockHandlers(w)
      gmessage(
        msg = strMsgFileNameMissing,
        title = strMsgFileNameRequired,
        icon = "error",
        parent = w
      )
      unblockHandlers(w)
    }
  })


  # DATASETS ------------------------------------------------------------------

  workspace_f2 <- gframe(
    text = strFrmRworkspace,
    markup = FALSE,
    pos = 0,
    horizontal = TRUE,
    container = file_tab,
    expand = FALSE
  )

  workspace_f2g1 <- ggroup(
    horizontal = TRUE,
    container = workspace_f2,
    expand = TRUE
  )

  ws_r_refresh_btn <- gbutton(text = strBtnRefresh, container = workspace_f2g1)

  ws_r_drp <- gcombobox(
    items = c(
      strDrpObject,
      listObjects(
        env = .strvalidator_env,
        obj.class = .object_classes_import
      )
    ),
    selected = 1,
    editable = FALSE,
    container = workspace_f2g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  #  ws_r_load_btn <- gbutton(text = strBtnLoad, container = workspace_f2g1)

  addHandlerChanged(ws_r_refresh_btn, handler = function(h, ...) {
    .refreshWs()
  })

  addHandlerChanged(ws_r_drp, handler = function(h, ...) {
    #    addHandlerChanged(ws_r_load_btn, handler = function(h, ...) {

    # Get selected dataset name.
    val_name <- svalue(ws_r_drp)

    if (!is.na(val_name) && !is.null(val_name)) {

      # Load dataset.
      saveObject(
        name = val_name, object = get(val_name),
        parent = w, env = .strvalidator_env, debug = debug
      )

      # Update list.
      .refreshLoaded()
    }
  })


  # STR TYPING KIT ------------------------------------------------------------

  # DRY LAB  ##################################################################

  dry_grid <- glayout(container = drylab_tab, spacing = 2)

  # VIEW ----------------------------------------------------------------------

  dry_grid[1, 1] <- dry_view_btn <- gbutton(text = strBtnView, container = dry_grid)

  dry_grid[1, 2] <- glabel(
    text = strLblViewDataset,
    container = dry_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(dry_view_btn, handler = function(h, ...) {

    # Open GUI.
    editData_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # MAKE KIT ------------------------------------------------------------------

  dry_grid[2, 1] <- dry_kit_btn <- gbutton(text = strBtnKits, container = dry_grid)

  dry_grid[2, 2] <- glabel(
    text = strLblKits,
    container = dry_grid,
    anchor = c(-1, 0)
  )

  dry_grid[3, 1] <- dry_plot_kit_btn <- gbutton(
    text = strBtnPlotKit,
    container = dry_grid
  )

  dry_grid[3, 2] <- glabel(
    text = strLblPlotKit,
    container = dry_grid,
    anchor = c(-1, 0)
  )

  dry_grid[4, 1] <- dry_bins_btn <- gbutton(
    text = strBtnBins,
    container = dry_grid
  )

  dry_grid[4, 2] <- glabel(
    text = strLblBins,
    container = dry_grid,
    anchor = c(-1, 0)
  )

  dry_grid[5, 1] <- dry_ol_btn <- gbutton(
    text = strBtnOl,
    container = dry_grid
  )

  dry_grid[5, 2] <- glabel(
    text = strLblOl,
    container = dry_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(dry_kit_btn, handler = function(h, ...) {

    # Open GUI.
    makeKit_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  addHandlerChanged(dry_plot_kit_btn, handler = function(h, ...) {

    # Open GUI.
    plotKit_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  addHandlerChanged(dry_bins_btn, handler = function(h, ...) {

    # Open GUI.
    calculateOverlap_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  addHandlerChanged(dry_ol_btn, handler = function(h, ...) {

    # Open GUI.
    calculateOL_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # TOOLS  ####################################################################

  tools_grid <- glayout(container = tools_tab, spacing = 2)

  # EDIT ----------------------------------------------------------------------

  tools_grid[1, 1] <- tools_view_btn <- gbutton(
    text = strBtnEdit,
    container = tools_grid
  )

  tools_grid[1, 2] <- glabel(
    text = strLblEdit,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_view_btn, handler = function(h, ...) {

    # Open GUI.
    editData_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = TRUE, debug = debug, parent = w
    )
  })

  # TRIM ----------------------------------------------------------------------

  tools_grid[2, 1] <- tools_trim_btn <- gbutton(
    text = strBtnTrim,
    container = tools_grid
  )

  tools_grid[2, 2] <- glabel(
    text = strLblTrim,
    container = tools_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(tools_trim_btn, handler = function(h, ...) {

    # Open GUI.
    trim_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # SLIM ----------------------------------------------------------------------

  tools_grid[3, 1] <- tools_slim_btn <- gbutton(
    text = strBtnSlim,
    container = tools_grid
  )

  tools_grid[3, 2] <- glabel(
    text = strLblSlim,
    container = tools_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(tools_slim_btn, handler = function(h, ...) {

    # Open GUI.
    slim_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # FILTER --------------------------------------------------------------------

  tools_grid[4, 1] <- tools_filter_btn <- gbutton(
    text = strBtnFilter,
    container = tools_grid
  )

  tools_grid[4, 2] <- glabel(
    text = strLblFilter,
    container = tools_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(tools_filter_btn, handler = function(h, ...) {
    filterProfile_gui(env = .strvalidator_env, savegui = .save_gui, parent = w)
  })

  # CROP ----------------------------------------------------------------------

  tools_grid[5, 1] <- tools_crop_btn <- gbutton(
    text = strBtnCrop,
    container = tools_grid
  )

  tools_grid[5, 2] <- glabel(
    text = strLblCrop,
    container = tools_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(tools_crop_btn, handler = function(h, ...) {

    # Open GUI.
    cropData_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # GUESS ---------------------------------------------------------------------

  tools_grid[6, 1] <- tools_guess_btn <- gbutton(
    text = strBtnGuess,
    container = tools_grid
  )

  tools_grid[6, 2] <- glabel(
    text = strLblGuess,
    container = tools_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(tools_guess_btn, handler = function(h, ...) {
    guessProfile_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # DYE -----------------------------------------------------------------------

  tools_grid[7, 1] <- tools_addDye_btn <- gbutton(
    text = strBtnDye,
    container = tools_grid
  )

  tools_grid[7, 2] <- glabel(
    text = strLblDye,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_addDye_btn, handler = function(h, ...) {

    # Open GUI.
    addDye_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # ADD MARKER ----------------------------------------------------------------

  tools_grid[8, 1] <- tools_addMarker_btn <- gbutton(
    text = strBtnMarker,
    container = tools_grid
  )

  tools_grid[8, 2] <- glabel(
    text = strLblMarker,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_addMarker_btn, handler = function(h, ...) {

    # Open GUI.
    addMarker_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # ADD SIZE ------------------------------------------------------------------

  tools_grid[9, 1] <- tools_addSize_btn <- gbutton(
    text = strBtnSize,
    container = tools_grid
  )

  tools_grid[9, 2] <- glabel(
    text = strLblSize,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_addSize_btn, handler = function(h, ...) {

    # Open GUI.
    addSize_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # ADD DATA -------------------------------------------------------------------

  tools_grid[10, 1] <- tools_addData_btn <- gbutton(
    text = strBtnData,
    container = tools_grid
  )

  tools_grid[10, 2] <- glabel(
    text = strLblData,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_addData_btn, handler = function(h, ...) {

    # Open GUI.
    addData_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # CHECK SUBSET --------------------------------------------------------------

  tools_grid[11, 1] <- tools_check_btn <- gbutton(
    text = strBtnCheck,
    container = tools_grid
  )

  tools_grid[11, 2] <- glabel(
    text = strLblCheck,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_check_btn, handler = function(h, ...) {

    # Open GUI.
    checkSubset_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # COMBINE -------------------------------------------------------------------

  tools_grid[12, 1] <- tools_combine_btn <- gbutton(
    text = strBtnCombine,
    container = tools_grid
  )

  tools_grid[12, 2] <- glabel(
    text = strLblCombine,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_combine_btn, handler = function(h, ...) {

    # Open GUI.
    combine_gui(env = .strvalidator_env, debug = debug, parent = w)
  })

  # COLUMNS -------------------------------------------------------------------

  tools_grid[13, 1] <- tools_columns_btn <- gbutton(
    text = strBtnColumns,
    container = tools_grid
  )

  tools_grid[13, 2] <- glabel(
    text = strLblColumns,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_columns_btn, handler = function(h, ...) {

    # Open GUI.
    columns_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # CALCULATE HETEROZYGOUS ----------------------------------------------------

  tools_grid[14, 1] <- tools_copies_btn <- gbutton(
    text = strBtnCopies,
    container = tools_grid
  )

  tools_grid[14, 2] <- glabel(
    text = strLblCopies,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_copies_btn, handler = function(h, ...) {

    # Open GUI.
    calculateCopies_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # GENERATE EPG --------------------------------------------------------------

  tools_grid[15, 1] <- tools_epg_btn <- gbutton(
    text = strBtnEPG,
    container = tools_grid
  )

  tools_grid[15, 2] <- glabel(
    text = strLblEPG,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_epg_btn, handler = function(h, ...) {

    # Open GUI.
    generateEPG_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # GENERATE EPG2 -------------------------------------------------------------

  tools_grid[16, 1] <- tools_epg2_btn <- gbutton(
    text = strBtnEPG2,
    container = tools_grid
  )

  tools_grid[16, 2] <- glabel(
    text = strLblEPG2,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_epg2_btn, handler = function(h, ...) {

    # Open GUI.
    plotEPG2_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # AT  #######################################################################

  at_grid <- glayout(container = at_tab, spacing = 2)


  # VIEW ----------------------------------------------------------------------

  at_grid[1, 1] <- at_view_btn <- gbutton(text = strBtnView, container = at_grid)

  at_grid[1, 2] <- glabel(
    text = strLblViewDataset,
    container = at_grid, anchor = c(-1, 0)
  )

  addHandlerChanged(at_view_btn, handler = function(h, ...) {

    # Open GUI.
    editData_gui(
      env = .strvalidator_env, savegui =
        .save_gui, edit = FALSE, debug = debug, parent = w
    )
  })

  # CALCULATE -----------------------------------------------------------------

  at_grid[3, 1] <- at_calculate_btn <- gbutton(
    text = strBtnCalculate,
    container = at_grid
  )

  at_grid[3, 2] <- glabel(
    text = strLblAT,
    container = at_grid, anchor = c(-1, 0)
  )

  addHandlerChanged(at_calculate_btn, handler = function(h, ...) {

    # Open GUI.
    calculateAT_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # CALCULATE -----------------------------------------------------------------

  at_grid[4, 1] <- at6_calculate_btn <- gbutton(
    text = strBtnCalculate,
    container = at_grid
  )

  at_grid[4, 2] <- glabel(
    text = strLblAT6,
    container = at_grid, anchor = c(-1, 0)
  )

  addHandlerChanged(at6_calculate_btn, handler = function(h, ...) {

    # Open GUI.
    calculateAT6_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT AT -------------------------------------------------------------------

  at_grid[5, 1] <- at_plot_btn <- gbutton(text = strBtnPlot, container = at_grid)

  at_grid[5, 2] <- glabel(
    text = strLblPlotAT6,
    container = at_grid
  )

  addHandlerChanged(at_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotAT_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # STUTTER  ##################################################################


  stutter_grid <- glayout(container = stutter_tab, spacing = 2)


  # VIEW ----------------------------------------------------------------------

  stutter_grid[1, 1] <- stutter_view_btn <- gbutton(
    text = strBtnView,
    container = stutter_grid
  )

  stutter_grid[1, 2] <- glabel(
    text = strLblViewDataset,
    container = stutter_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(stutter_view_btn, handler = function(h, ...) {

    # Open GUI.
    editData_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # CALCULATE -----------------------------------------------------------------

  stutter_grid[3, 1] <- stutter_calculate_btn <- gbutton(
    text = strBtnCalculate,
    container = stutter_grid
  )

  stutter_grid[3, 2] <- glabel(
    text = strLblStutter,
    container = stutter_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(stutter_calculate_btn, handler = function(h, ...) {

    # Open GUI.
    calculateStutter_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT STUTTER --------------------------------------------------------------

  stutter_grid[4, 1] <- stutter_plot_btn <- gbutton(
    text = strBtnPlot,
    container = stutter_grid
  )

  stutter_grid[4, 2] <- glabel(
    text = strLblPlotStutter,
    container = stutter_grid
  )

  addHandlerChanged(stutter_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotStutter_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS GLOBAL -------------------------------------------------

  stutter_grid[5, 1] <- stutter_stats_global_btn <- gbutton(
    text = strBtnStatistics,
    container = stutter_grid
  )

  stutter_grid[5, 2] <- glabel(
    text = strLblStatStutterGlobal,
    container = stutter_grid
  )

  addHandlerChanged(stutter_stats_global_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Ratio"),
      group = NULL, count = c("Allele"), quant = 0.95,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS MARKER -------------------------------------------------

  stutter_grid[6, 1] <- stutter_stats_marker_btn <- gbutton(
    text = strBtnStatistics,
    container = stutter_grid
  )

  stutter_grid[6, 2] <- glabel(
    text = strLblStatStutterMarker,
    container = stutter_grid
  )

  addHandlerChanged(stutter_stats_marker_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Ratio"),
      group = c("Marker"), count = c("Allele"), quant = 0.95,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS STUTTER ------------------------------------------------

  stutter_grid[7, 1] <- stutter_stats_stutter_btn <- gbutton(
    text = strBtnStatistics,
    container = stutter_grid
  )

  stutter_grid[7, 2] <- glabel(
    text = strLblStatStutterStutter,
    container = stutter_grid
  )

  addHandlerChanged(stutter_stats_stutter_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Ratio"),
      group = c("Marker", "Type"), count = c("Allele"), quant = 0.95,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # BALANCE  ##################################################################


  balance_g1 <- glayout(container = balance_tab, spacing = 2)

  # VIEW ----------------------------------------------------------------------

  balance_g1[1, 1] <- balance_view_btn <- gbutton(
    text = strBtnView,
    container = balance_g1
  )

  balance_g1[1, 2] <- glabel(
    text = strLblViewDataset,
    container = balance_g1,
    anchor = c(-1, 0)
  )

  addHandlerChanged(balance_view_btn, handler = function(h, ...) {

    # Open GUI.
    editData_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })



  # ALLELE BALANCE ============================================================

  balance_hb_frm <- gframe(
    text = strFrmHb,
    horizontal = FALSE, container = balance_tab
  )

  balance_hb <- glayout(container = balance_hb_frm, spacing = 2)

  # CALCULATE -----------------------------------------------------------------

  balance_hb[1, 1] <- balance_hb_calc_btn <- gbutton(
    text = strBtnCalculate,
    container = balance_hb
  )

  balance_hb[1, 2] <- glabel(
    text = strLblHb,
    container = balance_hb
  )

  addHandlerChanged(balance_hb_calc_btn, handler = function(h, ...) {

    # Open GUI.
    calculateHb_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT ----------------------------------------------------------------------

  balance_hb[2, 1] <- balance_hb_plot_btn <- gbutton(
    text = strBtnPlot,
    container = balance_hb
  )

  balance_hb[2, 2] <- glabel(
    text = strLblPlotBalance,
    container = balance_hb
  )

  addHandlerChanged(balance_hb_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotBalance_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS GLOBAL -------------------------------------------------

  balance_hb[3, 1] <- balance_stats_global_btn <- gbutton(
    text = strBtnStatistics,
    container = balance_hb
  )

  balance_hb[3, 2] <- glabel(
    text = strLblStatBalanceGlobal,
    container = balance_hb
  )

  addHandlerChanged(balance_stats_global_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Hb"),
      group = NULL, count = NULL, quant = 0.05,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS MARKER -------------------------------------------------

  balance_hb[4, 1] <- balance_stats_marker_btn <- gbutton(
    text = strBtnStatistics,
    container = balance_hb
  )

  balance_hb[4, 2] <- glabel(
    text = strLblStatBalanceMarker,
    container = balance_hb
  )

  addHandlerChanged(balance_stats_marker_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Hb"),
      group = c("Marker"), count = NULL, quant = 0.05,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PROFILE BALANCE ===========================================================

  balance_lb_frm <- gframe(
    text = strFrmLb,
    horizontal = FALSE, container = balance_tab
  )

  balance_lb <- glayout(container = balance_lb_frm, spacing = 2)

  # CALCULATE -----------------------------------------------------------------

  balance_lb[1, 1] <- balance_lb_calc_btn <- gbutton(
    text = strBtnCalculate,
    container = balance_lb
  )

  balance_lb[1, 2] <- glabel(
    text = strLblLb,
    container = balance_lb
  )


  addHandlerChanged(balance_lb_calc_btn, handler = function(h, ...) {

    # Open GUI.
    calculateLb_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT ----------------------------------------------------------------------

  balance_lb[2, 1] <- balance_lb_plot_btn <- gbutton(
    text = strBtnPlot,
    container = balance_lb
  )

  balance_lb[2, 2] <- glabel(
    text = strLblPlotBalance,
    container = balance_lb
  )

  addHandlerChanged(balance_lb_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotBalance_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS GLOBAL -------------------------------------------------

  balance_lb[3, 1] <- balance_stats_global_btn <- gbutton(
    text = strBtnStatistics,
    container = balance_lb
  )

  balance_lb[3, 2] <- glabel(
    text = strLblStatBalanceGlobal,
    container = balance_lb
  )

  addHandlerChanged(balance_stats_global_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Lb"),
      group = NULL, count = NULL, quant = 0.05,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS MARKER -------------------------------------------------

  balance_lb[4, 1] <- balance_stats_marker_btn <- gbutton(
    text = strBtnStatistics,
    container = balance_lb
  )

  balance_lb[4, 2] <- glabel(
    text = strLblStatBalanceMarker,
    container = balance_lb
  )

  addHandlerChanged(balance_stats_marker_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Lb"),
      group = c("Marker"), count = NULL, quant = 0.05,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # CAPILLARY BALANCE =========================================================

  balance_f3 <- gframe(
    text = strFrmCapillary,
    horizontal = FALSE, container = balance_tab
  )

  balance_g3 <- glayout(container = balance_f3, spacing = 2)


  # CALCULATE -----------------------------------------------------------------

  balance_g3[1, 1] <- balance_g3_calc_btn <- gbutton(
    text = strBtnCalculate,
    container = balance_g3
  )

  balance_g3[1, 2] <- glabel(
    text = strLblCapillary,
    container = balance_g3
  )


  addHandlerChanged(balance_g3_calc_btn, handler = function(h, ...) {

    # Open GUI.
    calculateCapillary_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT ----------------------------------------------------------------------

  balance_g3[2, 1] <- balance_g3_plot_btn <- gbutton(
    text = strBtnPlot,
    container = balance_g3
  )

  balance_g3[2, 2] <- glabel(
    text = strLblPlotCapillary,
    container = balance_g3
  )

  addHandlerChanged(balance_g3_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotCapillary_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS CAPILLARY ----------------------------------------------

  balance_g3[3, 1] <- balance_g3_stats_cap_btn <- gbutton(
    text = strBtnStatistics,
    container = balance_g3
  )

  balance_g3[3, 2] <- glabel(
    text = strLblStatCapillaryCap,
    container = balance_g3
  )

  addHandlerChanged(balance_g3_stats_cap_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Mean.Height"),
      group = c("Capillary"), count = NULL, quant = 0.75,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS INJECTION ----------------------------------------------

  balance_g3[1, 3] <- balance_g3_stats_inj_btn <- gbutton(
    text = strBtnStatistics,
    container = balance_g3
  )

  balance_g3[1, 4] <- glabel(
    text = strLblStatCapillaryInj,
    container = balance_g3
  )

  addHandlerChanged(balance_g3_stats_inj_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Mean.Height"),
      group = c("Injection"), count = NULL, quant = 0.75,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS ROW ----------------------------------------------------

  balance_g3[2, 3] <- balance_g3_stats_row_btn <- gbutton(
    text = strBtnStatistics,
    container = balance_g3
  )

  balance_g3[2, 4] <- glabel(
    text = strLblStatCapillaryRow,
    container = balance_g3
  )

  addHandlerChanged(balance_g3_stats_row_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Mean.Height"),
      group = c("Well"), count = NULL, quant = 0.75,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS RUN ----------------------------------------------------

  balance_g3[3, 3] <- balance_g3_stats_run_btn <- gbutton(
    text = strBtnStatistics,
    container = balance_g3
  )

  balance_g3[3, 4] <- glabel(
    text = strLblStatCapillaryRun,
    container = balance_g3
  )

  addHandlerChanged(balance_g3_stats_run_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Mean.Height"),
      group = c("Run"), count = NULL, quant = 0.75,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS INSTRUMENT ---------------------------------------------

  balance_g3[4, 3] <- balance_g3_stats_ins_btn <- gbutton(
    text = strBtnStatistics,
    container = balance_g3
  )

  balance_g3[4, 4] <- glabel(
    text = strLblStatCapillaryIns,
    container = balance_g3
  )

  addHandlerChanged(balance_g3_stats_ins_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Mean.Height"),
      group = c("Instrument"), count = NULL, quant = 0.75,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # MARKER RATIO ==============================================================

  balance_f4 <- gframe(
    text = strFrmRatio,
    horizontal = FALSE, container = balance_tab
  )

  balance_g4 <- glayout(container = balance_f4, spacing = 2)

  # CALCULATE -----------------------------------------------------------------

  balance_g4[1, 1] <- balance_g4_calc_btn <- gbutton(
    text = strBtnCalculate,
    container = balance_g4
  )

  balance_g4[1, 2] <- glabel(
    text = strLblRatio,
    container = balance_g4
  )

  addHandlerChanged(balance_g4_calc_btn, handler = function(h, ...) {

    # Open GUI.
    calculateRatio_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT ----------------------------------------------------------------------

  balance_g4[2, 1] <- balance_g4_plot_btn <- gbutton(
    text = strBtnPlot,
    container = balance_g4
  )

  balance_g4[2, 2] <- glabel(
    text = strLblPlotRatio,
    container = balance_g4
  )

  addHandlerChanged(balance_g4_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotRatio_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # CONCORDANCE  ##############################################################


  conc_grid <- glayout(container = concordance_tab, spacing = 2)

  # VIEW ----------------------------------------------------------------------

  conc_grid[1, 1] <- conc_view_btn <- gbutton(
    text = strBtnView,
    container = conc_grid
  )

  conc_grid[1, 2] <- glabel(
    text = strLblViewDataset,
    container = conc_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(conc_view_btn, handler = function(h, ...) {

    # Open GUI.
    editData_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # CALCULATE -----------------------------------------------------------------

  conc_grid[2, 1] <- conc_calculate_btn <- gbutton(
    text = strBtnCalculate,
    container = conc_grid
  )

  conc_grid[2, 2] <- glabel(
    text = strLblConcordance,
    container = conc_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(conc_calculate_btn, handler = function(h, ...) {

    # Open GUI.
    calculateConcordance_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # DROPOUT  ##################################################################


  drop_grid <- glayout(container = drop_tab, spacing = 2)

  # VIEW ----------------------------------------------------------------------

  drop_grid[1, 1] <- drop_view_btn <- gbutton(
    text = strBtnView,
    container = drop_grid
  )

  drop_grid[1, 2] <- glabel(
    text = strLblViewDataset,
    container = drop_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(drop_view_btn, handler = function(h, ...) {

    # Open GUI.
    editData_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # SCORE -----------------------------------------------------------------.---

  drop_grid[2, 1] <- drop_score_btn <- gbutton(
    text = strBtnScore,
    container = drop_grid
  )

  drop_grid[2, 2] <- glabel(
    text = strLblScore,
    container = drop_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(drop_score_btn, handler = function(h, ...) {

    # Open GUI.
    calculateDropout_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # CALCULATE ------------------------------------------------------------------

  drop_grid[3, 1] <- drop_calculate_btn <- gbutton(
    text = strBtnCalculate,
    container = drop_grid
  )

  drop_grid[3, 2] <- glabel(
    text = strLblDropout,
    container = drop_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(drop_calculate_btn, handler = function(h, ...) {

    # Open GUI.
    calculateAllT_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # LOGISTIC REGRESSION -------------------------------------------------------

  drop_grid[4, 1] <- drop_model_btn <- gbutton(
    text = strBtnModel,
    container = drop_grid
  )

  drop_grid[4, 2] <- glabel(
    text = strLblModel,
    container = drop_grid
  )

  addHandlerChanged(drop_model_btn, handler = function(h, ...) {

    # Open GUI.
    modelDropout_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT DROPOUT --------------------------------------------------------------

  drop_grid[5, 1] <- drop_plot_btn <- gbutton(
    text = strBtnPlot,
    container = drop_grid
  )

  drop_grid[5, 2] <- glabel(
    text = strLblPlotDropout,
    container = drop_grid
  )

  addHandlerChanged(drop_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotDropout_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # SUMMARY TABLE -------------------------------------------------------------

  # MIXTURE  ##################################################################


  mix_grid <- glayout(container = mixture_tab, spacing = 2)

  # VIEW ----------------------------------------------------------------------

  mix_grid[1, 1] <- mix_view_btn <- gbutton(
    text = strBtnView,
    container = mix_grid
  )

  mix_grid[1, 2] <- glabel(
    text = strLblViewDataset,
    container = mix_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(mix_view_btn, handler = function(h, ...) {

    # Open GUI.
    editData_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # CALCULATE -----------------------------------------------------------------

  mix_grid[2, 1] <- mix_calculate_btn <- gbutton(
    text = strBtnCalculate,
    container = mix_grid
  )

  mix_grid[2, 2] <- glabel(
    text = strLblMixture,
    container = mix_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(mix_calculate_btn, handler = function(h, ...) {

    # Open GUI.
    calculateMixture_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT MIXTURE --------------------------------------------------------------

  # SUMMARY TABLE -------------------------------------------------------------

  # RESULT  ###################################################################


  result_grid <- glayout(container = result_tab, spacing = 2)

  # VIEW ----------------------------------------------------------------------

  result_grid[1, 1] <- result_view_btn <- gbutton(
    text = strBtnView,
    container = result_grid
  )

  result_grid[1, 2] <- glabel(
    text = strLblViewDataset,
    container = result_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(result_view_btn, handler = function(h, ...) {

    # Open GUI.
    editData_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # RESULT TYPE ===============================================================

  result_f1 <- gframe(
    text = strFrmType,
    horizontal = FALSE, container = result_tab
  )

  result_g1 <- glayout(container = result_f1, spacing = 2)


  # CALCULATE -----------------------------------------------------------------

  result_g1[1, 1] <- result_g1_calc_btn <- gbutton(
    text = strBtnCalculate,
    container = result_g1
  )

  result_g1[1, 2] <- glabel(
    text = strLblType,
    container = result_g1,
    anchor = c(-1, 0)
  )


  addHandlerChanged(result_g1_calc_btn, handler = function(h, ...) {

    # Open GUI.
    calculateResultType_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT RESULT TYPE ----------------------------------------------------------

  result_g1[2, 1] <- result_g1_plot_btn <- gbutton(
    text = strBtnPlot,
    container = result_g1
  )

  result_g1[2, 2] <- glabel(
    text = strLblPlotType,
    container = result_g1
  )

  addHandlerChanged(result_g1_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotResultType_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PEAKS =====================================================================

  result_f2 <- gframe(
    text = strFrmPeaks,
    horizontal = FALSE, container = result_tab
  )

  result_g2 <- glayout(container = result_f2, spacing = 2)


  # CALCULATE -----------------------------------------------------------------

  result_g2[1, 1] <- result_g2_calc_btn <- gbutton(
    text = strBtnCalculate,
    container = result_g2
  )

  result_g2[1, 2] <- glabel(
    text = strLblPeaks,
    container = result_g2,
    anchor = c(-1, 0)
  )

  addHandlerChanged(result_g2_calc_btn, handler = function(h, ...) {

    # Open GUI.
    calculatePeaks_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT PEAKS ----------------------------------------------------------------

  result_g2[2, 1] <- result_g2_plot_btn <- gbutton(
    text = strBtnPlot,
    container = result_g2
  )

  result_g2[2, 2] <- glabel(
    text = strLblPlotPeaks,
    container = result_g2
  )

  addHandlerChanged(result_g2_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotPeaks_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # SUMMARY STATISTICS ========================================================

  result_f3 <- gframe(
    text = strFrmStatistics,
    horizontal = FALSE, container = result_tab
  )

  result_g3 <- glayout(container = result_f3, spacing = 2)

  # CALCULATE PEAK HEIGHT -----------------------------------------------------

  result_g3[1, 1] <- result_g3_height_btn <- gbutton(
    text = strBtnCalculate,
    container = result_g3
  )

  result_g3[1, 2] <- glabel(
    text = strLblHeight,
    container = result_g3
  )

  addHandlerChanged(result_g3_height_btn, handler = function(h, ...) {

    # Open GUI.
    calculateHeight_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS --------------------------------------------------------

  result_g3[1, 3] <- result_g3_stats_btn <- gbutton(
    text = strBtnStatistics,
    container = result_g3
  )

  result_g3[1, 4] <- glabel(
    text = strLblStatistics,
    container = result_g3
  )

  addHandlerChanged(result_g3_stats_btn, handler = function(h, ...) {

    # Open GUI.
    calculateStatistics_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # DISTRIBUTIONS =============================================================

  result_f4 <- gframe(
    text = strFrmDistribution,
    horizontal = FALSE, container = result_tab
  )

  result_g4 <- glayout(container = result_f4, spacing = 2)

  # PLOT PEAKS ----------------------------------------------------------------

  result_g4[1, 1] <- result_g4_plot_btn <- gbutton(
    text = strBtnPlot,
    container = result_g4
  )

  result_g4[1, 2] <- glabel(
    text = strLblDistribution,
    container = result_g4
  )

  addHandlerChanged(result_g4_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotDistribution_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT GROUPS ---------------------------------------------------------------

  result_g4[1, 3] <- result_g4_group_btn <- gbutton(
    text = strBtnPlot,
    container = result_g4
  )

  result_g4[1, 4] <- glabel(
    text = strLblGroups,
    container = result_g4
  )

  addHandlerChanged(result_g4_group_btn, handler = function(h, ...) {

    # Open GUI.
    plotGroups_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # DROPIN ====================================================================

  result_f5 <- gframe(
    text = strFrmDropin,
    horizontal = FALSE, container = result_tab
  )

  result_g5 <- glayout(container = result_f5, spacing = 2)


  # CALCULATE -----------------------------------------------------------------

  result_g5[1, 1] <- result_g5_calc_btn <- gbutton(
    text = strBtnCalculate,
    container = result_g5
  )

  result_g5[1, 2] <- glabel(
    text = strLblSpikes,
    container = result_g5,
    anchor = c(-1, 0)
  )

  addHandlerChanged(result_g5_calc_btn, handler = function(h, ...) {

    # Open GUI.
    calculateSpike_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # FILTER PEAKS --------------------------------------------------------------

  result_g5[1, 3] <- result_g5_filter_btn <- gbutton(
    text = strBtnFilter,
    container = result_g5
  )

  result_g5[1, 4] <- glabel(
    text = strLblFilterSpikes,
    container = result_g5
  )

  addHandlerChanged(result_g5_filter_btn, handler = function(h, ...) {

    # Open GUI.
    removeSpike_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # CALCULATE ALLELE ----------------------------------------------------------

  result_g5[2, 1] <- result_g5_allele_btn <- gbutton(
    text = strBtnCalculate,
    container = result_g5
  )

  result_g5[2, 2] <- glabel(
    text = strLblArtefacts,
    container = result_g5
  )

  addHandlerChanged(result_g5_allele_btn, handler = function(h, ...) {

    # Open GUI.
    calculateAllele_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # REMOVE ARTEFACTS ----------------------------------------------------------

  result_g5[2, 3] <- result_g5_artefact_btn <- gbutton(
    text = strBtnFilter,
    container = result_g5
  )

  result_g5[2, 4] <- glabel(
    text = strLblFilterArtefacts,
    container = result_g5
  )

  addHandlerChanged(result_g5_artefact_btn, handler = function(h, ...) {

    # Open GUI.
    removeArtefact_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT CONTAMINATION --------------------------------------------------------

  result_g5[3, 1] <- result_g5_cont_btn <- gbutton(
    text = strBtnPlot,
    container = result_g5
  )

  result_g5[3, 2] <- glabel(
    text = strLblPlotContamination,
    container = result_g5
  )

  addHandlerChanged(result_g5_cont_btn, handler = function(h, ...) {

    # Open GUI.
    plotContamination_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # SLOPE =====================================================================

  result_f6 <- gframe(
    text = strFrmSlope,
    horizontal = FALSE, container = result_tab
  )

  result_g6 <- glayout(container = result_f6, spacing = 2)

  # CALCULATE -----------------------------------------------------------------

  result_g6[1, 1] <- result_g6_calc_btn <- gbutton(
    text = strBtnCalculate,
    container = result_g6
  )

  result_g6[1, 2] <- glabel(
    text = strLblSlope,
    container = result_g6,
    anchor = c(-1, 0)
  )

  addHandlerChanged(result_g6_calc_btn, handler = function(h, ...) {

    # Open GUI.
    calculateSlope_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT ----------------------------------------------------------------------

  result_g6[2, 1] <- result_g6_plot_btn <- gbutton(
    text = strBtnPlot,
    container = result_g6
  )

  result_g6[2, 2] <- glabel(
    text = strLblPlotSlope,
    container = result_g6
  )

  addHandlerChanged(result_g6_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotSlope_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PRECISION  ################################################################


  precision_grid <- glayout(container = precision_tab, spacing = 2)

  # VIEW ----------------------------------------------------------------------

  precision_grid[1, 1] <- precision_view_btn <- gbutton(
    text = strBtnView,
    container = precision_grid
  )

  precision_grid[1, 2] <- glabel(
    text = strLblViewDataset,
    container = precision_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(precision_view_btn, handler = function(h, ...) {

    # Open GUI.
    editData_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # FILTER DATA ---------------------------------------------------------------

  precision_grid[2, 1] <- precision_filter_btn <- gbutton(
    text = strBtnFilter,
    container = precision_grid
  )

  precision_grid[2, 2] <- glabel(
    text = strLblFilter,
    container = precision_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(precision_filter_btn, handler = function(h, ...) {

    # Open GUI.
    filterProfile_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT RESULT TYPE ----------------------------------------------------------

  precision_grid[3, 1] <- precision_plot_btn <- gbutton(
    text = strBtnPlot,
    container = precision_grid
  )

  precision_grid[3, 2] <- glabel(
    text = strLblPrecision,
    container = precision_grid
  )

  addHandlerChanged(precision_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotPrecision_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS SIZE ---------------------------------------------------

  precision_grid[4, 1] <- precision_stats_size_btn <- gbutton(
    text = strBtnStatistics,
    container = precision_grid
  )

  precision_grid[4, 2] <- glabel(
    text = strLblStatPrecisionSize,
    container = precision_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(precision_stats_size_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Size"),
      group = c("Marker", "Allele"), count = NULL, quant = 0.50,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS DATA.POINT ---------------------------------------------

  precision_grid[5, 1] <- precision_stats_dp_btn <- gbutton(
    text = strBtnStatistics,
    container = precision_grid
  )

  precision_grid[5, 2] <- glabel(
    text = strLblStatPrecisionDataPoint,
    container = precision_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(precision_stats_dp_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Data.Point"),
      group = c("Marker", "Allele"), count = NULL, quant = 0.50,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS HEIGHT -------------------------------------------------

  precision_grid[6, 1] <- precision_stats_height_btn <- gbutton(
    text = strBtnStatistics,
    container = precision_grid
  )

  precision_grid[6, 2] <- glabel(
    text = strLblStatPrecisionHeight,
    container = precision_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(precision_stats_height_btn, handler = function(h, ...) {

    # Get most recent object.
    tmp <- listObjects(
      env = .strvalidator_env, obj.class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculateStatistics_gui(
      data = tmp[1], target = c("Height"),
      group = c("Marker", "Allele"), count = NULL, quant = 0.95,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PULLUP  ###################################################################

  pull_grid <- glayout(container = pullup_tab, spacing = 2)

  # VIEW ----------------------------------------------------------------------

  pull_grid[1, 1] <- pull_view_btn <- gbutton(
    text = strBtnView,
    container = pull_grid
  )

  pull_grid[1, 2] <- glabel(
    text = strLblViewDataset,
    container = pull_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(pull_view_btn, handler = function(h, ...) {

    # Open GUI.
    editData_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # CALCULATE -----------------------------------------------------------------

  pull_grid[2, 1] <- pull_calculate_btn <- gbutton(
    text = strBtnCalculate,
    container = pull_grid
  )

  pull_grid[2, 2] <- glabel(
    text = strLblPullup,
    container = pull_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(pull_calculate_btn, handler = function(h, ...) {

    # Open GUI.
    calculatePullup_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT PULLUP ---------------------------------------------------------------

  pull_grid[3, 1] <- pull_plot_btn <- gbutton(
    text = strBtnPlot,
    container = pull_grid
  )

  pull_grid[3, 2] <- glabel(
    text = strLblPlotPullup,
    container = pull_grid
  )

  addHandlerChanged(pull_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotPullup_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # MAIN EVENT HANDLERS #########################################################
  addHandlerChanged(nb, handler = function(h, ...) {
    if (debug) {
      print("NOTEBOOK CHANGED")
      print(if (is.null(h$page.no)) svalue(h$obj) else h$page.no)
    }

    # Refresh depending on active tab.
    # tab <- svalue(nb)
    tab <- if (is.null(h$page.no)) svalue(h$obj) else h$page.no
    tabName <- names(nb)[tab]

    # Check if a tab name exist and then perform tasks.
    if (length(tabName) != 0) {
      if (tabName == strTabWorkspace) {
        .refreshLoaded()
        .refreshWs()
      }

      if (tabName == strTabProject) {
        .updateProjectList()
      }
    } # End check.
  })

  addHandlerFocus(w, handler = function(h, ...) {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
      print("FOCUS")
    }

    # Refresh depending on active tab.
    tab <- svalue(nb)
    tabName <- names(nb)[tab]

    # Check if a tab name exist and then perform tasks.
    if (length(tabName) != 0) {
      if (tabName == strTabWorkspace) {
        .refreshLoaded()
        .refreshWs()
      }
    } # End check.
  })

  # INTERNAL FUNCTIONS ########################################################

  .loadSavedSettings <- function() {

    # First load save flag.
    if (exists(".strvalidator_savegui", envir = .strvalidator_env, inherits = FALSE)) {
      svalue(savegui_chk) <- get(".strvalidator_savegui", envir = .strvalidator_env)
    }

    # Then load settings if true.
    if (svalue(savegui_chk)) {
      if (exists(".strvalidator_project_dir", envir = .strvalidator_env, inherits = FALSE)) {
        svalue(project_fb) <- get(".strvalidator_project_dir", envir = .strvalidator_env)
      }
      if (exists(".strvalidator_last_open_dir", envir = .strvalidator_env, inherits = FALSE)) {
        .ws_last_open_dir <- get(".strvalidator_last_open_dir", envir = .strvalidator_env)
      }
      if (exists(".strvalidator_show_description", envir = .strvalidator_env, inherits = FALSE)) {
        visible(project_f3) <- get(".strvalidator_show_description", envir = .strvalidator_env)
      }
    }

    if (debug) {
      print("Saved settings loaded!")
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_savegui", value = svalue(savegui_chk), envir = .strvalidator_env)
      assign(x = ".strvalidator_project_dir", value = svalue(project_fb), envir = .strvalidator_env)
      assign(x = ".strvalidator_last_open_dir", value = .ws_last_open_dir, envir = .strvalidator_env)
      assign(x = ".strvalidator_show_description", value = visible(project_f3), envir = .strvalidator_env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_savegui", envir = .strvalidator_env, inherits = FALSE)) {
        remove(".strvalidator_savegui", envir = .strvalidator_env)
      }
      if (exists(".strvalidator_project_dir", envir = .strvalidator_env, inherits = FALSE)) {
        remove(".strvalidator_project_dir", envir = .strvalidator_env)
      }
      if (exists(".strvalidator_last_open_dir", envir = .strvalidator_env, inherits = FALSE)) {
        remove(".strvalidator_last_open_dir", envir = .strvalidator_env)
      }
      if (exists(".strvalidator_show_description", envir = .strvalidator_env, inherits = FALSE)) {
        remove(".strvalidator_show_description", envir = .strvalidator_env)
      }

      if (debug) {
        print("Settings cleared!")
      }
    }

    if (debug) {
      print("Settings saved!")
    }
  }

  .refreshWs <- function() {

    # Get data frames in global workspace.
    dfs <- listObjects(env = .GlobalEnv, obj.class = .object_classes_import)

    # Get current list in dropdown.
    cList <- svalue(ws_r_drp)

    # Only populate dropdown if there are new objects available.
    if (!is.null(dfs) && !all(dfs %in% cList)) {
      blockHandler(ws_r_drp)

      # Populate drop list.
      ws_r_drp[] <- c(strDrpObject, dfs)

      # Select first item.
      svalue(ws_r_drp, index = TRUE) <- 1

      unblockHandler(ws_r_drp)
    }
  }

  .refreshLoaded <- function() {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    # Get list of objects.
    dfs <- listObjects(env = .strvalidator_env, obj.class = .object_classes_view)

    # Get size of objects.
    dfsSize <- sapply(dfs, function(x) object.size(get(x, envir = .strvalidator_env)))
    dfsSize <- unname(dfsSize)
    dfsSize <- as.numeric(dfsSize)

    if (!is.null(dfs)) {

      # Populate table.
      blockHandler(ws_loaded_tbl)
      ws_loaded_tbl[, ] <- data.frame(
        Object = dfs, Size = dfsSize,
        stringsAsFactors = FALSE
      )
      unblockHandler(ws_loaded_tbl)
    } else {

      # No objects in environment. Load empty data.frame to clear previous list.
      blockHandler(ws_loaded_tbl)
      ws_loaded_tbl[, ] <- .object_empty_df
      unblockHandler(ws_loaded_tbl)
    }
  }

  .updateProjectList <- function() {

    # Get project folder.
    projectdir <- svalue(project_fb)

    # If nothing, use working directory.
    if (length(projectdir) == 0 || nchar(projectdir) == 0) {
      projectdir <- getwd()

      blockHandlers(project_fb)
      svalue(project_fb) <- projectdir
      unblockHandlers(project_fb)

      message(
        "Project directory set to current working directory: ",
        projectdir
      )
    }

    # Create filter for only 'RData' files.
    fileFilter <- paste(".*", "\\.", "RData", sep = "")

    # Get list of result files.
    .project_path_list <<- list.files(
      path = projectdir, pattern = fileFilter,
      full.names = TRUE, recursive = FALSE,
      ignore.case = TRUE, include.dirs = FALSE
    )

    .project_name_list <<- list.files(
      path = projectdir, pattern = fileFilter,
      full.names = FALSE, recursive = FALSE,
      ignore.case = TRUE, include.dirs = FALSE
    )

    df <- file.info(.project_path_list)

    # Check if any project in list.
    if (length(.project_name_list) > 0) {

      # Update projects list.
      project_tbl[, ] <- data.frame(
        Project = .project_name_list,
        Date = paste(df$mtime),
        Size = df$size,
        Id = seq(length(.project_name_list)),
        stringsAsFactors = FALSE
      )

      message("Updated project list with 'RData' files found in ", projectdir)
    } else {

      # Reset projects list.
      project_tbl[, ] <- data.frame(
        Name = strStrNoProject, Date = "",
        Size = "", Id = "",
        stringsAsFactors = FALSE
      )

      message("No 'RData' files found in ", projectdir)

      # Reset description.
      svalue(proj_info_lbl) <- strLblProject
      svalue(proj_info_txt) <- strStrProjectDescription
    }
  }

  # SHOW GUI ##################################################################

  # Show GUI and first tab.
  svalue(nb) <- 1
  visible(w) <- TRUE
  focus(w)
  message("STR-validator graphical user interface loaded!")
} # END OF GUI
