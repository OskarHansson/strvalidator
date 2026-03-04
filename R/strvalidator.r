# IMPORTANT: To manually run R CMD check in RStudio all packages must be installed in
# both the 32 and 64 bit version. Make sure it is possible to start manually

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

# NOTE: Can't import data frame named 'drop'
# NOTE: Buttons named 'Plot' will show up 'plot'.
# NOTE: Some button names will change due to locale.


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
  
  # GUI scope (used for language lookup)
  fnc <- get_gui_scope()
  
  if (debug) {
    message("IN: ", fnc)
  }
  
  # About text (welcome tab)
  .about_txt <- get_strings(about = TRUE)
  if (is.null(.about_txt)) {
    .about_txt <- "Language file not found."
  }
  
  # Load language file for this GUI
  lng_strings <- get_strings(gui = fnc)
  
  # Default strings (normalized keys)
  default_strings <- list(
    
    # General -----------------------------------------------------------------
    STR_CHK_GUI        = "Save GUI settings",
    STR_CHK_DEBUG      = "Print debug info to console",
    STR_BTN_HELP       = "Help",
    
    # Tabs --------------------------------------------------------------------
    STR_TAB_WELCOME     = "Welcome",
    STR_TAB_WORKSPACE   = "Workspace",
    STR_TAB_PROJECT     = "Projects",
    STR_TAB_DRYLAB      = "DryLab",
    STR_TAB_TOOLS       = "Tools",
    STR_TAB_AT          = "AT",
    STR_TAB_STUTTER     = "Stutter",
    STR_TAB_BALANCE     = "Balance",
    STR_TAB_CONCORDANCE = "Concordance",
    STR_TAB_DROPOUT     = "Dropout",
    STR_TAB_MIXTURE     = "Mixture",
    STR_TAB_RESULT      = "Result",
    STR_TAB_PRECISION   = "Precision",
    STR_TAB_PULLUP      = "Pull-up",
    
    # Buttons -----------------------------------------------------------------
    STR_BTN_VIEW        = "View",
    STR_BTN_OPEN        = "Open",
    STR_BTN_ADD         = "Add",
    STR_BTN_DELETE      = "Delete",
    STR_BTN_SAVE        = "Save",
    STR_BTN_SAVE_AS     = "Save As",
    STR_BTN_IMPORT      = "Import",
    STR_BTN_EXPORT      = "Export",
    STR_BTN_REFRESH     = "Refresh",
    STR_BTN_RENAME      = "Rename",
    STR_BTN_PLOT        = "Plot",
    STR_BTN_CALCULATE   = "Calculate",
    STR_BTN_STATISTICS  = "Statistics",
    STR_BTN_FILTER      = "Filter",
    
    STR_BTN_WEBPAGE     = "STR-validator website",
    STR_BTN_VIDEO       = "Video tutorials",
    STR_BTN_FACEBOOK    = "Facebook page",
    STR_BTN_SUPPORT     = "Support forum",
    STR_BTN_REPORT      = "Report bugs",
    STR_BTN_SOURCE      = "Source code",
    STR_BTN_CRAN        = "CRAN page",
    STR_BTN_LICENSE     = "License",
    
    STR_BTN_BINS           = "Analyse Overlap",
    STR_BTN_CHECK          = "Check",
    STR_BTN_COLUMNS        = "Columns",
    STR_BTN_COMBINE        = "Combine",
    STR_BTN_COPIES         = "Copies",
    STR_BTN_CROP           = "Crop",
    STR_BTN_DATA           = "Data",
    STR_BTN_DYE            = "Dye",
    STR_BTN_EDIT           = "Edit",
    STR_BTN_EPG            = "EPG",
    STR_BTN_EPG2           = "EPG2",
    STR_BTN_GUESS          = "Guess",
    STR_BTN_KITS           = "Kits",
    STR_BTN_LOAD           = "Load object",
    STR_BTN_MARKER         = "Marker",
    STR_BTN_MODEL          = "Model",
    STR_BTN_NEW            = "New",
    STR_BTN_OL             = "Analyse OL",
    STR_BTN_PLOT_KIT       = "Plot Kit",
    STR_BTN_SCORE          = "Score",
    STR_BTN_SIZE           = "Size",
    STR_BTN_SLIM           = "Slim",
    STR_BTN_TRIM           = "Trim",
    
    # Dropdowns ---------------------------------------------------------------
    STR_DRP_OBJECT         = "<Select object>",
    

    # Tooltips ----------------------------------------------------------------
    STR_TIP_WEBPAGE     = "General information, workshops, and tutorials",
    STR_TIP_VIDEO       = "STR-validator YouTube channel",
    STR_TIP_FACEBOOK    = "News, tips, and other information",
    STR_TIP_SUPPORT     = "Get help from the Facebook user community",
    STR_TIP_REPORT      = "Report bugs, errors, and issues",
    STR_TIP_SOURCE      = "Take a look at future, current, and past source code",
    STR_TIP_CRAN        = "Official CRAN page with address to maintainer and version archive",
    
    STR_TIP_ADD            = "Merge a project with the current project",
    STR_TIP_DELETE         = "Delete selected project from the file system",
    STR_TIP_DELETE_OBJECT  = "Delete selected object",
    STR_TIP_EXPORT         = "Open the export dialogue with the selected objects",
    STR_TIP_IMPORT         = "Import data from file",
    STR_TIP_NEW_PROJECT    = "Create a new project",
    STR_TIP_OPEN           = "Open selected project",
    STR_TIP_OPEN_PROJECT   = "Open project",
    STR_TIP_REFRESH        = "Refresh the workspace",
    STR_TIP_RENAME_OBJECT  = "Rename selected object",
    STR_TIP_SAVE_AS        = "Choose a location and save project",
    STR_TIP_SAVE_DESCRIPTION = "Save project description",
    STR_TIP_SAVE_PROJECT   = "Save project",
    STR_TIP_VIEW           = "View selected object",
    
    # Frames ------------------------------------------------------------------
    STR_FRM_PROJECT     = "Projects",
    STR_FRM_DESCRIPTION = "Description",
    STR_FRM_RWORKSPACE   = "Load objects from R workspace",
    
    STR_FRM_CAPILLARY      = "Capillary balance",
    STR_FRM_DISTRIBUTION   = "Distributions",
    STR_FRM_DROPIN         = "Drop-in tools",
    STR_FRM_HB             = "Heterozygote balance (intra-locus)",
    STR_FRM_LB             = "Profile balance (inter-locus)",
    STR_FRM_PEAKS          = "Number of peaks",
    STR_FRM_RATIO          = "Marker peak height ratio",
    STR_FRM_RWORKSPACE     = "Load objects from R workspace",
    STR_FRM_SLOPE          = "Profile slope",
    STR_FRM_STATISTICS     = "Summary statistics",
    STR_FRM_TYPE           = "Result types",
    
    # Labels / Frames ------------------------------------------------------
    
    STR_LBL_FOLDER      = "Folder:",
    STR_LBL_PROJECT     = "Project:",
    STR_LBL_VIEW_DATASET = "View a dataset",
    STR_LBL_ARTEFACTS      = "Identify possible artefacts",
    STR_LBL_AT             = "Calculate analytical threshold (AT1, AT2, AT4, AT7)",
    STR_LBL_AT6            = "Calculate analytical threshold (AT6)",
    STR_LBL_BINS           = "Compare bins overlap for kits",
    STR_LBL_CAPILLARY      = "Calculate capillary balance for a dataset",
    STR_LBL_CHECK          = "Check the subsetting of a dataset",
    STR_LBL_COLUMNS        = "Perform actions on columns",
    STR_LBL_COMBINE        = "Combine two datasets",
    STR_LBL_CONCORDANCE    = "Calculate concordance between multiple datasets",
    STR_LBL_COPIES         = "Calculate allele copies",
    STR_LBL_CROP           = "Discard, or replace data",
    STR_LBL_DATA           = "Add new information to a dataset",
    STR_LBL_DISTRIBUTION   = "Plot distributions for data",
    STR_LBL_DROPOUT        = "Calculate stochastic thresholds",
    STR_LBL_DYE            = "Add dye information according to kit",
    STR_LBL_EDIT           = "Edit a dataset",
    STR_LBL_EPG            = "Generate EPG like plot",
    STR_LBL_EPG2           = "Generate interactive EPG like plot",
    STR_LBL_FILTER         = "Filter a dataset using a reference set",
    STR_LBL_FILTER_ARTEFACTS = "Remove artefacts",
    STR_LBL_FILTER_SPIKES  = "Remove spikes",
    STR_LBL_GROUPS         = "Plot cumulative distribution for multiple groups",
    STR_LBL_GUESS          = "Guess the profile from raw DNA result",
    STR_LBL_HB             = "Calculate heterozygote balance",
    STR_LBL_HEIGHT         = "Calculate peak height metrics",
    STR_LBL_KITS           = "Add new kits or edit kits file",
    STR_LBL_LB             = "Calculate profile balance",
    STR_LBL_MARKER         = "Add missing markers to dataset",
    STR_LBL_MIXTURE        = "Calculate mixture for a dataset",
    STR_LBL_MODEL          = "Model and plot dropout risk",
    STR_LBL_OL             = "Compare risk of getting off-ladder alleles for kits",
    STR_LBL_PEAKS          = "Count the number of peaks in sample",
    STR_LBL_PLOT_AT6       = "Create plots for analysed data (AT6)",
    STR_LBL_PLOT_BALANCE   = "Create plots for analysed data",
    STR_LBL_PLOT_CAPILLARY = "Create plots for capillary balance data",
    STR_LBL_PLOT_CONTAMINATION = "Plot contamination",
    STR_LBL_PLOT_DROPOUT   = "Create plots for analysed data",
    STR_LBL_PLOT_KIT       = "Plot marker ranges for kits",
    STR_LBL_PLOT_PEAKS     = "Create plots for peak data",
    STR_LBL_PLOT_PULLUP    = "Create plots for pull-up data",
    STR_LBL_PLOT_RATIO     = "Create plots for marker ratio data",
    STR_LBL_PLOT_SLOPE     = "Plot slope data",
    STR_LBL_PLOT_STUTTER   = "Create plots for stutter data",
    STR_LBL_PLOT_TYPE      = "Create plots for result type data",
    STR_LBL_PRECISION      = "Calculate precision",
    STR_LBL_PULLUP         = "Calculate spectral pull-up/bleed-through",
    STR_LBL_RATIO          = "Calculate locus ratio for a dataset",
    STR_LBL_SCORE          = "Score dropouts for a dataset",
    STR_LBL_SIZE           = "Add approximate size to alleles in a dataset",
    STR_LBL_SLIM           = "Slim a dataset to 'long' format",
    STR_LBL_SLOPE          = "Calculate the profile slope",
    STR_LBL_SPIKES         = "Identify possible spikes",
    STR_LBL_STATISTICS     = "Calculate summary statistics",
    STR_LBL_STAT_BALANCE_DYE = "Calculate summary statistics by dye",
    STR_LBL_STAT_BALANCE_GLOBAL = "Calculate global summary statistics",
    STR_LBL_STAT_BALANCE_MARKER = "Calculate summary statistics by marker",
    STR_LBL_STAT_CAPILLARY_CAP = "Calculate summary statistics by capillary",
    STR_LBL_STAT_CAPILLARY_INJ = "Calculate summary statistics by injection",
    STR_LBL_STAT_CAPILLARY_INS = "Calculate summary statistics by instrument",
    STR_LBL_STAT_CAPILLARY_ROW = "Calculate summary statistics by plate row",
    STR_LBL_STAT_CAPILLARY_RUN = "Calculate summary statistics by run",
    STR_LBL_STAT_PRECISION_DATA_POINT = "Calculate summary statistics for Data.Point",
    STR_LBL_STAT_PRECISION_HEIGHT = "Calculate summary statistics for Height",
    STR_LBL_STAT_PRECISION_SIZE = "Calculate summary statistics for Size",
    STR_LBL_STAT_STUTTER_GLOBAL = "Calculate global summary statistics",
    STR_LBL_STAT_STUTTER_MARKER = "Calculate summary statistics by marker",
    STR_LBL_STAT_STUTTER_STUTTER = "Calculate summary statistics by marker and stutter type",
    STR_LBL_STUTTER        = "Calculate stutters for a dataset",
    STR_LBL_TRIM           = "Trim/discard samples or columns from a dataset",
    STR_LBL_TYPE           = "Calculate result types for a dataset",
    
    
    # Project strings ------------------------------------------------------
    STR_STR_NO_PROJECT        = "[No project found]",
    STR_STR_DESCRIPTION      = "Write a project description here!",
    STR_STR_PROJECT_DESCRIPTION = "[Project description]",
    
    # Messages -------------------------------------------------------------
    STR_MSG_TITLE_ERROR      = "Error",
    STR_MSG_TITLE_INFO       = "Info",
    STR_MSG_TITLE_CONFIRM    = "Confirm",
    
    STR_MSG_NO_OBJECT        = "No object selected!",
    STR_MSG_EXPORT           = "Please select the objects to export!",
    STR_MSG_TYPE_NOT_SUPPORTED = "object type not supported!",
    STR_MSG_PROJECT_SAVED    = "Project saved!\n\n",
    STR_MSG_DIR_NOT_FOUND    = "The project directory was not found",
    
    STR_MSG_FILE_NAME_MISSING = "A file name must be provided",
    STR_MSG_FILE_NAME_REQUIRED = "File name required",
    STR_MSG_INPUT_PROJECT  = "Input project name",
    STR_MSG_NEW            = "Are you sure you want to create a new project?\\nAny changes to current project since last save will be lost!",
    STR_MSG_NOT_FOUND      = "The workspace file was not found",
    STR_MSG_NO_OBJECT_SELECTED = "No object selected!",
    STR_MSG_OVERWRITE      = "\\nalready exist!\\n\\n Overwrite?",
    STR_MSG_PROJECT_NOT_SAVED = "Project was not saved!",
    STR_MSG_PROPERTY_NOT_FOUND = "Property not found",
    STR_MSG_RENAME         = "Currently you can only rename one object at a time!",
    STR_MSG_SELECT_DIR_SAVE = "Select a directory to save project in",
    STR_MSG_SELECT_OBJECT  = "Please select an object!",
    STR_MSG_SELECT_WORKSPACE = "Select a saved workspace or dataset",
    STR_MSG_TITLE_DIR_NOT_FOUND = "Directory not found",
    STR_MSG_TITLE_NOT_FOUND = "File not found",
    STR_MSG_TITLE_NOT_SUPPORTED = "Unable to view object",
    STR_MSG_TITLE_SAVE_AS  = "Save as",
    STR_MSG_USE_SAVE_AS    = "No project name or path!\\nUse 'Save As' instead"
    
  )
  
  # Merge defaults with language file
  strings <- update_strings_with_language_file(
    default_strings,
    lng_strings$value
  )
  
  # WINDOW ####################################################################

  # Main window.
  w <- gwindow(
    title = paste(
      "STR-validator", packageVersion("strvalidator"),
      " - a forensic validation toolbox"
    ),
    visible = FALSE,
    name = "strvalidator"
  )

  # Vertical main group.
  gv <- ggroup(
    horizontal = FALSE,
    use.scrollwindow = FALSE,
    container = w,
    expand = TRUE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE)
  
  gh_left <- ggroup(container = gh)
  gh_right <- ggroup(container = gh)
  
  savegui_chk <- gcheckbox(text = strings$STR_CHK_GUI, checked = TRUE, container = gh_left)
  debug_chk <- gcheckbox(text = strings$STR_CHK_DEBUG, checked = debug, container = gh_left)
  
  addSpring(gh)
  
  help_btn <- gbutton(text = strings$STR_BTN_HELP, container = gh_right)

  addHandlerChanged(savegui_chk, handler = function(h, ...) {
    # Update variable.
    .save_gui <<- svalue(savegui_chk)
  })

  addHandlerChanged(debug_chk, handler = function(h, ...) {
    # Update variable.
    debug <<- svalue(debug_chk)
  })
  
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
    label = strings$STR_TAB_WELCOME,
    index = 1
  )

  project_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strings$STR_TAB_PROJECT,
    index = 2
  )

  file_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strings$STR_TAB_WORKSPACE,
    index = 3
  )

  drylab_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strings$STR_TAB_DRYLAB,
    index = 4
  )

  tools_tab <- ggroup(
    horizontal = FALSE,
    spacing = 2,
    use.scrollwindow = FALSE,
    container = nb,
    label = strings$STR_TAB_TOOLS,
    index = 5
  )

  at_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strings$STR_TAB_AT,
    index = 6
  )

  stutter_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strings$STR_TAB_STUTTER,
    index = 7
  )

  balance_tab <- ggroup(
    horizontal = FALSE,
    spacing = 2,
    use.scrollwindow = FALSE,
    container = nb,
    label = strings$STR_TAB_BALANCE,
    index = 8
  )

  concordance_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strings$STR_TAB_CONCORDANCE,
    index = 9
  )

  drop_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strings$STR_TAB_DROPOUT,
    index = 10
  )

  mixture_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strings$STR_TAB_MIXTURE,
    index = 11
  )

  result_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strings$STR_TAB_RESULT,
    index = 12
  )

  precision_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strings$STR_TAB_PRECISION,
    index = 13
  )

  pullup_tab <- ggroup(
    horizontal = FALSE,
    spacing = 4,
    use.scrollwindow = FALSE,
    container = nb,
    label = strings$STR_TAB_PULLUP,
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

  webpage_btn <- gbutton(text = strings$STR_BTN_WEBPAGE, container = button_group)
  tooltip(webpage_btn) <- strings$STR_TIP_WEBPAGE

  addHandlerChanged(webpage_btn, handler = function(h, ...) {
    browseURL("https://sites.google.com/site/forensicapps/strvalidator")
  })

  youtube_btn <- gbutton(text = strings$STR_BTN_VIDEO, container = button_group)
  tooltip(youtube_btn) <- strings$STR_TIP_VIDEO

  addHandlerChanged(youtube_btn, handler = function(h, ...) {
    browseURL("https://www.youtube.com/channel/UCs7TxzK21OKvWebQygxAHHA")
  })

  facebook_btn <- gbutton(text = strings$STR_BTN_FACEBOOK, container = button_group)
  tooltip(facebook_btn) <- strings$STR_TIP_FACEBOOK

  addHandlerChanged(facebook_btn, handler = function(h, ...) {
    browseURL("https://www.facebook.com/STRvalidator")
  })

  community_btn <- gbutton(text = strings$STR_BTN_SUPPORT, container = button_group)
  tooltip(community_btn) <- strings$STR_TIP_SUPPORT

  addHandlerChanged(community_btn, handler = function(h, ...) {
    browseURL("https://www.facebook.com/groups/strvalidator/")
  })

  report_btn <- gbutton(text = strings$STR_BTN_REPORT, container = button_group)
  tooltip(report_btn) <- strings$STR_TIP_REPORT

  addHandlerChanged(report_btn, handler = function(h, ...) {
    browseURL("https://github.com/OskarHansson/strvalidator/issues")
  })

  source_btn <- gbutton(text = strings$STR_BTN_SOURCE, container = button_group)
  tooltip(source_btn) <- strings$STR_TIP_SOURCE

  addHandlerChanged(source_btn, handler = function(h, ...) {
    browseURL("https://github.com/OskarHansson/strvalidator")
  })

  cran_btn <- gbutton(text = strings$STR_BTN_CRAN, container = button_group)
  tooltip(cran_btn) <- strings$STR_TIP_CRAN

  addHandlerChanged(cran_btn, handler = function(h, ...) {
    browseURL("https://cran.r-project.org/web/packages/strvalidator/index.html")
  })

  start_license_btn <- gbutton(text = strings$STR_BTN_LICENSE, container = button_group, expand = FALSE)

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

  glabel(text = strings$STR_LBL_FOLDER, anchor = c(-1, 0), container = project_f1)

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
    text = strings$STR_FRM_PROJECT,
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

  project_open_btn <- gbutton(text = strings$STR_BTN_OPEN, container = project_g1)
  tooltip(project_open_btn) <- strings$STR_TIP_OPEN

  project_add_btn <- gbutton(text = strings$STR_BTN_ADD, container = project_g1)
  tooltip(project_add_btn) <- strings$STR_TIP_ADD

  project_delete_btn <- gbutton(text = strings$STR_BTN_DELETE, container = project_g1)
  tooltip(project_delete_btn) <- strings$STR_TIP_DELETE

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
        svalue(nb) <- match(strings$STR_TAB_WORKSPACE, names(nb))
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
        svalue(proj_info_lbl) <- strings$STR_LBL_PROJECT
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
      Name = strings$STR_STR_NO_PROJECT, Date = "",
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
            description <- strings$STR_STR_DESCRIPTION
          }

          # Load description.
          svalue(proj_info_lbl) <- paste(strings$STR_LBL_PROJECT, val_name)
          svalue(proj_info_txt) <- description
        }
      }
    } else {
      # Reset description.
      svalue(proj_info_lbl) <- strings$STR_LBL_PROJECT
      svalue(proj_info_txt) <- strings$STR_STR_PROJECT_DESCRIPTION
    }
  })

  # DESCRIPTION ---------------------------------------------------------------

  # Horizontal main group.
  project_f3 <- gexpandgroup(
    text = strings$STR_FRM_DESCRIPTION,
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

  project_save_btn <- gbutton(text = strings$STR_BTN_SAVE, container = project_g3)
  tooltip(project_save_btn) <- strings$STR_TIP_SAVE_DESCRIPTION

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
    text = strings$STR_LBL_PROJECT, anchor = c(-1, 0),
    container = project_g4
  )
  proj_info_txt <- gtext(
    text = strings$STR_STR_PROJECT_DESCRIPTION, height = 50, expand = TRUE,
    wrap = TRUE, container = project_g4, fill = TRUE
  )

  # WORKSPACE #################################################################

  # LOADED DATASETS -----------------------------------------------------------

  workspace_f1 <- gframe(
    text = strings$STR_FRM_PROJECT,
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

  ws_new_btn <- gbutton(text = strings$STR_BTN_NEW, container = workspace_f1g1)
  tooltip(ws_new_btn) <- strings$STR_TIP_NEW_PROJECT

  ws_open_btn <- gbutton(text = strings$STR_BTN_OPEN, container = workspace_f1g1)
  tooltip(ws_open_btn) <- strings$STR_TIP_OPEN_PROJECT

  ws_save_btn <- gbutton(text = strings$STR_BTN_SAVE, container = workspace_f1g1)
  tooltip(ws_save_btn) <- strings$STR_TIP_SAVE_PROJECT

  ws_saveas_btn <- gbutton(text = strings$STR_BTN_SAVE_AS, container = workspace_f1g1)
  tooltip(ws_saveas_btn) <- strings$STR_TIP_SAVE_AS

  ws_import_btn <- gbutton(text = strings$STR_BTN_IMPORT, container = workspace_f1g1)
  tooltip(ws_import_btn) <- strings$STR_TIP_IMPORT

  ws_export_btn <- gbutton(text = strings$STR_BTN_EXPORT, container = workspace_f1g1)
  tooltip(ws_export_btn) <- strings$STR_TIP_EXPORT

  ws_add_btn <- gbutton(text = strings$STR_BTN_ADD, container = workspace_f1g1)
  tooltip(ws_add_btn) <- strings$STR_TIP_ADD

  ws_refresh_btn <- gbutton(text = strings$STR_BTN_REFRESH, container = workspace_f1g1)
  tooltip(ws_refresh_btn) <- strings$STR_TIP_REFRESH

  ws_remove_btn <- gbutton(text = strings$STR_BTN_DELETE, container = workspace_f1g1)
  tooltip(ws_remove_btn) <- strings$STR_TIP_DELETE_OBJECT

  ws_rename_btn <- gbutton(text = strings$STR_BTN_RENAME, container = workspace_f1g1)
  tooltip(ws_rename_btn) <- strings$STR_TIP_RENAME_OBJECT

  ws_view_btn <- gbutton(text = strings$STR_BTN_VIEW, container = workspace_f1g1)
  tooltip(ws_view_btn) <- strings$STR_TIP_VIEW

  ws_loaded_tbl <- gWidgets2::gtable(
    items = .object_empty_df,
    multiple = TRUE,
    chosencol = 1,
    expand = TRUE,
    container = workspace_f1
  )

  addHandlerChanged(ws_new_btn, handler = function(h, ...) {
    blockHandlers(w)
    response <- gconfirm(msg = strings$STR_MSG_NEW)
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
      save_object(
        name = NULL, object = datanew, suggest = objectName,
        parent = w, remove = objectName, env = .strvalidator_env,
        debug = debug
      )

      .refreshLoaded()
    } else {
      gmessage(
        msg = strings$STR_MSG_RENAME,
        title = strings$STR_MSG_TITLE_ERROR,
        icon = "error",
        parent = w
      )
    }
  })

  addHandlerChanged(ws_open_btn, handler = function(h, ...) {
    val_env <- .strvalidator_env

    blockHandlers(w)
    ws_path <- gfile(
      text = strings$STR_MSG_SELECT_WORKSPACE, type = "open",
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
            msg = strings$STR_MSG_NOT_FOUND,
            title = strings$STR_MSG_TITLE_NOT_FOUND,
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
      text = strings$STR_MSG_SELECT_WORKSPACE, type = "open",
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
            msg = strings$STR_MSG_NOT_FOUND,
            title = strings$STR_MSG_TITLE_NOT_FOUND,
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
        msg = strings$STR_MSG_EXPORT,
        title = strings$STR_MSG_NO_OBJECT_SELECTED, icon = "info", parent = w
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
        dt <- DT::datatable(val_data,
          rownames = FALSE, filter = "top",
          extensions = "Buttons", options = list(dom = "Blfrtip", buttons = c("copy", "csv", "excel", "pdf", "print"))
        )
        print(dt)
      } else if (any(c("ggplot", "plotly", "datatables") %in% val_class)) {
        # View object.
        print(val_data)
      } else {
        blockHandlers(w)
        gmessage(
          msg = paste(val_class, strings$STR_MSG_TYPE_NOT_SUPPORTED),
          title = strings$STR_MSG_TITLE_NOT_SUPPORTED, icon = "error", parent = w
        )
        unblockHandlers(w)
      }
    } else {
      blockHandlers(w)
      gmessage(
        msg = strings$STR_MSG_SELECT_OBJECT,
        title = strings$STR_MSG_NO_OBJECT_SELECTED, icon = "info", parent = w
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
        msg = strings$STR_MSG_NO_OBJECT_SELECTED, title = strings$STR_MSG_TITLE_ERROR,
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
            msg = paste(strings$STR_MSG_PROJECT_SAVED, ws_full_name),
            title = "STR-validator",
            icon = "info",
            parent = w
          )
          unblockHandlers(w)

          message("Project saved as: ", ws_full_name)
        } else {
          blockHandlers(w)
          gmessage(
            msg = strings$STR_MSG_DIR_NOT_FOUND,
            title = strings$STR_MSG_TITLE_DIR_NOT_FOUND,
            icon = "error",
            parent = w
          )
          unblockHandlers(w)
        }
      } else {
        blockHandlers(w)
        gmessage(
          msg = strings$STR_MSG_FILE_NAME_MISSING,
          title = strings$STR_MSG_FILE_NAME_REQUIRED,
          icon = "error",
          parent = w
        )
        unblockHandlers(w)
      }
    } else {
      blockHandlers(w)
      gmessage(
        msg = strings$STR_MSG_USE_SAVE_AS,
        title = strings$STR_MSG_PROPERTY_NOT_FOUND,
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
      text = strings$STR_MSG_SELECT_DIR_SAVE,
      type = "selectdir",
      filter = list("R files" = list(patterns = c("*.R", "*.Rdata"))),
      multi = FALSE
    )
    unblockHandlers(w)

    # Ask for project name.
    blockHandlers(w)
    ws_name <- ginput(
      msg = strings$STR_MSG_INPUT_PROJECT,
      text = "",
      title = strings$STR_MSG_TITLE_SAVE_AS,
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
            strings$STR_MSG_OVERWRITE
          ),
          title = strings$STR_MSG_TITLE_CONFIRM, icon = "question", parent = w
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
            msg = paste(strings$STR_MSG_PROJECT_SAVED, ws_full_name),
            title = "STR-validator",
            icon = "info",
            parent = w
          )
          unblockHandlers(w)
        } else {
          blockHandlers(w)
          gmessage(
            msg = strings$STR_MSG_DIR_NOT_FOUND,
            title = strings$STR_MSG_TITLE_DIR_NOT_FOUND,
            icon = "error",
            parent = w
          )
          unblockHandlers(w)
        }
      } else {
        blockHandlers(w)
        gmessage(
          msg = strings$STR_MSG_PROJECT_NOT_SAVED,
          title = strings$STR_MSG_TITLE_INFO,
          icon = "info",
          parent = w
        )
        unblockHandlers(w)
      }
    } else {
      blockHandlers(w)
      gmessage(
        msg = strings$STR_MSG_FILE_NAME_MISSING,
        title = strings$STR_MSG_FILE_NAME_REQUIRED,
        icon = "error",
        parent = w
      )
      unblockHandlers(w)
    }
  })


  # DATASETS ------------------------------------------------------------------

  workspace_f2 <- gframe(
    text = strings$STR_FRM_RWORKSPACE,
    horizontal = TRUE,
    container = file_tab,
    expand = FALSE
  )

  workspace_f2g1 <- ggroup(
    horizontal = TRUE,
    container = workspace_f2,
    expand = TRUE,
    fill = "x"
  )

  ws_r_refresh_btn <- gbutton(text = strings$STR_BTN_REFRESH, container = workspace_f2g1)

  ws_r_drp <- gcombobox(
    items = c(
      strings$STR_DRP_OBJECT,
      list_objects(
        env = .strvalidator_env,
        obj_class = .object_classes_import
      )
    ),
    selected = 1,
    editable = FALSE,
    container = workspace_f2g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  #  ws_r_load_btn <- gbutton(text = strings$STR_BTN_LOAD, container = workspace_f2g1)

  addHandlerChanged(ws_r_refresh_btn, handler = function(h, ...) {
    .refreshWs()
  })

  addHandlerChanged(ws_r_drp, handler = function(h, ...) {
    #    addHandlerChanged(ws_r_load_btn, handler = function(h, ...) {

    # Get selected dataset name.
    val_name <- svalue(ws_r_drp)

    if (!is.na(val_name) && !is.null(val_name)) {
      # Load dataset.
      save_object(
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

  dry_grid[1, 1] <- dry_view_btn <- gbutton(text = strings$STR_BTN_VIEW, container = dry_grid)

  dry_grid[1, 2] <- glabel(
    text = strings$STR_LBL_VIEW_DATASET,
    container = dry_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(dry_view_btn, handler = function(h, ...) {
    # Open GUI.
    edit_data_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # MAKE KIT ------------------------------------------------------------------

  dry_grid[2, 1] <- dry_kit_btn <- gbutton(text = strings$STR_BTN_KITS, container = dry_grid)

  dry_grid[2, 2] <- glabel(
    text = strings$STR_LBL_KITS,
    container = dry_grid,
    anchor = c(-1, 0)
  )

  dry_grid[3, 1] <- dry_plot_kit_btn <- gbutton(
    text = strings$STR_BTN_PLOT_KIT,
    container = dry_grid
  )

  dry_grid[3, 2] <- glabel(
    text = strings$STR_LBL_PLOT_KIT,
    container = dry_grid,
    anchor = c(-1, 0)
  )

  dry_grid[4, 1] <- dry_bins_btn <- gbutton(
    text = strings$STR_BTN_BINS,
    container = dry_grid
  )

  dry_grid[4, 2] <- glabel(
    text = strings$STR_LBL_BINS,
    container = dry_grid,
    anchor = c(-1, 0)
  )

  dry_grid[5, 1] <- dry_ol_btn <- gbutton(
    text = strings$STR_BTN_OL,
    container = dry_grid
  )

  dry_grid[5, 2] <- glabel(
    text = strings$STR_LBL_OL,
    container = dry_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(dry_kit_btn, handler = function(h, ...) {
    # Open GUI.
    manage_kits_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  addHandlerChanged(dry_plot_kit_btn, handler = function(h, ...) {
    # Open GUI.
    plot_kit_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  addHandlerChanged(dry_bins_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_overlap_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  addHandlerChanged(dry_ol_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_ol_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # TOOLS  ####################################################################

  tools_grid <- glayout(container = tools_tab, spacing = 2)

  # EDIT ----------------------------------------------------------------------

  tools_grid[1, 1] <- tools_view_btn <- gbutton(
    text = strings$STR_BTN_EDIT,
    container = tools_grid
  )

  tools_grid[1, 2] <- glabel(
    text = strings$STR_LBL_EDIT,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_view_btn, handler = function(h, ...) {
    # Open GUI.
    edit_data_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = TRUE, debug = debug, parent = w
    )
  })

  # TRIM ----------------------------------------------------------------------

  tools_grid[2, 1] <- tools_trim_btn <- gbutton(
    text = strings$STR_BTN_TRIM,
    container = tools_grid
  )

  tools_grid[2, 2] <- glabel(
    text = strings$STR_LBL_TRIM,
    container = tools_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(tools_trim_btn, handler = function(h, ...) {
    # Open GUI.
    trim_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # SLIM ----------------------------------------------------------------------

  tools_grid[3, 1] <- tools_slim_btn <- gbutton(
    text = strings$STR_BTN_SLIM,
    container = tools_grid
  )

  tools_grid[3, 2] <- glabel(
    text = strings$STR_LBL_SLIM,
    container = tools_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(tools_slim_btn, handler = function(h, ...) {
    # Open GUI.
    slim_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # FILTER --------------------------------------------------------------------

  tools_grid[4, 1] <- tools_filter_btn <- gbutton(
    text = strings$STR_BTN_FILTER,
    container = tools_grid
  )

  tools_grid[4, 2] <- glabel(
    text = strings$STR_LBL_FILTER,
    container = tools_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(tools_filter_btn, handler = function(h, ...) {
    filter_profile_gui(env = .strvalidator_env, savegui = .save_gui, parent = w)
  })

  # CROP ----------------------------------------------------------------------

  tools_grid[5, 1] <- tools_crop_btn <- gbutton(
    text = strings$STR_BTN_CROP,
    container = tools_grid
  )

  tools_grid[5, 2] <- glabel(
    text = strings$STR_LBL_CROP,
    container = tools_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(tools_crop_btn, handler = function(h, ...) {
    # Open GUI.
    crop_data_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # GUESS ---------------------------------------------------------------------

  tools_grid[6, 1] <- tools_guess_btn <- gbutton(
    text = strings$STR_BTN_GUESS,
    container = tools_grid
  )

  tools_grid[6, 2] <- glabel(
    text = strings$STR_LBL_GUESS,
    container = tools_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(tools_guess_btn, handler = function(h, ...) {
    guess_profile_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # DYE -----------------------------------------------------------------------

  tools_grid[7, 1] <- tools_addDye_btn <- gbutton(
    text = strings$STR_BTN_DYE,
    container = tools_grid
  )

  tools_grid[7, 2] <- glabel(
    text = strings$STR_LBL_DYE,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_addDye_btn, handler = function(h, ...) {
    # Open GUI.
    add_dye_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # ADD MARKER ----------------------------------------------------------------

  tools_grid[8, 1] <- tools_addMarker_btn <- gbutton(
    text = strings$STR_BTN_MARKER,
    container = tools_grid
  )

  tools_grid[8, 2] <- glabel(
    text = strings$STR_LBL_MARKER,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_addMarker_btn, handler = function(h, ...) {
    # Open GUI.
    add_marker_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # ADD SIZE ------------------------------------------------------------------

  tools_grid[9, 1] <- tools_addSize_btn <- gbutton(
    text = strings$STR_BTN_SIZE,
    container = tools_grid
  )

  tools_grid[9, 2] <- glabel(
    text = strings$STR_LBL_SIZE,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_addSize_btn, handler = function(h, ...) {
    # Open GUI.
    add_size_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # ADD DATA -------------------------------------------------------------------

  tools_grid[10, 1] <- tools_addData_btn <- gbutton(
    text = strings$STR_BTN_DATA,
    container = tools_grid
  )

  tools_grid[10, 2] <- glabel(
    text = strings$STR_LBL_DATA,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_addData_btn, handler = function(h, ...) {
    # Open GUI.
    add_data_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # CHECK SUBSET --------------------------------------------------------------

  tools_grid[11, 1] <- tools_check_btn <- gbutton(
    text = strings$STR_BTN_CHECK,
    container = tools_grid
  )

  tools_grid[11, 2] <- glabel(
    text = strings$STR_LBL_CHECK,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_check_btn, handler = function(h, ...) {
    # Open GUI.
    check_subset_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # COMBINE -------------------------------------------------------------------

  tools_grid[12, 1] <- tools_combine_btn <- gbutton(
    text = strings$STR_BTN_COMBINE,
    container = tools_grid
  )

  tools_grid[12, 2] <- glabel(
    text = strings$STR_LBL_COMBINE,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_combine_btn, handler = function(h, ...) {
    # Open GUI.
    combine_gui(env = .strvalidator_env, debug = debug, parent = w)
  })

  # COLUMNS -------------------------------------------------------------------

  tools_grid[13, 1] <- tools_columns_btn <- gbutton(
    text = strings$STR_BTN_COLUMNS,
    container = tools_grid
  )

  tools_grid[13, 2] <- glabel(
    text = strings$STR_LBL_COLUMNS,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_columns_btn, handler = function(h, ...) {
    # Open GUI.
    columns_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # CALCULATE HETEROZYGOUS ----------------------------------------------------

  tools_grid[14, 1] <- tools_copies_btn <- gbutton(
    text = strings$STR_BTN_COPIES,
    container = tools_grid
  )

  tools_grid[14, 2] <- glabel(
    text = strings$STR_LBL_COPIES,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_copies_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_copies_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # GENERATE EPG --------------------------------------------------------------

  tools_grid[15, 1] <- tools_epg_btn <- gbutton(
    text = strings$STR_BTN_EPG,
    container = tools_grid
  )

  tools_grid[15, 2] <- glabel(
    text = strings$STR_LBL_EPG,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_epg_btn, handler = function(h, ...) {
    # Open GUI.
    generate_epg_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # GENERATE EPG2 -------------------------------------------------------------

  tools_grid[16, 1] <- tools_epg2_btn <- gbutton(
    text = strings$STR_BTN_EPG2,
    container = tools_grid
  )

  tools_grid[16, 2] <- glabel(
    text = strings$STR_LBL_EPG2,
    container = tools_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(tools_epg2_btn, handler = function(h, ...) {
    # Open GUI.
    plot_epg2_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # AT  #######################################################################

  at_grid <- glayout(container = at_tab, spacing = 2)


  # VIEW ----------------------------------------------------------------------

  at_grid[1, 1] <- at_view_btn <- gbutton(text = strings$STR_BTN_VIEW, container = at_grid)

  at_grid[1, 2] <- glabel(
    text = strings$STR_LBL_VIEW_DATASET,
    container = at_grid, anchor = c(-1, 0)
  )

  addHandlerChanged(at_view_btn, handler = function(h, ...) {
    # Open GUI.
    edit_data_gui(
      env = .strvalidator_env, savegui =
        .save_gui, edit = FALSE, debug = debug, parent = w
    )
  })

  # CALCULATE -----------------------------------------------------------------

  at_grid[3, 1] <- at_calculate_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = at_grid
  )

  at_grid[3, 2] <- glabel(
    text = strings$STR_LBL_AT,
    container = at_grid, anchor = c(-1, 0)
  )

  addHandlerChanged(at_calculate_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_at_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # CALCULATE -----------------------------------------------------------------

  at_grid[4, 1] <- at6_calculate_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = at_grid
  )

  at_grid[4, 2] <- glabel(
    text = strings$STR_LBL_AT6,
    container = at_grid, anchor = c(-1, 0)
  )

  addHandlerChanged(at6_calculate_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_at6_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT AT -------------------------------------------------------------------

  at_grid[5, 1] <- at_plot_btn <- gbutton(text = strings$STR_BTN_PLOT, container = at_grid)

  at_grid[5, 2] <- glabel(
    text = strings$STR_LBL_PLOT_AT6,
    container = at_grid
  )

  addHandlerChanged(at_plot_btn, handler = function(h, ...) {
    # Open GUI.
    plot_at_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # STUTTER  ##################################################################


  stutter_grid <- glayout(container = stutter_tab, spacing = 2)


  # VIEW ----------------------------------------------------------------------

  stutter_grid[1, 1] <- stutter_view_btn <- gbutton(
    text = strings$STR_BTN_VIEW,
    container = stutter_grid
  )

  stutter_grid[1, 2] <- glabel(
    text = strings$STR_LBL_VIEW_DATASET,
    container = stutter_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(stutter_view_btn, handler = function(h, ...) {
    # Open GUI.
    edit_data_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # CALCULATE -----------------------------------------------------------------

  stutter_grid[3, 1] <- stutter_calculate_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = stutter_grid
  )

  stutter_grid[3, 2] <- glabel(
    text = strings$STR_LBL_STUTTER,
    container = stutter_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(stutter_calculate_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_stutter_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT STUTTER --------------------------------------------------------------

  stutter_grid[4, 1] <- stutter_plot_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = stutter_grid
  )

  stutter_grid[4, 2] <- glabel(
    text = strings$STR_LBL_PLOT_STUTTER,
    container = stutter_grid
  )

  addHandlerChanged(stutter_plot_btn, handler = function(h, ...) {
    # Open GUI.
    plot_stutter_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS GLOBAL -------------------------------------------------

  stutter_grid[5, 1] <- stutter_stats_global_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = stutter_grid
  )

  stutter_grid[5, 2] <- glabel(
    text = strings$STR_LBL_STAT_STUTTER_GLOBAL,
    container = stutter_grid
  )

  addHandlerChanged(stutter_stats_global_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Ratio"),
      group = NULL, count = c("Allele"), quant = 0.95,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS MARKER -------------------------------------------------

  stutter_grid[6, 1] <- stutter_stats_marker_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = stutter_grid
  )

  stutter_grid[6, 2] <- glabel(
    text = strings$STR_LBL_STAT_STUTTER_MARKER,
    container = stutter_grid
  )

  addHandlerChanged(stutter_stats_marker_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Ratio"),
      group = c("Marker"), count = c("Allele"), quant = 0.95,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS STUTTER ------------------------------------------------

  stutter_grid[7, 1] <- stutter_stats_stutter_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = stutter_grid
  )

  stutter_grid[7, 2] <- glabel(
    text = strings$STR_LBL_STAT_STUTTER_STUTTER,
    container = stutter_grid
  )

  addHandlerChanged(stutter_stats_stutter_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
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
    text = strings$STR_BTN_VIEW,
    container = balance_g1
  )

  balance_g1[1, 2] <- glabel(
    text = strings$STR_LBL_VIEW_DATASET,
    container = balance_g1,
    anchor = c(-1, 0)
  )

  addHandlerChanged(balance_view_btn, handler = function(h, ...) {
    # Open GUI.
    edit_data_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })



  # ALLELE BALANCE ============================================================

  balance_hb_frm <- gframe(
    text = strings$STR_FRM_HB,
    horizontal = FALSE, container = balance_tab
  )

  balance_hb <- glayout(container = balance_hb_frm, spacing = 2)

  # CALCULATE -----------------------------------------------------------------

  balance_hb[1, 1] <- balance_hb_calc_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = balance_hb
  )

  balance_hb[1, 2] <- glabel(
    text = strings$STR_LBL_HB,
    container = balance_hb
  )

  addHandlerChanged(balance_hb_calc_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_hb_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT ----------------------------------------------------------------------

  balance_hb[2, 1] <- balance_hb_plot_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = balance_hb
  )

  balance_hb[2, 2] <- glabel(
    text = strings$STR_LBL_PLOT_BALANCE,
    container = balance_hb
  )

  addHandlerChanged(balance_hb_plot_btn, handler = function(h, ...) {
    # Open GUI.
    plot_balance_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS GLOBAL -------------------------------------------------

  balance_hb[3, 1] <- balance_stats_global_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = balance_hb
  )

  balance_hb[3, 2] <- glabel(
    text = strings$STR_LBL_STAT_BALANCE_GLOBAL,
    container = balance_hb
  )

  addHandlerChanged(balance_stats_global_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Hb"),
      group = NULL, count = NULL, quant = 0.05,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS MARKER -------------------------------------------------

  balance_hb[4, 1] <- balance_stats_marker_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = balance_hb
  )

  balance_hb[4, 2] <- glabel(
    text = strings$STR_LBL_STAT_BALANCE_MARKER,
    container = balance_hb
  )

  addHandlerChanged(balance_stats_marker_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Hb"),
      group = c("Marker"), count = NULL, quant = 0.05,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PROFILE BALANCE ===========================================================

  balance_lb_frm <- gframe(
    text = strings$STR_FRM_LB,
    horizontal = FALSE, container = balance_tab
  )

  balance_lb <- glayout(container = balance_lb_frm, spacing = 2)

  # CALCULATE -----------------------------------------------------------------

  balance_lb[1, 1] <- balance_lb_calc_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = balance_lb
  )

  balance_lb[1, 2] <- glabel(
    text = strings$STR_LBL_LB,
    container = balance_lb
  )


  addHandlerChanged(balance_lb_calc_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_lb_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT ----------------------------------------------------------------------

  balance_lb[2, 1] <- balance_lb_plot_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = balance_lb
  )

  balance_lb[2, 2] <- glabel(
    text = strings$STR_LBL_PLOT_BALANCE,
    container = balance_lb
  )

  addHandlerChanged(balance_lb_plot_btn, handler = function(h, ...) {
    # Open GUI.
    plot_balance_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS GLOBAL -------------------------------------------------

  balance_lb[1, 3] <- balance_stats_global_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = balance_lb
  )

  balance_lb[1, 4] <- glabel(
    text = strings$STR_LBL_STAT_BALANCE_GLOBAL,
    container = balance_lb
  )

  addHandlerChanged(balance_stats_global_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Lb"),
      group = NULL, count = NULL, quant = 0.5,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS MARKER -------------------------------------------------

  balance_lb[2, 3] <- balance_stats_marker_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = balance_lb
  )

  balance_lb[2, 4] <- glabel(
    text = strings$STR_LBL_STAT_BALANCE_MARKER,
    container = balance_lb
  )

  addHandlerChanged(balance_stats_marker_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Lb"),
      group = c("Marker"), count = NULL, quant = 0.5,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS DYE ----------------------------------------------------
  
  balance_lb[3, 3] <- balance_stats_dye_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = balance_lb
  )
  
  balance_lb[3, 4] <- glabel(
    text = strings$STR_LBL_STAT_BALANCE_DYE,
    container = balance_lb
  )
  
  addHandlerChanged(balance_stats_dye_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )
    
    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Lb"),
      group = c("Dye"), count = NULL, quant = 0.5,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })
  
  # CAPILLARY BALANCE =========================================================

  balance_f3 <- gframe(
    text = strings$STR_FRM_CAPILLARY,
    horizontal = FALSE, container = balance_tab
  )

  balance_g3 <- glayout(container = balance_f3, spacing = 2)


  # CALCULATE -----------------------------------------------------------------

  balance_g3[1, 1] <- balance_g3_calc_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = balance_g3
  )

  balance_g3[1, 2] <- glabel(
    text = strings$STR_LBL_CAPILLARY,
    container = balance_g3
  )


  addHandlerChanged(balance_g3_calc_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_capillary_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT ----------------------------------------------------------------------

  balance_g3[2, 1] <- balance_g3_plot_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = balance_g3
  )

  balance_g3[2, 2] <- glabel(
    text = strings$STR_LBL_PLOT_CAPILLARY,
    container = balance_g3
  )

  addHandlerChanged(balance_g3_plot_btn, handler = function(h, ...) {
    # Open GUI.
    plot_capillary_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS CAPILLARY ----------------------------------------------

  balance_g3[3, 1] <- balance_g3_stats_cap_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = balance_g3
  )

  balance_g3[3, 2] <- glabel(
    text = strings$STR_LBL_STAT_CAPILLARY_CAP,
    container = balance_g3
  )

  addHandlerChanged(balance_g3_stats_cap_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Mean.Height"),
      group = c("Capillary"), count = NULL, quant = 0.75,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS INJECTION ----------------------------------------------

  balance_g3[1, 3] <- balance_g3_stats_inj_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = balance_g3
  )

  balance_g3[1, 4] <- glabel(
    text = strings$STR_LBL_STAT_CAPILLARY_INJ,
    container = balance_g3
  )

  addHandlerChanged(balance_g3_stats_inj_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Mean.Height"),
      group = c("Injection"), count = NULL, quant = 0.75,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS ROW ----------------------------------------------------

  balance_g3[2, 3] <- balance_g3_stats_row_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = balance_g3
  )

  balance_g3[2, 4] <- glabel(
    text = strings$STR_LBL_STAT_CAPILLARY_ROW,
    container = balance_g3
  )

  addHandlerChanged(balance_g3_stats_row_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Mean.Height"),
      group = c("Well"), count = NULL, quant = 0.75,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS RUN ----------------------------------------------------

  balance_g3[3, 3] <- balance_g3_stats_run_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = balance_g3
  )

  balance_g3[3, 4] <- glabel(
    text = strings$STR_LBL_STAT_CAPILLARY_RUN,
    container = balance_g3
  )

  addHandlerChanged(balance_g3_stats_run_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Mean.Height"),
      group = c("Run"), count = NULL, quant = 0.75,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS INSTRUMENT ---------------------------------------------

  balance_g3[4, 3] <- balance_g3_stats_ins_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = balance_g3
  )

  balance_g3[4, 4] <- glabel(
    text = strings$STR_LBL_STAT_CAPILLARY_INS,
    container = balance_g3
  )

  addHandlerChanged(balance_g3_stats_ins_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Mean.Height"),
      group = c("Instrument"), count = NULL, quant = 0.75,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # MARKER RATIO ==============================================================

  balance_f4 <- gframe(
    text = strings$STR_FRM_RATIO,
    horizontal = FALSE, container = balance_tab
  )

  balance_g4 <- glayout(container = balance_f4, spacing = 2)

  # CALCULATE -----------------------------------------------------------------

  balance_g4[1, 1] <- balance_g4_calc_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = balance_g4
  )

  balance_g4[1, 2] <- glabel(
    text = strings$STR_LBL_RATIO,
    container = balance_g4
  )

  addHandlerChanged(balance_g4_calc_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_ratio_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT ----------------------------------------------------------------------

  balance_g4[2, 1] <- balance_g4_plot_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = balance_g4
  )

  balance_g4[2, 2] <- glabel(
    text = strings$STR_LBL_PLOT_RATIO,
    container = balance_g4
  )

  addHandlerChanged(balance_g4_plot_btn, handler = function(h, ...) {
    # Open GUI.
    plot_ratio_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # CONCORDANCE  ##############################################################


  conc_grid <- glayout(container = concordance_tab, spacing = 2)

  # VIEW ----------------------------------------------------------------------

  conc_grid[1, 1] <- conc_view_btn <- gbutton(
    text = strings$STR_BTN_VIEW,
    container = conc_grid
  )

  conc_grid[1, 2] <- glabel(
    text = strings$STR_LBL_VIEW_DATASET,
    container = conc_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(conc_view_btn, handler = function(h, ...) {
    # Open GUI.
    edit_data_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # CALCULATE -----------------------------------------------------------------

  conc_grid[2, 1] <- conc_calculate_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = conc_grid
  )

  conc_grid[2, 2] <- glabel(
    text = strings$STR_LBL_CONCORDANCE,
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
    text = strings$STR_BTN_VIEW,
    container = drop_grid
  )

  drop_grid[1, 2] <- glabel(
    text = strings$STR_LBL_VIEW_DATASET,
    container = drop_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(drop_view_btn, handler = function(h, ...) {
    # Open GUI.
    edit_data_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # SCORE -----------------------------------------------------------------.---

  drop_grid[2, 1] <- drop_score_btn <- gbutton(
    text = strings$STR_BTN_SCORE,
    container = drop_grid
  )

  drop_grid[2, 2] <- glabel(
    text = strings$STR_LBL_SCORE,
    container = drop_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(drop_score_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_dropout_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # CALCULATE ------------------------------------------------------------------

  drop_grid[3, 1] <- drop_calculate_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = drop_grid
  )

  drop_grid[3, 2] <- glabel(
    text = strings$STR_LBL_DROPOUT,
    container = drop_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(drop_calculate_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_all_t_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # LOGISTIC REGRESSION -------------------------------------------------------

  drop_grid[4, 1] <- drop_model_btn <- gbutton(
    text = strings$STR_BTN_MODEL,
    container = drop_grid
  )

  drop_grid[4, 2] <- glabel(
    text = strings$STR_LBL_MODEL,
    container = drop_grid
  )

  addHandlerChanged(drop_model_btn, handler = function(h, ...) {
    # Open GUI.
    model_dropout_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT DROPOUT --------------------------------------------------------------

  drop_grid[5, 1] <- drop_plot_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = drop_grid
  )

  drop_grid[5, 2] <- glabel(
    text = strings$STR_LBL_PLOT_DROPOUT,
    container = drop_grid
  )

  addHandlerChanged(drop_plot_btn, handler = function(h, ...) {
    # Open GUI.
    plot_dropout_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # SUMMARY TABLE -------------------------------------------------------------

  # MIXTURE  ##################################################################


  mix_grid <- glayout(container = mixture_tab, spacing = 2)

  # VIEW ----------------------------------------------------------------------

  mix_grid[1, 1] <- mix_view_btn <- gbutton(
    text = strings$STR_BTN_VIEW,
    container = mix_grid
  )

  mix_grid[1, 2] <- glabel(
    text = strings$STR_LBL_VIEW_DATASET,
    container = mix_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(mix_view_btn, handler = function(h, ...) {
    # Open GUI.
    edit_data_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # CALCULATE -----------------------------------------------------------------

  mix_grid[2, 1] <- mix_calculate_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = mix_grid
  )

  mix_grid[2, 2] <- glabel(
    text = strings$STR_LBL_MIXTURE,
    container = mix_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(mix_calculate_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_mixture_gui(
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
    text = strings$STR_BTN_VIEW,
    container = result_grid
  )

  result_grid[1, 2] <- glabel(
    text = strings$STR_LBL_VIEW_DATASET,
    container = result_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(result_view_btn, handler = function(h, ...) {
    # Open GUI.
    edit_data_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # RESULT TYPE ===============================================================

  result_f1 <- gframe(
    text = strings$STR_FRM_TYPE,
    horizontal = FALSE, container = result_tab
  )

  result_g1 <- glayout(container = result_f1, spacing = 2)


  # CALCULATE -----------------------------------------------------------------

  result_g1[1, 1] <- result_g1_calc_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = result_g1
  )

  result_g1[1, 2] <- glabel(
    text = strings$STR_LBL_TYPE,
    container = result_g1,
    anchor = c(-1, 0)
  )


  addHandlerChanged(result_g1_calc_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_result_type_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT RESULT TYPE ----------------------------------------------------------

  result_g1[2, 1] <- result_g1_plot_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = result_g1
  )

  result_g1[2, 2] <- glabel(
    text = strings$STR_LBL_PLOT_TYPE,
    container = result_g1
  )

  addHandlerChanged(result_g1_plot_btn, handler = function(h, ...) {
    # Open GUI.
    plot_result_type_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PEAKS =====================================================================

  result_f2 <- gframe(
    text = strings$STR_FRM_PEAKS,
    horizontal = FALSE, container = result_tab
  )

  result_g2 <- glayout(container = result_f2, spacing = 2)


  # CALCULATE -----------------------------------------------------------------

  result_g2[1, 1] <- result_g2_calc_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = result_g2
  )

  result_g2[1, 2] <- glabel(
    text = strings$STR_LBL_PEAKS,
    container = result_g2,
    anchor = c(-1, 0)
  )

  addHandlerChanged(result_g2_calc_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_peaks_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT PEAKS ----------------------------------------------------------------

  result_g2[2, 1] <- result_g2_plot_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = result_g2
  )

  result_g2[2, 2] <- glabel(
    text = strings$STR_LBL_PLOT_PEAKS,
    container = result_g2
  )

  addHandlerChanged(result_g2_plot_btn, handler = function(h, ...) {
    # Open GUI.
    plot_peaks_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # SUMMARY STATISTICS ========================================================

  result_f3 <- gframe(
    text = strings$STR_FRM_STATISTICS,
    horizontal = FALSE, container = result_tab
  )

  result_g3 <- glayout(container = result_f3, spacing = 2)

  # CALCULATE PEAK HEIGHT -----------------------------------------------------

  result_g3[1, 1] <- result_g3_height_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = result_g3
  )

  result_g3[1, 2] <- glabel(
    text = strings$STR_LBL_HEIGHT,
    container = result_g3
  )

  addHandlerChanged(result_g3_height_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_height_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS --------------------------------------------------------

  result_g3[1, 3] <- result_g3_stats_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = result_g3
  )

  result_g3[1, 4] <- glabel(
    text = strings$STR_LBL_STATISTICS,
    container = result_g3
  )

  addHandlerChanged(result_g3_stats_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_statistics_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # DISTRIBUTIONS =============================================================

  result_f4 <- gframe(
    text = strings$STR_FRM_DISTRIBUTION,
    horizontal = FALSE, container = result_tab
  )

  result_g4 <- glayout(container = result_f4, spacing = 2)

  # PLOT PEAKS ----------------------------------------------------------------

  result_g4[1, 1] <- result_g4_plot_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = result_g4
  )

  result_g4[1, 2] <- glabel(
    text = strings$STR_LBL_DISTRIBUTION,
    container = result_g4
  )

  addHandlerChanged(result_g4_plot_btn, handler = function(h, ...) {
    # Open GUI.
    plot_distribution_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT GROUPS ---------------------------------------------------------------

  result_g4[1, 3] <- result_g4_group_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = result_g4
  )

  result_g4[1, 4] <- glabel(
    text = strings$STR_LBL_GROUPS,
    container = result_g4
  )

  addHandlerChanged(result_g4_group_btn, handler = function(h, ...) {
    # Open GUI.
    plot_groups_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # DROPIN ====================================================================

  result_f5 <- gframe(
    text = strings$STR_FRM_DROPIN,
    horizontal = FALSE, container = result_tab
  )

  result_g5 <- glayout(container = result_f5, spacing = 2)


  # CALCULATE -----------------------------------------------------------------

  result_g5[1, 1] <- result_g5_calc_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = result_g5
  )

  result_g5[1, 2] <- glabel(
    text = strings$STR_LBL_SPIKES,
    container = result_g5,
    anchor = c(-1, 0)
  )

  addHandlerChanged(result_g5_calc_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_spike_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # FILTER PEAKS --------------------------------------------------------------

  result_g5[1, 3] <- result_g5_filter_btn <- gbutton(
    text = strings$STR_BTN_FILTER,
    container = result_g5
  )

  result_g5[1, 4] <- glabel(
    text = strings$STR_LBL_FILTER_SPIKES,
    container = result_g5
  )

  addHandlerChanged(result_g5_filter_btn, handler = function(h, ...) {
    # Open GUI.
    remove_spike_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # CALCULATE ALLELE ----------------------------------------------------------

  result_g5[2, 1] <- result_g5_allele_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = result_g5
  )

  result_g5[2, 2] <- glabel(
    text = strings$STR_LBL_ARTEFACTS,
    container = result_g5
  )

  addHandlerChanged(result_g5_allele_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_allele_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # REMOVE ARTEFACTS ----------------------------------------------------------

  result_g5[2, 3] <- result_g5_artefact_btn <- gbutton(
    text = strings$STR_BTN_FILTER,
    container = result_g5
  )

  result_g5[2, 4] <- glabel(
    text = strings$STR_LBL_FILTER_ARTEFACTS,
    container = result_g5
  )

  addHandlerChanged(result_g5_artefact_btn, handler = function(h, ...) {
    # Open GUI.
    remove_artefact_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT CONTAMINATION --------------------------------------------------------

  result_g5[3, 1] <- result_g5_cont_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = result_g5
  )

  result_g5[3, 2] <- glabel(
    text = strings$STR_LBL_PLOT_CONTAMINATION,
    container = result_g5
  )

  addHandlerChanged(result_g5_cont_btn, handler = function(h, ...) {
    # Open GUI.
    plot_contamination_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # SLOPE =====================================================================

  result_f6 <- gframe(
    text = strings$STR_FRM_SLOPE,
    horizontal = FALSE, container = result_tab
  )

  result_g6 <- glayout(container = result_f6, spacing = 2)

  # CALCULATE -----------------------------------------------------------------

  result_g6[1, 1] <- result_g6_calc_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = result_g6
  )

  result_g6[1, 2] <- glabel(
    text = strings$STR_LBL_SLOPE,
    container = result_g6,
    anchor = c(-1, 0)
  )

  addHandlerChanged(result_g6_calc_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_slope_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT ----------------------------------------------------------------------

  result_g6[2, 1] <- result_g6_plot_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = result_g6
  )

  result_g6[2, 2] <- glabel(
    text = strings$STR_LBL_PLOT_SLOPE,
    container = result_g6
  )

  addHandlerChanged(result_g6_plot_btn, handler = function(h, ...) {
    # Open GUI.
    plot_slope_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PRECISION  ################################################################


  precision_grid <- glayout(container = precision_tab, spacing = 2)

  # VIEW ----------------------------------------------------------------------

  precision_grid[1, 1] <- precision_view_btn <- gbutton(
    text = strings$STR_BTN_VIEW,
    container = precision_grid
  )

  precision_grid[1, 2] <- glabel(
    text = strings$STR_LBL_VIEW_DATASET,
    container = precision_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(precision_view_btn, handler = function(h, ...) {
    # Open GUI.
    edit_data_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # CALCULATE PRECISION -------------------------------------------------------

  precision_grid[2, 1] <- precision_calculate_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = precision_grid
  )

  precision_grid[2, 2] <- glabel(
    text = strings$STR_LBL_PRECISION,
    container = precision_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(precision_calculate_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_precision_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT PRECISION ------------------------------------------------------------

  precision_grid[3, 1] <- precision_plot_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = precision_grid
  )

  precision_grid[3, 2] <- glabel(
    text = strings$STR_LBL_PRECISION,
    container = precision_grid
  )

  addHandlerChanged(precision_plot_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_precision_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS SIZE ---------------------------------------------------

  precision_grid[4, 1] <- precision_stats_size_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = precision_grid
  )

  precision_grid[4, 2] <- glabel(
    text = strings$STR_LBL_STAT_PRECISION_SIZE,
    container = precision_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(precision_stats_size_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Size"),
      group = c("Marker", "Allele"), count = NULL, quant = 0.50,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS DATA.POINT ---------------------------------------------

  precision_grid[5, 1] <- precision_stats_dp_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = precision_grid
  )

  precision_grid[5, 2] <- glabel(
    text = strings$STR_LBL_STAT_PRECISION_DATA_POINT,
    container = precision_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(precision_stats_dp_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
      data = tmp[1], target = c("Data.Point"),
      group = c("Marker", "Allele"), count = NULL, quant = 0.50,
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY STATISTICS HEIGHT -------------------------------------------------

  precision_grid[6, 1] <- precision_stats_height_btn <- gbutton(
    text = strings$STR_BTN_STATISTICS,
    container = precision_grid
  )

  precision_grid[6, 2] <- glabel(
    text = strings$STR_LBL_STAT_PRECISION_HEIGHT,
    container = precision_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(precision_stats_height_btn, handler = function(h, ...) {
    # Get most recent object.
    tmp <- list_objects(
      env = .strvalidator_env, obj_class = "data.frame",
      sort = "time", decreasing = TRUE, debug = debug
    )

    # Open GUI.
    calculate_statistics_gui(
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
    text = strings$STR_BTN_VIEW,
    container = pull_grid
  )

  pull_grid[1, 2] <- glabel(
    text = strings$STR_LBL_VIEW_DATASET,
    container = pull_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(pull_view_btn, handler = function(h, ...) {
    # Open GUI.
    edit_data_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = FALSE, debug = debug, parent = w
    )
  })

  # CALCULATE -----------------------------------------------------------------

  pull_grid[2, 1] <- pull_calculate_btn <- gbutton(
    text = strings$STR_BTN_CALCULATE,
    container = pull_grid
  )

  pull_grid[2, 2] <- glabel(
    text = strings$STR_LBL_PULLUP,
    container = pull_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(pull_calculate_btn, handler = function(h, ...) {
    # Open GUI.
    calculate_pullup_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT PULLUP ---------------------------------------------------------------

  pull_grid[3, 1] <- pull_plot_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = pull_grid
  )

  pull_grid[3, 2] <- glabel(
    text = strings$STR_LBL_PLOT_PULLUP,
    container = pull_grid
  )

  addHandlerChanged(pull_plot_btn, handler = function(h, ...) {
    # Open GUI.
    plot_pullup_gui(
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
      if (tabName == strings$STR_TAB_WORKSPACE) {
        .refreshLoaded()
        .refreshWs()
      }

      if (tabName == strings$STR_TAB_PROJECT) {
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
      if (tabName == strings$STR_TAB_WORKSPACE) {
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
      if (exists(".strvalidator_debug_chk", envir = .strvalidator_env, inherits = FALSE)) {
        svalue(debug_chk) <- get(".strvalidator_debug_chk", envir = .strvalidator_env)
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
      assign(x = ".strvalidator_debug_chk", value = svalue(debug_chk), envir = .strvalidator_env)
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
      if (exists(".strvalidator_debug_chk", envir = .strvalidator_env, inherits = FALSE)) {
        remove(".strvalidator_debug_chk", envir = .strvalidator_env)
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
    dfs <- list_objects(env = .GlobalEnv, obj_class = .object_classes_import)

    # Get current list in dropdown.
    cList <- svalue(ws_r_drp)

    # Only populate dropdown if there are new objects available.
    if (!is.null(dfs) && !all(dfs %in% cList)) {
      blockHandler(ws_r_drp)

      # Populate drop list.
      ws_r_drp[] <- c(strings$STR_DRP_OBJECT, dfs)

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
    dfs <- list_objects(env = .strvalidator_env, obj_class = .object_classes_view)

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
        Name = strings$STR_STR_NO_PROJECT, Date = "",
        Size = "", Id = "",
        stringsAsFactors = FALSE
      )

      message("No 'RData' files found in ", projectdir)

      # Reset description.
      svalue(proj_info_lbl) <- strings$STR_LBL_PROJECT
      svalue(proj_info_txt) <- strings$STR_STR_PROJECT_DESCRIPTION
    }
  }

  # SHOW GUI ##################################################################

  # Show GUI and first tab.
  svalue(nb) <- 1
  visible(w) <- TRUE
  focus(w)
  message("STR-validator graphical user interface loaded!")
} # END OF GUI
