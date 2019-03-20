################################################################################
# TODO LIST
# TODO: Object size not sorted correct (seem to sort as character)
# TODO: USe viwweports instead of grid.arrange in complex plots?
# http://www.imachordata.com/extra-extra-get-your-gridextra/#comment-146
# TODO: Bug in 'save description': if project A is open in the 'workspace' and
# description is changed in 'Projects' and the project is save from the ws
# the variable holding the description will be overwritten by an empty/previous value.
# (must check which project is open and update both?)

# IMPORTANT: To manually run R CMD check in RStudio all packages must be installed in
# both the 32 and 64 bit version. Make sure it is possible to start manually
# (GTK+ must be installed by clicking 'OK' on the message box).

# See http://r-pkgs.had.co.nz/release.html for advice on release.
# IMPORTANT: Use devtools::spell_check() to check spelling.
# IMPORTANT: Use devtools::check_win_devel() to check on R-dev.
# IMPORTANT: Use devtools::check_win_release() to check on current R.
# IMPORTANT: Use devtools::check_win_oldrelease() to test on previous major R.
# IMPORTANT: Use revdepcheck::revdep_check() to check reverse dependencies
# (Does not work - gui related error).
# IMPORTANT: Use devtools::release() to submitt to CRAN.
# NB! The error below indicates some problem with the test server (try again later).
# Error in curl::curl_fetch_memory(url, handle = h) : Timeout was reached

# Versioning convention (x.yy.z[.9###]):
# Increment x on major change.
# Increment yy on new features.
# Increment z on minor changes and bug fixes.
# [optional]Increment ### on development versions.
# NB! Write changes in NEWS for x.yy.z.9### versions, but move changes to NEWS
# under x.yy.z upon release official version.

# NOTE:
# NOTE: Can't import data frame named 'drop'
# NOTE: Buttons named 'Plot' will show up 'plot'.
# NOTE: Some button names will change due to locale.

################################################################################
# CHANGE LOG (last 20 changes)
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
# 31.07.2017: Fixed error when pressing 'New' project.
# 19.07.2017: Updated the export_gui call with new argument 'obj' and logic.
# 18.07.2017: Changed 'Edit' button to 'View' button in all tabs except 'Tools'.
# 17.07.2017: Fixed problems with updating the project list.
# 17.07.2017: Changed notebook argument 'pageno' to 'page.no'.
# 17.07.2017: Removed some unnecessary calls to .refreshLoaded() and blocked handlers.
# 17.07.2017: Fixed "Error in paste(...) : argument "msg" is missing, with no default".
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 13.07.2017: Fixed "Error in get(val_obj..." clicking View with no object selected.

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
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Global variables.
  .strvalidator_env <- new.env()
  .separator <- .Platform$file.sep # Platform dependent path separator.
  .save_gui <- TRUE
  .ws_last_open_dir <- getwd()
  .start_tab_name <- "Welcome"
  .file_tab_name <- "Workspace"
  .project_tab_name <- "Projects"
  .drylab_tab_name <- "DryLab"
  .edit_tab_name <- "Tools"
  .at_tab_name <- "AT"
  .stutter_tab_name <- "Stutter"
  .balance_tab_name <- "Balance"
  .concordance_tab_name <- "Concordance"
  .drop_tab_name <- "Dropout"
  .mixture_tab_name <- "Mixture"
  .result_tab_name <- "Result"
  .precision_tab_name <- "Precision"
  .pullup_tab_name <- "Pull-up"
  .object_classes_view <- c("data.frame", "ggplot")
  .object_classes_import <- c("data.frame", "ggplot")
  .project_description_variable <- ".strvalidator_project_description"
  .project_tmp_env <- new.env()
  .project_name_list <- NULL
  .project_path_list <- NULL
  .ws_name_variable <- ".strvalidator_project_name"
  .ws_path_variable <- ".strvalidator_project_path"


  # MAIN WINDOW  ##############################################################

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

  savegui_chk <- gcheckbox(text = "Save GUI settings", checked = TRUE, container = gh)

  addHandlerChanged(savegui_chk, handler = function(h, ...) {

    # Update variable.
    .save_gui <<- svalue(savegui_chk)
  })

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("strvalidator", help_type = "html"))
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
    spacing = 10,
    use.scrollwindow = FALSE,
    container = nb,
    label = .start_tab_name,
    index = 1
  )

  project_tab <- ggroup(
    horizontal = FALSE,
    spacing = 10,
    use.scrollwindow = FALSE,
    container = nb,
    label = .project_tab_name,
    index = 2
  )

  file_tab <- ggroup(
    horizontal = FALSE,
    spacing = 10,
    use.scrollwindow = FALSE,
    container = nb,
    label = .file_tab_name,
    index = 3
  )

  drylab_tab <- ggroup(
    horizontal = FALSE,
    spacing = 10,
    use.scrollwindow = FALSE,
    container = nb,
    label = .drylab_tab_name,
    index = 4
  )

  edit_tab <- ggroup(
    horizontal = FALSE,
    spacing = 5,
    use.scrollwindow = FALSE,
    container = nb,
    label = .edit_tab_name,
    index = 5
  )

  at_tab <- ggroup(
    horizontal = FALSE,
    spacing = 10,
    use.scrollwindow = FALSE,
    container = nb,
    label = .at_tab_name,
    index = 6
  )

  stutter_tab <- ggroup(
    horizontal = FALSE,
    spacing = 10,
    use.scrollwindow = FALSE,
    container = nb,
    label = .stutter_tab_name,
    index = 7
  )

  balance_tab <- ggroup(
    horizontal = FALSE,
    spacing = 5,
    use.scrollwindow = FALSE,
    container = nb,
    label = .balance_tab_name,
    index = 8
  )

  concordance_tab <- ggroup(
    horizontal = FALSE,
    spacing = 10,
    use.scrollwindow = FALSE,
    container = nb,
    label = .concordance_tab_name,
    index = 9
  )

  drop_tab <- ggroup(
    horizontal = FALSE,
    spacing = 10,
    use.scrollwindow = FALSE,
    container = nb,
    label = .drop_tab_name,
    index = 10
  )

  mixture_tab <- ggroup(
    horizontal = FALSE,
    spacing = 10,
    use.scrollwindow = FALSE,
    container = nb,
    label = .mixture_tab_name,
    index = 11
  )

  result_tab <- ggroup(
    horizontal = FALSE,
    spacing = 10,
    use.scrollwindow = FALSE,
    container = nb,
    label = .result_tab_name,
    index = 12
  )

  precision_tab <- ggroup(
    horizontal = FALSE,
    spacing = 10,
    use.scrollwindow = FALSE,
    container = nb,
    label = .precision_tab_name,
    index = 13
  )

  pullup_tab <- ggroup(
    horizontal = FALSE,
    spacing = 10,
    use.scrollwindow = FALSE,
    container = nb,
    label = .pullup_tab_name,
    index = 14
  )

  # START #####################################################################

  # Vertical main group.
  start_f1 <- gframe(
    horizontal = FALSE,
    container = start_tab,
    spacing = 5,
    expand = TRUE,
    fill = TRUE
  )

  # STR TYPING KIT ------------------------------------------------------------

  about_txt <- paste("strvalidator is a package developed for validation ",
    "and process control of methods and instruments in a forensic genetic ",
    "laboratory. The graphical user interface STR-validator make it easy to ",
    "analyse data in accordance with European Network of Forensic Science ",
    "Institutes (ENFSI) and Scientific Working Group on DNA Analysis Methods ",
    "(SWGDAM) validation guidelines.",
    "\n\n",
    "In order to assure correct results, most of the core functions are ",
    "extensively validated using the 'testthat' package before a new version ",
    "is released (see STR-validator webpage for details).",
    "\n\n",
    "STR-validator is a product of the PhD work performed by Oskar Hansson ",
    "(thesis available at the STR-validator website), ",
    "which was partly funded by the European Union seventh Framework ",
    "Programme (FP7/2007-2013) under grant agreement no 285487 (EUROFORGEN-NoE).",
    "\n\n",
    "Please cite as:",
    "\n",
    "Hansson O, Gill P, Egeland T (2014). \"STR-validator: An open source ",
    "platform for validation and process control.\" Forensic Science ",
    "International: Genetics, 13, 154-166. doi: 10.1016/j.fsigen.2014.07.009 ",
    "\n\n",
    "Contributions to the strvalidator package or user community is more than welcome. ",
    "Contact the developer to:\n ",
    "- improve existing functionality or add new\n ",
    "- translate course material, manuals, or tutorial\n ",
    "- collaborate to implement new functions\n ",
    "- add tests to validate functions\n ",
    "\n",
    "Created and maintained by:\n",
    "Oskar Hansson, Forensic Genetics (Oslo University Hospital, Norway)",
    sep = ""
  )

  gtext(
    text = about_txt, width = NULL, height = NULL, font.attr = NULL,
    wrap = TRUE, expand = TRUE, container = start_f1, fill = TRUE,
    anchor = c(-1, 0)
  )

  button_group <- ggroup(container = start_f1)

  webpage_btn <- gbutton(text = "STR-validator website", container = button_group)
  tooltip(webpage_btn) <- "General information, workshops, and tutorials"

  addHandlerChanged(webpage_btn, handler = function(h, ...) {
    browseURL("https://sites.google.com/site/forensicapps/strvalidator")
  })
  
  youtube_btn <- gbutton(text = "Video tutorials", container = button_group)
  tooltip(youtube_btn) <- "STR-validator YouTube channel"
  
  addHandlerChanged(youtube_btn, handler = function(h, ...) {
    browseURL("https://www.youtube.com/channel/UCs7TxzK21OKvWebQygxAHHA")
  })
  
  facebook_btn <- gbutton(text = "Facebook page", container = button_group)
  tooltip(facebook_btn) <- "News, tips, and other information"

  addHandlerChanged(facebook_btn, handler = function(h, ...) {
    browseURL("https://www.facebook.com/STRvalidator")
  })

  community_btn <- gbutton(text = "Support forum", container = button_group)
  tooltip(community_btn) <- "Get help from the Facebook user community"

  addHandlerChanged(community_btn, handler = function(h, ...) {
    browseURL("https://www.facebook.com/groups/strvalidator/")
  })

  report_btn <- gbutton(text = "Report bugs", container = button_group)
  tooltip(report_btn) <- "Report bugs, errors, and issues"

  addHandlerChanged(report_btn, handler = function(h, ...) {
    browseURL("https://github.com/OskarHansson/strvalidator/issues")
  })

  source_btn <- gbutton(text = "Source code", container = button_group)
  tooltip(source_btn) <- "Take a look at future, current, and past source code"

  addHandlerChanged(source_btn, handler = function(h, ...) {
    browseURL("https://github.com/OskarHansson/strvalidator")
  })

  cran_btn <- gbutton(text = "CRAN page", container = button_group)
  tooltip(cran_btn) <- "Official CRAN page with address to maintainer and version archive"

  addHandlerChanged(cran_btn, handler = function(h, ...) {
    browseURL("https://cran.r-project.org/web/packages/strvalidator/index.html")
  })

  start_license_btn <- gbutton(text = "License", container = button_group, expand = FALSE)

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
    spacing = 5,
    expand = TRUE
  )

  # FOLDER --------------------------------------------------------------------

  glabel(text = "Folder:", anchor = c(-1, 0), container = project_f1)

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
    text = "Projects",
    horizontal = TRUE,
    spacing = 5,
    container = project_f1,
    expand = TRUE
  )

  # Button group.
  project_g1 <- ggroup(
    horizontal = FALSE,
    spacing = 5,
    container = project_f2,
    expand = FALSE
  )

  project_open_btn <- gbutton(text = "Open", container = project_g1)
  tooltip(project_open_btn) <- "Open selected project"

  project_add_btn <- gbutton(text = "Add", container = project_g1)
  tooltip(project_add_btn) <- "Merge with current project"

  project_delete_btn <- gbutton(text = "Delete", container = project_g1)
  tooltip(project_delete_btn) <- "Delete selected project from the file system"

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
        svalue(nb) <- match(.file_tab_name, names(nb))
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
        svalue(proj_info_lbl) <- paste("Project:")
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
      Name = "[No project found]", Date = "",
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

        # Load project in temporary environment.
        load(file = val_prj, envir = val_env, verbose = FALSE)
        if (exists(x = val_obj, envir = val_env, inherits = FALSE)) {
          description <- get(x = val_obj, envir = val_env, inherits = FALSE)
        } else {
          description <- "Write a project description here!"
        }

        # Load description.
        svalue(proj_info_lbl) <- paste("Project:", val_name)
        svalue(proj_info_txt) <- description
      }
    } else {

      # Reset description.
      svalue(proj_info_lbl) <- "Project:"
      svalue(proj_info_txt) <- "[Project description]"
    }
  })

  # DESCRIPTION ---------------------------------------------------------------

  # Horizontal main group.
  project_f3 <- gframe(
    text = "Description",
    horizontal = TRUE,
    spacing = 5,
    container = project_f1,
    expand = TRUE
  )

  # Button group.
  project_g3 <- ggroup(
    horizontal = FALSE, spacing = 5,
    container = project_f3, expand = FALSE
  )

  project_save_btn <- gbutton(text = "Save", container = project_g3)
  tooltip(project_save_btn) <- "Save project description"

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
    spacing = 5,
    container = project_f3,
    expand = TRUE,
    fill = TRUE
  )

  # Project description window.
  proj_info_lbl <- glabel(
    text = "Project:", anchor = c(-1, 0),
    container = project_g4
  )
  proj_info_txt <- gtext(
    text = "[Project description]", height = 50, expand = TRUE,
    wrap = TRUE, container = project_g4, fill = TRUE
  )

  # WORKSPACE #################################################################

  # LOADED DATASETS -----------------------------------------------------------

  workspace_f1 <- gframe(
    text = "Project",
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

  ws_new_btn <- gbutton(text = "New", container = workspace_f1g1)
  tooltip(ws_new_btn) <- "Create a new project"

  ws_open_btn <- gbutton(text = "Open", container = workspace_f1g1)
  tooltip(ws_open_btn) <- "Open project"

  ws_save_btn <- gbutton(text = "Save", container = workspace_f1g1)
  tooltip(ws_save_btn) <- "Save project"

  ws_saveas_btn <- gbutton(text = "Save As", container = workspace_f1g1)
  tooltip(ws_saveas_btn) <- "Choose a location and save project"

  ws_import_btn <- gbutton(text = "Import", container = workspace_f1g1)
  tooltip(ws_import_btn) <- "Import data from file"

  ws_export_btn <- gbutton(text = "Export", container = workspace_f1g1)
  tooltip(ws_export_btn) <- "Open the export dialogue with the selected objects"

  ws_add_btn <- gbutton(text = "Add", container = workspace_f1g1)
  tooltip(ws_add_btn) <- "Merge a project with the current project"

  ws_refresh_btn <- gbutton(text = "Refresh", container = workspace_f1g1)
  tooltip(ws_refresh_btn) <- "Refresh the workspace"

  ws_remove_btn <- gbutton(text = "Delete", container = workspace_f1g1)
  tooltip(ws_remove_btn) <- "Delete selected object"

  ws_rename_btn <- gbutton(text = "Rename", container = workspace_f1g1)
  tooltip(ws_rename_btn) <- "Rename selected object"

  ws_view_btn <- gbutton(text = "View", container = workspace_f1g1)
  tooltip(ws_view_btn) <- "View selected object"

  ws_loaded_tbl <- gWidgets2::gtable(
    items = data.frame(
      Object = "[Object]", Size = "[Size]",
      stringsAsFactors = FALSE
    ),
    multiple = TRUE,
    chosencol = 1,
    expand = TRUE,
    container = workspace_f1
  )


  addHandlerChanged(ws_new_btn, handler = function(h, ...) {
    msg <- paste("Are you sure you want to create a new project?\n",
      "Any changes to current project since last save will be lost!",
      sep = ""
    )

    blockHandlers(w)
    response <- gconfirm(msg = msg)
    unblockHandlers(w)

    if (response) {

      # Create a new environment.
      .strvalidator_env <<- new.env()
      print("A new project environment was created.")
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
        msg = "Currently you can only rename one object at a time!",
        title = "Error",
        icon = "error",
        parent = w
      )
    }
  })

  addHandlerChanged(ws_open_btn, handler = function(h, ...) {
    val_env <- .strvalidator_env

    blockHandlers(w)
    ws_path <- gfile(
      text = "Select a saved workspace or dataset", type = "open",
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
          .loadSavedSettings()

          # Save last used directory.
          .ws_last_open_dir <<- dirname(ws_path)
        } else {
          blockHandlers(w)
          gmessage(
            msg = "The workspace file was not found",
            title = "File not found",
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
      text = "Select a saved workspace or dataset", type = "open",
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
            msg = "The workspace file was not found",
            title = "File not found",
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

    if (!is.null(val) && !is.na(val) && length(val) > 0) {

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
        msg = "Please select the objects to export!",
        title = "No object selected", icon = "info", parent = w
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

        # Open GUI.
        editData_gui(
          env = .strvalidator_env,
          savegui = .save_gui,
          data = get(val_obj, envir = .strvalidator_env),
          name = val_obj,
          edit = FALSE, debug = debug, parent = w
        )
      } else if ("ggplot" %in% val_class) {

        # Plot object.
        print(val_data)
      } else {
        blockHandlers(w)
        gmessage(
          msg = paste("Object of type", val_class, "not supported!"),
          title = "Unable to view object", icon = "error", parent = w
        )
        unblockHandlers(w)
      }
    } else {
      blockHandlers(w)
      gmessage(
        msg = "Please select an object!",
        title = "No object selected", icon = "info", parent = w
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
        msg = "No object selected!", title = "Error",
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
            msg = paste("Project saved!\n\n", ws_full_name),
            title = "STR-validator",
            icon = "info",
            parent = w
          )
          unblockHandlers(w)

          message("Project saved as: ", ws_full_name)
        } else {
          blockHandlers(w)
          gmessage(
            msg = "The project directory was not found",
            title = "Directory not found",
            icon = "error",
            parent = w
          )
          unblockHandlers(w)
        }
      } else {
        blockHandlers(w)
        gmessage(
          msg = "A file name must be given",
          title = "File name required",
          icon = "error",
          parent = w
        )
        unblockHandlers(w)
      }
    } else {
      blockHandlers(w)
      gmessage(
        msg = "No project name or path!\nUse 'Save As' instead.",
        title = "Property not found",
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
      text = "Select a directory to save project in",
      type = "selectdir",
      filter = list("R files" = list(patterns = c("*.R", "*.Rdata"))),
      multi = FALSE
    )
    unblockHandlers(w)

    # Ask for project name.
    blockHandlers(w)
    ws_name <- ginput(
      msg = "Input project name",
      text = "",
      title = "Save as",
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
            "\nalready exist!\n\n Overwrite?"
          ),
          title = "Confirm", icon = "question", parent = w
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
            msg = paste("Project saved!\n\n", ws_full_name),
            title = "STR-validator",
            icon = "info",
            parent = w
          )
          unblockHandlers(w)
        } else {
          blockHandlers(w)
          gmessage(
            msg = "The project directory was not found",
            title = "Directory not found",
            icon = "error",
            parent = w
          )
          unblockHandlers(w)
        }
      } else {
        blockHandlers(w)
        gmessage(
          msg = "Project was not saved!",
          title = "Info",
          icon = "info",
          parent = w
        )
        unblockHandlers(w)
      }
    } else {
      blockHandlers(w)
      gmessage(
        msg = "A file name must be given",
        title = "File name required",
        icon = "error",
        parent = w
      )
      unblockHandlers(w)
    }
  })


  # DATASETS ------------------------------------------------------------------

  workspace_f2 <- gframe(
    text = "Load objects from R workspace",
    markup = FALSE,
    pos = 0,
    horizontal = TRUE,
    container = file_tab,
    expand = FALSE
  )

  workspace_f2g1 <- ggroup(
    horizontal = FALSE,
    container = workspace_f2,
    expand = FALSE
  )

  ws_r_refresh_btn <- gbutton(text = "Refresh dropdown", container = workspace_f2g1)

  ws_r_load_btn <- gbutton(text = "Load object", container = workspace_f2g1)

  ws_r_drp <- gcombobox(
    items = c(
      "<Select object>",
      listObjects(
        env = .strvalidator_env,
        obj.class = .object_classes_import
      )
    ),
    selected = 1,
    editable = FALSE,
    container = workspace_f2g1,
    ellipsize = "none"
  )

  addHandlerChanged(ws_r_refresh_btn, handler = function(h, ...) {
    .refreshWs()
  })

  addHandlerChanged(ws_r_load_btn, handler = function(h, ...) {

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

  dry_grid <- glayout(container = drylab_tab, spacing = 5)

  # VIEW ----------------------------------------------------------------------

  dry_grid[1, 1] <- dry_view_btn <- gbutton(text = "View", container = dry_grid)

  dry_grid[1, 2] <- glabel(
    text = "View a dataset.",
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

  dry_grid[2, 1] <- dry_kit_btn <- gbutton(text = "Kits", container = dry_grid)

  dry_grid[2, 2] <- glabel(
    text = "Add new kits or edit kits file.",
    container = dry_grid,
    anchor = c(-1, 0)
  )

  dry_grid[3, 1] <- dry_plot_kit_btn <- gbutton(
    text = "Plot Kit",
    container = dry_grid
  )

  dry_grid[3, 2] <- glabel(
    text = "Plot marker ranges for kits.",
    container = dry_grid,
    anchor = c(-1, 0)
  )

  dry_grid[4, 1] <- dry_bins_btn <- gbutton(
    text = "Analyse Overlap",
    container = dry_grid
  )

  dry_grid[4, 2] <- glabel(
    text = "Compare bins overlap for kits.",
    container = dry_grid,
    anchor = c(-1, 0)
  )

  dry_grid[5, 1] <- dry_ol_btn <- gbutton(text = "Analyse OL", container = dry_grid)

  dry_grid[5, 2] <- glabel(
    text = "Compare risk of getting off-ladder alleles for kits.",
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

  # EDIT  #####################################################################

  edit_grid <- glayout(container = edit_tab, spacing = 5)

  # EDIT ----------------------------------------------------------------------

  edit_grid[1, 1] <- edit_view_btn <- gbutton(
    text = "Edit",
    container = edit_grid
  )

  edit_grid[1, 2] <- glabel(
    text = "Edit a dataset.",
    container = edit_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(edit_view_btn, handler = function(h, ...) {

    # Open GUI.
    editData_gui(
      env = .strvalidator_env, savegui = .save_gui,
      edit = TRUE, debug = debug, parent = w
    )
  })

  # TRIM ----------------------------------------------------------------------

  edit_grid[2, 1] <- edit_trim_btn <- gbutton(
    text = "Trim",
    container = edit_grid
  )

  edit_grid[2, 2] <- glabel(
    text = "Trim/discard samples or columns from a dataset.",
    container = edit_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(edit_trim_btn, handler = function(h, ...) {

    # Open GUI.
    trim_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # SLIM ----------------------------------------------------------------------

  edit_grid[3, 1] <- edit_slim_btn <- gbutton(
    text = "Slim",
    container = edit_grid
  )

  edit_grid[3, 2] <- glabel(
    text = "Slim a dataset to 'long' format.",
    container = edit_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(edit_slim_btn, handler = function(h, ...) {

    # Open GUI.
    slim_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # FILTER --------------------------------------------------------------------

  edit_grid[4, 1] <- edit_filter_btn <- gbutton(
    text = "Filter",
    container = edit_grid
  )

  edit_grid[4, 2] <- glabel(
    text = "Filter a dataset using a reference set.",
    container = edit_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(edit_filter_btn, handler = function(h, ...) {
    filterProfile_gui(env = .strvalidator_env, savegui = .save_gui, parent = w)
  })

  # CROP ----------------------------------------------------------------------

  edit_grid[5, 1] <- edit_crop_btn <- gbutton(
    text = "Crop",
    container = edit_grid
  )

  edit_grid[5, 2] <- glabel(
    text = "Discard, or replace data.",
    container = edit_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(edit_crop_btn, handler = function(h, ...) {

    # Open GUI.
    cropData_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # GUESS ---------------------------------------------------------------------

  edit_grid[6, 1] <- edit_guess_btn <- gbutton(
    text = "Guess",
    container = edit_grid
  )

  edit_grid[6, 2] <- glabel(
    text = "Guess the profile from raw DNA result.",
    container = edit_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(edit_guess_btn, handler = function(h, ...) {
    guessProfile_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # DYE -----------------------------------------------------------------------

  edit_grid[7, 1] <- edit_addDye_btn <- gbutton(
    text = "Dye",
    container = edit_grid
  )

  edit_grid[7, 2] <- glabel(
    text = "Add dye information according to kit.",
    container = edit_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(edit_addDye_btn, handler = function(h, ...) {

    # Open GUI.
    addDye_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # ADD MARKER ----------------------------------------------------------------

  edit_grid[8, 1] <- edit_addMarker_btn <- gbutton(
    text = "Marker",
    container = edit_grid
  )

  edit_grid[8, 2] <- glabel(
    text = "Add missing markers to dataset.",
    container = edit_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(edit_addMarker_btn, handler = function(h, ...) {

    # Open GUI.
    addMarker_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # ADD SIZE ------------------------------------------------------------------

  edit_grid[9, 1] <- edit_addSize_btn <- gbutton(
    text = "Size",
    container = edit_grid
  )

  edit_grid[9, 2] <- glabel(
    text = "Add approximate size to alleles in a dataset.",
    container = edit_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(edit_addSize_btn, handler = function(h, ...) {

    # Open GUI.
    addSize_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # ADD DATA -------------------------------------------------------------------

  edit_grid[10, 1] <- edit_addData_btn <- gbutton(
    text = "Data",
    container = edit_grid
  )

  edit_grid[10, 2] <- glabel(
    text = "Add new information to a dataset.",
    container = edit_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(edit_addData_btn, handler = function(h, ...) {

    # Open GUI.
    addData_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # CHECK SUBSET --------------------------------------------------------------

  edit_grid[11, 1] <- edit_check_btn <- gbutton(
    text = "Check",
    container = edit_grid
  )

  edit_grid[11, 2] <- glabel(
    text = "Check the subsetting of a dataset.",
    container = edit_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(edit_check_btn, handler = function(h, ...) {

    # Open GUI.
    checkSubset_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # COMBINE -------------------------------------------------------------------

  edit_grid[12, 1] <- edit_combine_btn <- gbutton(
    text = "Combine",
    container = edit_grid
  )

  edit_grid[12, 2] <- glabel(
    text = "Combine two datasets.",
    container = edit_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(edit_combine_btn, handler = function(h, ...) {

    # Open GUI.
    combine_gui(env = .strvalidator_env, debug = debug, parent = w)
  })

  # COLUMNS -------------------------------------------------------------------

  edit_grid[13, 1] <- edit_columns_btn <- gbutton(
    text = "Columns",
    container = edit_grid
  )

  edit_grid[13, 2] <- glabel(
    text = "Perform actions on columns.",
    container = edit_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(edit_columns_btn, handler = function(h, ...) {

    # Open GUI.
    columns_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # CALCULATE HETEROZYGOUS ----------------------------------------------------

  edit_grid[14, 1] <- edit_copies_btn <- gbutton(
    text = "Copies",
    container = edit_grid
  )

  edit_grid[14, 2] <- glabel(
    text = "Calculate allele copies.",
    container = edit_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(edit_copies_btn, handler = function(h, ...) {

    # Open GUI.
    calculateCopies_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # CALCULATE H ---------------------------------------------------------------

  edit_grid[15, 1] <- edit_h_btn <- gbutton(
    text = "Height",
    container = edit_grid
  )

  edit_grid[15, 2] <- glabel(
    text = "Calculate peak height metrics.",
    container = edit_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(edit_h_btn, handler = function(h, ...) {

    # Open GUI.
    calculateHeight_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # GENERATE EPG --------------------------------------------------------------

  edit_grid[16, 1] <- edit_epg_btn <- gbutton(text = "EPG", container = edit_grid)

  edit_grid[16, 2] <- glabel(
    text = "Generate EPG like plot.",
    container = edit_grid,
    anchor = c(-1, 0)
  )

  addHandlerChanged(edit_epg_btn, handler = function(h, ...) {

    # Open GUI.
    generateEPG_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # AT  #######################################################################


  at_grid <- glayout(container = at_tab, spacing = 5)


  # VIEW ----------------------------------------------------------------------

  at_grid[1, 1] <- at_view_btn <- gbutton(text = "View", container = at_grid)

  at_grid[1, 2] <- glabel(
    text = "View a dataset.",
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
    text = "Calculate",
    container = at_grid
  )

  at_grid[3, 2] <- glabel(
    text = "Calculate analytical threshold (AT1, AT2, AT4, AT7).",
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
    text = "Calculate",
    container = at_grid
  )

  at_grid[4, 2] <- glabel(
    text = "Calculate analytical threshold (AT6).",
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

  at_grid[5, 1] <- at_plot_btn <- gbutton(text = "Plot", container = at_grid)

  at_grid[5, 2] <- glabel(
    text = "Create plots for analysed data (AT6).",
    container = at_grid
  )

  addHandlerChanged(at_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotAT_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # STUTTER  ##################################################################


  stutter_grid <- glayout(container = stutter_tab, spacing = 5)


  # VIEW ----------------------------------------------------------------------

  stutter_grid[1, 1] <- stutter_view_btn <- gbutton(
    text = "View",
    container = stutter_grid
  )

  stutter_grid[1, 2] <- glabel(
    text = "View a dataset.",
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
    text = "Calculate",
    container = stutter_grid
  )

  stutter_grid[3, 2] <- glabel(
    text = "Calculate stutters for a dataset.",
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
    text = "Plot",
    container = stutter_grid
  )

  stutter_grid[4, 2] <- glabel(
    text = "Create plots for analysed data.",
    container = stutter_grid
  )

  addHandlerChanged(stutter_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotStutter_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # SUMMARY TABLE -------------------------------------------------------------

  stutter_grid[5, 1] <- stutter_table_btn <- gbutton(
    text = "Summarize",
    container = stutter_grid
  )

  stutter_grid[5, 2] <- glabel(
    text = "Summarize stutter data in a table.",
    container = stutter_grid
  )

  addHandlerChanged(stutter_table_btn, handler = function(h, ...) {

    # Open GUI.
    tableStutter_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # BALANCE  ##################################################################


  balance_g1 <- glayout(container = balance_tab, spacing = 5)

  # VIEW ----------------------------------------------------------------------

  balance_g1[1, 1] <- balance_view_btn <- gbutton(
    text = "View",
    container = balance_g1
  )

  balance_g1[1, 2] <- glabel(
    text = "View a dataset.",
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

  balance_f2 <- gframe(
    text = "Intra-locus and inter-locus balance",
    horizontal = FALSE, container = balance_tab
  )

  balance_g2 <- glayout(container = balance_f2, spacing = 5)

  # CALCULATE -----------------------------------------------------------------

  # FUNCTION 1.
  balance_g2[1, 1] <- balance_g2_calc_1_btn <- gbutton(
    text = "Calculate",
    container = balance_g2
  )

  balance_g2[1, 2] <- glabel(
    text = "Calculate intra-locus balance (heterozygote balance).",
    container = balance_g2
  )


  addHandlerChanged(balance_g2_calc_1_btn, handler = function(h, ...) {

    # Open GUI.
    calculateHb_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # FUNCTION 2.
  balance_g2[2, 1] <- balance_g2_calc_2_btn <- gbutton(
    text = "Calculate",
    container = balance_g2
  )

  balance_g2[2, 2] <- glabel(
    text = "Calculate inter-locus balance (profile balance).",
    container = balance_g2
  )


  addHandlerChanged(balance_g2_calc_2_btn, handler = function(h, ...) {

    # Open GUI.
    calculateLb_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PLOT ----------------------------------------------------------------------

  balance_g2[3, 1] <- balance_g2_plot_btn <- gbutton(
    text = "Plot",
    container = balance_g2
  )

  balance_g2[3, 2] <- glabel(
    text = "Create plots for analysed data",
    container = balance_g2
  )

  addHandlerChanged(balance_g2_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotBalance_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY TABLE -------------------------------------------------------------

  balance_g2[4, 1] <- balance_table_btn <- gbutton(
    text = "Summarize",
    container = balance_g2
  )

  balance_g2[4, 2] <- glabel(
    text = "Calculate summary statistics for balance data.",
    container = balance_g2
  )

  addHandlerChanged(balance_table_btn, handler = function(h, ...) {

    # Open GUI.
    tableBalance_gui(env = .strvalidator_env, savegui = .save_gui, debug = debug, parent = w)
  })

  # CAPILLARY BALANCE =========================================================

  balance_f3 <- gframe(
    text = "Capillary balance",
    horizontal = FALSE, container = balance_tab
  )

  balance_g3 <- glayout(container = balance_f3, spacing = 5)


  # CALCULATE -----------------------------------------------------------------

  balance_g3[1, 1] <- balance_g3_calc_btn <- gbutton(
    text = "Calculate",
    container = balance_g3
  )

  balance_g3[1, 2] <- glabel(
    text = "Calculate capillary balance for a dataset.",
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
    text = "Plot",
    container = balance_g3
  )

  balance_g3[2, 2] <- glabel(
    text = "Create plots for analysed data",
    container = balance_g3
  )

  addHandlerChanged(balance_g3_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotCapillary_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY -------------------------------------------------------------------

  balance_g3[3, 1] <- balance_g3_tab_btn <- gbutton(
    text = "Summarize",
    container = balance_g3
  )

  balance_g3[3, 2] <- glabel(
    text = "Create summary table for analysed data",
    container = balance_g3
  )

  addHandlerChanged(balance_g3_tab_btn, handler = function(h, ...) {

    # Open GUI.
    tableCapillary_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # MARKER RATIO ==============================================================

  balance_f4 <- gframe(
    text = "Marker peak height ratio",
    horizontal = FALSE, container = balance_tab
  )

  balance_g4 <- glayout(container = balance_f4, spacing = 5)

  # CALCULATE -----------------------------------------------------------------

  balance_g4[1, 1] <- balance_g4_calc_btn <- gbutton(
    text = "Calculate",
    container = balance_g4
  )

  balance_g4[1, 2] <- glabel(
    text = "Calculate locus ratio for a dataset.",
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
    text = "Plot",
    container = balance_g4
  )

  balance_g4[2, 2] <- glabel(
    text = "Create plots for analysed data",
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


  conc_grid <- glayout(container = concordance_tab, spacing = 5)

  # VIEW ----------------------------------------------------------------------

  conc_grid[1, 1] <- conc_view_btn <- gbutton(text = "View", container = conc_grid)

  conc_grid[1, 2] <- glabel(
    text = "View a dataset.",
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
    text = "Calculate",
    container = conc_grid
  )

  conc_grid[2, 2] <- glabel(
    text = "Calculate concordance for multiple datasets.",
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


  drop_grid <- glayout(container = drop_tab, spacing = 5)

  # VIEW ----------------------------------------------------------------------

  drop_grid[1, 1] <- drop_view_btn <- gbutton(
    text = "View",
    container = drop_grid
  )

  drop_grid[1, 2] <- glabel(
    text = "View a dataset.",
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
    text = "Score",
    container = drop_grid
  )

  drop_grid[2, 2] <- glabel(
    text = "Score dropouts for a dataset.",
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
    text = "Calculate",
    container = drop_grid
  )

  drop_grid[3, 2] <- glabel(
    text = "Calculate stochastic thresholds.",
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
    text = "Model",
    container = drop_grid
  )

  drop_grid[4, 2] <- glabel(
    text = "Model and plot dropout risk",
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
    text = "Plot",
    container = drop_grid
  )

  drop_grid[5, 2] <- glabel(
    text = "Create plots for analysed data.",
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


  mix_grid <- glayout(container = mixture_tab, spacing = 5)

  # VIEW ----------------------------------------------------------------------

  mix_grid[1, 1] <- mix_view_btn <- gbutton(text = "View", container = mix_grid)

  mix_grid[1, 2] <- glabel(
    text = "View a dataset.",
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
    text = "Calculate",
    container = mix_grid
  )

  mix_grid[2, 2] <- glabel(
    text = "Calculate mixture for a dataset.",
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


  result_grid <- glayout(container = result_tab, spacing = 5)

  # VIEW ----------------------------------------------------------------------

  result_grid[1, 1] <- result_view_btn <- gbutton(
    text = "View",
    container = result_grid
  )

  result_grid[1, 2] <- glabel(
    text = "View a dataset.",
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
    text = "Result types",
    horizontal = FALSE, container = result_tab
  )

  result_g1 <- glayout(container = result_f1, spacing = 5)


  # CALCULATE -----------------------------------------------------------------

  result_g1[1, 1] <- result_g1_calc_btn <- gbutton(
    text = "Calculate",
    container = result_g1
  )

  result_g1[1, 2] <- glabel(
    text = "Calculate result types for a dataset.",
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
    text = "Plot",
    container = result_g1
  )

  result_g1[2, 2] <- glabel(
    text = "Create plots for analysed data",
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
    text = "Number of peaks",
    horizontal = FALSE, container = result_tab
  )

  result_g2 <- glayout(container = result_f2, spacing = 5)


  # CALCULATE -----------------------------------------------------------------

  result_g2[1, 1] <- result_g2_calc_btn <- gbutton(
    text = "Calculate",
    container = result_g2
  )

  result_g2[1, 2] <- glabel(
    text = "Count the number of peaks in sample.",
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
    text = "Plot",
    container = result_g2
  )

  result_g2[2, 2] <- glabel(
    text = "Create plots for analysed data",
    container = result_g2
  )

  addHandlerChanged(result_g2_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotPeaks_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # PEAK HEIGHT ===============================================================

  result_f3 <- gframe(
    text = "Peak height metrics",
    horizontal = FALSE, container = result_tab
  )

  result_g3 <- glayout(container = result_f3, spacing = 5)

  # PLOT PEAKS ----------------------------------------------------------------

  result_g3[1, 1] <- result_g3_calc_btn <- gbutton(
    text = "Calculate",
    container = result_g3
  )

  result_g3[1, 2] <- glabel(
    text = "Calculate average and total peak height",
    container = result_g3
  )

  addHandlerChanged(result_g3_calc_btn, handler = function(h, ...) {

    # Open GUI.
    calculateHeight_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })


  # DISTRIBUTIONS =============================================================

  result_f4 <- gframe(
    text = "Distributions",
    horizontal = FALSE, container = result_tab
  )

  result_g4 <- glayout(container = result_f4, spacing = 5)

  # PLOT PEAKS ----------------------------------------------------------------

  result_g4[1, 1] <- result_g4_plot_btn <- gbutton(
    text = "Plot",
    container = result_g4
  )

  result_g4[1, 2] <- glabel(
    text = "Plot distributions for analysed data",
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
    text = "Plot",
    container = result_g4
  )

  result_g4[1, 4] <- glabel(
    text = "Plot cumulative distribution for multiple groups",
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
    text = "Drop-in tools",
    horizontal = FALSE, container = result_tab
  )

  result_g5 <- glayout(container = result_f5, spacing = 5)


  # CALCULATE -----------------------------------------------------------------

  result_g5[1, 1] <- result_g5_calc_btn <- gbutton(
    text = "Calculate",
    container = result_g5
  )

  result_g5[1, 2] <- glabel(
    text = "Identify possible spikes.",
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
    text = "Filter",
    container = result_g5
  )

  result_g5[1, 4] <- glabel(
    text = "Remove spikes.",
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
    text = "Calculate",
    container = result_g5
  )

  result_g5[2, 2] <- glabel(
    text = "Identify possible artefacts.",
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
    text = "Filter",
    container = result_g5
  )

  result_g5[2, 4] <- glabel(
    text = "Remove artefacts.",
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
    text = "Plot",
    container = result_g5
  )

  result_g5[3, 2] <- glabel(
    text = "Plot contamination.",
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
    text = "Profile slope",
    horizontal = FALSE, container = result_tab
  )

  result_g6 <- glayout(container = result_f6, spacing = 5)


  # CALCULATE -----------------------------------------------------------------

  result_g6[1, 1] <- result_g6_calc_btn <- gbutton(
    text = "Calculate",
    container = result_g6
  )

  result_g6[1, 2] <- glabel(
    text = "Calculate the profile slope.",
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
    text = "Plot",
    container = result_g6
  )

  result_g6[2, 2] <- glabel(
    text = "Plot slope data.",
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


  precision_grid <- glayout(container = precision_tab, spacing = 5)

  # VIEW ----------------------------------------------------------------------

  precision_grid[1, 1] <- precision_view_btn <- gbutton(
    text = "View",
    container = precision_grid
  )

  precision_grid[1, 2] <- glabel(
    text = "View a dataset.",
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

  # PLOT RESULT TYPE ----------------------------------------------------------

  precision_grid[2, 1] <- precision_plot_btn <- gbutton(
    text = "Plot",
    container = precision_grid
  )

  precision_grid[2, 2] <- glabel(
    text = "Create plots for analysed data",
    container = precision_grid
  )

  addHandlerChanged(precision_plot_btn, handler = function(h, ...) {

    # Open GUI.
    plotPrecision_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # SUMMARY TABLE -------------------------------------------------------------

  precision_grid[3, 1] <- precision_table_btn <- gbutton(
    text = "Summarize",
    container = precision_grid
  )

  precision_grid[3, 2] <- glabel(
    text = "Summarize precision data in a table.",
    container = precision_grid,
    anchor = c(-1, 0)
  )


  addHandlerChanged(precision_table_btn, handler = function(h, ...) {

    # Open GUI.
    tablePrecision_gui(
      env = .strvalidator_env, savegui = .save_gui,
      debug = debug, parent = w
    )
  })

  # PULLUP  ###################################################################


  pull_grid <- glayout(container = pullup_tab, spacing = 5)

  # VIEW ----------------------------------------------------------------------

  pull_grid[1, 1] <- pull_view_btn <- gbutton(
    text = "View",
    container = pull_grid
  )

  pull_grid[1, 2] <- glabel(
    text = "View a dataset.",
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
    text = "Calculate",
    container = pull_grid
  )

  pull_grid[2, 2] <- glabel(
    text = "Calculate spectral pull-up (aka. bleed-through).",
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
    text = "Plot",
    container = pull_grid
  )

  pull_grid[3, 2] <- glabel(
    text = "Create plots for analysed data",
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
      if (tabName == .file_tab_name) {
        .refreshLoaded()
        .refreshWs()
      }

      if (tabName == .project_tab_name) {
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
      if (tabName == .file_tab_name) {
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

    if (!is.null(dfs)) {
      blockHandler(ws_r_drp)

      # Populate drop list.
      ws_r_drp[] <- c("<Select dataframe>", dfs)

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
        Name = "[No project found]", Date = "",
        Size = "", Id = "",
        stringsAsFactors = FALSE
      )

      message("No 'RData' files found in ", projectdir)

      # Reset description.
      svalue(proj_info_lbl) <- "Project:"
      svalue(proj_info_txt) <- "[Project description]"
    }
  }

  # SHOW GUI ##################################################################

  # Show GUI and first tab.
  svalue(nb) <- 1
  visible(w) <- TRUE
  focus(w)
  message("STR-validator graphical user interface loaded!")
} # END OF GUI
