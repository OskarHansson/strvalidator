################################################################################
# TODO LIST
# TODO: Issue error if (ref) not slimmed.

################################################################################
# CHANGE LOG
# 17.05.2013: listDataFrames() -> listObjects()
# 09.05.2013: .result removed, added save as group.
# 25.04.2013: First version.

#' @title Filter Profile GUI
#'
#' @description
#' \code{filterProfile_gui} is a GUI simplifying the filtering of typing data.
#'
#' @details All data not matching the reference will be discarded. Useful for
#' filtering stutters and artifacts from raw typing data.
#' @param env environment in wich to search for data frames.
#' @param debug logical indicating printing debug information.
#' 

filterProfile_gui <- function(env=parent.frame(), debug=FALSE){
  
  # Load dependencies.  
  require(gWidgets)
  options(guiToolkit="RGtk2")
  
  gData <- NULL
  gDataName <- NULL
  gRef <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  
  w <- gwindow(title="Filter profile", visible=FALSE)
  
  gv <- ggroup(horizontal=FALSE,
               spacing=8,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # FRAME 0 ###################################################################
  
  f0 <- gframe(text = "Datasets",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  g0 <- glayout(container = f0, spacing = 1)
  
  # Datasets ------------------------------------------------------------------
  
  g0[1,1] <- glabel(text="Select dataset:", container=g0)

  g0[1,2] <- dataset_drp <- gdroplist(items=c("<Select dataset>",
                                   listObjects(env=env,
                                               objClass="data.frame")), 
                           selected = 1,
                           editable = FALSE,
                           container = g0)
  
  g0[1,3] <- g0_samples_lbl <- glabel(text=" 0 samples", container=g0)
  
  addHandlerChanged(dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_drp)
    
    if(exists(val_obj, envir=env)){

      gData <<- get(val_obj, envir=env)
      requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
      
      if(!all(requiredCol %in% colnames(gData))){
  
        gData <<- NULL
        svalue(dataset_drp, index=TRUE) <- 1
        svalue(g0_samples_lbl) <- " 0 samples"
        svalue(f2_save_edt) <- ""
        
        message <- paste("The dataset is not typing data\n\n",
                         "The following columns are required:\n",
                         paste(requiredCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 
        
      } else {
  
        gDataName <<- val_obj
        
        samples <- length(unique(gData$Sample.Name))
        svalue(g0_samples_lbl) <- paste("", samples, "samples")
        svalue(f2_save_edt) <- paste(gDataName, "_filter", sep="")
        
      }
      
    } else {
      
      gData <<- NULL
      svalue(dataset_drp, index=TRUE) <- 1
      svalue(g0_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
      
    }
  } )  
  
  g0[2,1] <- glabel(text="Select reference dataset:", container=g0)
  
  g0[2,2] <- refset_drp <- gdroplist(items=c("<Select dataset>",
                                   listObjects(env=env,
                                               objClass="data.frame")), 
                           selected = 1,
                           editable = FALSE,
                           container = g0) 
  
  g0[2,3] <- g0_ref_lbl <- glabel(text=" 0 references", container=g0)
  
  addHandlerChanged(refset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(refset_drp)
    
    if(exists(val_obj, envir=env)){
      
      gRef <<- get(val_obj, envir=env)

      requiredCol <- c("Sample.Name", "Marker", "Allele")
      
      if(!all(requiredCol %in% colnames(gData))){
        
        gRef <<- NULL
        svalue(refset_drp, index=TRUE) <- 1
        svalue(g0_ref_lbl) <- " 0 references"
        
        message <- paste("The dataset is not a reference dataset\n\n",
                         "The following columns are required:\n",
                         paste(requiredCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 
        
      } else {
        
        ref <- length(unique(gRef$Sample.Name))
        svalue(g0_ref_lbl) <- paste("", ref, "references")
        
      }
      
    } else {
      
      gRef <<- NULL
      svalue(refset_drp, index=TRUE) <- 1
      svalue(g0_ref_lbl) <- " 0 references"
      
    }
    
  } )  

  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Filter settings",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 

  add_missing_loci_chk <- gcheckbox(text="Add missing loci",
                                    checked = FALSE,
                                    container = f1)
  
  keep_na_chk <- gcheckbox(text="Keep NA",
                           checked = FALSE,
                           container = f1)
  
  ignore_case_chk <- gcheckbox(text="Ignore case",
                           checked = TRUE,
                           container = f1)

  # FRAME 2 ###################################################################
  
  f2 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Name for result:", container=f2)
  
  f2_save_edt <- gedit(text="", container=f2)

  # BUTTON ####################################################################
  
  
  filter_btn <- gbutton(text="Filter profile",
                        border=TRUE,
                        container=gv)
  
  addHandlerChanged(filter_btn, handler = function(h, ...) {
    
    val_add_missing_loci <- svalue(add_missing_loci_chk)
    val_keep_na <- svalue(keep_na_chk)
    val_ignore_case <- svalue(ignore_case_chk)
    val_name <- svalue(f2_save_edt)
    
    if(!is.null(gData) & !is.null(gRef)){
      
      # Change button.
      svalue(filter_btn) <- "Processing..."
      enabled(filter_btn) <- FALSE
  
      datanew <- filterProfile(data=gData,
                               ref=gRef,
                               addMissingLoci=val_add_missing_loci,
                               keepNA=val_keep_na,
                               ignoreCase=val_ignore_case)
      
      # Save data.
      assign(val_name, datanew, envir=env)
      
      if(debug){
        print(datanew)
        print(paste("EXIT:", match.call()[[1]]))
      }
      
      # Close GUI.
      dispose(w)
    
    } else {
      
      message <- "A dataset and a reference dataset have to be selected."
      
      gmessage(message, title="Datasets not selected",
               icon = "error",
               parent = w) 
      
    }
    
  } )

  
  # Show GUI.
  visible(w) <- TRUE
  
}
