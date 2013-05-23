################################################################################
# TODO LIST
# TODO: .

################################################################################
# CHANGE LOG
# 17.05.2013: listDataFrames() -> listObjects()
# 09.05.2013: .result removed, added save as group.
# 27.04.2013: Added selection of dataframes from provided environment.
# <27.04.2013: First version.

#' @title Calculate Stutters
#'
#' @description
#' \code{calculateStutter_gui} is a GUI wrapper for the \code{calculateStutter}
#'  function.
#'
#' @details
#' Simplifies the use of the \code{calculateStutter} function by providing 
#' a graphical user interface to it.
#' 
#' @param env environment in wich to search for data frames and save result.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame in slim format.
#' 

calculateStutter_gui <- function(env=parent.frame(), debug=FALSE){
  
  # Load dependencies.  
  library(gWidgets)
  options(guiToolkit="RGtk2")
  
  gData <- NULL
  gDataName <- NULL
  gRef <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  w <- gwindow(title="Calculate stutter proportions", visible=FALSE)
  
  g <- ggroup(horizontal=FALSE,
              spacing=5,
              use.scrollwindow=FALSE,
              container = w,
              expand=FALSE) 
  
  
  # FRAME 0 ###################################################################
  
  f0 <- gframe(text = "Datasets",
               horizontal=FALSE,
               spacing = 5,
               container = g) 
  
  f0g0 <- glayout(container = f0, spacing = 1)
  
  # Datasets ------------------------------------------------------------------
  
  f0g0[1,1] <- glabel(text="Select dataset:", container=f0g0)
  
  f0g0[1,2] <- f0_dataset_drp <- gdroplist(items=c("<Select dataset>",
                                              listObjects(env=env,
                                                          objClass="data.frame")), 
                                      selected = 1,
                                      editable = FALSE,
                                      container = f0g0)
  
  f0g0[1,3] <- f0_samples_lbl <- glabel(text=" 0 samples", container=f0g0)
  
  addHandlerChanged(f0_dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(f0_dataset_drp)
    
    if(exists(val_obj, envir=env)){
      
      gData <<- get(val_obj, envir=env)
      requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
      
      if(!all(requiredCol %in% colnames(gData))){
        
        gData <<- NULL
        svalue(f0_dataset_drp, index=TRUE) <- 1
        svalue(f0_samples_lbl) <- " 0 samples"
        svalue(f2_save_edt) <- ""
        
        message <- paste("The following columns are required:\n",
                         paste(requiredCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 
        
      } else {
        
        gDataName <<- val_obj
        
        samples <- length(unique(gData$Sample.Name))
        svalue(f0_samples_lbl) <- paste("", samples, "samples")
        svalue(f2_save_edt) <- paste(gDataName, "_stutter", sep="")
        
      }
      
    } else {
      
      gData <<- NULL
      svalue(f0_dataset_drp, index=TRUE) <- 1
      svalue(f0_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
      
    }
  } )  
  
  f0g0[2,1] <- glabel(text="Select reference dataset:", container=f0g0)
  
  f0g0[2,2] <- f0_refset_drp <- gdroplist(items=c("<Select dataset>",
                                             listObjects(env=env,
                                                         objClass="data.frame")), 
                                     selected = 1,
                                     editable = FALSE,
                                     container = f0g0) 
  
  f0g0[2,3] <- f0_ref_lbl <- glabel(text=" 0 references", container=f0g0)
  
  addHandlerChanged(f0_refset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(f0_refset_drp)
    
    if(exists(val_obj, envir=env)){
      
      gRef <<- get(val_obj, envir=env)
      
      requiredCol <- c("Sample.Name", "Marker", "Allele")
      
      if(!all(requiredCol %in% colnames(gData))){
        
        gRef <<- NULL
        svalue(f0_refset_drp, index=TRUE) <- 1
        svalue(f0_ref_lbl) <- " 0 references"
        
        message <- paste("The dataset is not a reference dataset\n\n",
                         "The following columns are required:\n",
                         paste(requiredCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 
        
      } else {
        
        refs <- length(unique(gRef$Sample.Name))
        svalue(f0_ref_lbl) <- paste("", refs, "references")
        
      }
      
    } else {
      
      gRef <<- NULL
      svalue(f0_refset_drp, index=TRUE) <- 1
      svalue(f0_ref_lbl) <- " 0 references"
      
    }
    
  } )  

  # FRAME 1 ###################################################################
  
  f1 <- gframe("Level of interference", horizontal=FALSE, container=g)
  
  glabel(text="Calculate stutter proportions in the the following range:",
                      container=f1, anchor=c(-1 ,0))

  f1g1 <- ggroup(horizontal=TRUE,
               spacing=5,
               use.scrollwindow=FALSE,
               container = f1,
               expand=FALSE) 
  
  f1g1_range_b_spb <- gspinbutton (from = 0, to = 3, by = 1,
                            value = 1, digits = 0,
                            container = f1g1) 

  glabel(text="backward stutters to", container=f1g1, anchor=c(-1 ,0))

  f1g1_range_f_spb <- gspinbutton (from = 0, to = 3, by = 1,
                              value = 0, digits = 0,
                              container = f1g1) 
  
  glabel(text="forward stutters.", container=f1g1, anchor=c(-1 ,0))

  interference_f <- gframe("Level of interference", horizontal=FALSE, container=g)
  
  options <- c("no overlap between stutters and alleles",
               "stutter-stutter interference allowed",
               "stutter-allele interference allowed")
  
  interference_opt <- gradio(items=options,
                       selected=1,
                       horizontal=FALSE,
                       container=interference_f)
  
  
  # FRAME 2 ###################################################################
  
  f2 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = g) 
  
  glabel(text="Name for result:", container=f2)
  
  f2_save_edt <- gedit(text="", container=f2)

  # BUTTON ####################################################################

  if(debug){
    print("BUTTON")
  }  
  
  calculate_btn <- gbutton(text="Calculate",
                      border=TRUE,
                      container=g)
  
  addHandlerChanged(calculate_btn, handler = function(h, ...) {
    
    # Get values.
    val_back <- svalue(f1g1_range_b_spb)
    val_forward <- svalue(f1g1_range_f_spb)
    val_interference <- svalue(interference_opt, index=TRUE)
    val_data <- gData
    val_ref <- gRef
    val_name <- svalue(f2_save_edt)
    
    if(!is.null(gData) & !is.null(gRef)){
        
      if(debug){
        print("val_data")
        print(head(val_data))
        print("val_ref")
        print(head(val_ref))
        print("val_back")
        print(val_back)
        print("val_forward")
        print(val_forward)
        print("val_interference")
        print(val_interference)
      }
      
      # Change button.
      svalue(calculate_btn) <- "Processing..."
      enabled(calculate_btn) <- FALSE
      
      datanew <- calculateStutter(data=val_data, ref=val_ref, 
                                  back=val_back, forward=val_forward,
                                  interference=val_interference)
      
      # Save data.
      assign(val_name, datanew, envir=env)

      if(debug){
        print("datanew")
        print(head(datanew))
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
