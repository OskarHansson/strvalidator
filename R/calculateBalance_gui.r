################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 17.05.2013: listDataFrames() -> listObjects()
# 09.05.2013: .result removed, added save as group.
# 18.04.2013: Added reference drop down and ref in call to calculateBalance.
# 14.04.2013: First version.

#' @title Calculate Balance
#'
#' @description
#' \code{calculateBalance_gui} is a GUI wrapper for the \code{calculateBalance}
#'  function.
#'
#' @details
#' Simplifies the use of the \code{calculateBalance} function by providing 
#' a graphical user interface.
#' 
#' @param env environment in wich to search for data frames and save result.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame in slim format.
#' 

calculateBalance_gui <- function(env=parent.frame(), debug=FALSE){
  
  # Load dependencies.  
  require("gWidgets")
  options(guiToolkit="RGtk2")

  # Variables and constants.
  gData <- NULL
  gDataName <- NULL
  gRef <- NULL
  separator <- .Platform$file.sep # Platform dependent path separator.
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # WINDOW ####################################################################
  
  if(debug){
    print("WINDOW")
  }  

  w <- gwindow(title="Calculate balance", visible=FALSE)
  
  gv <- ggroup(horizontal=FALSE,
               spacing=8,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # FRAME 0 ###################################################################
  
  if(debug){
    print("FRAME 0")
  }  
  
  f0 <- gframe(text = "Datasets",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  g0 <- glayout(container = f0, spacing = 1)

  # Dataset -------------------------------------------------------------------
  
  g0[1,1] <- glabel(text="Select dataset:", container=g0)
  
  dfs <- c("<Select a dataset>", listObjects(env=env, objClass="data.frame"))
  
  g0[1,2] <- g0_data_drp <- gdroplist(items=dfs, 
                           selected = 1,
                           editable = FALSE,
                           container = g0)
  g0[1,3] <- g0_data_samples_lbl <- glabel(text="", container=g0)
  
  addHandlerChanged(g0_data_drp, handler = function (h, ...) {
    
    val_obj <- svalue(g0_data_drp)
    
    if(exists(val_obj, envir=env)){
      
      gData <<- get(val_obj, envir=env)
      
      # Check if required columns...
      requiredCol <- c("Sample.Name", "Marker", "Dye", "Height")
      slimmed <- sum(grepl("Height",names(gData), fixed=TRUE)) == 1
      
      if(!all(requiredCol %in% colnames(gData))){
        
        gData <<- NULL
        svalue(g0_data_drp, index=TRUE) <- 1
        svalue(g0_data_samples_lbl) <- ""
        svalue(f4_save_edt) <- ""
        
        message <- paste("The following columns are required:\n",
                         paste(requiredCol, collapse ="\n"),
                         "\n\nFix the dataset in the 'EDIT' tab", sep="")
        
        gmessage(message, title="message",
                 icon = "error",
                 parent = w) 
        
      } else if (!slimmed) {
  
        gData <<- NULL
        svalue(g0_data_drp, index=TRUE) <- 1
        svalue(g0_data_samples_lbl) <- ""
        svalue(f4_save_edt) <- ""
        
        message <- paste("The dataset is too fat!\n\n",
                         "There can only be 1 'Height' column\n",
                         "Slim the dataset in the 'EDIT' tab", sep="")
        
        gmessage(message, title="message",
                 icon = "error",
                 parent = w) 
        
      }else {
        
        gDataName <<- val_obj
        
        svalue(g0_data_samples_lbl) <- paste(length(unique(gData$Sample.Name)),
                                          "samples.")
        # Get min/max peak height.
        dataMin <- min(as.numeric(gData$Height, na.rm=TRUE))
        dataMax <- max(as.numeric(gData$Height, na.rm=TRUE))
        svalue(f2_min_lbl) <- paste("(dataset min:", dataMin, ")")
        svalue(f2_max_lbl) <- paste("(dataset max:", dataMax, ")")
        
        svalue(f4_save_edt) <- paste(gDataName, "_balance", sep="")
        
      }
      
    } else {
      
      svalue(g0_data_samples_lbl) <- ""
      gData <<- NULL
      svalue(f4_save_edt) <- ""
      
    }    
  } )  

  # Reference -----------------------------------------------------------------
  
  g0[2,1] <- glabel(text="Select reference dataset:", container=g0)

  # NB! dfs defined in previous section.
  g0[2,2] <- g0_ref_drp <- gdroplist(items=dfs, 
                                   selected = 1,
                                   editable = FALSE,
                                   container = g0)
  g0[2,3] <- g0_ref_samples_lbl <- glabel(text="", container=g0)
  
  addHandlerChanged(g0_ref_drp, handler = function (h, ...) {
    
    val_obj <- svalue(g0_ref_drp)
    
    if(exists(val_obj, envir=env)){
      
      gRef <<- get(val_obj, envir=env)
      
      # Check if required columns...
      requiredCol <- c("Sample.Name", "Marker", "Allele")
      slimmed <- sum(grepl("Allele",names(gRef), fixed=TRUE)) == 1
      
      if(!all(requiredCol %in% colnames(gRef))){
        
        gRef <<- NULL
        svalue(g0_ref_drp, index=TRUE) <- 1
        svalue(g0_ref_samples_lbl) <- ""
        
        message <- paste("The dataset is missing one or more columns.\n\n",
                         "The following columns are required:\n",
                         paste(requiredCol, collapse="\n"),
                         "\n\nFix the dataset in the 'EDIT' tab", sep="")
        
        gmessage(message, title="message",
                 icon = "error",
                 parent = w) 
        
      } else if (!slimmed) {
        
        gRef <<- NULL
        svalue(g0_ref_drp, index=TRUE) <- 1
        svalue(g0_ref_samples_lbl) <- ""
        
        message <- paste("The dataset is too fat!\n\n",
                         "There can only be 1 'Allele' column\n",
                         "Slim the dataset in the 'EDIT' tab", sep="")
        
        gmessage(message, title="message",
                 icon = "error",
                 parent = w) 
        
      }else {
        
        svalue(g0_ref_samples_lbl) <- paste(length(unique(gRef$Sample.Name)),
                                          "samples.")
        
      }
      
    } else {
      
      svalue(g0_ref_samples_lbl) <- ""
      gRef <<- NULL
      
    }    
  } )  
  
  # FRAME 1 ###################################################################
  
  if(debug){
    print("FRAME 1")
  }  
  
  f1 <- gframe(text = "Settings",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  g1 <- glayout(container = f1, spacing = 1)
  
  f1_options1 <- c("Calculate balance for each sample",
                "Calculate average balance across all samples")
  
  g1[1,1] <- f1_perSample_opt <- gradio(items=f1_options1,
                             selected=1,
                             horizontal=FALSE,
                             container=g1)
  
  g1[2,1] <- glabel("", container=g1)  # Adds some space.
                                     
  f1_options2 <- c("Calculate locus balance proportional to the whole sample",
                "Normalise locus balance to the locus with the highest total peak height")
  
  g1[3,1] <- f1_lb_opt <- gradio(items=f1_options2,
                                     selected=1,
                                     horizontal=FALSE,
                                     container=g1)

  g1[4,1] <- glabel("", container=g1)  # Adds some space.

  options3 <- c("Calculate locus balance within each dye",
                "Calculate locus balance global across all dyes")
  
  g1[5,1] <- f1_perDye_opt <- gradio(items=options3,
                                     selected=1,
                                     horizontal=FALSE,
                                     container=g1)

  # FRAME 2 ###################################################################
  
  if(debug){
    print("FRAME 2")
  }  

  f2 <- gframe(text = "Trim dataset",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  g2 <- glayout(container = f2, spacing = 1)
  
  g2[1,1] <- glabel(text="Exclude peaks below:", container=g2)
  g2[1,2] <- f2_min_txt <- gedit(text="", container=g2)
  g2[1,3] <- f2_min_lbl <- glabel(text=paste("(dataset min:", NA, ")"),
                                  container=g2)
                    
  g2[2,1] <- glabel(text="Exclude peaks above:", container=g2)
  g2[2,2] <- f2_max_txt <- gedit(text="", container=g2)
  g2[2,3] <- f2_max_lbl <- glabel(text=paste("(dataset max:", NA, ")"),
                                  container=g2)
  
  # FRAME 3 ###################################################################
  
  if(debug){
    print("FRAME 3")
  }  
  
  f3 <- gframe(text = "Sample name matching",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  g3 <- glayout(container = f3, spacing = 1)
  
  f3_options <- c("Reference (e.g. 'F' match 'F', 'F1' and 'AFG')",
                "Dataset (e.g. 'F' match 'F' but not 'F1' or 'AFG')")
  
  g3[1,1] <- f3_match_opt <- gradio(items=f3_options,
                              selected=1,
                              horizontal=FALSE,
                              container=g3)
  
  g3[2,1] <- f3_ignore <- gcheckbox(text="Ignore case",
                                  checked=TRUE,
                                  container=g3)

  # FRAME 4 ###################################################################
  
  if(debug){
    print("FRAME 4")
  }  

  f4 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Name for result:", container=f4)
  
  f4_save_edt <- gedit(text="", container=f4)

  # BUTTON ####################################################################

  if(debug){
    print("BUTTON")
  }  
  
  calculate_btn <- gbutton(text="Calculate",
                      border=TRUE,
                      container=gv)
  
  addHandlerChanged(calculate_btn, handler = function(h, ...) {
    
    # Get values.
    val_perSample <- svalue(f1_perSample_opt, index=TRUE) == 1
    val_lb <- svalue(f1_lb_opt, index=TRUE)
    val_perDye <- svalue(f1_perDye_opt, index=TRUE) == 1
    val_min <- as.numeric(svalue(f2_min_txt))
    val_max <- as.numeric(svalue(f2_max_txt))
    val_ignore <- svalue(f3_ignore)
    val_match <- svalue(f3_match_opt, index=TRUE)
    val_data <- gData
    val_ref <- gRef
    
    if(debug){
      print("Read Values:")
      print("val_perSample")
      print(val_perSample)
      print("val_lb")
      print(val_lb)
      print("val_perDye")
      print(val_perDye)
      print("val_min")
      print(val_min)
      print("val_max")
      print(val_max)
      print("val_ignore")
      print(val_ignore)
    }
    
    if(!is.null(gData) & !is.null(gRef)){
      
      if(val_lb == 1){
        val_lb <- "prop"
      } else {
        val_lb <- "norm"
      }

      if(val_match == 1){
        val_match <- "ref"
      } else {
        val_match <- "data"
      }
      
      if(is.na(val_min)){
        val_min <- NULL
      }
      
      if(is.na(val_max)){
        val_max <- NULL
      }
      
      if(debug){
        print("Sent Values:")
        print("val_perSample")
        print(val_perSample)
        print("val_lb")
        print(val_lb)
        print("val_perDye")
        print(val_perDye)
        print("val_min")
        print(val_min)
        print("val_max")
        print(val_max)
        print("val_ignore")
        print(val_ignore)
        print("val_match")
        print(val_match)
      }
  
      # Change button.
      svalue(calculate_btn) <- "Processing..."
      enabled(calculate_btn) <- FALSE
      
      datanew <- calculateBalance(data=val_data,
                                  ref=val_ref,
                                  perSample=val_perSample,
                                  lb=val_lb,
                                  perDye=val_perDye,
                                  minHeight=val_min,
                                  maxHeight=val_max,
                                  ignoreCase=val_ignore,
                                  matchSource=val_match)
      
      # Save data.
      assign(".result", datanew, envir=env)
  
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
