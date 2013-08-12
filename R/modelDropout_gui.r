################################################################################
# TODO LIST
# TODO: use xmin/ymin for threshold line.
# TODO: parameter perLocus.

################################################################################
# CHANGE LOG
# 19.07.2013: Changed edit widget to spinbutton for 'shape' and 'alpha'.
# 18.07.2013: Check before overwrite object.
# 15.07.2013: Save as ggplot object to workspace instead of image.
# 15.07.2013: Added save GUI settings.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 05.06.2013: Added option to exclude gender marker and dropdown for kit.
# 04.06.2013: Fixed bug in 'missingCol'.
# 24.05.2013: Improved error message for missing columns.
# 17.05.2013: save plot moved to external function.
# 17.05.2013: listDataFrames() -> listObjects()
# 15.05.2013: Heights as character, added as.numeric.
# 12.05.2013: First version.

#' @title model and plot drop-out events
#'
#' @description
#' \code{modelDropout_gui} model probability of dropout and plots a graph.
#'
#' @details
#' Models the probability of dropout P(D) using logistic regression
#' logit P(P;H) = B0 + B1*log(H), where 'H' is the peak height.
#' Produce a plot showing the model prediction, optionally with given prediction interval.
#' Parameters 'xmin', 'ymin', and 'ymax' affect the plot, 
#' and 'col.line' and 'colConf' is the colour of the prediction line and 
#' prediction interval lines respectively.
#' 
#' @param env environment in wich to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @references
#' Peter Gill, Roberto Puch-Solis, James Curran,
#'  The low-template-DNA (stochastic) threshold-Its determination relative to
#'  risk analysis for national DNA databases,
#'  Forensic Science International: Genetics, Volume 3, Issue 2, March 2009,
#'  Pages 104-111, ISSN 1872-4973, 10.1016/j.fsigen.2008.11.009.
#' \url{http://www.sciencedirect.com/science/article/pii/S1872497308001798}

modelDropout_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE){

  # Load dependencies.  
  require(ggplot2)
  library(gWidgets)
  options(guiToolkit="RGtk2")
  
  # Global variables.
  .gData <- NULL
  .gPlot <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # Main window.
  w <- gwindow(title="Plot dropout prediction", visible=FALSE)
  
  # Handler for saving GUI state.
  addHandlerDestroy(w, handler = function (h, ...) {
    .saveSettings()
  })

  gv <- ggroup(horizontal=FALSE,
               spacing=8,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # FRAME 0 ###################################################################
  
  f0 <- gframe(text = "Dataset",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Select dataset:", container=f0)
  
  dataset_drp <- gdroplist(items=c("<Select dataset>",
                                   listObjects(env=env,
                                               objClass="data.frame")), 
                           selected = 1,
                           editable = FALSE,
                           container = f0) 
  
  glabel(text=" and the kit used:", container=f0)
  
  kit_drp <- gdroplist(items=getKit(), 
                       selected = 1,
                       editable = FALSE,
                       container = f0) 

  addHandlerChanged(dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_drp)
    
    if(exists(val_obj, envir=env, inherits = FALSE)){

      .gData <<- get(val_obj, envir=env)
      
      requiredCol <- c("Marker", "Allele", "Height", "Dropout")
    
      # Check if suitable for plot dropout...
      if(!all(requiredCol %in% colnames(.gData))){
      
        missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]
        
        message <- paste("Additional columns required:\n",
                         paste(missingCol, collapse="\n"), sep="")
        
        gmessage(message, title="message",
                 icon = "info",
                 parent = w) 
      
        # Reset components.
        .gData <<- NULL
        svalue(f5_save_edt) <- ""
        
      } else {

        # Load or change components.
        
        # Suggest name.
        svalue(f5_save_edt) <- paste(val_obj, "_ggplot", sep="")
        
        # Detect kit.
        kitIndex <- detectKit(.gData)
        # Select in dropdown.
        svalue(kit_drp, index=TRUE) <- kitIndex
        
        # Only heterozygotes can be analysed.
        if("Heterozygous" %in% names(.gData)){
          # Make sure numeric, then find min and max.
          heights <- as.numeric(.gData$Height[.gData$Heterozygous==1])
          svalue(e1g2_predlow_edt) <- min(heights, na.rm=TRUE)
          svalue(e1g2_predhigh_edt) <- max(heights, na.rm=TRUE)
        } else {
          # Make sure numeric, then find min and max.
          heights <- as.numeric(.gData$Height)
          svalue(e1g2_predlow_edt) <- min(heights)
          svalue(e1g2_predhigh_edt) <- max(heights)
        }
        
      }
    } else {

      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      
      message <- paste("Select a dropout dataset")
      
      gmessage(message, title="Could not find dataset",
               icon = "error",
               parent = w) 
    } 
  } )  
  
  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Options",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  f1g1 <- glayout(container = f1, spacing = 1)

  # Legends
  #legendModel <- "Fitted model (" # B0 + B1 will be added.
  #legendConf <- paste(conf*100,"% prediction interval")
  #legend.col <- col.line
  
  f1g1[1,1] <- glabel(text="Plot title:", container=f1g1)
  f1g1[1,2] <- f1_title_edt <- gedit(text="Dropout probability as a function of present-allele height",
                                   width=50,
                                   container=f1g1)
  
  f1g1[2,1] <- glabel(text="X title:", container=f1g1)
  f1g1[2,2] <- f1_x_title_edt <- gedit(text="Peak height (RFU)",
                                     width=50,
                                     container=f1g1)
  
  f1g1[3,1] <- glabel(text="Y title:", container=f1g1)
  f1g1[3,2] <- f1_y_title_edt <- gedit(text="Dropout probability, P(D)",
                                     width=50,
                                     container=f1g1)
  
  f1_savegui_chk <- gcheckbox(text="Save GUI settings",
                              checked=FALSE,
                              container=f1)
  
  f1_removeAM_chk <- gcheckbox(text="Exclude gender marker",
                               checked = TRUE,
                               container = f1)

  f1_printmodel_chk <- gcheckbox(text="Print model",
                                 checked = FALSE,
                                 container = f1)
  
  # FRAME 7 ###################################################################
  
  f7 <- gframe(text = "Plot dropout data",
               horizontal=FALSE,
               container = gv) 
  
  f7g1 <- glayout(container = f7)
  
  f7g1[1,1] <- f7_plot_drop_btn <- gbutton(text="Plot predicted dropout probability",
                                           border=TRUE,
                                           container=f7g1) 
  
  
  addHandlerChanged(f7_plot_drop_btn, handler = function(h, ...) {
    
    if(!is.null(.gData)){
      enabled(f7_plot_drop_btn) <- FALSE
      svalue(f7_plot_drop_btn) <- "Processing..."
      .plotDrop()
      svalue(f7_plot_drop_btn) <- "Plot dropout probability"
      enabled(f7_plot_drop_btn) <- TRUE
    } else {
      message <- paste("Select a dropoout dataset")
      
      gmessage(message, title="Could not find dataset",
               icon = "error",
               parent = w) 
    }
    
  } )
  
  
  # FRAME 5 ###################################################################
  
  f5 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Name for result:", container=f5)
  
  f5_save_edt <- gedit(text="", container=f5)

  f5_save_btn <- gbutton(text = "Save",
                         border=TRUE,
                         container = f5) 
  
  addHandlerChanged(f5_save_btn, handler = function(h, ...) {
    
    val_name <- svalue(f5_save_edt)

    # Change button.
    svalue(f5_save_btn) <- "Processing..."
    enabled(f5_save_btn) <- FALSE
    
    # Save data.
    saveObject(name=val_name, object=.gPlot, parent=w, env=env)
    
    # Change button.
    svalue(f5_save_btn) <- "Save"
    enabled(f5_save_btn) <- TRUE
    
  } )
  
  
  
  # ADVANCED OPTIONS ##########################################################

  # EXPAND 1 ##################################################################

  e1 <- gexpandgroup(text="Dropout prediction and threshold",
                     horizontal=FALSE,
                     container = f1)
  
  # GROUP 1 -------------------------------------------------------------------

  e1g1 <- glayout(container = e1)
  
  e1g1[1,1] <- e1g1_threshold_chk <- gcheckbox(text="Mark threshold @ P(!D):",
                                               checked = TRUE,
                                               container = e1g1)

  e1g1[1,3] <- e1g1_interval_spn <- gspinbutton (from=0, to=1, by=0.01,
                                                 value=0.95,
                                                 container=e1g1)

  # GROUP 2 -------------------------------------------------------------------
  
  e1g2 <- glayout(container = e1)

  e1g2[1,1] <- e1g2_prediction_chk <- gcheckbox(text="Predict P(D) between RFU:",
                                                checked = TRUE,
                                                container = e1g2)
  
  e1g2[1,2] <- e1g2_predlow_edt <- gedit(text="", width = 6,
                                         container = e1g2)
  
  e1g2[1,3] <- e1g2_predhigh_edt <- gedit(text="", width = 6,
                                          container = e1g2)
  
  linetypes <- c("blank", "solid", "dashed", "dotted", "dotdash","longdash","twodash")
  
  e1g2[2,2:3] <- glabel("Line type") 
  e1g2[2,4] <- e1g2_predlinetype_drp <- gdroplist(items=linetypes,
                                                  selected=2,
                                                  container = e1g2)
  
  e1g2[3,2:3] <- glabel("Line colour") 
  e1g2[3,4] <- e1g2_colpred_drp <- gdroplist(items=palette(),
                                             selected=2,
                                             container = e1g2)
  
  # GROUP 3 -------------------------------------------------------------------
  
  e1g3 <- glayout(container = e1)
  
  e1g3[1,1] <- e1g3_interval_chk <- gcheckbox(text="Mark prediction interval",
                                                checked = TRUE,
                                                container = e1g3)
  
  linetypes <- c("blank", "solid", "dashed", "dotted", "dotdash","longdash","twodash")
  
  e1g3[1,2] <- glabel("Alpha") 
  e1g3[1,3] <- e1g3_interval_spb <- gspinbutton (from=0, to=1, by=0.01,
                                                     value=0.25,
                                                     container=e1g3)
  
  e1g3[1,4] <- glabel("Fill colour") 
  e1g3[1,5] <- e1g3_interval_drp <- gdroplist(items=palette(),
                                             selected=2,
                                             container = e1g3)
  
  # EXPAND 2 ##################################################################
  
  e2 <- gexpandgroup(text="Data points",
                     horizontal=FALSE,
                     container = f1)
  
  e2g1 <- glayout(container = e2)
  
  e2g1[1,1] <- e2_plotpoints_chk <- gcheckbox(text="Plot data points",
                                               checked = TRUE,
                                               container = e2g1)
  e2g1[1,2] <- glabel(text="Shape:", container=e2g1)
  e2g1[1,3] <- e2g1_shape_spb <- gspinbutton(from=0, to=25,
                                         by=1, value=18,
                                         container=e2g1)
    
  e2g1[1,4] <- glabel(text="Alpha:", container=e2g1)
  e2g1[1,5] <- e2g1_alpha_spb <- gspinbutton(from=0, to=1,
                                         by=0.01, value=0.60,
                                         container=e2g1)
  
  e2g1[1,6] <- glabel(text="Jitter (h/v):", container=e2g1)
  e2g1[1,7] <- e2g1_jitterh_edt <- gedit(text="0", width=4, container=e2g1)
  e2g1[1,8] <- e2g1_jitterv_edt <- gedit(text="0", width=4, container=e2g1)
  
  # EXPAND 3 ##################################################################
  
  e3 <- gexpandgroup(text="Axes",
                     horizontal=FALSE,
                     container = f1)
  
  e3g1 <- glayout(container = e3, spacing = 1)
  
  e3g1[1,1:2] <- glabel(text="Limit Y axis (min-max)", container=e3g1)
  e3g1[2,1] <- e3_y_min_edt <- gedit(text="0", width=5, container=e3g1)
  e3g1[2,2] <- e3_y_max_edt <- gedit(text="1", width=5, container=e3g1)
  
  e3g1[3,1:2] <- glabel(text="Limit X axis (min-max)", container=e3g1)
  e3g1[4,1] <- e3_x_min_edt <- gedit(text="", width=5, container=e3g1)
  e3g1[4,2] <- e3_x_max_edt <- gedit(text="", width=5, container=e3g1)
  
  e3g1[1,3] <- glabel(text="    ", container=e3g1) # Add some space.
  
  e3g1[1,4] <- glabel(text="Scales:", container=e3g1)
  e3g1[2:4,4] <- e3_scales_opt <- gradio(items=c("fixed","free_x","free_y","free"),
                                       selected = 2,
                                       horizontal = FALSE,
                                       container = e3g1)
  enabled(e3_scales_opt) <- FALSE # Not in use...
  
  # FRAME 4 ###################################################################
  
  e4 <- gexpandgroup(text="X labels",
                     horizontal=FALSE,
                     container = f1)
  
  e4g1 <- glayout(container = e4)
  
  e4g1[1,1] <- glabel(text="Text size (pts):", container=e4g1)
  e4g1[1,2] <- e4_size_edt <- gedit(text="8", width=4, container=e4g1)
  
  e4g1[1,3] <- glabel(text="Angle:", container=e4g1)
  e4g1[1,4] <- e4_angle_spb <- gspinbutton (from=0, to=360, by=1,
                                          value=0,
                                          container=e4g1) 
  
  e4g1[2,1] <- glabel(text="Justification (v/h):", container=e4g1)
  e4g1[2,2] <- e4_vjust_spb <- gspinbutton (from=0, to=1, by=0.1,
                                          value=0.5,
                                          container=e4g1)
  
  e4g1[2,3] <- e4_hjust_spb <- gspinbutton (from=0, to=1, by=0.1,
                                          value=0.5,
                                          container=e4g1)
  
  
  # MODEL #####################################################################
  
  .modelDropout <- function(fData, conf=0.95, xmin=NA, xmax=NA){

    if(debug){
      print(paste("IN:", match.call()[[1]]))
      print("head(fData)")
      print(head(fData))
      print("conf")
      print(conf)
      print("xmin")
      print(xmin)
      print("xmax")
      print(xmax)
    }
    
    # Remove homozygous loci
    if("Heterozygous" %in% names(fData)){
      n0 <- nrow(fData)
      fData <- fData[fData$Heterozygous==1,]
      n1 <- nrow(fData)
      message(paste("Analyse",n1,"homozygous data rows out of", n0))
    }
    
    # Remove gender marker.
    if(svalue(f1_removeAM_chk)){
      n0 <- nrow(fData)
      kit <- getKit(svalue(kit_drp))
      genderMarker <- kit$locus[kit$genderMarker]
      fData <- fData[fData$Marker != genderMarker, ]
      n1 <- nrow(fData)
      message(paste("Analyse",n1,"rows (",n0-n1,"gender marker rows removed."))
    }

    # Xmin.
    if(is.na(xmin)){
      # Get xmin from dataset.
      xmin <- min(fData$Height)
    } else if(xmin < min(fData$Height, na.rm=TRUE)){
      warning("xmin is lower than the lowest value in dataset.
              Prediction will be made outside the range of the data.",
              call. = TRUE, immediate. = FALSE, domain = NULL)
    }
    
    # Xmax.
    if(is.na(xmax)){
      # Get xmax from dataset.
      xmax <- max(fData$Height)
    } else if(xmax > max(fData$Height, na.rm=TRUE)){
      warning("xmax exceeds the highest value in dataset.
              Prediction will be made outside the range of the data.",
              call. = TRUE, immediate. = FALSE, domain = NULL)
    }
    
    # Model.
    model <- glm(Dropout~log(Height), data=fData, family=binomial ("logit"))
    sumfit <- summary(model)
    
    # Build prediction interval.
    predictedData = seq(xmin, xmax)
    y = plogis(model$coefficients[1] + model$coefficients[2] * log(predictedData))
    xy = data.frame(Height = predictedData)
    yhat = predict(model, xy, type = "link", se.fit = TRUE)
    # Calculate the number of standard deviations that 'conf*100%' of the data lies within.
    qSD <- qnorm(1-(1-conf)/2)
    upperlogit <- yhat$fit + qSD * yhat$se.fit
    lowerlogit <- yhat$fit - qSD * yhat$se.fit
    ucl <- plogis(upperlogit)
    lcl <- plogis(lowerlogit)
    
    # Create legend text.
    legendModel <- paste("Fitted model for dropout ~ log(height) [\u03B20=", round(model$coefficients[1],3),
                         ", \u03B21=", round(model$coefficients[2],3),"]", sep="")

    # Save prediction in a dataframe.
    predictDf <- data.frame(Height=predictedData, Prob=y, ucl=ucl, lcl=lcl)

    # Draw threshold lines.
    b0<-sumfit$coefficients[1]
    b1<-sumfit$coefficients[2]
    p<-1-conf
    py<-log(p)-log(1-p)
    #py<-exp((px-b0)/b1)
    px<-exp((py-b0)/b1)
    
    # Add other information.
    attr(predictDf,'legend') <- legendModel
    attr(predictDf,'tx') <- px
    attr(predictDf,'p') <- p
    attr(predictDf,'xmax') <- xmax
    
    return(predictDf)

  }
  
  # FUNCTIONS #################################################################
  
  
  .plotDrop <- function(){
    
    # Get values.
    val_interval <- svalue(e1g1_interval_spn)
    val_predline <- svalue(e1g2_predlinetype_drp)
    val_predcol <- svalue(e1g2_colpred_drp)
    
    val_pred_xmin <- as.numeric(svalue(e1g2_predlow_edt))
    val_pred_xmax <- as.numeric(svalue(e1g2_predhigh_edt))
    val_title <- svalue(f1_title_edt)
    val_xtitle <- svalue(f1_x_title_edt)
    val_ytitle <- svalue(f1_y_title_edt)
    val_shape <- as.numeric(svalue(e2g1_shape_spb))
    val_alpha <- as.numeric(svalue(e2g1_alpha_spb))
    val_jitterh <- as.numeric(svalue(e2g1_jitterh_edt))
    val_jitterv <- as.numeric(svalue(e2g1_jitterv_edt))
    val_ymin <- as.numeric(svalue(e3_y_min_edt))
    val_ymax <- as.numeric(svalue(e3_y_max_edt))
    val_xmin <- as.numeric(svalue(e3_x_min_edt))
    val_xmax <- as.numeric(svalue(e3_x_max_edt))
    val_angle <- as.numeric(svalue(e4_angle_spb))
    val_vjust <- as.numeric(svalue(e4_vjust_spb))
    val_hjust <- as.numeric(svalue(e4_hjust_spb))
    val_size <- as.numeric(svalue(e4_size_edt))
    val_scales <- svalue(e3_scales_opt)
    val_data <- .modelDropout(fData=.gData,conf=val_interval,val_pred_xmin,val_pred_xmax)
    val_model <- svalue(f1_printmodel_chk)
    val_points <- svalue(e2_plotpoints_chk)
    val_threshold <- svalue(e1g1_threshold_chk)
    val_prediction <- svalue(e1g2_prediction_chk)
    val_prediction_interval <- svalue(e1g3_interval_chk)
    val_interval_col <- svalue(e1g3_interval_drp)
    val_interval_alpha <- svalue(e1g3_interval_spb)
    
    
    if(debug){
      print("val_title")
      print(val_title)
      print("val_xtitle")
      print(val_xtitle)
      print("val_ytitle")
      print(val_ytitle)
      print("val_shape")
      print(val_shape)
      print("val_alpha")
      print(val_alpha)
      print("val_ymin")
      print(val_ymin)
      print("val_ymax")
      print(val_ymax)
      print("val_angle")
      print(val_angle)
      print("val_vjust")
      print(val_vjust)
      print("val_hjust")
      print(val_hjust)
      print("val_size")
      print(val_size)
      print("str(val_data)")
      print(str(val_data))
    }
    
    
    if (!is.na(val_data) && !is.null(val_data)){
      
      
      # Call functions.
      # Add color information.
      #.gData <- addDye(data=.gData, kit=val_kit)
      # Sort by marker in kit
      #.gData <- sortMarkers(data=.gData,
      #                    kit=val_kit,
      #                    addMissingLevels = TRUE)

      
      # Plotting global dropout probability.
      gp <- ggplot(data = val_data, aes_string(y = "Prob", x="Height")) + geom_line() 
      
      if(val_points){
          gp <- gp + geom_point(data=.gData, aes_string(x="Height", y="Dropout"),
                              shape=val_shape, alpha=val_alpha, 
                              position=position_jitter(width=val_jitterh,
                                                       height=val_jitterv)) 
      }
      
      
      if(val_prediction_interval){
        gp <- gp + geom_ribbon(data = val_data,
                               aes_string(y = "Prob", ymin = "lcl", ymax = "ucl"),
                               fill = val_interval_col,
                               alpha = val_interval_alpha) 
        
      }

      # Limit y-axis.
      if(!is.na(val_ymin) && !is.na(val_ymax)){
        gp <- gp + ylim(val_ymin,val_ymax)
      }
      
      
      if(val_threshold){
        px <- attr(val_data,"tx")
        p <- attr(val_data,"p")
        if(debug){
          print("val_predcol")
          print(val_predcol)
          print("val_predline")
          print(val_predline)
        }
        # Add threshold label.
        text <- paste("P(D)=", p, "@ T =", round(px,0), "rfu")

        thresholdLabel <- data.frame(Height = px,
                                     Prob = p,
                                     label = text)

        # Horizontal threshold line.
        if(!is.na(val_xmin)){
          xtemp <- val_xmin
        } else {
          xtemp <- 0
        }
        gp <- gp + geom_segment(data=thresholdLabel,
                                aes_string(x = xtemp, y = "Prob",
                                    xend = "Height",
                                    yend = "Prob"),
                                    color = val_predcol,
                                    linetype=val_predline)
        # Vertical threshold line.
        if(!is.na(val_xmin)){
          ytemp <- val_ymin
        } else {
          ytemp <- 0
        }
        gp <- gp + geom_segment(data=thresholdLabel,
                                aes_string(x = "Height", y = ytemp, 
                                    xend = "Height",
                                    yend = "Prob"),
                                    color = val_predcol,
                                    linetype=val_predline)
        
        gp <- gp + geom_text(data = thresholdLabel,
                             aes_string(x = "Height", y = "Prob", label = "label"),
                             hjust=0, vjust=0)
        #gp <- gp + geom_text(aes(x = px, y = p, label = text))
      }
      
      if(val_model){
# TODO: fix text box etc...
#         #Example
#         Labels <- c("Alabama", "Alaska", "Arizona", "Arkansas", 
#                     "Pennsylvania + California")
#         TextFrame <- data.frame(X = 4:8, Y = 4:8, LAB = Labels)
#         TextFrame <- transform(TextFrame,
#                                w = strwidth(LAB, 'inches') + 0.25,
#                                h = strheight(LAB, 'inches') + 0.25
#         )
#         
#         ggplot(data = SampleFrame,aes(x = X, y = Y)) + 
#           geom_point(size = 20) +
#         geom_rect(data = TextFrame, aes(xmin = X - w/2, xmax = X + w/2, 
#                                         ymin = Y - h/2, ymax = Y + h/2), fill = "grey80") +
#        geom_text(data = TextFrame,aes(x = X, y = Y, label = LAB), size = 4)
        
        text <- attr(val_data,"legend")
        xmax <- attr(val_data,"xmax")
        modelLabel <- data.frame(Height = px,
                                 Prob = p,
                                 label = text,
                                 xmax= xmax)
        # Add threshold label.
#        gp <- gp + geom_rect(data=modelLabel, aes(xmin = xmax, xmax = xmax,
#                                                  y = Inf, label = label),
#                             hjust=1, vjust=1)
        gp <- gp + geom_text(data=modelLabel, aes_string(x = Inf, y = Inf, label = "label"),
                        hjust=1, vjust=1)
      }

      # Limit x-axis.
      if(!is.na(val_xmin) && !is.na(val_xmax)){
        gp <- gp + xlim(val_xmin,val_xmax)
      }
      
      gp <- gp + theme(axis.text.x=element_text(angle=val_angle,
                                                hjust=val_hjust,
                                                vjust=val_vjust,
                                                size=val_size))
      flush.console()
      gp <- gp + labs(title=val_title)
      gp <- gp + xlab(val_xtitle)
      gp <- gp + ylab(val_ytitle)
      
      print(gp)
      
      # Store in global variable.
      .gPlot <<- gp
      
    } else {
      
      gmessage(message="Data frame is NULL or NA!",
               title="Error",
               icon = "error")      
      
    } 
    
  }

  # INTERNAL FUNCTIONS ########################################################
  
  .loadSavedSettings <- function(){
    
    # First check status of save flag.
    if(!is.null(savegui)){
      svalue(f1_savegui_chk) <- savegui
      enabled(f1_savegui_chk) <- FALSE
      if(debug){
        print("Save GUI status set!")
      }  
    } else {
      # Load save flag.
      if(exists(".modelDropout_gui_savegui", envir=env, inherits = FALSE)){
        svalue(f1_savegui_chk) <- get(".modelDropout_gui_savegui", envir=env)
      }
      if(debug){
        print("Save GUI status loaded!")
      }  
    }
    if(debug){
      print(svalue(f1_savegui_chk))
    }  
    
    # Then load settings if true.
    if(svalue(f1_savegui_chk)){
      if(exists(".modelDropout_gui_title", envir=env, inherits = FALSE)){
        svalue(f1_title_edt) <- get(".modelDropout_gui_title", envir=env)
      }
      if(exists(".modelDropout_gui_x_title", envir=env, inherits = FALSE)){
        svalue(f1_x_title_edt) <- get(".modelDropout_gui_x_title", envir=env)
      }
      if(exists(".modelDropout_gui_y_title", envir=env, inherits = FALSE)){
        svalue(f1_y_title_edt) <- get(".modelDropout_gui_y_title", envir=env)
      }
      if(exists(".modelDropout_gui_remove_am", envir=env, inherits = FALSE)){
        svalue(f1_removeAM_chk) <- get(".modelDropout_gui_remove_am", envir=env)
      }
      if(exists(".modelDropout_gui_print_model", envir=env, inherits = FALSE)){
        svalue(f1_printmodel_chk) <- get(".modelDropout_gui_print_model", envir=env)
      }
      if(exists(".modelDropout_gui_mark_threshold", envir=env, inherits = FALSE)){
        svalue(e1g1_threshold_chk) <- get(".modelDropout_gui_mark_threshold", envir=env)
      }
      if(exists(".modelDropout_gui_confidence_interval", envir=env, inherits = FALSE)){
        svalue(e1g1_interval_spn) <- get(".modelDropout_gui_confidence_interval", envir=env)
      }
      if(exists(".modelDropout_gui_predict", envir=env, inherits = FALSE)){
        svalue(e1g2_prediction_chk) <- get(".modelDropout_gui_predict", envir=env)
      }
      if(exists(".modelDropout_gui_predict_low", envir=env, inherits = FALSE)){
        svalue(e1g2_predlow_edt) <- get(".modelDropout_gui_predict_low", envir=env)
      }
      if(exists(".modelDropout_gui_predict_high", envir=env, inherits = FALSE)){
        svalue(e1g2_predhigh_edt) <- get(".modelDropout_gui_predict_high", envir=env)
      }
      if(exists(".modelDropout_gui_predict_line", envir=env, inherits = FALSE)){
        svalue(e1g2_predlinetype_drp) <- get(".modelDropout_gui_predict_line", envir=env)
      }
      if(exists(".modelDropout_gui_predict_color", envir=env, inherits = FALSE)){
        svalue(e1g2_colpred_drp) <- get(".modelDropout_gui_predict_color", envir=env)
      }
      if(exists(".modelDropout_gui_mark_interval", envir=env, inherits = FALSE)){
        svalue(e1g3_interval_chk) <- get(".modelDropout_gui_mark_interval", envir=env)
      }
      if(exists(".modelDropout_gui_interval_alpha", envir=env, inherits = FALSE)){
        svalue(e1g3_interval_spb) <- get(".modelDropout_gui_interval_alpha", envir=env)
      }
      if(exists(".modelDropout_gui_interval_color", envir=env, inherits = FALSE)){
        svalue(e1g3_interval_drp) <- get(".modelDropout_gui_interval_color", envir=env)
      }
      if(exists(".modelDropout_gui_points_plot", envir=env, inherits = FALSE)){
        svalue(e2_plotpoints_chk) <- get(".modelDropout_gui_points_plot", envir=env)
      }
      if(exists(".modelDropout_gui_points_shape", envir=env, inherits = FALSE)){
        svalue(e2g1_shape_spb) <- get(".modelDropout_gui_points_shape", envir=env)
      }
      if(exists(".modelDropout_gui_points_alpha", envir=env, inherits = FALSE)){
        svalue(e2g1_alpha_spb) <- get(".modelDropout_gui_points_alpha", envir=env)
      }
      if(exists(".modelDropout_gui_points_jitterh", envir=env, inherits = FALSE)){
        svalue(e2g1_jitterh_edt) <- get(".modelDropout_gui_points_jitterh", envir=env)
      }
      if(exists(".modelDropout_gui_points_jitterv", envir=env, inherits = FALSE)){
        svalue(e2g1_jitterv_edt) <- get(".modelDropout_gui_points_jitterv", envir=env)
      }
      if(exists(".modelDropout_gui_axes_y_min", envir=env, inherits = FALSE)){
        svalue(e3_y_min_edt) <- get(".modelDropout_gui_axes_y_min", envir=env)
      }
      if(exists(".modelDropout_gui_axes_y_max", envir=env, inherits = FALSE)){
        svalue(e3_y_max_edt) <- get(".modelDropout_gui_axes_y_max", envir=env)
      }
      if(exists(".modelDropout_gui_axes_x_min", envir=env, inherits = FALSE)){
        svalue(e3_x_min_edt) <- get(".modelDropout_gui_axes_x_min", envir=env)
      }
      if(exists(".modelDropout_gui_axes_x_max", envir=env, inherits = FALSE)){
        svalue(e3_x_max_edt) <- get(".modelDropout_gui_axes_x_max", envir=env)
      }
      if(exists(".modelDropout_gui_xlabel_size", envir=env, inherits = FALSE)){
        svalue(e4_size_edt) <- get(".modelDropout_gui_xlabel_size", envir=env)
      }
      if(exists(".modelDropout_gui_xlabel_angle", envir=env, inherits = FALSE)){
        svalue(e4_angle_spb) <- get(".modelDropout_gui_xlabel_angle", envir=env)
      }
      if(exists(".modelDropout_gui_xlabel_justh", envir=env, inherits = FALSE)){
        svalue(e4_hjust_spb) <- get(".modelDropout_gui_xlabel_justh", envir=env)
      }
      if(exists(".modelDropout_gui_xlabel_justv", envir=env, inherits = FALSE)){
        svalue(e4_vjust_spb) <- get(".modelDropout_gui_xlabel_justv", envir=env)
      }
      
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(f1_savegui_chk)){
      
      assign(x=".modelDropout_gui_savegui", value=svalue(f1_savegui_chk), envir=env)
      assign(x=".modelDropout_gui_title", value=svalue(f1_title_edt), envir=env)
      assign(x=".modelDropout_gui_x_title", value=svalue(f1_x_title_edt), envir=env)
      assign(x=".modelDropout_gui_y_title", value=svalue(f1_y_title_edt), envir=env)
      assign(x=".modelDropout_gui_remove_am", value=svalue(f1_removeAM_chk), envir=env)
      assign(x=".modelDropout_gui_print_model", value=svalue(f1_printmodel_chk), envir=env)
      assign(x=".modelDropout_gui_mark_threshold", value=svalue(e1g1_threshold_chk), envir=env)
      assign(x=".modelDropout_gui_confidence_interval", value=svalue(e1g1_interval_spn), envir=env)
      assign(x=".modelDropout_gui_predict", value=svalue(e1g2_prediction_chk), envir=env)
      assign(x=".modelDropout_gui_predict_low", value=svalue(e1g2_predlow_edt), envir=env)
      assign(x=".modelDropout_gui_predict_high", value=svalue(e1g2_predhigh_edt), envir=env)
      assign(x=".modelDropout_gui_predict_line", value=svalue(e1g2_predlinetype_drp), envir=env)
      assign(x=".modelDropout_gui_predict_color", value=svalue(e1g2_colpred_drp), envir=env)
      assign(x=".modelDropout_gui_mark_interval", value=svalue(e1g3_interval_chk), envir=env)
      assign(x=".modelDropout_gui_interval_alpha", value=svalue(e1g3_interval_spb), envir=env)
      assign(x=".modelDropout_gui_interval_color", value=svalue(e1g3_interval_drp), envir=env)
      assign(x=".modelDropout_gui_points_plot", value=svalue(e2_plotpoints_chk), envir=env)
      assign(x=".modelDropout_gui_points_shape", value=svalue(e2g1_shape_spb), envir=env)
      assign(x=".modelDropout_gui_points_alpha", value=svalue(e2g1_alpha_spb), envir=env)
      assign(x=".modelDropout_gui_points_jitterh", value=svalue(e2g1_jitterh_edt), envir=env)
      assign(x=".modelDropout_gui_points_jitterv", value=svalue(e2g1_jitterv_edt), envir=env)
      assign(x=".modelDropout_gui_axes_y_min", value=svalue(e3_y_min_edt), envir=env)
      assign(x=".modelDropout_gui_axes_y_max", value=svalue(e3_y_max_edt), envir=env)
      assign(x=".modelDropout_gui_axes_x_min", value=svalue(e3_x_min_edt), envir=env)
      assign(x=".modelDropout_gui_axes_x_max", value=svalue(e3_x_max_edt), envir=env)
      assign(x=".modelDropout_gui_xlabel_size", value=svalue(e4_size_edt), envir=env)
      assign(x=".modelDropout_gui_xlabel_angle", value=svalue(e4_angle_spb), envir=env)
      assign(x=".modelDropout_gui_xlabel_justh", value=svalue(e4_hjust_spb), envir=env)
      assign(x=".modelDropout_gui_xlabel_justv", value=svalue(e4_vjust_spb), envir=env)
            
    } else { # or remove all saved values if false.
      
      if(exists(".modelDropout_gui_savegui", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_savegui", envir = env)
      }
      if(exists(".modelDropout_gui_title", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_title", envir = env)
      }
      if(exists(".modelDropout_gui_x_title", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_x_title", envir = env)
      }
      if(exists(".modelDropout_gui_y_title", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_y_title", envir = env)
      }
      if(exists(".modelDropout_gui_remove_am", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_remove_am", envir = env)
      }
      if(exists(".modelDropout_gui_print_model", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_print_model", envir = env)
      }
      if(exists(".modelDropout_gui_mark_threshold", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_mark_threshold", envir = env)
      }
      if(exists(".modelDropout_gui_confidence_interval", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_confidence_interval", envir = env)
      }
      if(exists(".modelDropout_gui_predict", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_predict", envir = env)
      }
      if(exists(".modelDropout_gui_predict_low", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_predict_low", envir = env)
      }
      if(exists(".modelDropout_gui_predict_high", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_predict_high", envir = env)
      }
      if(exists(".modelDropout_gui_predict_line", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_predict_line", envir = env)
      }
      if(exists(".modelDropout_gui_predict_color", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_predict_color", envir = env)
      }
      if(exists(".modelDropout_gui_mark_interval", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_mark_interval", envir = env)
      }
      if(exists(".modelDropout_gui_interval_alpha", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_interval_alpha", envir = env)
      }
      if(exists(".modelDropout_gui_interval_color", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_interval_color", envir = env)
      }
      if(exists(".modelDropout_gui_points_plot", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_points_plot", envir = env)
      }
      if(exists(".modelDropout_gui_points_shape", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_points_shape", envir = env)
      }
      if(exists(".modelDropout_gui_points_alpha", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_points_alpha", envir = env)
      }
      if(exists(".modelDropout_gui_points_jitterh", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_points_jitterh", envir = env)
      }
      if(exists(".modelDropout_gui_points_jitterv", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_points_jitterv", envir = env)
      }
      if(exists(".modelDropout_gui_axes_y_min", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_axes_y_min", envir = env)
      }
      if(exists(".modelDropout_gui_axes_y_max", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_axes_y_max", envir = env)
      }
      if(exists(".modelDropout_gui_axes_x_min", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_axes_x_min", envir = env)
      }
      if(exists(".modelDropout_gui_axes_x_max", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_axes_x_max", envir = env)
      }
      if(exists(".modelDropout_gui_xlabel_size", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_xlabel_size", envir = env)
      }
      if(exists(".modelDropout_gui_xlabel_angle", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_xlabel_angle", envir = env)
      }
      if(exists(".modelDropout_gui_xlabel_justh", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_xlabel_justh", envir = env)
      }
      if(exists(".modelDropout_gui_xlabel_justv", envir=env, inherits = FALSE)){
        remove(".modelDropout_gui_xlabel_justv", envir = env)
      }
      
      
      if(debug){
        print("Settings cleared!")
      }
    }
    
    if(debug){
      print("Settings saved!")
    }
    
  }
  
  # END GUI ###################################################################
  
  # Load GUI settings.
  .loadSavedSettings()
  
  # Show GUI.
  visible(w) <- TRUE
  
}
