################################################################################
# TODO LIST
# TODO: use xmin/ymin for threshold line.
# TODO: parameter perLocus.

################################################################################
# CHANGE LOG
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
#' @param debug logical indicating printing debug information.
#' 

modelDropout_gui <- function(env=parent.frame(), debug=FALSE){

  # Load dependencies.  
  require(ggplot2)
  library(gWidgets)
  options(guiToolkit="RGtk2")
  
  gData <- NULL
  gData_name <- NULL
  dropout_plot <- NULL
  separator <- .Platform$file.sep # Platform dependent path separator.
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  
  w <- gwindow(title="Plot dropout prediction", visible=FALSE)
  
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
  
  addHandlerChanged(dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_drp)
    
    if(exists(val_obj, envir=env)){

      gData <<- get(val_obj, envir=env)
      
      requiredCol <- c("Marker", "Allele", "Height", "Dropout")
    
      # Check if suitable for plot dropout...
      if(!all(requiredCol %in% colnames(gData))){
      
      gData <<- NULL
      
      message <- paste("The dataset is not a dropout table\n\n",
                       "The following columns are required:\n",
                       paste(requiredCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "info",
               parent = w) 
      
      } else {
        
        gData_name <<- val_obj
        
        # Only heterozygotes can be analysed.
        if("Heterozygous" %in% names(gData)){
          # Make sure numeric, then find min and max.
          heights <- as.numeric(gData$Height[gData$Heterozygous==1])
          svalue(e1g2_predlow_edt) <- min(heights, na.rm=TRUE)
          svalue(e1g2_predhigh_edt) <- max(heights, na.rm=TRUE)
        } else {
          # Make sure numeric, then find min and max.
          heights <- as.numeric(gData$Height)
          svalue(e1g2_predlow_edt) <- min(heights)
          svalue(e1g2_predhigh_edt) <- max(heights)
        }
        
      }
    } else {
      gData <<- NULL
      
      message <- paste("Select a dropoout dataset")
      
      gmessage(message, title="Could not find dataset",
               icon = "error",
               parent = w) 
    } 
  } )  
  
  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Plot settings",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  grid1 <- glayout(container = f1, spacing = 1)

  # Legends
  #legendModel <- "Fitted model (" # B0 + B1 will be added.
  #legendConf <- paste(conf*100,"% prediction interval")
  #legend.col <- col.line
  
  grid1[1,1] <- glabel(text="Plot title:", container=grid1)
  grid1[1,2] <- title_edt <- gedit(text="Dropout probability as a function of present-allele height",
                                   width=50,
                                   container=grid1)
  
  grid1[2,1] <- glabel(text="X title:", container=grid1)
  grid1[2,2] <- x_title_edt <- gedit(text="Peak height (RFU)",
                                     width=50,
                                     container=grid1)
  
  grid1[3,1] <- glabel(text="Y title:", container=grid1)
  grid1[3,2] <- y_title_edt <- gedit(text="Dropout probability, P(D)",
                                     width=50,
                                     container=grid1)
  
  grid1[4,1] <- f1g1_printmodel_chk <- gcheckbox(text="Print model",
                                                 checked = FALSE,
                                                 container = grid1)
  

  
  # FRAME 7 ###################################################################
  
  f7 <- gframe(text = "Plot dropout data",
               horizontal=FALSE,
               container = gv) 
  
  grid7 <- glayout(container = f7)
  
  grid7[1,1] <- plot_drop_btn <- gbutton(text="Plot dropout probability",
                                           border=TRUE,
                                           container=grid7) 
  
  
  addHandlerChanged(plot_drop_btn, handler = function(h, ...) {
    
    if(!is.null(gData)){
      enabled(plot_drop_btn) <- FALSE
      svalue(plot_drop_btn) <- "Processing..."
      .plotDrop()
      svalue(plot_drop_btn) <- "Plot dropout probability"
      enabled(plot_drop_btn) <- TRUE
    } else {
      message <- paste("Select a dropoout dataset")
      
      gmessage(message, title="Could not find dataset",
               icon = "error",
               parent = w) 
    }
    
  } )
  
  
  # FRAME 5 ###################################################################
  
  f5 <- gframe(text = "Save plot as image",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  grid5 <- glayout(container = f5)
  
  grid5[1,1] <- g5_save_btn <- gbutton(text = "Save",
                                       border=TRUE,
                                       container = grid5) 
  
  
  addHandlerChanged(g5_save_btn, handler = function(h, ...) {
    
    savePlot_gui(object=dropout_plot)
    
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
  e1g2[2,4] <- e1g2_linetypepred_drp <- gdroplist(items=linetypes,
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
  
  grid2 <- glayout(container = e2)
  
  grid2[1,1] <- e2_plotpoints_chk <- gcheckbox(text="Plot data points",
                                               checked = TRUE,
                                               container = grid2)
  grid2[1,2] <- glabel(text="Shape:", container=grid2)
  grid2[1,3] <- shape_txt <- gedit(text="18", width=4, container=grid2)
  
  grid2[1,4] <- glabel(text="Alpha:", container=grid2)
  grid2[1,5] <- alpha_txt <- gedit(text="0.6", width=4, container=grid2)
  
  grid2[1,6] <- glabel(text="Jitter (h/v):", container=grid2)
  grid2[1,7] <- jitterh_txt <- gedit(text="0", width=4, container=grid2)
  grid2[1,8] <- jitterv_txt <- gedit(text="0", width=4, container=grid2)
  
  # EXPAND 3 ##################################################################
  
  e3 <- gexpandgroup(text="Axes",
                     horizontal=FALSE,
                     container = f1)
  
  grid3 <- glayout(container = e3, spacing = 1)
  
  grid3[1,1:2] <- glabel(text="Limit Y axis (min-max)", container=grid3)
  grid3[2,1] <- y_min_txt <- gedit(text="0", width=5, container=grid3)
  grid3[2,2] <- y_max_txt <- gedit(text="1", width=5, container=grid3)
  
  grid3[3,1:2] <- glabel(text="Limit X axis (min-max)", container=grid3)
  grid3[4,1] <- x_min_txt <- gedit(text="", width=5, container=grid3)
  grid3[4,2] <- x_max_txt <- gedit(text="", width=5, container=grid3)
  
  grid3[1,3] <- glabel(text="    ", container=grid3) # Add some space.
  
  grid3[1,4] <- glabel(text="Scales:", container=grid3)
  grid3[2:4,4] <- scales_opt <- gradio(items=c("fixed","free_x","free_y","free"),
                                       selected = 2,
                                       horizontal = FALSE,
                                       container = grid3)
  enabled(scales_opt) <- FALSE # Not in use...
  
  # FRAME 4 ###################################################################
  
  e4 <- gexpandgroup(text="X labels",
                     horizontal=FALSE,
                     container = f1)
  
  grid4 <- glayout(container = e4)
  
  grid4[1,1] <- glabel(text="Text size (pts):", container=grid4)
  grid4[1,2] <- size_txt <- gedit(text="8", width=4, container=grid4)
  
  grid4[1,3] <- glabel(text="Angle:", container=grid4)
  grid4[1,4] <- angle_spb <- gspinbutton (from=0, to=360, by=1,
                                          value=0,
                                          container=grid4) 
  
  grid4[2,1] <- glabel(text="Justification (v/h):", container=grid4)
  grid4[2,2] <- vjust_spb <- gspinbutton (from=0, to=1, by=0.1,
                                          value=0.5,
                                          container=grid4)
  
  grid4[2,3] <- hjust_spb <- gspinbutton (from=0, to=1, by=0.1,
                                          value=0.5,
                                          container=grid4)
  
  
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
    val_predline <- svalue(e1g2_linetypepred_drp)
    val_predcol <- svalue(e1g2_colpred_drp)
    
    val_pred_xmin <- as.numeric(svalue(e1g2_predlow_edt))
    val_pred_xmax <- as.numeric(svalue(e1g2_predhigh_edt))
    val_title <- svalue(title_edt)
    val_xtitle <- svalue(x_title_edt)
    val_ytitle <- svalue(y_title_edt)
    val_shape <- as.numeric(svalue(shape_txt))
    val_alpha <- as.numeric(svalue(alpha_txt))
    val_jitterh <- as.numeric(svalue(jitterh_txt))
    val_jitterv <- as.numeric(svalue(jitterv_txt))
    val_ymin <- as.numeric(svalue(y_min_txt))
    val_ymax <- as.numeric(svalue(y_max_txt))
    val_xmin <- as.numeric(svalue(x_min_txt))
    val_xmax <- as.numeric(svalue(x_max_txt))
    val_angle <- as.numeric(svalue(angle_spb))
    val_vjust <- as.numeric(svalue(vjust_spb))
    val_hjust <- as.numeric(svalue(hjust_spb))
    val_size <- as.numeric(svalue(size_txt))
    val_scales <- svalue(scales_opt)
    val_data <- .modelDropout(fData=gData,conf=val_interval,val_pred_xmin,val_pred_xmax)
    val_model <- svalue(f1g1_printmodel_chk)
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
      #gData <- addDye(data=gData, kit=val_kit)
      # Sort by marker in kit
      #gData <- sortMarkers(data=gData,
      #                    kit=val_kit,
      #                    addMissingLevels = TRUE)

      
      # Plotting global dropout probability.
      gp <- ggplot(data = val_data, aes_string(y = "Prob", x="Height")) + geom_line() 
      
      if(val_points){
          gp <- gp + geom_point(data=gData, aes_string(x="Height", y="Dropout"),
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
      
      if(!is.na(val_ymin) && !is.na(val_ymax)){
        gp <- gp + ylim(val_ymin,val_ymax)
      }
      
      
      if(val_threshold){
        px <- attr(val_data,"tx")
        p <- attr(val_data,"p")
flush.console()
print(head(val_data))
print(px)
print(p)
        print("val_predcol")
        print(val_predcol)
        print("val_predline")
        print(val_predline)
        flush.console()        
        # Add threshold label.
        text <- paste("P(D)=", p, "@ T =", round(px,0), "rfu")

        thresholdLabel <- data.frame(Height = px,
                                     Prob = p,
                                     label = text)

        # Horizontal threshold line.
        gp <- gp + geom_segment(data=thresholdLabel,
                                aes_string(x = 0, y = "Prob",
                                    xend = "Height",
                                    yend = "Prob"),
                                    color = val_predcol,
                                    linetype=val_predline)
        # Vertical threshold line.
        gp <- gp + geom_segment(data=thresholdLabel,
                                aes_string(x = "Height", y = 0, 
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
      dropout_plot <<- gp
      
    } else {
      
      gmessage(message="Data frame is NULL or NA!",
               title="Error",
               icon = "error")      
      
    } 
    
  }
  
  # Show GUI.
  visible(w) <- TRUE
  
}
