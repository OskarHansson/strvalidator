################################################################################
# TODO LIST
# TODO: change to ggplot2 perform lm and use add CI all-in-one?
# TODO: parameter perLocus.

################################################################################
# CHANGE LOG
# 08: Roxygenized.

#' @title model drop-out events
#'
#' @description
#' \code{modelDropout} model probability of dropout and plots a graph.
#'
#' @details
#' Models the probability of dropout P(D) using logistic regression
#' logit P(P;H) = ß0 + ß1*log(H), where 'H' is the peak height.
#' Produce a plot showing the model prediction, optionally with given prediction interval.
#' Parameters 'xmin', 'ymin', and 'ymax' affect the plot, 
#' and 'col.line' and 'col.conf' is the colour of the prediction line and 
#' prediction interval lines respectively.
#' 
#' @param data data frame in GeneMapper format containing at least a column 'Allele'.
#' @param plotPI logical. TRUE if prediction intervals should be plotted 
#' with the given confidence limit 'conf'.
#' @param conf numerical specifying confidence limit for the prediction interval.
#' @param xmin numerical setting for the x axis.in the plot.
#' @param xmax numerical setting for the x axis.in the plot.
#' @param ymin numerical setting for the y axis.in the plot.
#' @param ymax numerical setting for the y axis.in the plot.
#' @param col.line value setting the line color of the prediction.
#' @param col.conf value setting the line color of the prediction interval.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' print("Example will come")

modelDropout <- function(data, plotPI=TRUE, conf=0.95, xmin=NA, xmax=NA,
                         ymin=0, ymax=1, col.line=1, col.conf=3){
  
  # Legends
  legend.model <- "Fitted model (" # ß0 + ß1 will be added.
  legend.conf <- paste(conf*100,"% prediction interval")
  legend.col <- col.line
  
  # Xmin.
  if(is.na(xmin)){
    # Get xmin from dataset.
    xmin <- min(data$Height)
  } else if(xmin<min(data$Height)){
    warning("xmin is lower than the lowest value in dataset.
            Prediction will be made outside the range of the data.",
            call. = TRUE, immediate. = FALSE, domain = NULL)
  }
  
  # Xmax.
  if(is.na(xmax)){
    # Get xmax from dataset.
    xmax <- max(data$Height)
  } else if(xmax>max(data$Height)){
    warning("xmax exceeds the highest value in dataset.
            Prediction will be made outside the range of the data.",
            call. = TRUE, immediate. = FALSE, domain = NULL)
  }
  
  # Model.
  model<-glm(Dropout~log(Height), data=data.drop,family=binomial ("logit"))
  sumfit<-summary(model)
  
  # Build prediction interval and plot.
  predict.data = seq(xmin, xmax)
  y = plogis(model$coefficients[1] + model$coefficients[2] * log(predict.data))
  xy = data.frame(Height = predict.data)
  yhat = predict(model, xy, type = "link", se.fit = TRUE)
  # Calculate the number of standard deviations that 'conf*100%' of the data lies within.
  qSD <- qnorm(1-(1-conf)/2)
  upperlogit = yhat$fit + qSD * yhat$se.fit
  lowerlogit = yhat$fit - qSD * yhat$se.fit
  ucl = plogis(upperlogit)
  lcl = plogis(lowerlogit)
  
  # Plot probability.
  plot(predict.data, y, ylim = c(ymin, ymax), type = "l", lwd = 2, col=col.line,
       ylab = "Dropout probability, P(D)", yaxs="i",
       xlab = "Peak height (rfu)", las = 1)
  
  # Create legend text.
  legend.text <- paste(legend.model, "ß0=", round(model$coefficients[1],3),
                       ", ß1=", round(model$coefficients[2],3),")", sep="")
  
  # Prediction interval.
  if(plotPI){
    
    # Plot prediction intervals.
    lines(predict.data, ucl, lty = 2, lwd = 2, col=col.conf)
    lines(predict.data, lcl, lty = 2, lwd = 2, col=col.conf)
    
    # Update legend text and line color.
    legend.text <- c(legend.text, legend.conf)
    legend.col <- c(legend.col, col.conf)
    
    # Draw threshold lines.
    b0<-sumfit$coefficients[1]
    b1<-sumfit$coefficients[2]
    p<-1-conf
    px<-log(p)-log(1-p)
    rfu<-exp((px-b0)/b1)
    xv<-c(rfu,rfu,p,p)
    yv<-c(0,p,p,rfu)
    lines(x=xv,y=yv, lty = 3, lwd = 1, col=col.line)
    if(rfu>xmax){
      xpos<-0.9*xmax
    } else {
      xpos<-rfu
    }
    text(x=xpos, y=p,
         labels=paste("P(D)=", p, "@ T =", round(rfu,0), "rfu"),
         pos=3, offset = 2, cex = 0.8)
  }
  
  # Add legend.
  legend(x=xmax/1.02, y=ymax/1.02, xjust=1,
         legend=legend.text, col = legend.col,
         lty=c(1,2), lwd=2)
  
  return(NULL)
}
