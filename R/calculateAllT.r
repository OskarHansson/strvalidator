################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 25.07.2018: Added rm.sex option passed to .clean function, and added model message.
# 17.07.2018: First version.

#' @title Calculate Stochastic Thresholds
#'
#' @description
#' Calculates point estimates for the stochastic threshold using multiple models.
#'
#' @details
#' Expects output from \code{\link{calculateDropout}} as input.
#' The function calls \code{\link{calculateT}} repeatedly to estimate the
#' stochastic threshold using different models. The output is a data.frame
#' summarizing the result. Use the \code{\link{modelDropout_gui}} to plot
#' individual models.
#' 
#' Explanation of the result:
#' Explanatory_variable - Drop-out is the dependent variable.
#' An allele in heterozygous markers in the reference profile is chosen and
#' drop-out is scored if the other allele is not observed in the sample, i.e. 
#' below the LDT. The 'Random' method chose a random allele, while the 'LMW' 
#' and 'HMW' method chose the low and high molecular weight allele, respectively.
#' The 'Locus' method score drop-out if any of the two alleles has dropped out. 
#' As explanatory variable the peak height of the surviving allele '(Ph)', 
#' average profile peak height '(H)', the logarithm of the surviving allele
#' 'log(Ph)', and the logarithm of the average profile peak height 'log(H)'
#' is used.
#' P(dropout)=x.xx@@T - is the point estimate for corresponding to
#' the specified accepted risk of drop-out.
#' P(dropout>x.xx)<0.05@@T - is the conservative point estimate corresponding
#' to a stochastic threshold with a risk <0.05 that the actual drop-out
#' probability is >x.xx
#' Hosmer-Lemeshow_p - p-value from the Hosmer-Lemeshow test. A value <0.05
#' indicates poor fit between the model and the observations.
#' 
#' @param data output from \code{\link{calculateDropout}}.
#' @param kit character string to define the kit which is required to remove sex markers.
#' @param p.dropout numeric accepted risk of dropout at the stochastic threshold. Default=0.01.
#' @param p.conservative numeric accepted risk that the actual probability of 
#' dropout is >p.dropout at the conservative estimate. Default=0.05.
#' @param rm.sex logical default=TRUE removes sex markers defined for the given \code{kit}.
#' @param debug logical indicating printing debug information.
#' 
#' @return TRUE
#' 
#' @export
#' 
#' @importFrom utils help str head
#'  
#' @seealso \code{\link{calculateDropout}}, \code{\link{calculateT}},
#'  \code{\link{modelDropout_gui}}, \code{\link{plotDropout_gui}}


calculateAllT <- function(data, kit, p.dropout=0.01, p.conservative=0.05, rm.sex=TRUE, debug=FALSE){

  # CHECK DATA ----------------------------------------------------------------
  
  # Check dataset columns.
  if(! "MethodX" %in% names(data)){
    stop("'data' must contain a column 'MethodX'.", call. = TRUE)
  }
  
  if(! "Method1" %in% names(data)){
    stop("'data' must contain a column 'Method1'.", call. = TRUE)
  }
  
  if(! "Method2" %in% names(data)){
    stop("'data' must contain a column 'Method2'.", call. = TRUE)
  }
  
  if(! "MethodL" %in% names(data)){
    stop("'data' must contain a column 'MethodL'.", call. = TRUE)
  }
  
  if(! "Height" %in% names(data)){
    stop("'data' must contain a column 'Height'.", call. = TRUE)
  }
  
  if(! "MethodL.Ph" %in% names(data)){
    stop("'data' must contain a column 'MethodL.Ph'.", call. = TRUE)
  }
  
  if(! "H" %in% names(data)){
    stop("'data' must contain a column 'H'.", call. = TRUE)
  }
  
  # Check kit.  
  if(!is.null(kit)){
    if(is.null(nrow(getKit(kit = kit)))){
      stop("'kit' was not found in the kit definition file.")
    }
  }
  
  # Check numeric arguments.
  if(length(p.dropout) != 1){
    stop("'p.dropout' must be of length 1.", call. = TRUE)
  }
  if(length(p.conservative) != 1){
    stop("'p.conservative' must be of length 1.", call. = TRUE)
  }
  
  # Check numeric arguments.
  if(!is.numeric(p.dropout)){
    stop("'p.dropout' must be numeric.", call. = TRUE)
  }
  if(!is.numeric(p.conservative)){
    stop("'p.conservative' must be numeric.", call. = TRUE)
  }
  
  # Check numeric arguments.
  if(! 0 <= p.dropout &&  p.dropout <= 1){
    stop("'p.dropout' must be numeric [0,1].", call. = TRUE)
  }
  if(! 0 <= p.conservative &&  p.conservative <= 1){
    stop("'p.conservative' must be numeric [0,1].", call. = TRUE)
  }
  
  # FUNCTIONS -----------------------------------------------------------------
  
  # Internal function to clead data befor analysis.
  .clean <- function(data, rm.sex=TRUE){
    
    message("Model drop-out for dataset with:")
    message(paste(nrow(data), " rows.", sep = ""))
    
    # Remove homozygous loci
    if(any(data$Heterozygous == 1, na.rm = TRUE)){
      n0 <- nrow(data)
      data <- data[is.na(data$Heterozygous) | data$Heterozygous == 1, ]
      n1 <- nrow(data)
      message(paste(n1, " rows after removing ", n0 - n1, " homozygous rows.", sep = ""))
    }
    
    # Remove locus droput. NB! Only for MethodL, other methods can use data below LDT.
    # Therefore we cannot use the 'Dropout' column.
    if(any(data$Dep == 2, na.rm = TRUE)){
      n0 <- nrow(data)
      data <- data[is.na(data$Dep) | data$Dep != 2, ]
      n1 <- nrow(data)
      message(paste(n1, " rows after removing ", n0 - n1, " locus drop-out rows.", sep = ""))
    }
    
    # Remove sex markers.
    if(rm.sex){
      n0 <- nrow(data)
      sexMarkers <- getKit(kit=kit, what="Sex.Marker")
      for(m in seq(along=sexMarkers)){
        data <- data[data$Marker != sexMarkers[m], ]
      }
      n1 <- nrow(data)
      message(paste(n1, " rows after removing ", n0 - n1, " sex marker rows.", sep = ""))
    }
    
    # Remove NA Explanatory.
    if(any(is.na(data$Exp))){
      n0 <- nrow(data)
      data <- data[!is.na(data$Exp), ]
      n1 <- nrow(data)
      message(paste(n1, " rows after removing ", n0 - n1, " NA rows in explanatory column.", sep = ""))
    }
    
    # Remove NA Dependent.
    if(any(is.na(data$Dep))){
      n0 <- nrow(data)
      data <- data[!is.na(data$Dep), ]
      n1 <- nrow(data)
      message(paste(n1, " rows after removing ", n0 - n1, " NA rows in dependent column.", sep = ""))
    }
    
    message(paste(nrow(data), " rows in total for analysis.", sep = ""))
    
    if(debug){
      print(data)
    }
    
    return(data)
    
  }
  
  .store <- function(df, index, title, res){
    # Extract result and store in data.frame.
    
    df[index,1] <- title
    df[index,2] <- res["T"]
    df[index,3] <- res["Tc"]
    df[index,4] <- res["p"]
    df[index,5] <- res["B0"]
    df[index,6] <- res["B1"]
    df[index,7] <- res["obs"]
    df[index,8] <- res["drop"]
    
    if(debug){
      print(df)
    }
    
    return(df)
    
  }
  
  # PREPARE -------------------------------------------------------------------
  
  # Create headers.
  head_exp <- "Explanatory_variable"
  head_t <- paste0("P(dropout)=", p.dropout, "@T")
  head_tc <- paste0("P(dropout>", p.dropout, ")<", p.conservative, "@T")
  head_p <- "Hosmer-Lemeshow_p"
  head_b0 <- "\u03B20"
  head_b1 <- "\u03B21"
  head_obs <- "Observed"
  head_drop <- "Dropout"
  headers <- c(head_exp, head_t, head_tc, head_p, head_b0, head_b1, head_obs, head_drop)
  
  # Create pre-allocated result dataframe. 
  df <- data.frame(matrix(nrow = 16, ncol = 8))
  names(df) <- headers 

  # CALCULATE -----------------------------------------------------------------
  
  # Normal data.
  
  # Random.
  data$Dep <- data$MethodX
  data$Exp <- data$Height
  message("Calculating Random (Ph)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = FALSE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 1, title = "Random (Ph)", res = res)
  
  # LMW
  data$Dep <- data$Method1
  data$Exp <- data$Height
  message("Calculating LMW (Ph)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = FALSE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 2, title = "LMW (Ph)", res = res)
  
  # HMW
  data$Dep <- data$Method2
  data$Exp <- data$Height
  message("Calculating HMW (Ph)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = FALSE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 3, title = "HMW (Ph)", res = res)
  
  # Locus
  data$Dep <- data$MethodL
  data$Exp <- data$MethodL.Ph
  message("Calculating Locus (Ph)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = FALSE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 4, title = "Locus (Ph)", res = res)
  
  # Use average peak height.
  
  # Random.
  data$Dep <- data$MethodX
  data$Exp <- data$H
  message("Calculating Random (H)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = FALSE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 5, title = "Random (H)", res = res)
  
  # LMW
  data$Dep <- data$Method1
  data$Exp <- data$H
  message("Calculating LMW (H)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = FALSE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 6, title = "LMW (H)", res = res)
  
  # HMW
  data$Dep <- data$Method2
  data$Exp <- data$H
  message("Calculating HMW (H)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = FALSE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 7, title = "HMW (H)", res = res)
  
  # Locus
  data$Dep <- data$MethodL
  data$Exp <- data$H
  message("Calculating Locus (H)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = FALSE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 8, title = "Locus (H)", res = res)
  
  # Log data.
  
  # Random.
  data$Dep <- data$MethodX
  data$Exp <- data$Height
  message("Calculating Random log(Ph)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = TRUE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 9, title = "Random log(Ph)", res = res)
  
  # LMW
  data$Dep <- data$Method1
  data$Exp <- data$Height
  message("Calculating LMW log(Ph)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = TRUE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 10, title = "LMW log(Ph)", res = res)
  
  # HMW
  data$Dep <- data$Method2
  data$Exp <- data$Height
  message("Calculating HMW log(Ph)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = TRUE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 11, title = "HMW log(Ph)", res = res)
  
  # Locus
  data$Dep <- data$MethodL
  data$Exp <- data$MethodL.Ph
  message("Calculating Locus log(Ph)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = TRUE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 12, title = "Locus log(Ph)", res = res)
  
  # Use log average peak height.
  
  # Random.
  data$Dep <- data$MethodX
  data$Exp <- data$H
  message("Calculating Random log(H)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = TRUE,
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 13, title = "Random log(H)", res = res)
  
  # LMW
  data$Dep <- data$Method1
  data$Exp <- data$H
  message("Calculating LMW log(H)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = TRUE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 14, title = "LMW log(H)", res = res)
  
  # HMW
  data$Dep <- data$Method2
  data$Exp <- data$H
  message("Calculating HMW log(H)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = TRUE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 15, title = "HMW log(H)", res = res)
  
  # Locus
  data$Dep <- data$MethodL
  data$Exp <- data$H
  message("Calculating Locus log(H)...")
  res <- calculateT(data = .clean(data, rm.sex), log.model = TRUE, 
                    p.dropout = p.dropout, pred.int = 1 - p.conservative)
  df <- .store(df = df, index = 16, title = "Locus log(H)", res = res)
  
  
  # Round to a suitable number of digits.
  df[,2] <- round(x = as.numeric(df[,2]),digits = 0)
  df[,3] <- round(x = as.numeric(df[,3]),digits = 0)
  df[,4] <- round(x = as.numeric(df[,4]),digits = 4)
  df[,5] <- round(x = as.numeric(df[,5]),digits = 4)
  df[,6] <- round(x = as.numeric(df[,6]),digits = 4)
  df[,7] <- round(x = as.numeric(df[,7]),digits = 0)
  df[,8] <- round(x = as.numeric(df[,8]),digits = 0)
  
  return(df)
  
}  

