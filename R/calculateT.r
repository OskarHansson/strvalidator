################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 17.07.2018: First version.

#' @title Calculate Stochastic Threshold
#'
#' @description
#' Calculates point estimates for the stochastic threshold.
#'
#' @details
#' Given a data.frame with observed values for the dependent variable
#' (column 'Dep') and explanary values (column 'Exp') point estimates
#' corresponding to a risk level of \code{p.dropout} are calculated
#' using logistic regression: \code{glm(Dep~Exp, family=binomial("logit")}.
#' A conservative estimate is calculated from the \code{pred.int}.
#' In addition the model parameters B0 (intercept) and B1 (slope),
#' Hosmer-Lemeshow test statistic (p-value), and the number of observed
#' and dropped out alleles is returned.
#'
#' @param data data.frame with dependent and explanatory values in columns named 'Dep' and 'Exp'.
#' @param log.model logical indicating if data should be log tranformed. Default=FALSE.
#' @param p.dropout numeric accepted risk to calculate point estimate for. Default=0.01.
#' @param pred.int numeric prediction interval. Default=0.95.
#' @param debug logical indicating printing debug information.
#'
#' @return vector with named parameters
#'
#' @export
#'
#' @importFrom utils str
#' @importFrom stats glm binomial fitted predict plogis qnorm
#'
#' @seealso \code{\link{calculateDropout}}, \code{\link{calculateAllT}},
#'  \code{\link{modelDropout_gui}}, \code{\link{plotDropout_gui}}


calculateT <- function(data, log.model = FALSE, p.dropout = 0.01, pred.int = 0.95, debug = FALSE) {

  # Calculate alpha for prediction interval.
  pred.int.alpha <- 1 - pred.int

  if (debug) {
    print("data")
    print(str(data))
    print("log.model")
    print(log.model)
    print("p.dropout")
    print(p.dropout)
    print("pred.int")
    print(pred.int)
    print("pred.int.alpha")
    print(pred.int.alpha)
  }

  message("Calculate point estimate for stochastic threshold.")

  # CHECK DATA ----------------------------------------------------------------

  # Check dataset columns.
  if (!any(grepl("Dep", names(data)))) {
    stop("'data' must contain a column 'Dep'.",
         call. = TRUE)
  }

  if (!any(grepl("Exp", names(data)))) {
    stop("'data' must contain a column 'Exp'.",
         call. = TRUE)
  }

  # Check logical arguments.
  if (!is.logical(log.model)) {
    stop("'log.model' must be logical.")
  }

  # Check numeric arguments.
  if (length(p.dropout) != 1) {
    stop("'p.dropout' must be of length 1.", call. = TRUE)
  }
  if (length(pred.int) != 1) {
    stop("'pred.int' must be of length 1.", call. = TRUE)
  }

  # Check numeric arguments.
  if (!is.numeric(p.dropout)) {
    stop("'p.dropout' must be numeric.", call. = TRUE)
  }
  if (!is.numeric(pred.int)) {
    stop("'pred.int' must be numeric.", call. = TRUE)
  }

  # Check numeric arguments.
  if (!0 <= p.dropout && p.dropout <= 1) {
    stop("'p.dropout' must be numeric [0,1].", call. = TRUE)
  }
  if (!0 <= pred.int && pred.int <= 1) {
    stop("'pred.int' must be numeric [0,1].", call. = TRUE)
  }

  # PREPARE -------------------------------------------------------------------

  # Check if numeric data.
  if (!is.numeric(data$Dep)) {
    message("Dependent variable converted to numeric.")
    data$Dep <- as.numeric(data$Dep)
  }

  # Check if numeric data.
  if (!is.numeric(data$Exp)) {
    message("Exploratory variable converted to numeric.")
    data$Exp <- as.numeric(data$Exp)
  }

  # Model -------------------------------------------------------------------

  # Build prediction range.
  val_pred_xmin <- min(data$Exp, na.rm = TRUE)
  val_pred_xmax <- max(data$Exp, na.rm = TRUE)
  xplot <- seq(val_pred_xmin, val_pred_xmax)
  predRange <- data.frame(Exp = xplot)

  # Get some statistics.
  n_total <- length(data$Exp)
  n_drop <- length(data$Exp[data$Dep == 1])
  n_obs <- length(data$Exp[data$Dep == 0])

  message("Observations (explanatory variable) n=", n_total)
  message("Observed values min=", val_pred_xmin, " and max=", val_pred_xmax)
  message("Observations with dropout n=", n_drop)
  message("Observations without dropout n=", n_obs)


  # Convert to log values.
  if (log.model) {
    data$Exp <- log(data$Exp)
    predRange$Exp <- log(predRange$Exp)
  }

  # Perform logistic regression on the selected column.
  dropoutModel <- glm(Dep ~ Exp, family = binomial("logit"), data = data)
  sumfit <- summary(dropoutModel)

  # Calculate model score.
  hos <- NA
  if (requireNamespace("ResourceSelection", quietly = TRUE)) {
    # p-value <0.05 rejects the model.
    hos <- ResourceSelection::hoslem.test(dropoutModel$y, fitted(dropoutModel))
  }

  # Extract model parameters.
  b0 <- sumfit$coefficients[1]
  b1 <- sumfit$coefficients[2]

  # Calculate probabilities for the prediction range.
  ypred <- predict(dropoutModel, predRange, type = "link", se.fit = TRUE)

  # Calculate the prediction interval.
  ylower <- plogis(ypred$fit - qnorm(1 - pred.int.alpha / 2) * ypred$se) # Lower confidence limit.
  yupper <- plogis(ypred$fit + qnorm(1 - pred.int.alpha / 2) * ypred$se) # Upper confidence limit.

  # Calculate conservative prediction values.
  yconservative <- plogis(ypred$fit + qnorm(1 - pred.int.alpha) * ypred$se)

  # Calculate y values for plot.
  yplot <- plogis(ypred$fit)

  # Save prediction in a dataframe.
  predictionDf <- data.frame(Exp = xplot, Prob = yplot, yupper = yupper, ylower = ylower)

  # Calculate dropout threshold T.
  if (log.model) {
    drop_py <- log(p.dropout) - log(1 - p.dropout)
    t_dropout <- exp((drop_py - b0) / b1)
  } else {
    t_dropout <- (log(p.dropout / (1 - p.dropout)) - b0) / b1
  }

  if (!log.model) {
    if (t_dropout < 0) {
      if (debug) {
        print("t_dropout < 0 -> NA")
      }
      t_dropout <- NA # Can't handle negative values.
    }
  }

  # Calculate conservative threshold at P(D).
  t_dropout_cons <- xplot[min(which(yconservative < p.dropout))]

  # Create result.
  res <- c(T = t_dropout, Tc = t_dropout_cons, p = hos$p.value, B0 = b0, B1 = b1, obs = n_obs, drop = n_drop)

  return(res)

}
