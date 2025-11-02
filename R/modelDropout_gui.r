################################################################################
# CHANGE LOG (last 20 changes)
# 10.09.2022: Compacted the gui. Fixed narrow dropdowns. Removed destroy workaround.
# 09.07.2022: Fixed "...URLs which should use \doi (with the DOI name only)".
# 10.04.2020: Added language support.
# 20.03.2019: Fixed save object triggered when plotting if label is changed.
# 24.02.2019: Added option to use log10 y-axis scale.
# 23.02.2019: Compacted and tweaked gui for tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 25.07.2018: Added option to dump model data. Changed default P(D) to 0.01.
# 10.07.2018: Converts dependent and explanatory values to numeric if necessary.
# 18.07.2017: Fixed issue with infinite loop for the 'model' button.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed expanded 'gexpandgroup'.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 18.11.2016: Added reference.
# 16.06.2016: 'Save as' textbox expandable.
# 11.11.2015: Added importFrom ggplot2.
# 29.08.2015: Added importFrom.
# 19.08.2015: Added more information to the description.
# 18.06.2015: Rounded printed probabilities to three decimals.
# 20.03.2015: Rounded printed conservative drop-out threshold to integer.

#' @title Model And Plot Drop-out Events
#'
#' @description
#' Model the probability of drop-out and plot graphs.
#'
#' @details
#' \code{\link{calculateDropout}} score drop-out events relative to a user
#' defined LDT in four different ways:
#' (1) by reference to the low molecular weight allele (Method1),
#' (2) by reference to the high molecular weight allele (Method2),
#' (3) by reference to a random allele (MethodX), and
#' (4) by reference to the locus (MethodL).
#' Options 1-3 are recommended by the DNA commission (see reference),
#' while option 4 is included for experimental purposes.
#' Options 1-3 may discard many dropout events while option 4 catches all
#' drop-out events. On the other hand options 1-3 can score events below
#' the LDT, while option 4 cannot, making accurate predictions possible
#' below the LDT. This is also why the number of observed drop-out events
#' may differ between model plots and heatmap, scatterplot, and ecdf.
#'
#' Method X/1/2 records the peak height of the partner allele to be used as
#' the explanatory variable in the logistic regression. The locus method L also
#' do this when there has been a drop-out, if not the the mean peak height for
#' the locus is used. Peak heights for the locus method are stored in a
#' separate column.
#'
#' Using the scored drop-out events and the peak heights of the surviving
#' alleles the probability of drop-out can be modeled by logistic regression
#' as described in Appendix B in reference [1].
#' P(dropout|H) = B0 + B1*H, where 'H' is the peak height or log(peak height).
#' This produces a plot with the predicted probabilities for a range of peak heights.
#' There are options to print the model parameters, mark the stochastic
#' threshold at a specified probability of drop-out, include the underlying
#' observations, and to calculate a specified prediction interval.
#' A conservative estimate of the stochastic threshold can be calculated
#' from the prediction interval: the risk of observing a drop-out probability
#' greater than the specified threshold limit, at the conservative peak height,
#' is less than a specified value (e.g. 1-0.95=0.05). By default the gender
#' marker is excluded from the dataset used for modeling, and the peak height
#' is used as explanatory variable. The logarithm of the average peak height 'H'
#' can be used instead of the allele/locus peak height [3] (The implementation
#' of 'H' has limitations when dropout is present. See \code{\link{calculateHeight}}).
#' To evaluate the goodness of fit for the logistic regression the
#' Hosmer-Lemeshow test is used [4]. A value below 0.05 indicates a poor fit.
#' Alternatives to the logistic regression method are discussed in reference [5]
#' and [6].
#'
#' Explanation of the result:
#' Dropout - all alleles are scored according to the limit of detection threshold (LDT).
#' This is the observations and is not used for modeling.
#' Rfu - peak height of the surviving allele.
#' MethodX - a random reference allele is selected and drop-out is scored in
#' relation to the the partner allele.
#' Method1 - the low molecular weight allele is selected and drop-out is
#' scored if the high molecular weight allele is missing.
#' Method2 - the high molecular weight allele is selected and drop-out is
#' scored if the low molecular weight allele is missing.
#' MethodL - drop-out is scored per locus i.e. drop-out if any allele is missing.
#' MethodL.Ph - peak height of the surviving allele if one allele has dropped out,
#' or the average peak height if no drop-out.
#'
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @references
#' [1] Peter Gill et.al.,
#'  DNA commission of the International Society of Forensic Genetics:
#'  Recommendations on the evaluation of STR typing results that may
#'  include drop-out and/or drop-in using probabilistic methods,
#'  Forensic Science International: Genetics, Volume 6, Issue 6, December 2012,
#'  Pages 679-688, ISSN 1872-4973, 10.1016/j.fsigen.2012.06.002.
#' \doi{10.1016/j.fsigen.2012.06.002}
#' @references
#' [2] Peter Gill, Roberto Puch-Solis, James Curran,
#'  The low-template-DNA (stochastic) threshold-Its determination relative to
#'  risk analysis for national DNA databases,
#'  Forensic Science International: Genetics, Volume 3, Issue 2, March 2009,
#'  Pages 104-111, ISSN 1872-4973, 10.1016/j.fsigen.2008.11.009.
#' \doi{10.1016/j.fsigen.2008.11.009}
#' @references
#' [3] Torben Tvedebrink, Poul Svante Eriksen, Helle Smidt Mogensen, Niels Morling,
#'  Estimating the probability of allelic drop-out of STR alleles in forensic genetics,
#'  Forensic Science International: Genetics, Volume 3, Issue 4, September 2009,
#'  Pages 222-226, ISSN 1872-4973, 10.1016/j.fsigen.2009.02.002.
#'  \doi{10.1016/j.fsigen.2009.02.002}
#' @references
#' [4] H. DW Jr., S. Lemeshow, Applied Logistic Regression, John Wiley & Sons, 2004.
#' @references
#' [5] A.A. Westen, L.J.W. Grol, J. Harteveld, A.S. Matai, P. de Knijff, T. Sijen,
#'  Assessment of the stochastic threshold, back- and forward stutter filters
#'  and low template techniques for NGM,
#'  Forensic Science International: Genetetics, Volume 6, Issue 6 December 2012,
#'  Pages 708-715, ISSN 1872-4973, 10.1016/j.fsigen.2012.05.001.
#'  \doi{10.1016/j.fsigen.2012.05.001}
#' @references
#' [6] R. Puch-Solis, A.J. Kirkham, P. Gill, J. Read, S. Watson, D. Drew,
#'  Practical determination of the low template DNA threshold,
#'  Forensic Science International: Genetetics, Volume 5, Issue 5, November 2011,
#'  Pages 422-427, ISSN 1872-4973, 10.1016/j.fsigen.2010.09.001.
#'  \doi{10.1016/j.fsigen.2010.09.001}
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help str head
#' @importFrom grDevices palette
#' @importFrom stats glm binomial fitted predict plogis qnorm
#' @importFrom ggplot2 ggplot aes_string geom_line geom_point position_jitter
#'  geom_ribbon geom_segment geom_text coord_cartesian theme element_text labs
#'  xlab ylab
#'
#' @seealso \code{\link{calculateDropout}}, \code{\link{plotDropout_gui}}, \code{\link[ResourceSelection]{hoslem.test}}

modelDropout_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gPlot <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Plot dropout prediction"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dropout dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblKit <- "Kit:"
  strFrmOptions <- "Options"
  strChkOverride <- "Override automatic titles"
  strLblTitlePlot <- "Plot title:"
  strLblTitleX <- "X title:"
  strLblTitleY <- "Y title:"
  strLblRange <- "Dataset peak height range:"
  strChkLog <- "Log (Height)"
  strChkSex <- "Exclude sex markers"
  strLblNote <- "NB! Currently, the recommended methods are the first three options.\nThe fourth alternative has not been evaluated by the DNA Commission.\nSee 'Details' in 'Help' for more information."
  strLblModels <- "Model drop-out from scoring method:"
  strRadRandom <- "Relative a random allele and peak height of surviving allele"
  strRadLMW <- "Relative the low molecular weight allele and peak height of surviving allele"
  strRadHMW <- "Relative the high molecular weight allele and peak height of surviving allele"
  strRadLocus <- "Relative the locus and peak height of surviving allele, or mean locus peak height"
  strChkAverage <- "Use average peak height 'H' instead of allele/locus peak hight"
  strChkPrint <- "Print model"
  strChkDump <- "Dump model input"
  strExpThreshold <- "Drop-out prediction and threshold"
  strChkThreshold <- "Mark threshold @ P(D):"
  strLblLineType <- "Line type"
  strLblBlank <- "blank"
  strLblSolid <- "solid"
  strLblDashed <- "dashed"
  strLblDotted <- "dotted"
  strLblDotDash <- "dotdash"
  strLblLongDash <- "longdash"
  strLblTwoDash <- "twodash"
  strLblLineColour <- "Line colour"
  strChkPrintT <- "Print threshold value"
  strLblPredictionInterval <- "Prediction interval:"
  strChkPrintTcons <- "Print conservative T value"
  strChkPredictionInterval <- "Draw prediction interval:"
  strLblAlpha <- "Alpha:"
  strLblFill <- "Fill colour:"
  strExpPoints <- "Data points"
  strChkPoints <- "Plot data points"
  strLblShape <- "Shape:"
  strLblJitter <- "Jitter (h/v):"
  strExpAxes <- "Axes"
  strChkScaleLog <- "Use log10 scale at Y axis"
  strLblNbMinMax <- "NB! Must provide both min and max value."
  strLblLimitY <- "Limit Y axis (min-max)"
  strLblLimitX <- "Limit X axis (min-max)"
  strExpLabels <- "X labels"
  strLblSize <- "Text size (pts):"
  strLblAngle <- "Angle:"
  strLblJustification <- "Justification (v/h):"
  strBtnPlot <- "Plot predicted drop-out probability"
  strBtnProcessing <- "Processing..."
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnSaveObject <- "Save as object"
  strBtnSaveImage <- "Save as image"
  strBtnObjectSaved <- "Object saved"
  strLblMainTitleAverage <- "Drop-out probability as a function of average peak height"
  strLblXTitleAverage <- "Average peak height 'H', (RFU)"
  strLblMainTitleHeight <- "Drop-out probability as a function of present-allele height"
  strLblXTitleHeight <- "Peak height, (RFU)"
  strLblYTitle <- "Drop-out probability, P(D)"
  strLblLegend <- "Model parameters:"
  strLblHosmer <- "\nHosmer-Lemeshow test: p = "
  strMsgDataset <- "A dataset must be selected."
  strMsgTitleDataset <- "Dataset not selected"
  strMsgNull <- "Data frame is NULL or NA!"
  strMsgTitleError <- "Error"
  strMsgIncomplete1 <- "Dataset is ok for drop-out analysis.\nHowever, additional columns are required for this analysis:\n"
  strMsgIncomplete2 <- "\n\nPlease try modeling using another scoring method."
  strMsgTitleIncomplete <- "Incomplete dataset"

  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  if (!is.null(dtStrings)) {
    # Get language strings, use default if not found.

    strtmp <- dtStrings["strWinTitle"]$value
    strWinTitle <- ifelse(is.na(strtmp), strWinTitle, strtmp)

    strtmp <- dtStrings["strChkGui"]$value
    strChkGui <- ifelse(is.na(strtmp), strChkGui, strtmp)

    strtmp <- dtStrings["strBtnHelp"]$value
    strBtnHelp <- ifelse(is.na(strtmp), strBtnHelp, strtmp)

    strtmp <- dtStrings["strFrmDataset"]$value
    strFrmDataset <- ifelse(is.na(strtmp), strFrmDataset, strtmp)

    strtmp <- dtStrings["strLblDataset"]$value
    strLblDataset <- ifelse(is.na(strtmp), strLblDataset, strtmp)

    strtmp <- dtStrings["strDrpDataset"]$value
    strDrpDataset <- ifelse(is.na(strtmp), strDrpDataset, strtmp)

    strtmp <- dtStrings["strLblKit"]$value
    strLblKit <- ifelse(is.na(strtmp), strLblKit, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkOverride"]$value
    strChkOverride <- ifelse(is.na(strtmp), strChkOverride, strtmp)

    strtmp <- dtStrings["strLblTitlePlot"]$value
    strLblTitlePlot <- ifelse(is.na(strtmp), strLblTitlePlot, strtmp)

    strtmp <- dtStrings["strLblTitleX"]$value
    strLblTitleX <- ifelse(is.na(strtmp), strLblTitleX, strtmp)

    strtmp <- dtStrings["strLblTitleY"]$value
    strLblTitleY <- ifelse(is.na(strtmp), strLblTitleY, strtmp)

    strtmp <- dtStrings["strLblRange"]$value
    strLblRange <- ifelse(is.na(strtmp), strLblRange, strtmp)

    strtmp <- dtStrings["strChkLog"]$value
    strChkLog <- ifelse(is.na(strtmp), strChkLog, strtmp)

    strtmp <- dtStrings["strChkSex"]$value
    strChkSex <- ifelse(is.na(strtmp), strChkSex, strtmp)

    strtmp <- dtStrings["strLblNote"]$value
    strLblNote <- ifelse(is.na(strtmp), strLblNote, strtmp)

    strtmp <- dtStrings["strLblModels"]$value
    strLblModels <- ifelse(is.na(strtmp), strLblModels, strtmp)

    strtmp <- dtStrings["strRadRandom"]$value
    strRadRandom <- ifelse(is.na(strtmp), strRadRandom, strtmp)

    strtmp <- dtStrings["strRadLMW"]$value
    strRadLMW <- ifelse(is.na(strtmp), strRadLMW, strtmp)

    strtmp <- dtStrings["strRadHMW"]$value
    strRadHMW <- ifelse(is.na(strtmp), strRadHMW, strtmp)

    strtmp <- dtStrings["strRadLocus"]$value
    strRadLocus <- ifelse(is.na(strtmp), strRadLocus, strtmp)

    strtmp <- dtStrings["strChkAverage"]$value
    strChkAverage <- ifelse(is.na(strtmp), strChkAverage, strtmp)

    strtmp <- dtStrings["strChkPrint"]$value
    strChkPrint <- ifelse(is.na(strtmp), strChkPrint, strtmp)

    strtmp <- dtStrings["strChkDump"]$value
    strChkDump <- ifelse(is.na(strtmp), strChkDump, strtmp)

    strtmp <- dtStrings["strExpThreshold"]$value
    strExpThreshold <- ifelse(is.na(strtmp), strExpThreshold, strtmp)

    strtmp <- dtStrings["strChkThreshold"]$value
    strChkThreshold <- ifelse(is.na(strtmp), strChkThreshold, strtmp)

    strtmp <- dtStrings["strLblLineType"]$value
    strLblLineType <- ifelse(is.na(strtmp), strLblLineType, strtmp)

    strtmp <- dtStrings["strLblBlank"]$value
    strLblBlank <- ifelse(is.na(strtmp), strLblBlank, strtmp)

    strtmp <- dtStrings["strLblSolid"]$value
    strLblSolid <- ifelse(is.na(strtmp), strLblSolid, strtmp)

    strtmp <- dtStrings["strLblDashed"]$value
    strLblDashed <- ifelse(is.na(strtmp), strLblDashed, strtmp)

    strtmp <- dtStrings["strLblDotted"]$value
    strLblDotted <- ifelse(is.na(strtmp), strLblDotted, strtmp)

    strtmp <- dtStrings["strLblDotDash"]$value
    strLblDotDash <- ifelse(is.na(strtmp), strLblDotDash, strtmp)

    strtmp <- dtStrings["strLblLongDash"]$value
    strLblLongDash <- ifelse(is.na(strtmp), strLblLongDash, strtmp)

    strtmp <- dtStrings["strLblTwoDash"]$value
    strLblTwoDash <- ifelse(is.na(strtmp), strLblTwoDash, strtmp)

    strtmp <- dtStrings["strLblLineColour"]$value
    strLblLineColour <- ifelse(is.na(strtmp), strLblLineColour, strtmp)

    strtmp <- dtStrings["strChkPrintT"]$value
    strChkPrintT <- ifelse(is.na(strtmp), strChkPrintT, strtmp)

    strtmp <- dtStrings["strLblPredictionInterval"]$value
    strLblPredictionInterval <- ifelse(is.na(strtmp), strLblPredictionInterval, strtmp)

    strtmp <- dtStrings["strChkPrintTcons"]$value
    strChkPrintTcons <- ifelse(is.na(strtmp), strChkPrintTcons, strtmp)

    strtmp <- dtStrings["strChkPredictionInterval"]$value
    strChkPredictionInterval <- ifelse(is.na(strtmp), strChkPredictionInterval, strtmp)

    strtmp <- dtStrings["strLblAlpha"]$value
    strLblAlpha <- ifelse(is.na(strtmp), strLblAlpha, strtmp)

    strtmp <- dtStrings["strLblFill"]$value
    strLblFill <- ifelse(is.na(strtmp), strLblFill, strtmp)

    strtmp <- dtStrings["strExpPoints"]$value
    strExpPoints <- ifelse(is.na(strtmp), strExpPoints, strtmp)

    strtmp <- dtStrings["strChkPoints"]$value
    strChkPoints <- ifelse(is.na(strtmp), strChkPoints, strtmp)

    strtmp <- dtStrings["strLblShape"]$value
    strLblShape <- ifelse(is.na(strtmp), strLblShape, strtmp)

    strtmp <- dtStrings["strLblJitter"]$value
    strLblJitter <- ifelse(is.na(strtmp), strLblJitter, strtmp)

    strtmp <- dtStrings["strExpAxes"]$value
    strExpAxes <- ifelse(is.na(strtmp), strExpAxes, strtmp)

    strtmp <- dtStrings["strChkScaleLog"]$value
    strChkScaleLog <- ifelse(is.na(strtmp), strChkScaleLog, strtmp)

    strtmp <- dtStrings["strLblNbMinMax"]$value
    strLblNbMinMax <- ifelse(is.na(strtmp), strLblNbMinMax, strtmp)

    strtmp <- dtStrings["strLblLimitY"]$value
    strLblLimitY <- ifelse(is.na(strtmp), strLblLimitY, strtmp)

    strtmp <- dtStrings["strLblLimitX"]$value
    strLblLimitX <- ifelse(is.na(strtmp), strLblLimitX, strtmp)

    strtmp <- dtStrings["strExpLabels"]$value
    strExpLabels <- ifelse(is.na(strtmp), strExpLabels, strtmp)

    strtmp <- dtStrings["strLblSize"]$value
    strLblSize <- ifelse(is.na(strtmp), strLblSize, strtmp)

    strtmp <- dtStrings["strLblAngle"]$value
    strLblAngle <- ifelse(is.na(strtmp), strLblAngle, strtmp)

    strtmp <- dtStrings["strLblJustification"]$value
    strLblJustification <- ifelse(is.na(strtmp), strLblJustification, strtmp)

    strtmp <- dtStrings["strBtnPlot"]$value
    strBtnPlot <- ifelse(is.na(strtmp), strBtnPlot, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnSaveObject"]$value
    strBtnSaveObject <- ifelse(is.na(strtmp), strBtnSaveObject, strtmp)

    strtmp <- dtStrings["strBtnSaveImage"]$value
    strBtnSaveImage <- ifelse(is.na(strtmp), strBtnSaveImage, strtmp)

    strtmp <- dtStrings["strBtnObjectSaved"]$value
    strBtnObjectSaved <- ifelse(is.na(strtmp), strBtnObjectSaved, strtmp)

    strtmp <- dtStrings["strLblMainTitleAverage"]$value
    strLblMainTitleAverage <- ifelse(is.na(strtmp), strLblMainTitleAverage, strtmp)

    strtmp <- dtStrings["strLblXTitleAverage"]$value
    strLblXTitleAverage <- ifelse(is.na(strtmp), strLblXTitleAverage, strtmp)

    strtmp <- dtStrings["strLblMainTitleHeight"]$value
    strLblMainTitleHeight <- ifelse(is.na(strtmp), strLblMainTitleHeight, strtmp)

    strtmp <- dtStrings["strLblXTitleHeight"]$value
    strLblXTitleHeight <- ifelse(is.na(strtmp), strLblXTitleHeight, strtmp)

    strtmp <- dtStrings["strLblYTitle"]$value
    strLblYTitle <- ifelse(is.na(strtmp), strLblYTitle, strtmp)

    strtmp <- dtStrings["strLblLegend"]$value
    strLblLegend <- ifelse(is.na(strtmp), strLblLegend, strtmp)

    strtmp <- dtStrings["strLblHosmer"]$value
    strLblHosmer <- ifelse(is.na(strtmp), strLblHosmer, strtmp)

    strtmp <- dtStrings["strMsgDataset"]$value
    strMsgDataset <- ifelse(is.na(strtmp), strMsgDataset, strtmp)

    strtmp <- dtStrings["strMsgTitleDataset"]$value
    strMsgTitleDataset <- ifelse(is.na(strtmp), strMsgTitleDataset, strtmp)

    strtmp <- dtStrings["strMsgNull"]$value
    strMsgNull <- ifelse(is.na(strtmp), strMsgNull, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)

    strtmp <- dtStrings["strMsgIncomplete1"]$value
    strMsgIncomplete1 <- ifelse(is.na(strtmp), strMsgIncomplete1, strtmp)

    strtmp <- dtStrings["strMsgIncomplete2"]$value
    strMsgIncomplete2 <- ifelse(is.na(strtmp), strMsgIncomplete2, strtmp)

    strtmp <- dtStrings["strMsgTitleIncomplete"]$value
    strMsgTitleIncomplete <- ifelse(is.na(strtmp), strMsgTitleIncomplete, strtmp)
  }

  # WINDOW ####################################################################

  # Main window.
  w <- gwindow(title = strWinTitle, visible = FALSE)

  # Runs when window is closed.
  addHandlerUnrealize(w, handler = function(h, ...) {
    # Save GUI state.
    .saveSettings()

    # Focus on parent window.
    if (!is.null(parent)) {
      focus(parent)
    }

    # Destroy window.
    return(FALSE)
  })

  gv <- ggroup(
    horizontal = FALSE,
    spacing = 5,
    use.scrollwindow = TRUE,
    container = w,
    expand = TRUE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(text = strChkGui, checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = strBtnHelp, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {
    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  # Datasets ------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = g0)

  dataset_drp <- gcombobox(
    items = c(
      strDrpDataset,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # Kit -----------------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblKit, container = g1)

  kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Height")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)

      ph_range <- NA
      if ("Heterozygous" %in% names(.gData)) {
        # Make sure numeric, then find range for heterozygotes.
        ph_range <- range(as.numeric(.gData$Height[.gData$Heterozygous == 1 & .gData$Dropout != 2]),
          na.rm = TRUE
        )
      } else {
        # Make sure numeric, then find min and max.
        ph_range <- range(as.numeric(.gData$Height), na.rm = TRUE)
      }
      svalue(f1g2_low_lbl) <- ph_range[1]
      svalue(f1g2_high_lbl) <- ph_range[2]

      # Suggest name.
      svalue(f5_save_edt) <- paste(val_obj, "_ggplot", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex

      # Check additional required columns and enable/disable plot button.
      .checkColumns()
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  titles_chk <- gcheckbox(
    text = strChkOverride,
    checked = FALSE, container = f1
  )


  addHandlerChanged(titles_chk, handler = function(h, ...) {
    .updateGui()
  })

  titles_group <- ggroup(
    container = f1, spacing = 1, horizontal = FALSE,
    expand = TRUE, fill = TRUE
  )

  # Legends
  glabel(text = strLblTitlePlot, container = titles_group, anchor = c(-1, 0))
  title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strLblTitleX, container = titles_group, anchor = c(-1, 0))
  x_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strLblTitleY, container = titles_group, anchor = c(-1, 0))
  y_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  # Group 2.
  f1g2 <- ggroup(horizontal = TRUE, spacing = 1, container = f1)
  glabel(text = strLblRange, container = f1g2)
  f1g2_low_lbl <- glabel(text = "", width = 6, container = f1g2)
  glabel(text = "-", container = f1g2)
  f1g2_high_lbl <- glabel(text = "", width = 6, container = f1g2)
  glabel(text = " RFU", container = f1g2)


  # Other options.
  log_model <- gcheckbox(text = strChkLog, checked = FALSE, container = f1)

  f1_sex_chk <- gcheckbox(
    text = strChkSex,
    checked = TRUE,
    container = f1
  )

  glabel(
    text = strLblNote,
    anchor = c(-1, 0), container = f1
  )

  glabel(text = strLblModels, anchor = c(-1, 0), container = f1)
  f1_column_opt <- gradio(
    items = c(strRadRandom, strRadLMW, strRadHMW, strRadLocus),
    selected = 2,
    horizontal = FALSE,
    container = f1
  )

  f1_h_chk <- gcheckbox(
    text = strChkAverage,
    checked = FALSE,
    container = f1
  )

  f1_printmodel_chk <- gcheckbox(
    text = strChkPrint,
    checked = FALSE,
    container = f1
  )

  f1_dump_chk <- gcheckbox(
    text = strChkDump, checked = FALSE,
    container = f1
  )

  addHandlerChanged(f1_column_opt, handler = function(h, ...) {
    .checkColumns()
  })

  addHandlerChanged(f1_h_chk, handler = function(h, ...) {
    .checkColumns()
  })

  # BUTTON ####################################################################

  plot_drop_btn <- gbutton(
    text = strBtnPlot,
    container = gv
  )


  addHandlerChanged(plot_drop_btn, handler = function(h, ...) {
    if (!is.null(.gData)) {
      enabled(plot_drop_btn) <- FALSE

      blockHandlers(plot_drop_btn)
      svalue(plot_drop_btn) <- strBtnProcessing
      unblockHandlers(plot_drop_btn)

      .plotDrop()

      blockHandlers(plot_drop_btn)
      svalue(plot_drop_btn) <- strBtnPlot
      unblockHandlers(plot_drop_btn)

      enabled(plot_drop_btn) <- TRUE
    } else {
      gmessage(
        msg = strMsgDataset,
        title = strMsgTitleDataset,
        icon = "error",
        parent = w
      )
    }
  })


  # FRAME 5 ###################################################################

  f5 <- gframe(
    text = strFrmSave,
    horizontal = TRUE,
    spacing = 1,
    container = gv
  )

  glabel(text = strLblSave, container = f5)

  f5_save_edt <- gedit(text = "", expand = TRUE, fill = TRUE, container = f5)

  f5_save_btn <- gbutton(text = strBtnSaveObject, container = f5)

  f5_ggsave_btn <- gbutton(text = strBtnSaveImage, container = f5)

  addHandlerChanged(f5_save_btn, handler = function(h, ...) {
    val_name <- svalue(f5_save_edt)

    # Change button.
    blockHandlers(f5_save_btn)
    svalue(f5_save_btn) <- strBtnProcessing
    unblockHandlers(f5_save_btn)
    enabled(f5_save_btn) <- FALSE

    # Save data.
    saveObject(
      name = val_name, object = .gPlot,
      parent = w, env = env, debug = debug
    )

    # Change button.
    blockHandlers(f5_save_btn)
    svalue(f5_save_btn) <- strBtnObjectSaved
    unblockHandlers(f5_save_btn)
  })

  addHandlerChanged(f5_ggsave_btn, handler = function(h, ...) {
    val_name <- svalue(f5_save_edt)

    # Save data.
    ggsave_gui(
      ggplot = .gPlot, name = val_name,
      parent = w, env = env, savegui = savegui, debug = debug
    )
  })


  # ADVANCED OPTIONS ##########################################################

  # EXPAND 1 ##################################################################

  e1 <- gexpandgroup(
    text = strExpThreshold,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e1) <- FALSE

  # FRAME 1 -------------------------------------------------------------------
  # DROPOUT THRESHOLD

  e1f1 <- gframe(text = "", horizontal = FALSE, container = e1)

  # Group 2.
  e1f1g2 <- ggroup(horizontal = TRUE, spacing = 1, container = e1f1)

  e1f1_threshold_chk <- gcheckbox(
    text = strChkThreshold,
    checked = TRUE,
    container = e1f1g2
  )

  e1f1_risk_spn <- gspinbutton(
    from = 0, to = 1, by = 0.001,
    value = 0.01,
    container = e1f1g2
  )

  # Group 3.
  e1f1g3 <- ggroup(horizontal = TRUE, spacing = 1, container = e1f1)

  e1_linetypes <- c(
    strLblBlank, strLblSolid, strLblDashed,
    strLblDotted, strLblDotDash, strLblLongDash,
    strLblTwoDash
  )

  glabel(strLblLineType, container = e1f1g3)

  e1f1_t_linetype_drp <- gcombobox(
    items = e1_linetypes,
    selected = 2,
    container = e1f1g3,
    ellipsize = "none"
  )

  glabel(strLblLineColour, container = e1f1g3)

  e1f1_t_linecolor_drp <- gcombobox(
    items = palette(),
    selected = 2,
    container = e1f1g3,
    ellipsize = "none"
  )

  # Group 4.
  e1f1g4 <- ggroup(horizontal = TRUE, spacing = 1, container = e1f1)

  e1f1_print_chk <- gcheckbox(
    text = strChkPrintT,
    checked = TRUE,
    container = e1f1g4
  )

  # FRAME 2 -------------------------------------------------------------------
  # PREDICTION INTERVAL

  e1f2 <- gframe(text = "", horizontal = FALSE, container = e1)

  # Group 1.
  e1f2g1 <- ggroup(horizontal = TRUE, spacing = 1, container = e1f2)


  glabel(text = strLblPredictionInterval, container = e1f2g1)

  e1f2_conf_spn <- gspinbutton(
    from = 0, to = 1, by = 0.001,
    value = 0.950,
    container = e1f2g1
  )

  # Group 2.
  e1f2g2 <- ggroup(horizontal = TRUE, spacing = 1, container = e1f2)

  e1f2_print_interval_chk <- gcheckbox(
    text = strChkPrintTcons,
    checked = TRUE,
    container = e1f2g2
  )

  # Group 3.
  e1f2g3 <- ggroup(horizontal = TRUE, spacing = 1, container = e1f2)

  e1f2_mark_interval_chk <- gcheckbox(
    text = strChkPredictionInterval,
    checked = TRUE,
    container = e1f2g3
  )

  glabel(text = strLblAlpha, container = e1f2g3)

  e1f2_interval_spb <- gspinbutton(
    from = 0, to = 1, by = 0.01,
    value = 0.25,
    container = e1f2g3
  )

  glabel(text = strLblFill, container = e1f2g3)
  e1f2_interval_drp <- gcombobox(
    items = palette(),
    selected = 2,
    container = e1f2g3,
    ellipsize = "none"
  )

  # EXPAND 2 ##################################################################

  e2 <- gexpandgroup(
    text = strExpPoints,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e2) <- FALSE

  e2f1 <- gframe(text = "", horizontal = FALSE, container = e2)

  e2g1 <- glayout(container = e2f1)

  e2g1[1, 1] <- e2g1_plotpoints_chk <- gcheckbox(
    text = strChkPoints,
    checked = TRUE,
    container = e2g1
  )
  e2g1[1, 2] <- glabel(text = strLblShape, container = e2g1)
  e2g1[1, 3] <- e2g1_shape_spb <- gspinbutton(
    from = 0, to = 25,
    by = 1, value = 18,
    container = e2g1
  )

  e2g1[1, 4] <- glabel(text = strLblAlpha, container = e2g1)
  e2g1[1, 5] <- e2g1_alpha_spb <- gspinbutton(
    from = 0, to = 1,
    by = 0.01, value = 0.60,
    container = e2g1
  )

  e2g1[1, 6] <- glabel(text = strLblJitter, container = e2g1)
  e2g1[1, 7] <- e2g1_jitterh_edt <- gedit(text = "0", width = 4, container = e2g1)
  e2g1[1, 8] <- e2g1_jitterv_edt <- gedit(text = "0", width = 4, container = e2g1)

  # EXPAND 3 ##################################################################

  e3 <- gexpandgroup(
    text = strExpAxes,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e3) <- FALSE

  e3f1 <- gframe(text = "", horizontal = FALSE, container = e3)

  log10_y_scale_chk <- gcheckbox(
    text = strChkScaleLog,
    checked = FALSE,
    container = e3f1
  )

  glabel(
    text = strLblNbMinMax,
    anchor = c(-1, 0), container = e3f1
  )

  e3g1 <- glayout(container = e3f1, spacing = 1)
  e3g1[1, 1:2] <- glabel(text = strLblLimitY, container = e3g1)
  e3g1[2, 1] <- e3g1_y_min_edt <- gedit(text = "", width = 5, container = e3g1)
  e3g1[2, 2] <- e3g1_y_max_edt <- gedit(text = "", width = 5, container = e3g1)

  e3g1[3, 1:2] <- glabel(text = strLblLimitX, container = e3g1)
  e3g1[4, 1] <- e3g1_x_min_edt <- gedit(text = "", width = 5, container = e3g1)
  e3g1[4, 2] <- e3g1_x_max_edt <- gedit(text = "", width = 5, container = e3g1)

  # FRAME 4 ###################################################################

  e4 <- gexpandgroup(
    text = strExpLabels,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e4) <- FALSE

  e4f1 <- gframe(text = "", horizontal = FALSE, container = e4)

  e4g1 <- glayout(container = e4f1)

  e4g1[1, 1] <- glabel(text = strLblSize, container = e4g1)
  e4g1[1, 2] <- e4g1_size_edt <- gedit(text = "8", width = 4, container = e4g1)

  e4g1[1, 3] <- glabel(text = strLblAngle, container = e4g1)
  e4g1[1, 4] <- e4g1_angle_spb <- gspinbutton(
    from = 0, to = 360, by = 1,
    value = 0,
    container = e4g1
  )

  e4g1[2, 1] <- glabel(text = strLblJustification, container = e4g1)
  e4g1[2, 2] <- e4g1_vjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1,
    value = 0.5,
    container = e4g1
  )

  e4g1[2, 3] <- e4g1_hjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1,
    value = 0.5,
    container = e4g1
  )


  # FUNCTIONS #################################################################

  .plotDrop <- function() {
    logModel <- svalue(log_model)

    # Get values.
    val_p_dropout <- svalue(e1f1_risk_spn)
    val_predint <- svalue(e1f2_conf_spn)
    val_predline <- svalue(e1f1_t_linetype_drp)
    val_predcol <- svalue(e1f1_t_linecolor_drp)

    val_titles <- svalue(titles_chk)
    val_title <- svalue(title_edt)
    val_xtitle <- svalue(x_title_edt)
    val_ytitle <- svalue(y_title_edt)
    val_column <- svalue(f1_column_opt, index = TRUE)
    val_sex <- svalue(f1_sex_chk)
    val_shape <- as.numeric(svalue(e2g1_shape_spb))
    val_alpha <- as.numeric(svalue(e2g1_alpha_spb))
    val_jitterh <- as.numeric(svalue(e2g1_jitterh_edt))
    val_jitterv <- as.numeric(svalue(e2g1_jitterv_edt))
    val_log10 <- svalue(log10_y_scale_chk)
    val_xmin <- as.numeric(svalue(e3g1_x_min_edt))
    val_xmax <- as.numeric(svalue(e3g1_x_max_edt))
    val_ymin <- as.numeric(svalue(e3g1_y_min_edt))
    val_ymax <- as.numeric(svalue(e3g1_y_max_edt))
    val_angle <- as.numeric(svalue(e4g1_angle_spb))
    val_vjust <- as.numeric(svalue(e4g1_vjust_spb))
    val_hjust <- as.numeric(svalue(e4g1_hjust_spb))
    val_size <- as.numeric(svalue(e4g1_size_edt))
    val_model <- svalue(f1_printmodel_chk)
    val_points <- svalue(e2g1_plotpoints_chk)
    val_threshold <- svalue(e1f1_threshold_chk)
    val_threshold_print <- svalue(e1f1_print_chk)
    val_prediction_interval <- svalue(e1f2_mark_interval_chk)
    val_prediction_print <- svalue(e1f2_print_interval_chk)
    val_interval_col <- svalue(e1f2_interval_drp)
    val_interval_alpha <- svalue(e1f2_interval_spb)
    val_h <- svalue(f1_h_chk)
    val_dump <- svalue(f1_dump_chk)

    # Calculate values.
    val_pi_alpha <- 1 - val_predint

    if (debug) {
      print("val_title")
      print(val_title)
      print("val_xtitle")
      print(val_xtitle)
      print("val_ytitle")
      print(val_ytitle)
      print("val_column")
      print(val_column)
      print("val_sex")
      print(val_sex)
      print("val_shape")
      print(val_shape)
      print("val_alpha")
      print(val_alpha)
      print("val_xmin")
      print(val_xmin)
      print("val_xmax")
      print(val_xmax)
      print("val_log10")
      print(val_log10)
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
      print("val_dump")
      print(val_dump)
      print("Before cleaning:")
      print(str(.gData))
    }

    # MODEL ###################################################################

    # Make copy of selected data frame (to allow re-plotting).
    obsData <- .gData

    # Get data for the selected analysis.
    if (val_column == 1) {
      obsData$Dep <- .gData$MethodX
      obsData$Exp <- .gData$Height
    } else if (val_column == 2) {
      obsData$Dep <- .gData$Method1
      obsData$Exp <- .gData$Height
    } else if (val_column == 3) {
      obsData$Dep <- .gData$Method2
      obsData$Exp <- .gData$Height
    } else if (val_column == 4) {
      obsData$Dep <- .gData$MethodL
      obsData$Exp <- .gData$MethodL.Ph
    }

    if (val_h) {
      # Use average peak height.
      obsData$Exp <- .gData$H
    }

    # Check if numeric.
    if (!is.numeric(obsData$Dep)) {
      obsData$Dep <- as.numeric(obsData$Dep)
      message("Dependent variable converted to numeric.")
    }
    if (!is.numeric(obsData$Exp)) {
      obsData$Exp <- as.numeric(obsData$Exp)
      message("Exploratory variable converted to numeric.")
    }

    # Clean -------------------------------------------------------------------

    message("Model drop-out for dataset with:")
    message(paste(nrow(obsData), " rows.", sep = ""))

    # Remove homozygous loci
    if ("Heterozygous" %in% names(obsData)) {
      n0 <- nrow(obsData)
      obsData <- obsData[obsData$Heterozygous == 1, ]
      n1 <- nrow(obsData)
      message(paste(n1, " rows after removing ", n0 - n1, " homozygous rows.", sep = ""))
    }

    # Remove locus droput. NB! Only for MethodL, other methods can use data below LDT.
    # Therefore we cannot use the 'Dropout' column.
    if ("Dep" %in% names(obsData)) {
      n0 <- nrow(obsData)
      obsData <- obsData[obsData$Dep != 2, ]
      n1 <- nrow(obsData)
      message(paste(n1, " rows after removing ", n0 - n1, " locus drop-out rows.", sep = ""))
    }

    # Remove sex markers.
    if (val_sex) {
      n0 <- nrow(obsData)
      sexMarkers <- getKit(kit = svalue(kit_drp), what = "Sex.Marker")
      for (m in seq(along = sexMarkers)) {
        obsData <- obsData[obsData$Marker != sexMarkers[m], ]
      }
      n1 <- nrow(obsData)
      message(paste(n1, " rows after removing ", n0 - n1, " sex marker rows.", sep = ""))
    }

    # Remove NA Explanatory.
    if (any(is.na(obsData$Exp))) {
      n0 <- nrow(obsData)
      obsData <- obsData[!is.na(obsData$Exp), ]
      n1 <- nrow(obsData)
      message(paste(n1, " rows after removing ", n0 - n1, " NA rows in explanatory column.", sep = ""))
    }

    # Remove NA Dependent.
    if (any(is.na(obsData$Dep))) {
      n0 <- nrow(obsData)
      obsData <- obsData[!is.na(obsData$Dep), ]
      n1 <- nrow(obsData)
      message(paste(n1, " rows after removing ", n0 - n1, " NA rows in dependent column.", sep = ""))
    }

    message(paste(nrow(obsData), " rows in total for analysis.", sep = ""))

    if (debug) {
      print("After cleaning:")
      print(str(obsData))
      print("NA in Exp/Dep:")
      print(any(is.na(obsData$Exp)))
      print(any(is.na(obsData$Dep)))
    }

    # Model -------------------------------------------------------------------

    # Build prediction range for smoother curve.
    val_pred_xmin <- min(obsData$Exp)
    val_pred_xmax <- max(obsData$Exp)
    xplot <- seq(val_pred_xmin, val_pred_xmax)
    predRange <- data.frame(Exp = xplot)

    # Create data for modeling.
    modData <- obsData

    # Dump model input data.
    if (val_dump) {
      sel_col_names <- c(
        "Sample.Name", "Marker", "Allele", "TPH", "H", "Peaks",
        "Expected", "Proportion", "Dep", "Exp"
      )
      selected <- names(modData) %in% sel_col_names
      saveObject(name = "model_data_dump", object = modData[, selected], parent = w, env = env)
    }

    # Convert to log values.
    if (logModel) {
      modData$Exp <- log(obsData$Exp)
      predRange$Exp <- log(predRange$Exp)
    }

    # Perform logistic regression on the selected column.
    dropoutModel <- glm(Dep ~ Exp, family = binomial("logit"), data = modData)
    sumfit <- summary(dropoutModel)

    # Calculate model score.
    hosOk <- FALSE
    if (requireNamespace("ResourceSelection", quietly = TRUE)) {
      # p-value <0.05 rejects the model.
      hos <- ResourceSelection::hoslem.test(dropoutModel$y, fitted(dropoutModel))
      hosOk <- TRUE
    }

    # Titles.
    if (val_titles) {
      mainTitle <- val_title
      xTitle <- val_xtitle
      yTitle <- val_ytitle
    } else {
      if (val_h) {
        mainTitle <- strLblMainTitleAverage
        xTitle <- strLblXTitleAverage
      } else {
        mainTitle <- strLblMainTitleHeight
        xTitle <- strLblXTitleHeight
      }
      yTitle <- strLblYTitle
    }

    # Extract model parameters.
    b0 <- sumfit$coefficients[1]
    b1 <- sumfit$coefficients[2]

    if (debug) {
      print("Model summary:")
      print(sumfit)
    }


    if (debug) {
      print("b0")
      print(b0)
      print("b1")
      print(b1)
    }

    # Calculate probabilities for the prediction range.
    ypred <- predict(dropoutModel, predRange, type = "link", se.fit = TRUE)

    # Calculate the prediction interval.
    ylower <- plogis(ypred$fit - qnorm(1 - val_pi_alpha / 2) * ypred$se) # Lower confidence limit.
    yupper <- plogis(ypred$fit + qnorm(1 - val_pi_alpha / 2) * ypred$se) # Upper confidence limit.

    # Calculate conservative prediction curve.
    yconservative <- plogis(ypred$fit + qnorm(1 - val_pi_alpha) * ypred$se)

    # Calculate y values for plot.
    yplot <- plogis(ypred$fit)

    # Create legend text.
    legendModel <- paste(strLblLegend, " \u03B20=", round(b0, 3),
      ", \u03B21=", round(b1, 3),
      sep = ""
    )

    if (hosOk) {
      # Add Hosmer-Lemeshow test.
      legendModel <- paste(legendModel,
        strLblHosmer, round(hos$p.value, 4),
        sep = ""
      )
    } else {
      message("Package 'ResourceSelection' is required for Hosmer-Lemeshow test.")
    }

    # Save prediction in a dataframe.
    predictionDf <- data.frame(Exp = xplot, Prob = yplot, yupper = yupper, ylower = ylower)

    if (debug) {
      print("predictionDf:")
      print(head(predictionDf, 100))
      print(str(predictionDf))
    }

    # Calculate dropout threshold T.
    if (logModel) {
      drop_py <- log(val_p_dropout) - log(1 - val_p_dropout)
      t_dropout <- exp((drop_py - b0) / b1)
    } else {
      t_dropout <- (log(val_p_dropout / (1 - val_p_dropout)) - b0) / b1
    }

    if (debug) {
      print(paste("t_dropout =", t_dropout))
    }

    if (!logModel) {
      if (t_dropout < 0) {
        if (debug) {
          print("t_dropout < 0 -> NA")
        }
        t_dropout <- NA # Can't handle negative values.
      }
    }

    # Calculate conservative threshold at P(D).
    t_dropout_cons <- xplot[min(which(yconservative < val_p_dropout))]
    # rfu1 <- predrange[min(which( yupper <val_p_dropout))] #too conservative?

    # PLOT ####################################################################

    if (!is.na(predictionDf) && !is.null(predictionDf)) {
      # Plotting global dropout probability.
      gp <- ggplot(
        data = predictionDf,
        aes_string(y = "Prob", x = "Exp")
      ) +
        geom_line()

      # Plot observed data points (heterozygotes).
      if (val_points) {
        gp <- gp + geom_point(
          data = obsData, aes_string(x = "Exp", y = "Dep"),
          shape = val_shape, alpha = val_alpha,
          position = position_jitter(
            width = val_jitterh,
            height = val_jitterv
          )
        )
      }

      # Prediction interval.
      if (val_prediction_interval) {
        gp <- gp + geom_ribbon(
          data = predictionDf,
          aes_string(y = "Prob", ymin = "ylower", ymax = "yupper"),
          fill = val_interval_col,
          alpha = val_interval_alpha
        )
      }

      # Dropout threshold.
      if (val_threshold) {
        # Initiate.
        thresholdLegend <- ""

        # Create threshold label.
        thresholdLegend <- paste("P(dropout|T=",
          round(t_dropout, 0),
          ")=",
          round(val_p_dropout, 3),
          sep = ""
        )

        # Add prediction interval.
        if (val_prediction_print) {
          thresholdLegend <- paste(thresholdLegend,
            "\n P(dropout>", round(val_p_dropout, 3),
            "|T=",
            round(t_dropout_cons, 0),
            ")<",
            val_pi_alpha * 100, "%",
            sep = ""
          )
        }

        # Make data frame.
        if (is.na(t_dropout)) {
          t_height <- 0
        } else {
          t_height <- t_dropout
        }
        thresholdLabel <- data.frame(
          Exp = t_height,
          Prob = val_p_dropout,
          label = thresholdLegend
        )

        if (debug) {
          print("thresholdLabel")
          print(thresholdLabel)
        }

        if (!is.na(t_dropout)) {
          if (debug) {
            print("Mark threshold")
          }

          # Horizontal threshold line.
          if (!is.na(val_xmin) && !is.na(val_xmax)) {
            xtemp <- val_xmin
          } else {
            xtemp <- 0
          }

          # Add horizontal threshold line.
          gp <- gp + geom_segment(
            data = thresholdLabel,
            aes_string(
              x = xtemp, y = "Prob",
              xend = "Exp",
              yend = "Prob"
            ),
            color = val_predcol,
            linetype = val_predline
          )

          if (debug) {
            print("Horizontal line added")
          }

          # Vertical threshold line.
          if (!is.na(val_ymin) && !is.na(val_ymax)) {
            ytemp <- val_ymin
          } else {
            ytemp <- 0
          }

          # Add vertical threshold line.
          gp <- gp + geom_segment(
            data = thresholdLabel,
            aes_string(
              x = "Exp", y = ytemp,
              xend = "Exp",
              yend = "Prob"
            ),
            color = val_predcol,
            linetype = val_predline
          )

          if (debug) {
            print("Vertical line added")
          }
        }

        # Print threshold label.
        if (val_threshold_print) {
          gp <- gp + geom_text(
            data = thresholdLabel,
            aes_string(x = "Exp", y = "Prob", label = "label"),
            hjust = 0, vjust = 0
          )
          if (debug) {
            print("Threshold printed")
          }
        }
      }

      # Print dropout model.
      if (val_model) {
        if (debug) {
          print("Print model")
        }

        # Create data frame.
        modelLabel <- data.frame(
          Exp = t_dropout,
          Prob = val_p_dropout,
          label = legendModel,
          xmax = val_pred_xmax
        )
        # Add model text.
        gp <- gp + geom_text(
          data = modelLabel, aes_string(x = Inf, y = Inf, label = "label"),
          hjust = 1, vjust = 1
        )
      }

      # Use log10 y scale.
      if (val_log10) {
        gp <- gp + scale_y_log10()
      }

      # Restrict y axis.
      if (!is.na(val_ymin) && !is.na(val_ymax)) {
        val_y <- c(val_ymin, val_ymax)
      } else {
        val_y <- NULL
      }
      # Restrict x axis.
      if (!is.na(val_xmin) && !is.na(val_xmax)) {
        val_x <- c(val_xmin, val_xmax)
      } else {
        val_x <- NULL
      }
      if (debug) {
        print(paste(
          "Zoom plot xmin/xmax,ymin/ymax:",
          paste(val_x, collapse = "/"),
          ",",
          paste(val_y, collapse = "/")
        ))
        print(str(val_x))
        print(str(val_y))
      }
      # Zoom in without dropping observations.
      gp <- gp + coord_cartesian(xlim = val_x, ylim = val_y)


      if (debug) {
        print("Apply theme and labels")
      }

      # Apply theme.
      gp <- gp + theme(axis.text.x = element_text(
        angle = val_angle,
        hjust = val_hjust,
        vjust = val_vjust,
        size = val_size
      ))

      # Add titles and labels.
      gp <- gp + labs(title = mainTitle)
      gp <- gp + xlab(xTitle)
      gp <- gp + ylab(yTitle)

      if (debug) {
        print("Plot")
      }

      # Plot.
      print(gp)

      # Change save button.
      blockHandlers(f5_save_btn)
      svalue(f5_save_btn) <- "Save as object"
      enabled(f5_save_btn) <- TRUE
      unblockHandlers(f5_save_btn)

      # Store in global variable.
      .gPlot <<- gp
    } else {
      gmessage(
        msg = strMsgNull,
        title = strMsgTitleError,
        icon = "error"
      )
    }
  }

  # INTERNAL FUNCTIONS ########################################################

  .updateGui <- function() {
    # Override titles.
    val <- svalue(titles_chk)

    if (val) {
      enabled(titles_group) <- TRUE
    } else {
      enabled(titles_group) <- FALSE
    }
  }

  .checkColumns <- function() {
    val_col <- svalue(f1_column_opt, index = TRUE)
    val_h <- svalue(f1_h_chk)
    requiredCol <- NULL
    missingCol <- NULL


    if (!is.null(.gData)) {
      # Enable button.
      enabled(plot_drop_btn) <- TRUE
      svalue(plot_drop_btn) <- "Plot predicted drop-out probability"

      # Check available modeling columns.
      if (val_h) {
        requiredCol <- c("H")
        if (!all(requiredCol %in% colnames(.gData))) {
          missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]
        }
      }

      # Check available modeling columns and enable/select.
      if (val_col == 1) {
        requiredCol <- c("MethodX")
        if (!all(requiredCol %in% colnames(.gData))) {
          missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]
        }
      } else if (val_col == 2) {
        requiredCol <- c("Method1")
        if (!all(requiredCol %in% colnames(.gData))) {
          missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]
        }
      } else if (val_col == 3) {
        requiredCol <- c("Method2")
        if (!all(requiredCol %in% colnames(.gData))) {
          missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]
        }
      } else if (val_col == 4) {
        requiredCol <- c("MethodL", "MethodL.Ph")
        if (!all(requiredCol %in% colnames(.gData))) {
          missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]
        }
      } else {
        message <- paste("Selection not supported!")

        gmessage(message,
          title = "Error",
          icon = "error",
          parent = w
        )

        # Disable button.
        enabled(plot_drop_btn) <- FALSE
      }

      if (!is.null(missingCol)) {
        gmessage(
          msg = paste(strMsgIncomplete1,
            paste(missingCol, collapse = "\n"),
            strMsgIncomplete2,
            sep = ""
          ),
          title = strMsgTitleIncomplete,
          icon = "info",
          parent = w
        )

        # Disable button.
        enabled(plot_drop_btn) <- FALSE
      }
    }
  }

  .loadSavedSettings <- function() {
    # First check status of save flag.
    if (!is.null(savegui)) {
      svalue(savegui_chk) <- savegui
      enabled(savegui_chk) <- FALSE
      if (debug) {
        print("Save GUI status set!")
      }
    } else {
      # Load save flag.
      if (exists(".strvalidator_modelDropout_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_modelDropout_gui_savegui", envir = env)
      }
      if (debug) {
        print("Save GUI status loaded!")
      }
    }
    if (debug) {
      print(svalue(savegui_chk))
    }

    # Then load settings if true.
    if (svalue(savegui_chk)) {
      if (exists(".strvalidator_modelDropout_gui_title", envir = env, inherits = FALSE)) {
        svalue(title_edt) <- get(".strvalidator_modelDropout_gui_title", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_title_chk", envir = env, inherits = FALSE)) {
        svalue(titles_chk) <- get(".strvalidator_modelDropout_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_x_title", envir = env, inherits = FALSE)) {
        svalue(x_title_edt) <- get(".strvalidator_modelDropout_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_y_title", envir = env, inherits = FALSE)) {
        svalue(y_title_edt) <- get(".strvalidator_modelDropout_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_sex", envir = env, inherits = FALSE)) {
        svalue(f1_sex_chk) <- get(".strvalidator_modelDropout_gui_sex", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_column", envir = env, inherits = FALSE)) {
        svalue(f1_column_opt) <- get(".strvalidator_modelDropout_gui_column", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_print_model", envir = env, inherits = FALSE)) {
        svalue(f1_printmodel_chk) <- get(".strvalidator_modelDropout_gui_print_model", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_mark_threshold", envir = env, inherits = FALSE)) {
        svalue(e1f1_threshold_chk) <- get(".strvalidator_modelDropout_gui_mark_threshold", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_risk", envir = env, inherits = FALSE)) {
        svalue(e1f1_risk_spn) <- get(".strvalidator_modelDropout_gui_risk", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_print_threshold", envir = env, inherits = FALSE)) {
        svalue(e1f1_print_chk) <- get(".strvalidator_modelDropout_gui_print_threshold", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_t_line", envir = env, inherits = FALSE)) {
        svalue(e1f1_t_linetype_drp) <- get(".strvalidator_modelDropout_gui_t_line", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_t_color", envir = env, inherits = FALSE)) {
        svalue(e1f1_t_linecolor_drp) <- get(".strvalidator_modelDropout_gui_t_color", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_print_interval", envir = env, inherits = FALSE)) {
        svalue(e1f2_print_interval_chk) <- get(".strvalidator_modelDropout_gui_print_interval", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_mark_interval", envir = env, inherits = FALSE)) {
        svalue(e1f2_mark_interval_chk) <- get(".strvalidator_modelDropout_gui_mark_interval", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_interval_alpha", envir = env, inherits = FALSE)) {
        svalue(e1f2_interval_spb) <- get(".strvalidator_modelDropout_gui_interval_alpha", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_interval_color", envir = env, inherits = FALSE)) {
        svalue(e1f2_interval_drp) <- get(".strvalidator_modelDropout_gui_interval_color", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_points_plot", envir = env, inherits = FALSE)) {
        svalue(e2g1_plotpoints_chk) <- get(".strvalidator_modelDropout_gui_points_plot", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_points_shape", envir = env, inherits = FALSE)) {
        svalue(e2g1_shape_spb) <- get(".strvalidator_modelDropout_gui_points_shape", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_points_alpha", envir = env, inherits = FALSE)) {
        svalue(e2g1_alpha_spb) <- get(".strvalidator_modelDropout_gui_points_alpha", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_points_jitterh", envir = env, inherits = FALSE)) {
        svalue(e2g1_jitterh_edt) <- get(".strvalidator_modelDropout_gui_points_jitterh", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_points_jitterv", envir = env, inherits = FALSE)) {
        svalue(e2g1_jitterv_edt) <- get(".strvalidator_modelDropout_gui_points_jitterv", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_axes_y_log10", envir = env, inherits = FALSE)) {
        svalue(log10_y_scale_chk) <- get(".strvalidator_modelDropout_gui_axes_y_log10", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_axes_y_min", envir = env, inherits = FALSE)) {
        svalue(e3g1_y_min_edt) <- get(".strvalidator_modelDropout_gui_axes_y_min", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_axes_y_max", envir = env, inherits = FALSE)) {
        svalue(e3g1_y_max_edt) <- get(".strvalidator_modelDropout_gui_axes_y_max", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_axes_x_min", envir = env, inherits = FALSE)) {
        svalue(e3g1_x_min_edt) <- get(".strvalidator_modelDropout_gui_axes_x_min", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_axes_x_max", envir = env, inherits = FALSE)) {
        svalue(e3g1_x_max_edt) <- get(".strvalidator_modelDropout_gui_axes_x_max", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_xlabel_size", envir = env, inherits = FALSE)) {
        svalue(e4g1_size_edt) <- get(".strvalidator_modelDropout_gui_xlabel_size", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_xlabel_angle", envir = env, inherits = FALSE)) {
        svalue(e4g1_angle_spb) <- get(".strvalidator_modelDropout_gui_xlabel_angle", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_xlabel_justh", envir = env, inherits = FALSE)) {
        svalue(e4g1_hjust_spb) <- get(".strvalidator_modelDropout_gui_xlabel_justh", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_xlabel_justv", envir = env, inherits = FALSE)) {
        svalue(e4g1_vjust_spb) <- get(".strvalidator_modelDropout_gui_xlabel_justv", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_h", envir = env, inherits = FALSE)) {
        svalue(f1_h_chk) <- get(".strvalidator_modelDropout_gui_h", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_dump", envir = env, inherits = FALSE)) {
        svalue(f1_dump_chk) <- get(".strvalidator_modelDropout_gui_dump", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_modelDropout_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_title", value = svalue(title_edt), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_title_chk", value = svalue(titles_chk), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_x_title", value = svalue(x_title_edt), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_y_title", value = svalue(y_title_edt), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_sex", value = svalue(f1_sex_chk), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_column", value = svalue(f1_column_opt), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_print_model", value = svalue(f1_printmodel_chk), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_mark_threshold", value = svalue(e1f1_threshold_chk), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_print_threshold", value = svalue(e1f1_print_chk), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_risk", value = svalue(e1f1_risk_spn), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_t_line", value = svalue(e1f1_t_linetype_drp), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_t_color", value = svalue(e1f1_t_linecolor_drp), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_print_interval", value = svalue(e1f2_print_interval_chk), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_mark_interval", value = svalue(e1f2_mark_interval_chk), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_interval_alpha", value = svalue(e1f2_interval_spb), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_interval_color", value = svalue(e1f2_interval_drp), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_points_plot", value = svalue(e2g1_plotpoints_chk), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_points_shape", value = svalue(e2g1_shape_spb), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_points_alpha", value = svalue(e2g1_alpha_spb), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_points_jitterh", value = svalue(e2g1_jitterh_edt), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_points_jitterv", value = svalue(e2g1_jitterv_edt), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_axes_y_log10", value = svalue(log10_y_scale_chk), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_axes_y_min", value = svalue(e3g1_y_min_edt), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_axes_y_max", value = svalue(e3g1_y_max_edt), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_axes_x_min", value = svalue(e3g1_x_min_edt), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_axes_x_max", value = svalue(e3g1_x_max_edt), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_xlabel_size", value = svalue(e4g1_size_edt), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_xlabel_angle", value = svalue(e4g1_angle_spb), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_xlabel_justh", value = svalue(e4g1_hjust_spb), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_xlabel_justv", value = svalue(e4g1_vjust_spb), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_h", value = svalue(f1_h_chk), envir = env)
      assign(x = ".strvalidator_modelDropout_gui_dump", value = svalue(f1_dump_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_modelDropout_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_title", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_title_chk", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_x_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_y_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_sex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_sex", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_column", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_column", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_print_model", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_print_model", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_mark_threshold", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_mark_threshold", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_print_threshold", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_print_threshold", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_risk", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_risk", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_t_line", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_t_line", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_t_color", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_t_color", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_print_interval", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_print_interval", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_mark_interval", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_mark_interval", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_interval_alpha", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_interval_alpha", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_interval_color", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_interval_color", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_points_plot", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_points_plot", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_points_shape", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_points_shape", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_points_alpha", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_points_alpha", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_points_jitterh", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_points_jitterh", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_points_jitterv", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_points_jitterv", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_axes_y_log10", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_axes_y_log10", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_axes_y_min", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_axes_y_min", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_axes_y_max", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_axes_y_max", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_axes_x_min", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_axes_x_min", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_axes_x_max", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_axes_x_max", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_xlabel_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_xlabel_size", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_xlabel_angle", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_xlabel_angle", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_xlabel_justh", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_xlabel_justh", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_xlabel_justv", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_xlabel_justv", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_h", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_h", envir = env)
      }
      if (exists(".strvalidator_modelDropout_gui_dump", envir = env, inherits = FALSE)) {
        remove(".strvalidator_modelDropout_gui_dump", envir = env)
      }

      if (debug) {
        print("Settings cleared!")
      }
    }

    if (debug) {
      print("Settings saved!")
    }
  }

  # END GUI ###################################################################

  # Load GUI settings.
  .loadSavedSettings()
  .updateGui()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
