\name{modelDropout_gui}
\alias{modelDropout_gui}
\title{model and plot drop-out events}
\usage{
  modelDropout_gui(env = parent.frame(), savegui = NULL,
    debug = FALSE)
}
\arguments{
  \item{env}{environment in wich to search for data frames
  and save result.}

  \item{savegui}{logical indicating if GUI settings should
  be saved in the environment.}

  \item{debug}{logical indicating printing debug
  information.}
}
\description{
  \code{modelDropout_gui} model probability of dropout and
  plots a graph.
}
\details{
  Models the probability of dropout P(D) using logistic
  regression logit P(P;H) = B0 + B1*log(H), where 'H' is
  the peak height. Produce a plot showing the model
  prediction, optionally with given prediction interval.
  Parameters 'xmin', 'ymin', and 'ymax' affect the plot,
  and 'col.line' and 'colConf' is the colour of the
  prediction line and prediction interval lines
  respectively.
}
\references{
  Peter Gill, Roberto Puch-Solis, James Curran, The
  low-template-DNA (stochastic) threshold-Its determination
  relative to risk analysis for national DNA databases,
  Forensic Science International: Genetics, Volume 3, Issue
  2, March 2009, Pages 104-111, ISSN 1872-4973,
  10.1016/j.fsigen.2008.11.009.
  \url{http://www.sciencedirect.com/science/article/pii/S1872497308001798}
}

