\name{modelDropout}
\alias{modelDropout}
\title{model drop-out events}
\usage{
  modelDropout(data, plotPI = TRUE, conf = 0.95, xmin = NA,
    xmax = NA, ymin = 0, ymax = 1, col.line = 1,
    col.conf = 3)
}
\arguments{
  \item{data}{data frame in GeneMapper format containing at
  least a column 'Allele'.}

  \item{plotPI}{logical. TRUE if prediction intervals
  should be plotted with the given confidence limit
  'conf'.}

  \item{conf}{numerical specifying confidence limit for the
  prediction interval.}

  \item{xmin}{numerical setting for the x axis.in the
  plot.}

  \item{xmax}{numerical setting for the x axis.in the
  plot.}

  \item{ymin}{numerical setting for the y axis.in the
  plot.}

  \item{ymax}{numerical setting for the y axis.in the
  plot.}

  \item{col.line}{value setting the line color of the
  prediction.}

  \item{col.conf}{value setting the line color of the
  prediction interval.}
}
\description{
  \code{modelDropout} model probability of dropout and
  plots a graph.
}
\details{
  Models the probability of dropout P(D) using logistic
  regression logit P(P;H) = ß0 + ß1*log(H), where 'H' is
  the peak height. Produce a plot showing the model
  prediction, optionally with given prediction interval.
  Parameters 'xmin', 'ymin', and 'ymax' affect the plot,
  and 'col.line' and 'col.conf' is the colour of the
  prediction line and prediction interval lines
  respectively.
}
\examples{
print("Example will come")
}
\keyword{internal}

