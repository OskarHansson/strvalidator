\name{modelDropout_gui}
\alias{modelDropout_gui}
\title{model and plot drop-out events}
\usage{
  modelDropout_gui(env = parent.frame(), debug = FALSE)
}
\arguments{
  \item{env}{environment in wich to search for data frames
  and save result.}

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

