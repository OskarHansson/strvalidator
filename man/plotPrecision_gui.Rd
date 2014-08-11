\name{plotPrecision_gui}
\alias{plotPrecision_gui}
\title{Plot Precision}
\usage{
plotPrecision_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
}
\arguments{
  \item{env}{environment in wich to search for data
  frames.}

  \item{savegui}{logical indicating if GUI settings should
  be saved in the environment.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
TRUE
}
\description{
\code{plotPrecision_gui} is a GUI simplifying the creation of plots from precision data.
}
\details{
Plot precision data for size, height, or data point as dotplot or
boxplot. Plot per marker or all in one. Use the mean value or the allele
designation as x-axis labels.
Automatic plot titles can be replaced by custom titles.
A name for the result is automatiaclly suggested.
The resulting plot can be saved as either a plot object or as an image.
}
\seealso{
\url{http://docs.ggplot2.org/current/} for details on plot settings.
}

