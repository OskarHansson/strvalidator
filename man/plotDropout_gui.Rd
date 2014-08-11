\name{plotDropout_gui}
\alias{plotDropout_gui}
\title{Plot Dropout}
\usage{
plotDropout_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
}
\arguments{
  \item{env}{environment in wich to search for data frames
  and save result.}

  \item{savegui}{logical indicating if GUI settings should
  be saved in the environment.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
TRUE
}
\description{
\code{plotDropout_gui} is a GUI simplifying the creation of plots from dropout data.
}
\details{
Plot dropout data as heatmap arranged by, average peak height,
amount, concentration, or sample name. It is also possible to plot the
empirical cumulative distribution (ecdp) of the peak heights of surviving heterozygote
alleles (with dropout of the parter allele), or a dotplot of all dropout events.
The peak height of homozygote alleles can be included in the ecdp.
Automatic plot titles can be replaced by custom titles.
A name for the result is automatiaclly suggested.
The resulting plot can be saved as either a plot object or as an image.
}
\seealso{
\url{http://docs.ggplot2.org/current/} for details on plot settings.
}

