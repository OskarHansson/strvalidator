\name{plotStutter_gui}
\alias{plotStutter_gui}
\title{Plot Stutter}
\usage{
plotStutter_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
\code{plotStutter_gui} is a GUI simplifying the creation of plots from stutter data.
}
\details{
Select data to plot in the drop-down menu. Check that the correct
kit has been detected. Plot stutter data by parent allele or by peak height.
Automatic plot titles can be replaced by custom titles.
A name for the result is automatiaclly suggested.
The resulting plot can be saved as either a plot object or as an image.
}
\seealso{
\url{http://docs.ggplot2.org/current/} for details on plot settings.
}

