\name{savePlot_gui}
\alias{savePlot_gui}
\title{Save plot GUI}
\usage{
  savePlot_gui(object = NULL, env = parent.frame(),
    debug = FALSE)
}
\arguments{
  \item{object}{plot object.}

  \item{env}{environment in wich to search for data
  frames.}

  \item{debug}{logical indicating printing debug
  information.}
}
\description{
  \code{savePlot_gui} is a GUI simplifying the saving of
  plots as images.
}
\details{
  Pass a plot object directly to the function or provide an
  environment to select one from. Specify name, dimensions,
  and resolution. The plot will be save in the selected
  folder.
}

