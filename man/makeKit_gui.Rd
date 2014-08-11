\name{makeKit_gui}
\alias{makeKit_gui}
\title{Make kit}
\usage{
makeKit_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
}
\arguments{
  \item{env}{environment in wich to search for data
  frames.}

  \item{savegui}{logical indicating if GUI settings should
  be saved in the environment. [Not currently used]}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
TRUE
}
\description{
\code{makeKit_gui} add new kits or edit the kit file.
}
\details{
A graphical user interface for reading information from 'bins' and
'panels' file for the creation of additional kits. It is also possible to
edit the short and full name of existing kits or removing kits.
The gender marker of each kits is auto detected but can be changed manually.
#' NB! Short name must be unique.
}
\seealso{
\code{\link{readBinsFile}}, \code{\link{readPanelsFile}}, \code{\link{combineBinsAndPanels}}
}

