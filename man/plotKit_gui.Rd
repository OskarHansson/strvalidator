\name{plotKit_gui}
\alias{plotKit_gui}
\title{Plot kit marker ranges}
\usage{
plotKit_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
\code{plotKit_gui} is a GUI for plotting marker ranges for kits.
}
\details{
Create an overview of the size range for markers in different kits.
It is possible to select multiple kits, specify titles, font size, distance
between two kits, distance between dye channels, and the transparency of dyes.
}

