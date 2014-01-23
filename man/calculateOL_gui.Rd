\name{calculateOL_gui}
\alias{calculateOL_gui}
\title{Bins Overlap}
\usage{
calculateOL_gui(env = parent.frame(), savegui = NULL, debug = TRUE)
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
\code{calculateOL_gui} is a GUI wrapper for the
\code{calculateOL} function.
}
\details{
By analysing the allelic ladders the risk for getting
off-ladder (OL) alleles are calculated. The frequencies
from a provided population database is used to calculate
the risk per marker and in total for the given kit(s).
Virtual alleles can be excluded from the calculation. Small
frequencies can be limited to the estimate 5/2N.
}

