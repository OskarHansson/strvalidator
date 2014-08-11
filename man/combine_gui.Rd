\name{combine_gui}
\alias{combine_gui}
\title{Combine two datasets}
\usage{
combine_gui(env = parent.frame(), debug = FALSE)
}
\arguments{
  \item{env}{environment in wich to search for data
  frames.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
TRUE
}
\description{
\code{combine_gui} is a GUI for combining two datasets.
}
\details{
Simple GUI to combine two datasets using the \code{\link{rbind}} function.
NB! Datasets must have identical column names.
}

