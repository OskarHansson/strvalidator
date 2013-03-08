\name{slim}
\alias{slim}
\title{Slim data frames}
\usage{
  slim(data, fix = NULL, stack = NULL, keepAllFixed = TRUE)
}
\arguments{
  \item{data}{data frame.}

  \item{fix}{vector of strings with colum names to keep
  fixed.}

  \item{stack}{vector of strings with colum names to slim.}

  \item{keepAllFixed}{logical, .}
}
\value{
  list with simulation results.
}
\description{
  \code{slim} slims data frames with GeneMapper data.
}
\details{
  \code{slim} repeated columns into single columns, also
  see \code{unslim()}. For example, the following data
  frame:
  Sample.Name|Marker|Allele.1|Allele.2|Size.1|Size.2|Data.Point..
  using this command: slim(data,
  fix=c("Sample.Name","Marker"), stack=c("Allele","Size"))
  would result in this data frame (NB! 'Data.Point' is
  lost): Sample.Name|Marker|Allele|Size
}
\examples{
ref1
# Format data frame.
ref1 <- slim(data=ref1, fix=c("Sample.Name","Marker"), stack=c("Allele"))
}
\keyword{internal}

