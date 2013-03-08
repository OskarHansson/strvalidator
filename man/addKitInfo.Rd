\name{addKitInfo}
\alias{addKitInfo}
\title{Adds all kit information}
\usage{
  addKitInfo(data, kit)
}
\arguments{
  \item{data}{Data frame containing at least a column named
  'Marker'.}

  \item{kit}{String or integer specifying the STR typing
  kit.}
}
\value{
  data.frame the original data frame containing additional
  columns.
}
\description{
  \code{addKitInfo} Adds all kit information as new columns
  to 'data' for each 'Marker' according to the specified
  'kit'.
}
\details{
  'data' contains short tandem repeat (STR) marker/locus
  names in the required column. Based on the provided kit
  name other information is attached to 'data' in new
  columns.
}
\examples{
# Get marker names for Promega PowerPlex ESX 17.
x <- data.frame(Marker = getKit("ESX17")$locus)
# Get other kit information using string name.
y <- addKitInfo(data=x, kit="ESX17")
print(x)
print(y)
}
\keyword{internal}

