\name{addDye}
\alias{addDye}
\title{Adds dye information to a data frame}
\usage{
  addDye(data, kit)
}
\arguments{
  \item{data}{Data frame containing at least a column
  'Marker'.}

  \item{kit}{String or integer specifying the STR typing
  kit.}
}
\value{
  data.frame the original data frame containing an
  additional column.
}
\description{
  \code{addDye} Adds a 'Dye' column to 'data' based on the
  information in 'kit'.
}
\details{
  Information about which dye each marker is labelled with
  in the specified STR typing kit 'kit' is added as a new
  column to a data frame containing at least a column
  'Marker'.
}
\examples{
# Get marker names for Promega PowerPlex ESX 17.
x <- data.frame(Marker = getKit("ESX17")$locus)
# Get other kit information using string name.
y <- addDye(data=x, kit="ESX17")
print(x)
print(y)
}
\keyword{internal}

