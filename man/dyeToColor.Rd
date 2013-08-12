\name{dyeToColor}
\alias{dyeToColor}
\title{Convert dye letter to color name.}
\usage{
  dyeToColor(data)
}
\arguments{
  \item{data}{data frame containing a 'Dye' column.}
}
\value{
  data.frame with additional column 'Color'.
}
\description{
  \code{dyeToColor} converts dye letters used in forensic
  DNA typing kits to color names in R.
}
\details{
  Primers in forensic STR typing kits are labelled with a
  fluorescent dye. The dyes are represented with single
  letters in exported result files. For visualisation in R
  these are matched to R color names.
}
\examples{
# Some common designation for fluorescent dyes.
dyes <- data.frame(Dye=c("B","B","B","G","G","Y","R","R"))
# Get corresponding R colors.
colors <- dyeToColor(data=dyes)
}
\keyword{internal}

