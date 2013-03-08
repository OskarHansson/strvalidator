\name{addData}
\alias{addData}
\title{Adds new data columns to a data frame}
\usage{
  addData(data, newData, byCol, exact = TRUE)
}
\arguments{
  \item{newData}{Data frame containing information you want
  to add to 'data'.}

  \item{data}{Data frame containing your main data.}
}
\value{
  data.frame the original data frame containing additional
  columns.
}
\description{
  \code{addData} Adds columns in 'newData' to 'data' by
  column 'byCol'.
}
\details{
  Information in columns in data frame 'newData' is added
  to data frame 'data' based on value in column 'byCol'.
}
\examples{
# Get marker names for Promega PowerPlex ESX 17. Expand data frame.
x <- data.frame(Marker = rep(getKit("ESX17")$locus, c(1,2,3)))
# Get offsets for Promega PowerPlex ESX 17.
y <- data.frame(Marker = getKit("ESX17")$locus, Offset = getKit("ESX17")$offset)
# Get other kit information using string name.
z <- addData(data=x, newData=y, byCol="Marker")
print(x)
print(y)
print(z)
}
\keyword{internal}

