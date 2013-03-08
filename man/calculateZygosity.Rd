\name{calculateZygosity}
\alias{calculateZygosity}
\title{Calculate zygosity}
\usage{
  calculateZygosity(data)
}
\arguments{
  \item{data}{Data frame containing at least a column named
  'Alleles'.}
}
\value{
  data.frame the original data frame containing additional
  columns.
}
\description{
  \code{calculateZygosity} Calculates the number of alleles
  in each marker.
}
\details{
  Adds a column 'Zygosity' to 'data'. Calculates the number
  of unique values in 'Allele*' columns. Indicates zygosity
  as '1' for homozygous and '2' for a heterozygous loci.
  Sample names must be unique.
}
\examples{
print("Example will come later")
}
\keyword{internal}

