\name{calculateH}
\alias{calculateH}
\title{Calculate average peak height.}
\usage{
  calculateH(data)
}
\arguments{
  \item{data}{a data frame with at least 'Sample.Name',
  'Z', and 'Height' columns.}
}
\value{
  data.frame with with three columns: 'Sample.Name', 'H',
  and 'Peaks'.
}
\description{
  \code{calculateH} calculates the average peak height for
  a sample.
}
\details{
  Calculates the average peak height (H) for a sample.
  Takes (GM-formatted) data for samples as input. Sample
  data must contain a column "Z", where 1 = heterozygote
  loci, and 2 = homozygote loci as known from the reference
  sample. Calculates H according to the formula: \eqn{H =
  sum(peak heights)/(n[het] + 2n[hom]} Where: n[het] =
  number of observed heterozygote alleles n[hom] = number
  of observed homozygote alleles
}

