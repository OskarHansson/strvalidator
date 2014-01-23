\name{calculateH}
\alias{calculateH}
\title{Calculate average peak height.}
\usage{
calculateH(data, debug = FALSE)
}
\arguments{
  \item{data}{a data frame in 'slim' format with at least
  columns 'Sample.Name', 'Heterozygous', and 'Height'.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
data.frame with with columns: 'Sample.Name', 'H', and
'Peaks'.
}
\description{
\code{calculateH} calculates the average peak height for a
sample.
}
\details{
Calculates the average peak height (H) for each sample.
Sample data must contain a column "Heterozygous", where 1 =
heterozygous loci, and 0 = homozygous loci as known from
the reference sample. Calculates H according to the
formula: \eqn{H = sum(peak heights)/(n[het] + 2n[hom]}
Where: n[het] = number of observed heterozygous alleles
n[hom] = number of observed homozygous alleles
}
\references{
Torben Tvedebrink, Poul Svante Eriksen, Helle Smidt
Mogensen, Niels Morling, Evaluating the weight of evidence
by using quantitative short tandem repeat data in DNA
mixtures Journal of the Royal Statistical Society: Series C
(Applied Statistics), Volume 59, Issue 5, 2010, Pages
855-874, 10.1111/j.1467-9876.2010.00722.x.
\url{http://dx.doi.org/10.1111/j.1467-9876.2010.00722.x}
}

