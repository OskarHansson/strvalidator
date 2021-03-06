% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateHeight.r
\name{calculateHeight}
\alias{calculateHeight}
\title{Calculate Peak Height.}
\usage{
calculateHeight(
  data,
  ref = NULL,
  na.replace = NULL,
  add = TRUE,
  exclude = NULL,
  sex.rm = FALSE,
  qs.rm = FALSE,
  kit = NULL,
  ignore.case = TRUE,
  exact = FALSE,
  word = FALSE,
  debug = FALSE
)
}
\arguments{
\item{data}{data.frame with at least columns 'Sample.Name' and 'Height'.}

\item{ref}{data.frame with at least columns 'Sample.Name' and 'Allele'.}

\item{na.replace}{replaces NA values in the final result.}

\item{add}{logical default is TRUE which will add or overwrite columns
'TPH', 'Peaks', 'H', and 'Proportion' in the provided 'data'.}

\item{exclude}{character vector (case sensitive) e.g. "OL" excludes rows with
"OL" in the 'Allele' column.}

\item{sex.rm}{logical, default FALSE to include sex markers in the analysis.}

\item{qs.rm}{logical, default TRUE to exclude quality sensors from the analysis.}

\item{kit}{character, required if sex.rm=TRUE or qs.rm=TRUE to define the kit.}

\item{ignore.case}{logical TRUE ignores case in sample name matching.}

\item{exact}{logical TRUE for exact sample name matching.}

\item{word}{logical TRUE to add word boundaries to sample name matching.}

\item{debug}{logical indicating printing debug information.}
}
\value{
data.frame with with at least columns 'Sample.Name', 'TPH', and 'Peaks'.
}
\description{
Calculate peak height metrics for samples.
}
\details{
Calculates the total peak height (TPH), and number of observed peaks (Peaks),
for each sample by default. If a reference dataset is provided average peak
height (H), and profile proportion (Proportion) are calculated.

H is calculated according to the formula:
\eqn{H = sum(peak heights)/(n[het] + 2n[hom]}
Where:
n[het] = number of observed heterozygous alleles
n[hom] = number of observed homozygous alleles

Tip: If it is known that all expected peaks are observed and no unexpected
peaks are present, the dataset can be used as a reference for itself.

Note: If a reference dataset is provided the known alleles will be extracted
from the dataset.
}
\references{
Torben Tvedebrink, Poul Svante Eriksen, Helle Smidt Mogensen, Niels Morling,
 Evaluating the weight of evidence by using quantitative short tandem repeat data in DNA mixtures
 Journal of the Royal Statistical Society: Series C (Applied Statistics),
 Volume 59, Issue 5, 2010,
 Pages 855-874, 10.1111/j.1467-9876.2010.00722.x.
\url{http://dx.doi.org/10.1111/j.1467-9876.2010.00722.x}
}
