\name{calculateBalance}
\alias{calculateBalance}
\title{Calculate balance}
\usage{
calculateBalance(data, ref, perSample = TRUE, lb = "prop", perDye = TRUE,
  hb = 1, ignoreCase = TRUE, debug = FALSE)
}
\arguments{
  \item{data}{a data frame containing at least
  'Sample.Name', 'Marker', 'Height', 'Allele', and Dye'.}

  \item{perSample}{logical, default TRUE calculates balance
  for each sample, FALSE calculates the average across all
  samples.}

  \item{lb}{string. 'prop' is defualt and locus balance is
  calculated proportionally 'norm' locus balance is
  normalised in relation to the locus with the highest
  total peakheight.}

  \item{perDye}{logical, default is TRUE and locus balance
  is calculated within each dye.  FALSE locus balance is
  calculated globally across all dyes.}

  \item{hb}{numerical, definition of heterozygous balance.
  hb=1; HMW/LMW, hb=2; Max1(Ph)/Max2(Ph).}

  \item{ignoreCase}{logical indicating if sample matching
  should ignore case.}
}
\value{
data.frame with with columns 'Sample.Name', 'Marker',
'Delta', 'Hb', 'Lb', 'MPH'. Or 'Sample.Name', 'Marker',
'Delta.Mean', 'Hb.n', 'Hb.Min', 'Hb.Mean', 'Hb.Sd',
'Hb.95', 'Lb.n', 'Lb.Min', 'Lb.Mean', 'Lb.Sd'.
}
\description{
\code{calculateBalance} calculates the inter and intra
locus balance.
}
\details{
Calculates the inter and intra locus balance for a dataset.
Only peaks corresponding to reference alleles will be
included in analysis (does not require filtered data). Also
calculates the allele size difference between heterozygous
alleles. Takes 'slimmed' data for samples and references as
input. NB! Requires at least one row for each marker per
sample, even if no data. NB! 'X' and 'Y' will be handled as
'1' and '2' respectively.
}
\examples{
data(ref2)
data(set2)
# Calculate average balances.
calculateBalance(data=set2, ref=ref2, perSample=FALSE)
}
\keyword{internal}

