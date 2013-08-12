\name{calculateBalance}
\alias{calculateBalance}
\title{Calculate balance}
\usage{
  calculateBalance(data, ref, perSample = TRUE,
    lb = "prop", perDye = TRUE, ignoreCase = TRUE,
    debug = FALSE)
}
\arguments{
  \item{data}{a data frame containing at least
  'Sample.Name', 'Marker', 'Height', 'Allele', and Dye'.}

  \item{perSample}{logical, default TRUE calculates balance
  for each sample, FALSE calculates the average across all
  samples.}

  \item{lb}{string. 'prop' is defualt and locus balance is
  calculated proportionally in relation to the whole
  sample. 'norm' locus balance is normalised in relation to
  the locus with the highest total peakheight.}

  \item{perDye}{logical, default is TRUE and locus balance
  is calculated within each dye.  FALSE locus balance is
  calculated globally.}

  \item{ignoreCase}{logical indicating if sample matching
  should ignore case.}
}
\value{
  data.frame with with columns 'Sample.Name', 'Marker',
  'Hb', 'Lb', 'MpH'. Or 'Sample.Name','Marker','Hb.n',
  'Hb.Mean', 'Hb.Sd', 'Hb.95','Lb.n', 'Lb.Mean', 'Lb.Sd'.
}
\description{
  \code{calculateBalance} calculates the inter and intra
  locus balance.
}
\details{
  Calculates the inter and intra locus balance for a
  filtered dataset. Takes 'slimmed' data for samples and
  references as input.At the moment it is better to discard
  data prior to analysis than to use min/maxHeight. NB!
  Requires at least one row for each marker per sample,
  even if no data.
}
\examples{
data(ref2)
data(set2)
# Calculate average balances.
calculateBalance(data=set2, ref=ref2, perSample=FALSE)
}
\keyword{internal}

