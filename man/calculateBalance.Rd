\name{calculateBalance}
\alias{calculateBalance}
\title{Calculate balance}
\usage{
  calculateBalance(data, perSample = TRUE, lb = "prop",
    perDye = TRUE, minHeight = NULL, maxHeight = NULL)
}
\arguments{
  \item{data}{a data frame containing at least
  'Sample.Name', 'Marker', 'Height', Dye', and 'Zygosity'.}

  \item{perSample}{logical, default TRUE calculates balance
  for each sample, FALSE calculates the avrage across all
  samples.}

  \item{lb}{string. 'prop' is defualt and locus balance is
  calculated proportionally in relation to the whole
  sample. 'norm' locus balance is normalised in relation to
  the locus with the highest total peakheight.}

  \item{perDye}{logical, default is TRUE and locus balance
  is calculated within each dye.  FALSE locus balance is
  calculated globally.}

  \item{minHeight}{integer giving the lower bound.}

  \item{maxHeight}{integer giving the upper bound.}
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
  Takes (GM-formatted) data for samples as input (Must be
  filtered to contain the clean profile??) Seem to give the
  same result filtered/not filtered). NB! Remove ladder
}
\examples{
data(ref1)
data(set1)
# Format data frame.
ref1 <- slim(data=ref1, fix=c("Sample.Name","Marker"), stack=c("Allele"))

# Indicate heterozygous / homozygous alleles.
ref1 <- calculateZygosity(ref1)

# Remove all except positive control samples.
set1 <- trim(data=set1, samples="PC")

# Format data frame.
set1 <- slim(data=set1, fix=c("Sample.Name","Marker"), stack=c("Allele","Height"))

# Filter out data matching alleles in reference.
set1 <- filterProfile(data=set1, ref=ref1)

# Add zygosity.
set1$Zygosity <- rep(ref1$Zygosity,8)

# Add dye.
set1 <- addDye(data=set1, kit="ESX17")

# Calculate average balances.
calculateBalance(data=set1, perSample=FALSE)
}
\keyword{internal}

