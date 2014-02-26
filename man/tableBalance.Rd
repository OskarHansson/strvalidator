\name{tableBalance}
\alias{tableBalance}
\title{Table stutter}
\usage{
tableBalance(data, scope = "locus", quant = 0.5)
}
\arguments{
  \item{data}{data frame from a balance analysis by
  \code{calculateBalance}.}

  \item{scope}{string, summarize 'global' or 'locus'.}

  \item{quant}{numeric, quantile to calculate.}
}
\value{
data.frame with summarized result.
}
\description{
\code{tableBalance} summarizes a balance analysis in table
format.
}
\details{
Summarize the balance analysis in table format with
different scope. (locus, or global). Returns a dataframe
with columns for marker name 'Marker', number of allele
ratios 'Hb.n', the minimum observed allele ratio 'Hb.Min',
the mean allele ratio 'Hb.Mean', its standard deviation
'Hb.Stdv', the XXth percentile 'Hb.Perc.XX', number of
locus ratios 'Lb.n', the minimum observed locus ratio
'Lb.Min', the mean locus ratio 'Lb.Mean', its standard
deviation 'Lb.Stdv', and the XXth percentile 'Lb.Perc.XX'.
For more details see \code{min}, \code{mean}, \code{sd},
\code{quantile}.
}

