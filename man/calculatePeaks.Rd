\name{calculatePeaks}
\alias{calculatePeaks}
\title{Count peaks}
\usage{
calculatePeaks(data, bins = c(0, 2, 3), labels = c("No contamination",
  "Drop-in contamination", "Gross contamination"), nool = FALSE,
  permarker = FALSE, debug = FALSE)
}
\arguments{
  \item{data}{data frame containing at least the columns
  'Sample.Name' and 'Height'.}

  \item{labels}{character vector defining the group labels
  (if any).}

  \item{bins}{numeric vector containing the maximum number
  of peaks required for a sample to get a specific label.}

  \item{nool}{logical if TRUE, off-ladder alleles 'OL'
  peaks will be discarded. if FALSE, all peaks will be
  included in the calculations.}

  \item{permarker}{logical if TRUE, peaks will counted per
  marker. if FALSE, peaks will counted per sample.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
data.frame with with additional columns 'Peaks', 'Group',
and 'Id'.
}
\description{
\code{calculatePeaks} calculates the number of peaks in a
sample.
}
\details{
Counts the number of peaks in a sample profile based on
values in the 'Height' column. Each sample can be labelled
according to custom labels defined by the number of peaks.
Peaks can be counted per sample or per marker per sample.
There is an option to discard off-ladder peaks ('OL'). The
default purpose for this function is to categorize
contamination in negative controls, but it can be used to
simply calculating the number of peaks in any sample. NB! A
column 'Peaks' for the number of peaks will be created.  If
present it will be overwritten. NB! A column 'Group' for
the sample group will be created.  If present it will be
overwritten. NB! A column 'Id' will be created by combining
the content in the 'Sample.Name' and 'File' column (if
available).  The unique entries in the 'Id' column will be
the definition of a sample.  If 'File' is present this
allows for identical sample names in different batches
(files) to be identified as different samples.  If 'Id' is
present it will be overwritten.
}

