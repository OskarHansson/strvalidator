\name{combineBinsAndPanels}
\alias{combineBinsAndPanels}
\title{combine bins and panels files.}
\usage{
combineBinsAndPanels(bin, panel)
}
\arguments{
  \item{bin}{data frame created from the 'bins' file.}

  \item{panel}{data frame created from the 'panels' file.}
}
\value{
data frame containing columns 'Panel', 'Marker', 'Allele',
'Size', 'Size.Min', 'Size.Max', 'Virtual', 'Color',
'Repeat', 'Marker.Min', 'Marker.Max', 'Offset',
'Short.Name', 'Full.Name'
}
\description{
\code{combineBinsAndPanels} combines useful information
into one dataset.
}
\details{
Combines information from two sources ('Bins' and 'Panels'
file) to create a dataset containing information about
panel name, marker name, alleles in the allelic ladder,
their size and size range, a flag indicating virtual
alleles, fluorophore color, repeat size, marker range. The
short name, full name, and gender marker flag is populated
through the \code{makeKit_gui} user interface. In addition
the function calculates an estimated offset for each
marker, which can be used for creating epg-like plots.
Note: offset is estimated by taking the smallest physical
ladder fragment e.g. 98.28 for D3 in ESX17. Round this to
an integer (98) and finally subtract the number of base
pair for that repeat i.e. 4*9=36, which gives an offset of
98-36 = 62 bp. Microvariants are handled by taking the
decimal part multiplied with 10 and adding this to the
number of base pair e.g. 9.3 = 4*9 + 0.3*10 = 39 bp.
}

