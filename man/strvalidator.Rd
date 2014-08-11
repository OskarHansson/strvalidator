\name{strvalidator}
\alias{strvalidator}
\title{Graphical user interface for the STR-validator package}
\usage{
strvalidator(debug = FALSE)
}
\arguments{
  \item{debug}{logical indicating printing debug
  information.}
}
\value{
TRUE
}
\description{
\code{strvalidator} is validation and process control toolbox for forensic genetics.
}
\details{
STR-validator (pronounced starvalidator) is a package developed
for validation and process control of both methods and instruments in a
forensic genetic laboratory setting.
This graphical user interface make it very easy to analyse validation
data in accordance with ENFSI and SWGDAM guidelines.
The code has been extensively tested in order to assure correct results.\cr\cr

Open the help page and click \code{Index} at the bottom of the page
to see a list of all functions.\cr\cr

Created by:\cr
Oskar Hansson, Department of Forensic Biology (NIPH, Norway)\cr\cr

General user information and tutorials:\cr
\url{https://sites.google.com/site/forensicapps/strvalidator}\cr\cr

Facebook:\cr
\url{https://www.facebook.com/pages/STR-validator/240891279451450?ref=tn_tnmn}\cr\cr

Please report bugs to:\cr
\url{https://github.com/OskarHansson/strvalidator/issues}\cr\cr

The source code and collaborative community is hosted at GitHub:\cr
\url{https://github.com/OskarHansson/strvalidator}\cr\cr
}
\examples{
# To start the graphical user interface.
\dontrun{
strvalidator()
}
}
\references{
Recommended Minimum Criteria for the Validation of Various Aspects of the DNA Profiling Process
\url{http://www.enfsi.eu/documents/minimum-validation-guidelines-dna-profiling-v2010}
Validation Guidelines for Forensic DNA Analysis Methods (2012)
\url{http://swgdam.org/SWGDAM_Validation_Guidelines_APPROVED_Dec_2012.pdf}
}

