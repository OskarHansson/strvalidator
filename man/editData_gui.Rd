\name{editData_gui}
\alias{editData_gui}
\title{Edit or view data frames}
\usage{
editData_gui(env = parent.frame(), data = NULL, name = NULL,
  edit = TRUE, debug = FALSE)
}
\arguments{
  \item{env}{environment in wich to search for data
  frames.}

  \item{data}{data.frame for instant viewing.}

  \item{name}{character string with the name of the
  provided dataset.}

  \item{edit}{logical TRUE for enable edit .}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
TRUE
}
\description{
\code{editData_gui} is a GUI simplifying editing and viewing of data frames.
}
\details{
Select a data frame from the dropdown and view/edit. Optionally
save as a new dataframe.
}
\seealso{
\code{\link{trim_gui}}, \code{\link{cropData_gui}}, \code{\link{combine_gui}}
}

