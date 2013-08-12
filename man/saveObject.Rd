\name{saveObject}
\alias{saveObject}
\title{Save object}
\usage{
  saveObject(name, object, parent = NULL,
    env = parent.frame())
}
\arguments{
  \item{name}{character giving the name of the object.}

  \item{object}{object to save.}

  \item{parent}{object specifying the parent GUI object to
  center the message box.}

  \item{env}{environment in wich to save and search for
  existing objects.}
}
\value{
  logical TRUE if object was saved FALSE if not.
}
\description{
  Saves an object in the specified environment.
}
\details{
  Saves an object with the given name in the specified
  environment if it does not exist. If the object exist a
  message box ask if the object should be overwritten.
}

