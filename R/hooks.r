.onLoad <- function(libname, pkgname){
  # Do whatever needs to be done when the package is loaded.
  #packageStartupMessage(paste("Loading strvalidator",utils::packageVersion("strvalidator")))
  #packageStartupMessage("Ignore the warning about gtable::gtable.")
  #packageStartupMessage("strvalidator must load both gWidgets::gtable and gtable::gtable." )
}

.onAttach <- function(libname, pkgname){
  # Do whatever needs to be done when the package is loaded.
  packageStartupMessage(paste("STR-validator",utils::packageVersion("strvalidator"), "loaded!"))
}