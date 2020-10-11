.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Authors Nandan Patil and Lakshmi Gangavati")
}

.onLoad <- function(libname, pkgname) {
  options(photobiology.verbose = getOption("verbose"))
}
